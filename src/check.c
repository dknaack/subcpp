static decl *
upsert_decl(decl **p, str key, arena *perm)
{
	for (u64 h = hash(key); *p; h <<= 2) {
		if (str_equals(key, (*p)->name)) {
			return *p;
		}

		p = &(*p)->child[h >> 62];
	}

	if (!perm) {
		return NULL;
	}

	*p = ALLOC(perm, 1, decl);
	(*p)->name = key;
	return *p;
}

static void
add_variable(scope *scope, str name, type *type, arena *arena)
{
	// TODO: report error when decl is already in scope
	decl *d = upsert_decl(&scope->head, name, arena);
	if (!d->type) {
		d->type = type;
		if (scope->tail) {
			scope->tail->next = d;
		}

		scope->tail = d;
	}
}

static type *
get_variable(scope *scope, str name)
{
	while (scope) {
		decl *d = upsert_decl(&scope->head, name, NULL);
		if (d) {
			return d->type;
		}

		scope = scope->parent;
	}

	return TYPE_NIL;
}

static void
push_scope(scope *s, arena *arena)
{
	scope *orig = ALLOC(arena, 1, scope);
	*orig = *s;
	s->parent = orig;
	s->head = s->tail = NULL;
}

static void
pop_scope(scope *s)
{
	b32 error = s->error;
	*s = *s->parent;
	s->error |= error;
}

static b32
type_equals(type *lhs, type *rhs)
{
	decl *l, *r;
	if (lhs->kind == TYPE_VOID || rhs->kind == TYPE_VOID) {
		return false;
	}

	if (lhs->kind != rhs->kind) {
		return false;
	}

	switch (lhs->kind) {
	case TYPE_FUNCTION:
		l = lhs->members;
		r = rhs->members;
		while (l && r) {
			if (!type_equals(l->type, r->type)) {
				return false;
			}

			l = l->next;
			r = r->next;
		}

		b32 result = (!l && !r);
		return result;
	default:
		return true;
	}
}

static b32
is_integer(type_kind type)
{
	switch (type) {
	case TYPE_CHAR:
	case TYPE_CHAR_UNSIGNED:
	case TYPE_SHORT:
	case TYPE_SHORT_UNSIGNED:
	case TYPE_INT:
	case TYPE_INT_UNSIGNED:
	case TYPE_LONG:
	case TYPE_LONG_UNSIGNED:
	case TYPE_LLONG:
	case TYPE_LLONG_UNSIGNED:
		return true;
	default:
		return false;
	}
}

static b32
is_pointer(type *type)
{
	b32 result = type->kind == TYPE_POINTER || type->kind == TYPE_ARRAY;
	return result;
}

static void
check_type(ast_node *node, scope *scope, arena *arena)
{
	if (!node) {
		return;
	}

	switch (node->kind) {
	case AST_INVALID:
		scope->error = true;
		break;
	case AST_ROOT:
		{
			for (ast_node *child = node->children; child != AST_NIL; child = child->next) {
				check_type(child, scope, arena);
			}
		} break;
	case AST_EXPR_BINARY:
		{
			ast_node *lhs = node->children;
			ast_node *rhs = lhs->next;
			check_type(lhs, scope, arena);

			u32 operator = node->value.i;
			if (operator == TOKEN_DOT) {
				if (lhs->type->kind != TYPE_STRUCT) {
					errorf(node->loc, "Left-hand side is not a struct");
					scope->error = true;
				}

				if (rhs->kind != AST_EXPR_IDENT) {
					errorf(node->loc, "Right-hand side is not an identifier");
					scope->error = true;
				}

				decl *s = upsert_decl(&lhs->type->members, rhs->value.s, NULL);
				rhs->type = s->type;
				node->type = s->type;
			} else if (operator == TOKEN_LBRACKET) {
				check_type(rhs, scope, arena);

				// NOTE: ensure that one operand is a pointer and the other one
				// is an integral type.
				if (is_pointer(lhs->type)) {
					node->type = lhs->type->children;
				} else if (is_pointer(rhs->type)) {
					node->type = rhs->type->children;
				} else {
					node->type = TYPE_NIL;
					scope->error = true;
					errorf(node->loc, "Incompatible types: %s, %s",
						type_get_name(lhs->type->kind),
						type_get_name(rhs->type->kind));
				}
			} else {
				check_type(rhs, scope, arena);

				// Apply integer promotion
				b32 same_sign = (lhs->type->kind & TYPE_UNSIGNED) == (rhs->type->kind & TYPE_UNSIGNED);
				if (lhs->type->kind == rhs->type->kind) {
					// do nothing
				} else if (same_sign) {
					if (lhs->type->kind < rhs->type->kind) {
						lhs->type->kind = rhs->type->kind;
					}
				} else {
					type *unsigned_type = (lhs->type->kind & TYPE_UNSIGNED ? lhs->type : rhs->type);
					type *signed_type = (lhs->type->kind & TYPE_UNSIGNED ? rhs->type : lhs->type);
					if (unsigned_type->kind > signed_type->kind) {
						signed_type->kind = unsigned_type->kind;
					} else {
						signed_type->kind |= TYPE_UNSIGNED;
						unsigned_type->kind = signed_type->kind;
					}
				}

				node->type = lhs->type;
			}
		} break;
	case AST_EXPR_CALL:
		{
			ast_node *called = node->children;
			check_type(called, scope, arena);
			if (called->type->kind != TYPE_FUNCTION) {
				scope->error = true;
				errorf(node->loc, "Not a function: %s", type_get_name(called->type->kind));
				break;
			}

			u32 param_index = 0;
			ast_node *param = called->next;
			type *return_type = called->type->children;
			decl *param_sym = called->type->members;
			while (param != AST_NIL || param_sym != NULL) {
				check_type(param, scope, arena);
				if (!type_equals(param_sym->type, param->type)) {
					errorf(node->loc, "Parameter %d has wrong type: Expected %s, but found %s",
						param_index + 1, type_get_name(param_sym->type->kind),
						type_get_name(param->type->kind));
					scope->error = true;
				}

				param_sym = param_sym->next;
				param = param->next;
				param_index++;
			}

			node->type = return_type;
		} break;
	case AST_EXPR_IDENT:
		{
			node->type = get_variable(scope, node->value.s);
			if (node->type == TYPE_NIL) {
				scope->error = true;
				errorf(node->loc, "Variable '%.*s' was never defined",
					(int)node->value.s.length, node->value.s.at);
			}
		} break;
	case AST_EXPR_INT:
		{
			node->type = &type_int;
		} break;
	case AST_EXPR_POSTFIX:
	case AST_EXPR_UNARY:
		{
			ast_node *operand = node->children;
			check_type(operand, scope, arena);
			switch (node->value.i) {
			case TOKEN_STAR:
				if (operand->type->kind == TYPE_POINTER) {
					node->type = operand->type->children;
				} else {
					node->type = TYPE_NIL;
					scope->error = true;
					errorf(node->loc, "Expected pointer type");
				}
				break;
			case TOKEN_AMP:
				node->type = type_create(TYPE_POINTER, arena);
				node->type->children = operand->type;
				break;
			case TOKEN_BANG:
			case TOKEN_PLUS:
			case TOKEN_MINUS:
				// TODO: ensure that type is integer
				node->type = operand->type;
				break;
			case TOKEN_PLUS_PLUS:
			case TOKEN_MINUS_MINUS:
				// TODO: ensure that type is integer or pointer
				node->type = operand->type;
				break;
			default:
				ASSERT(!"Invalid operator");
				break;
			}
		} break;
	case AST_STMT_BREAK:
		break;
	case AST_STMT_COMPOUND:
		{
			push_scope(scope, arena);
			for (ast_node *child = node->children; child != AST_NIL; child = child->next) {
				check_type(child, scope, arena);
			}

			pop_scope(scope);
		} break;
	case AST_DECL:
		{
			ast_node *type_specifier = node->children;
			check_type(type_specifier, scope, arena);

			for (ast_node *child = type_specifier->next; child != AST_NIL; child = child->next) {
				ast_node *declarator = child;
				ast_node *expr = AST_NIL;

				type *decl_type = type_specifier->type;
				str name = {0};
				while (declarator != AST_NIL) {
					switch (declarator->kind) {
					case AST_DECL_IDENT:
						{
							name = declarator->value.s;
							declarator->type = decl_type;
						} break;
					case AST_DECL_POINTER:
						{
							type *target = decl_type;
							// TODO: add qualifiers to the pointer type
							decl_type = type_create(TYPE_POINTER, arena);
							decl_type->children = target;
						} break;
					case AST_DECL_ARRAY:
						{
							type *target = decl_type;
							decl_type = type_create(TYPE_ARRAY, arena);
							decl_type->children = target;

							// TODO: Evaluate the array size of the declarator
							decl_type->size = 1;
							ast_node *size = declarator->children->next;
							if (size->kind == AST_EXPR_INT) {
								decl_type->size = size->value.i;
							}
						} break;
					case AST_DECL_FUNC:
						{
							type *target = decl_type;
							decl_type = type_create(TYPE_FUNCTION, arena);
							decl_type->children = target;

							push_scope(scope, arena);
							ast_node *param = declarator->children->next;
							while (param != AST_NIL) {
								check_type(param, scope, arena);
								param = param->next;
							}

							decl_type->members = scope->head;
							pop_scope(scope);
						} break;
					case AST_DECL_INIT:
						{
							// TODO: check type of expression
							expr = declarator->children->next;
						} break;
					default:
						// TODO: report syntax error
						ASSERT(!"Invalid node in declarator");
						break;
					}

					declarator = declarator->children;
				}

				ASSERT(name.at);
				add_variable(scope, name, decl_type, arena);
				// TODO: This should be removed when function declarators are
				// parsed correctly.
				// NOTE: Required for function declarators
				if (!node->type) {
					node->type = decl_type;
				}

				if (expr != AST_NIL) {
					check_type(expr, scope, arena);
				}
			}
		} break;
	case AST_DECL_INIT:
	case AST_DECL_POINTER:
	case AST_DECL_ARRAY:
	case AST_DECL_IDENT:
	case AST_DECL_FUNC:
		{
			// TODO: report syntax error when declarator is encountered here.
			ASSERT(!"Should be handled in decl");
		} break;
	case AST_STMT_CONTINUE:
	case AST_STMT_EMPTY:
		break;
	case AST_STMT_FOR:
	case AST_STMT_IF:
	case AST_STMT_DO_WHILE:
	case AST_STMT_WHILE:
		{
			for (ast_node *child = node->children; child != AST_NIL; child = child->next) {
				check_type(child, scope, arena);
			}
		} break;
	case AST_STMT_PRINT:
		{
			check_type(node->children, scope, arena);
		} break;
	case AST_STMT_RETURN:
		{
			check_type(node->children, scope, arena);
		} break;
	case AST_FUNCTION:
		{
			ast_node *decl = node->children;
			ast_node *body = decl->next;
			check_type(decl, scope, arena);
			node->type = scope->tail->type;
			node->value.s = scope->tail->name;
			push_scope(scope, arena);

			type *func_type = decl->type;
			for (struct decl *param = func_type->members; param; param = param->next) {
				add_variable(scope, param->name, param->type, arena);
			}

			check_type(body, scope, arena);
			pop_scope(scope);
		} break;
	case AST_TYPE_VOID:
		{
			node->type = &type_void;
		} break;
	case AST_TYPE_CHAR:
		{
			if (node->flags & AST_UNSIGNED) {
				node->type = &type_char_unsigned;
			} else {
				node->type = &type_char;
			}
		} break;
	case AST_TYPE_FLOAT:
		{
			node->type = &type_float;
		} break;
	case AST_TYPE_INT:
		{
			u32 int_flags = AST_LONG | AST_LLONG | AST_SHORT | AST_UNSIGNED;
			switch (node->flags & int_flags) {
			case AST_LLONG | AST_UNSIGNED:
				node->type = &type_llong_unsigned;
				break;
			case AST_LLONG:
				node->type = &type_llong;
				break;
			case AST_LONG | AST_UNSIGNED:
				node->type = &type_long_unsigned;
				break;
			case AST_LONG:
				node->type = &type_long;
				break;
			case AST_SHORT | AST_UNSIGNED:
				node->type = &type_short_unsigned;
				break;
			case AST_SHORT:
				node->type = &type_short;
				break;
			case AST_UNSIGNED:
				node->type = &type_int_unsigned;
				break;
			default:
				node->type = &type_int;
			}
		} break;
	case AST_TYPE_STRUCT:
		{
			// TODO
		} break;
	case AST_TYPE_STRUCT_DEF:
		{
			// TODO: add the struct tag to the scope and ensure that the
			// struct is only defined once.
			node->type = type_create(TYPE_STRUCT, arena);

			push_scope(scope, arena);
			for (ast_node *child = node->children; child != AST_NIL; child = child->next) {
				check_type(child, scope, arena);
			}

			node->type->members = scope->head;
			pop_scope(scope);
		} break;
	}
}
