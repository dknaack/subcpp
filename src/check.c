static symbol *
upsert_symbol(symbol **p, str key, arena *perm)
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

	*p = ALLOC(perm, 1, symbol);
	(*p)->name = key;
	return *p;
}

static void
add_variable(symbol_table *table, str name, type *type, arena *arena)
{
	// TODO: report error when symbol is already in scope
	symbol *s = upsert_symbol(&table->head, name, arena);
	if (!s->type) {
		s->type = type;
		if (table->tail) {
			table->tail->next = s;
		}

		table->tail = s;
	}
}

static type *
get_variable(symbol_table *table, str name)
{
	while (table) {
		symbol *s = upsert_symbol(&table->head, name, NULL);
		if (s) {
			return s->type;
		}

		table = table->parent;
	}

	return TYPE_NIL;
}

static void
push_scope(symbol_table *table, arena *arena)
{
	symbol_table *parent = ALLOC(arena, 1, symbol_table);
	*parent = *table;
	table->parent = parent;
	table->head = table->tail = NULL;
}

static void
pop_scope(symbol_table *table)
{
	b32 error = table->error;
	*table = *table->parent;
	table->error |= error;
}

static b32
type_equals(type *lhs, type *rhs)
{
	symbol *l, *r;
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
is_pointer(type *type)
{
	b32 result = type->kind == TYPE_POINTER
		|| type->kind == TYPE_ARRAY;
	return result;
}

static void
check_type(ast_node *node, symbol_table *symbols, arena *arena)
{
	if (!node) {
		return;
	}

	switch (node->kind) {
	case AST_INVALID:
		symbols->error = true;
		break;
	case AST_ROOT:
		{
			for (ast_node *child = node->children; child != AST_NIL; child = child->next) {
				check_type(child, symbols, arena);
			}
		} break;
	case AST_EXPR_BINARY:
		{
			ast_node *lhs = node->children;
			ast_node *rhs = lhs->next;
			check_type(lhs, symbols, arena);

			u32 operator = node->value.i;
			if (operator == TOKEN_DOT) {
				if (lhs->type->kind != TYPE_STRUCT) {
					errorf(node->loc, "Left-hand side is not a struct");
					symbols->error = true;
				}

				if (rhs->kind != AST_EXPR_IDENT) {
					errorf(node->loc, "Right-hand side is not an identifier");
					symbols->error = true;
				}

				symbol *s = upsert_symbol(&lhs->type->members, rhs->value.s, NULL);
				rhs->type = s->type;
				node->type = s->type;
			} else if (operator == TOKEN_LBRACKET) {
				check_type(rhs, symbols, arena);

				// NOTE: ensure that one operand is a pointer and the other one
				// is an integral type.
				if (is_pointer(lhs->type)) {
					node->type = lhs->type->children;
				} else if (is_pointer(rhs->type)) {
					node->type = rhs->type->children;
				} else {
					node->type = TYPE_NIL;
					symbols->error = true;
					errorf(node->loc, "Incompatible types: %s, %s",
						type_get_name(lhs->type->kind),
						type_get_name(rhs->type->kind));
				}
			} else {
				check_type(rhs, symbols, arena);

				// TODO: type coercion
				if (!type_equals(lhs->type, rhs->type)) {
					symbols->error = true;
					errorf(node->loc, "Incompatible types: %s, %s",
						type_get_name(lhs->type->kind),
						type_get_name(rhs->type->kind));
				}

				node->type = lhs->type;
			}
		} break;
	case AST_EXPR_CALL:
		{
			ast_node *called = node->children;
			check_type(called, symbols, arena);
			if (called->type->kind != TYPE_FUNCTION) {
				symbols->error = true;
				errorf(node->loc, "Not a function: %s", type_get_name(called->type->kind));
				break;
			}

			u32 param_index = 0;
			ast_node *param = called->next;
			type *return_type = called->type->children;
			symbol *param_sym = called->type->members;
			while (param != AST_NIL || param_sym != NULL) {
				check_type(param, symbols, arena);
				if (!type_equals(param_sym->type, param->type)) {
					errorf(node->loc, "Parameter %d has wrong type: Expected %s, but found %s",
						param_index + 1, type_get_name(param_sym->type->kind),
						type_get_name(param->type->kind));
					symbols->error = true;
				}

				param_sym = param_sym->next;
				param = param->next;
				param_index++;
			}

			node->type = return_type;
		} break;
	case AST_EXPR_IDENT:
		{
			node->type = get_variable(symbols, node->value.s);
			if (node->type == TYPE_NIL) {
				symbols->error = true;
				errorf(node->loc, "Variable '%.*s' was never defined",
					(int)node->value.s.length, node->value.s.at);
			}
		} break;
	case AST_EXPR_POSTFIX:
	case AST_EXPR_UNARY:
		{
			ast_node *operand = node->children;
			check_type(operand, symbols, arena);
			switch (node->value.i) {
			case TOKEN_STAR:
				if (operand->type->kind == TYPE_POINTER) {
					node->type = operand->type->children;
				} else {
					node->type = TYPE_NIL;
					symbols->error = true;
					errorf(node->loc, "Expected pointer type");
				}
				break;
			case TOKEN_AMP:
				node->type = type_create(TYPE_POINTER, arena);
				node->type->children = operand->type;
				break;
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
			push_scope(symbols, arena);
			for (ast_node *child = node->children; child != AST_NIL; child = child->next) {
				check_type(child, symbols, arena);
			}

			pop_scope(symbols);
		} break;
	case AST_DECL:
		{
			ast_node *type_specifier = node->children;
			check_type(type_specifier, symbols, arena);

			for (ast_node *child = type_specifier->next; child != AST_NIL; child = child->next) {
				ast_node *declarator = child;
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

							push_scope(symbols, arena);
							ast_node *param = declarator->children->next;
							while (param != AST_NIL) {
								check_type(param, symbols, arena);
								param = param->next;
							}

							decl_type->members = symbols->head;
							pop_scope(symbols);
						} break;
					case AST_DECL_INIT:
						{
							// TODO: check type of expression
						} break;
					default:
						// TODO: report syntax error
						ASSERT(!"Invalid node in declarator");
						break;
					}

					declarator = declarator->children;
				}

				ASSERT(name.at);
				add_variable(symbols, name, decl_type, arena);
				// TODO: This should be removed when function declarators are
				// parsed correctly.
				// NOTE: Required for function declarators
				if (!node->type) {
					node->type = decl_type;
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
				check_type(child, symbols, arena);
			}
		} break;
	case AST_STMT_PRINT:
		{
			check_type(node->children, symbols, arena);
		} break;
	case AST_STMT_RETURN:
		{
			check_type(node->children, symbols, arena);
		} break;
	case AST_FUNCTION:
		{
			ast_node *decl = node->children;
			ast_node *body = decl->next;
			check_type(decl, symbols, arena);
			node->type = symbols->tail->type;
			node->value.s = symbols->tail->name;
			push_scope(symbols, arena);

			type *func_type = decl->type;
			for (symbol *param = func_type->members; param; param = param->next) {
				add_variable(symbols, param->name, param->type, arena);
			}

			check_type(body, symbols, arena);
			pop_scope(symbols);
		} break;
	case AST_TYPE_VOID:
		{
			node->type = &type_void;
		} break;
	case AST_TYPE_CHAR:
		{
			node->type = &type_char;
		} break;
	case AST_TYPE_INT:
	case AST_EXPR_INT:
		{
			node->type = &type_int;
		} break;
	case AST_TYPE_STRUCT:
		{
			// TODO
		} break;
	case AST_TYPE_STRUCT_DEF:
		{
			// TODO: add the struct tag to the symbols and ensure that the
			// struct is only defined once.
			node->type = type_create(TYPE_STRUCT, arena);

			push_scope(symbols, arena);
			for (ast_node *child = node->children; child != AST_NIL; child = child->next) {
				check_type(child, symbols, arena);
			}

			node->type->members = symbols->head;
			pop_scope(symbols);
		} break;
	}
}
