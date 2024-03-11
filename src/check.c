typedef struct scope_entry scope_entry;
struct scope_entry {
	str key;
	ast_node *value;

	i64 depth;
	scope_entry *child[4];
};

typedef struct {
	scope_entry *root;
	isize depth;
	isize *count;
} scope;

static scope *
new_scope(scope *parent, arena *perm)
{
	scope *s = ALLOC(perm, 1, scope);
	if (parent) {
		memcpy(s, parent, sizeof(*s));
		s->depth++;
	}

	return s;
}

static ast_node **
scope_upsert(scope *s, str key, arena *perm)
{
	scope_entry **m = &s->root;
	isize depth = s->depth;

	for (u64 h = hash(key); *m; h <<= 2) {
		// TODO: This will not work if we insert a node with different depth
		if (str_equals((*m)->key, key)) {
			return &(*m)->value;
		}

		if ((*m)->depth != depth) {
			scope_entry *tmp = ALLOC(perm, 1, scope_entry);
			*m = memcpy(tmp, *m, sizeof(**m));
			(*m)->depth = depth;
		}

		m = &(*m)->child[h >> 62];
	}

	if (!perm) {
		return NULL;
	}

	*m = ALLOC(perm, 1, scope_entry);
	(*m)->key = key;
	(*m)->depth = depth;
	return &(*m)->value;
}

static b32
type_equals(type *lhs, type *rhs)
{
	member *l, *r;
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
merge_identifiers(ast_node *node, scope *s, arena *perm, b32 *error)
{
	scope *orig = NULL;
	arena temp = {0};

	if (*error || node->kind == AST_TYPE_STRUCT_DEF) {
		return;
	}

	// TODO: Ensure that this works for function declarators but also function
	// definitions. For function declarations the parameters should be only
	// accessible from the parameters themselves. In a definition, the
	// parameters should be accessible from within the function.
	if (node->kind == AST_STMT_LIST || node->kind == AST_STMT_FOR_INIT) {
		orig = s;
		temp = *perm;
		perm = &temp;
		s = new_scope(orig, perm);
	}

	while (node != AST_NIL) {
		for (i32 i = 0; i < 2; i++) {
			if ((node->child[i]->kind == AST_DECL || node->child[i]->kind == AST_EXTERN_DEF)
				&& (node->child[i]->symbol_id.value == 0)) {
				ASSERT(node->child[i]->symbol_id.value == 0);
				node->child[i]->symbol_id.value = (*s->count)++;
				*scope_upsert(s, node->child[i]->value.s, perm) = node->child[i];
			} else if (node->child[i]->kind == AST_EXPR_IDENT) {
				ast_node **resolved = scope_upsert(s, node->child[i]->value.s, NULL);
				if (resolved) {
					node->child[i] = *resolved;
				} else {
					errorf(node->loc, "Variable was never declared");
					*error = true;
				}
			}

		}

		merge_identifiers(node->child[0], s, perm, error);
		node = node->child[1];
	}
}

static void
check_type(ast_node *node, arena *arena, b32 *error)
{
	if (node == AST_NIL) {
		return;
	} else if (node->kind == AST_INIT_LIST) {
		if (!node->type) {
			return;
		}
	} else {
		if (node->type) {
			return;
		}

		check_type(node->child[0], arena, error);
		check_type(node->child[1], arena, error);
	}

	switch (node->kind) {
	case AST_INVALID:
		*error = true;
		break;
	case AST_EXPR_LIST:
	case AST_STMT_BREAK:
	case AST_STMT_LIST:
	case AST_DECL_LIST:
	case AST_STMT_CONTINUE:
	case AST_STMT_EMPTY:
	case AST_STMT_FOR_COND:
	case AST_STMT_FOR_POST:
	case AST_STMT_IF_COND:
	case AST_STMT_IF_ELSE:
	case AST_STMT_DO_WHILE:
	case AST_STMT_WHILE:
	case AST_STMT_PRINT:
	case AST_STMT_RETURN:
		// NOTE: Types are already checked above
		break;
	case AST_STMT_FOR_INIT:
		{
			ast_node *cond = node->child[1];
			ast_node *post = cond->child[1];
			ast_node *body = post->child[1];
			ASSERT(cond->kind == AST_STMT_FOR_COND);
			ASSERT(post->kind == AST_STMT_FOR_POST);
			ASSERT(is_statement(body->kind));
		} break;
	case AST_INIT_LIST:
		{
			ASSERT(node->type->kind == TYPE_STRUCT || node->type->kind == TYPE_ARRAY);
			member *member = node->type->members;

			// TODO: Check the type of each field in the initializer
			if (node->type->kind == TYPE_STRUCT) {
				while (member && node != AST_NIL) {
					if (node->child[0]->kind == AST_INIT_LIST) {
						node->child[0]->type = member->type;
					}

					check_type(node->child[0], arena, error);
					// TODO: Type conversion
					if (!type_equals(member->type, node->child[0]->type)) {
						//errorf(node->loc, "Invalid type");
					}

					member = member->next;
					node = node->child[1];
				}

				if (!member && node != AST_NIL) {
					errorf(node->loc, "Too many fields in the initializer");
				}
			} else if (node->type->kind == TYPE_ARRAY) {
				type *ty = node->type;
				while (node != AST_NIL) {
					check_type(node->child[0], arena, error);
					if (!type_equals(node->child[0]->type, ty->children)) {
						errorf(node->loc, "Invalid array member type");
					}

					node = node->child[1];
				}
			}
		} break;
	case AST_EXPR_MEMBER:
		{
			ast_node *operand = node->child[0];
			if (operand->type->kind != TYPE_STRUCT) {
				errorf(node->loc, "Left-hand side is not a struct");
				*error = true;
			}

			member *s = get_member(operand->type->members, node->value.s);
			if (s) {
				node->type = s->type;
			} else {
				errorf(node->loc, "Member does not exist");
			}
		} break;
	case AST_EXPR_BINARY:
		{
			ast_node *lhs = node->child[0];
			ast_node *rhs = node->child[1];

			u32 operator = node->value.i;
			if (operator == TOKEN_LBRACKET) {

				// NOTE: ensure that one operand is a pointer and the other one
				// is an integral type.
				if (is_pointer(lhs->type)) {
					node->type = lhs->type->children;
				} else if (is_pointer(rhs->type)) {
					node->type = rhs->type->children;
				} else {
					node->type = TYPE_NIL;
					*error = true;
					errorf(node->loc, "Incompatible types: %s, %s",
						type_get_name(lhs->type->kind),
						type_get_name(rhs->type->kind));
				}
			} else {
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
			ast_node *called = node->child[0];
			if (called->type->kind != TYPE_FUNCTION) {
				*error = true;
				errorf(node->loc, "Not a function: %s", type_get_name(called->type->kind));
				break;
			}

			u32 param_index = 0;
			ast_node *param = node->child[1];
			type *return_type = called->type->children;
			member *param_sym = called->type->members;
			while (param != AST_NIL || param_sym != NULL) {
				if (!type_equals(param_sym->type, param->child[0]->type)) {
					errorf(node->loc, "Parameter %d has wrong type: Expected %s, but found %s",
						param_index + 1, type_get_name(param_sym->type->kind),
						type_get_name(param->type->kind));
					*error = true;
				}

				param_sym = param_sym->next;
				param = param->child[1];
				param_index++;
			}

			node->type = return_type;
		} break;
	case AST_EXPR_IDENT:
		{
			ASSERT(!"This node should have been eliminated by merge_identifiers");
		} break;
	case AST_EXPR_INT:
		{
			node->type = &type_int;
		} break;
	case AST_EXPR_POSTFIX:
	case AST_EXPR_UNARY:
		{
			ast_node *operand = node->child[0];
			switch (node->value.i) {
			case TOKEN_STAR:
				if (operand->type->kind == TYPE_POINTER) {
					node->type = operand->type->children;
				} else {
					node->type = TYPE_NIL;
					*error = true;
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
	case AST_DECL:
	case AST_EXTERN_DEF:
		{
			node->type = node->child[0]->type;
			ast_node *initializer = node->child[1];
			if (initializer->kind == AST_INIT_LIST) {
				initializer->type = node->type;
				check_type(initializer, arena, error);
			} else {
				// TODO: Check type of the expression
			}
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
	case AST_TYPE_POINTER:
		{
			node->type = type_create(TYPE_POINTER, arena);
			node->type->children = node->child[0]->type;
		} break;
	case AST_TYPE_ARRAY:
		{
			node->type = type_create(TYPE_ARRAY, arena);
			// TODO: Evaluate the size of the array
			node->type->size = 1;
			node->type->children = node->child[1]->type;
		} break;
	case AST_TYPE_FUNC:
		{
			ast_node *param_list = node->child[0];
			ast_node *return_type = node->child[1];

			node->type = type_create(TYPE_FUNCTION, arena);
			member **m = &node->type->members;
			while (param_list != AST_NIL) {
				ast_node *param = param_list->child[0];
				*m = ALLOC(arena, 1, member);
				(*m)->name = param->value.s;
				(*m)->type = param->type;
				m = &(*m)->next;
				param_list = param_list->child[1];
			}

			node->type->children = return_type->type;
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

			// TODO: Collect the members of the struct
			//for (ast_node *child = node->children; child != AST_NIL; child = child->next) {}
		} break;
	}
}

static void
add_global_symbols(ast_node *node, symbol_table symtab)
{
	if (node == AST_NIL) {
		return;
	}

	add_global_symbols(node->child[0], symtab);
	add_global_symbols(node->child[1], symtab);

	if (node->kind == AST_EXTERN_DEF) {
		symbol *sym = &symtab.symbols[node->symbol_id.value];
		sym->name = node->value.s;
		sym->type = node->type;
		sym->definition = node->child[1];
		sym->is_global = true;
		sym->is_function = (sym->definition->kind == AST_STMT_LIST);
	}
}

static symbol_table
analyze(ast_node *root, arena *perm, b32 *error)
{
	symbol_table symtab = {0};
	scope *s = new_scope(NULL, perm);
	s->count = &symtab.count;

	merge_identifiers(root, s, perm, error);
	symtab.symbols = ALLOC(perm, symtab.count, symbol);
	check_type(root, perm, error);
	add_global_symbols(root, symtab);
	return symtab;
}
