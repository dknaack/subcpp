typedef struct scope_entry scope_entry;
struct scope_entry {
	str key;
	ast_id value;
	b8 is_type;

	i32 depth;
	scope_entry *child[4];
};

typedef struct {
	scope_entry *idents;
	scope_entry *tags;
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

static ast_id *
scope_upsert(scope *s, str key, arena *perm)
{
	scope_entry **m = &s->idents;
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
merge_identifiers(ast_pool *pool, ast_id node_id, scope *s, arena *perm, b32 *error)
{
	scope *orig = NULL;
	arena temp = {0};

	ast_node *node = ast_get(pool, node_id);
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

	for (;;) {
		for (i32 i = 0; i < 2; i++) {
			if (node->child[i].value == 0) {
				continue;
			}

			ast_node *child = ast_get(pool, node->child[i]);
			if ((child->kind == AST_DECL || child->kind == AST_EXTERN_DEF)
				&& (child->symbol_id.value == 0)) {
				ASSERT(child->symbol_id.value == 0);
				child->symbol_id.value = (*s->count)++;
				ASSERT(child->symbol_id.value != 0);
				*scope_upsert(s, child->value.s, perm) = node->child[i];
			} else if (child->kind == AST_EXPR_IDENT) {
				ast_id *resolved = scope_upsert(s, child->value.s, NULL);
				if (resolved) {
					ASSERT(ast_get(pool, *resolved)->symbol_id.value != 0);
					node->child[i] = *resolved;
				} else {
					errorf(node->loc, "Variable was never declared");
					*error = true;
				}
			}

		}

		if (node->child[0].value != 0) {
			merge_identifiers(pool, node->child[0], s, perm, error);
		}

		if (node->child[1].value == 0) {
			break;
		}

		node = ast_get(pool, node->child[1]);
	}
}

static void
check_type(ast_pool *pool, ast_id node_id, arena *arena, b32 *error)
{
	ast_node *node = ast_get(pool, node_id);
	if ((node->kind == AST_INIT_LIST) == (node->type == NULL)) {
		return;
	}

	if (node->child[0].value != 0) {
		check_type(pool, node->child[0], arena, error);
	}

	if (node->child[1].value != 0) {
		check_type(pool, node->child[1], arena, error);
	}

	switch (node->kind) {
	case AST_INVALID:
		*error = true;
		break;
	case AST_EXPR_LIST:
	case AST_STMT_BREAK:
	case AST_STMT_LIST:
	case AST_DECL_LIST:
	case AST_STMT_CASE:
	case AST_STMT_CONTINUE:
	case AST_STMT_DEFAULT:
	case AST_STMT_DO_WHILE:
	case AST_STMT_EMPTY:
	case AST_STMT_FOR_COND:
	case AST_STMT_FOR_POST:
	case AST_STMT_GOTO:
	case AST_STMT_IF_COND:
	case AST_STMT_IF_ELSE:
	case AST_STMT_LABEL:
	case AST_STMT_PRINT:
	case AST_STMT_RETURN:
	case AST_STMT_SWITCH:
	case AST_STMT_WHILE:
		// NOTE: Types are already checked above
		break;
	case AST_STMT_FOR_INIT:
		{
			ast_node *cond = ast_get(pool, node->child[1]);
			ast_node *post = ast_get(pool, cond->child[1]);
			ast_node *body = ast_get(pool, post->child[1]);
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
					ast_node *value = ast_get(pool, node->child[0]);
					if (value->kind == AST_INIT_LIST) {
						value->type = member->type;
					}

					check_type(pool, node->child[0], arena, error);
					// TODO: Type conversion
					if (!type_equals(member->type, value->type)) {
						//errorf(node->loc, "Invalid type");
					}

					member = member->next;
					node = ast_get(pool, node->child[1]);
				}

				if (!member && node != AST_NIL) {
					errorf(node->loc, "Too many fields in the initializer");
				}
			} else if (node->type->kind == TYPE_ARRAY) {
				type *ty = node->type;
				while (node != AST_NIL) {
					check_type(pool, node->child[0], arena, error);
					if (!type_equals(ast_get(pool, node->child[0])->type, ty->children)) {
						errorf(node->loc, "Invalid array member type");
					}

					node = ast_get(pool, node->child[1]);
				}
			}
		} break;
	case AST_EXPR_MEMBER:
		{
			ast_node *operand = ast_get(pool, node->child[0]);
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
			ast_node *lhs = ast_get(pool, node->child[0]);
			ast_node *rhs = ast_get(pool, node->child[1]);
			node->type = lhs->type;

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
			} else if (is_integer(lhs->type->kind) && is_integer(rhs->type->kind)) {
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
			}
		} break;
	case AST_EXPR_CALL:
		{
			ast_node *called = ast_get(pool, node->child[0]);
			if (called->type->kind != TYPE_FUNCTION) {
				*error = true;
				errorf(node->loc, "Not a function: %s", type_get_name(called->type->kind));
				break;
			}

			if (node->child[1].value != 0) {
				ast_node *param_list = ast_get(pool, node->child[1]);
				member *param_member = called->type->members;
				for (;;) {
					ast_node *param = ast_get(pool, param_list->child[0]);
					if (!type_equals(param_member->type, param->type)) {
						errorf(param->loc, "Invalid parameter type");
					}

					if (param_list->child[1].value == 0) {
						break;
					}

					param_list = ast_get(pool, param_list->child[1]);
				}
			}

			type *return_type = called->type->children;
			node->type = return_type;
		} break;
	case AST_EXPR_IDENT:
		{
			ASSERT(!"This node should have been eliminated by merge_identifiers");
		} break;
	case AST_EXPR_FLOAT:
		{
			node->type = &type_double;
		} break;
	case AST_EXPR_INT:
		{
			node->type = &type_int;
		} break;
	case AST_EXPR_POSTFIX:
	case AST_EXPR_UNARY:
		{
			ast_node *operand = ast_get(pool, node->child[0]);
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
			node->type = ast_get(pool, node->child[0])->type;
			ASSERT(node->type);
			if (node->child[1].value != 0) {
				ast_node *initializer = ast_get(pool, node->child[1]);
				if (initializer->kind == AST_INIT_LIST) {
					initializer->type = node->type;
					check_type(pool, node->child[1], arena, error);
				} else {
					// TODO: Check type of the expression
				}
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
			ast_node *type = ast_get(pool, node->child[1]);
			node->type = type_create(TYPE_POINTER, arena);
			node->type->children = type->type;
		} break;
	case AST_TYPE_ARRAY:
		{
			ast_node *type = ast_get(pool, node->child[1]);
			node->type = type_create(TYPE_ARRAY, arena);
			// TODO: Evaluate the size of the array
			node->type->size = 1;
			node->type->children = type->type;
		} break;
	case AST_TYPE_BITFIELD:
		{
			ast_node *type = ast_get(pool, node->child[1]);
			node->type = type_create(TYPE_BITFIELD, arena);
			// TODO: Evaluate the expression
			node->type->size = 1;
			node->type->children = type->type;
		} break;
	case AST_TYPE_FUNC:
		{
			node->type = type_create(TYPE_FUNCTION, arena);
			member **m = &node->type->members;
			ast_node *return_type = ast_get(pool, node->child[1]);

			if (node->child[0].value != 0) {
				ast_node *param_list = ast_get(pool, node->child[0]);
				for (;;) {
					ast_node *param = ast_get(pool, param_list->child[0]);
					*m = ALLOC(arena, 1, member);
					(*m)->name = param->value.s;
					(*m)->type = param->type;
					m = &(*m)->next;

					if (param_list->child[1].value == 0) {
						break;
					}

					param_list = ast_get(pool, param_list->child[1]);
				}
			}

			node->type->children = return_type->type;
		} break;
	case AST_TYPE_ENUM:
		{
			ASSERT(!"TODO");
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
			member **ptr = &node->type->members;
			ast_node *decls = ast_get(pool, node->child[0]);
			ASSERT(decls->kind == AST_DECL_LIST);
			while (decls != AST_NIL) {
				*ptr = ALLOC(arena, 1, member);
				(*ptr)->name = ast_get(pool, decls->child[0])->value.s;
				(*ptr)->type = ast_get(pool, decls->child[0])->type;
				ptr = &(*ptr)->next;
				decls = ast_get(pool, decls->child[1]);
			}
		} break;
	}
}

static void
add_global_symbols(ast_pool *pool, ast_id node_id, symbol_table symtab)
{
	ast_node *node = ast_get(pool, node_id);
	for (i32 i = 0; i < 2; i++) {
		if (node->child[i].value != 0) {
			add_global_symbols(pool, node->child[i], symtab);
		}
	}

	if (node->kind == AST_EXTERN_DEF) {
		ASSERT(node->symbol_id.value != 0);
		symbol *sym = &symtab.symbols[node->symbol_id.value];
		sym->name = node->value.s;
		sym->type = node->type;
		ASSERT(node->type);

		sym->linkage = LINK_DEFAULT;
		if (node->flags & AST_EXTERN) {
			sym->linkage = LINK_EXTERN;
		} else if (node->flags & AST_STATIC) {
			sym->linkage = LINK_STATIC;
		}

		sym->is_global = true;
		sym->is_function = (node->type->kind == TYPE_FUNCTION);
		sym->definition = node->child[1];
	}
}

static symbol_table
analyze(ast_pool *pool, arena *perm, b32 *error)
{
	symbol_table symtab = {0};
	scope *s = new_scope(NULL, perm);
	s->count = &symtab.count;
	// NOTE: Reserve the first symbol as a NIL symbol
	symtab.count++;

	merge_identifiers(pool, pool->root, s, perm, error);
	symtab.symbols = ALLOC(perm, symtab.count, symbol);
	if (!*error) {
		check_type(pool, pool->root, perm, error);
		add_global_symbols(pool, pool->root, symtab);
	}

	return symtab;
}
