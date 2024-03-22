typedef struct scope_entry scope_entry;
struct scope_entry {
	str key;
	ast_id value;
	b8 is_type;

	i32 depth;
	scope_entry *next;
};

typedef struct scope scope;
struct scope {
	scope *parent;
	scope_entry *idents;
	scope_entry *tags;
};

static scope
new_scope(scope *parent)
{
	scope s = {0};
	s.parent = parent;
	return s;
}

static ast_id *
scope_upsert_ident(scope *s, str key, arena *perm)
{
	if (!s) {
		return NULL;
	}

	scope_entry *e = NULL;
	for (e = s->idents; e; e = e->next) {
		if (str_equals(key, e->key)) {
			return &e->value;
		}
	}

	if (!perm) {
		return scope_upsert_ident(s->parent, key, NULL);
	}

	e = ALLOC(perm, 1, scope_entry);
	e->next = s->idents;
	e->key = key;
	s->idents = e;
	return &e->value;
}

static ast_id *
scope_upsert_tag(scope *s, str key, arena *perm)
{
	if (!s) {
		return NULL;
	}

	scope_entry *e = NULL;
	for (e = s->tags; e; e = e->next) {
		if (str_equals(key, e->key)) {
			return &e->value;
		}
	}

	if (!perm) {
		return scope_upsert_tag(s->parent, key, NULL);
	}

	e = ALLOC(perm, 1, scope_entry);
	e->next = s->idents;
	e->key = key;
	s->tags = e;
	return &e->value;
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
check_decls(ast_pool *pool, ast_id *node_id, scope *s, arena *perm, b32 *error)
{
	scope tmp = {0};

	ast_node *node = ast_get(pool, *node_id);
	if (*error) {
		return;
	}

	if (node->kind == AST_STMT_LIST || node->kind == AST_STMT_FOR1) {
		tmp = new_scope(s);
		s = &tmp;
	}

	ast_id *child = node_id;
	while (child->value != 0) {
		ast_node *node = ast_get(pool, *child);
		if (node->kind == AST_TYPE_STRUCT && node->value.s.at) {
			if (node->child[0].value != 0) {
				*scope_upsert_tag(s, node->value.s, perm) = *child;
			} else {
				// TODO: Opaque struct declarations
				ast_id *resolved = scope_upsert_tag(s, node->value.s, NULL);
				if (resolved) {
					*child = *resolved;
				}
			}
		} else if (node->kind == AST_DECL || node->kind == AST_EXTERN_DEF
			|| node->kind == AST_ENUMERATOR)
		{
			*scope_upsert_ident(s, node->value.s, perm) = *child;
		} else if (node->kind == AST_EXPR_IDENT) {
			ast_id *resolved = scope_upsert_ident(s, node->value.s, NULL);
			if (!resolved) {
				errorf(node->loc, "Variable was never declared");
				*error = true;
			} else {
				*child = *resolved;
			}
		}

		if (node->child[0].value != 0) {
			check_decls(pool, &node->child[0], s, perm, error);
		}

		if (node->kind == AST_EXTERN_DEF) {
			tmp = new_scope(s);
			s = &tmp;
		}

		child = &node->child[1];
	}
}

static i64
eval_ast(ast_pool *pool, ast_id node_id)
{
	i64 result = 0;
	ast_node *node = ast_get(pool, node_id);
	switch (node->kind) {
	case AST_EXPR_INT:
		{
			result = node->value.i;
		} break;
	case AST_EXPR_BINARY:
		{
			i64 lhs = eval_ast(pool, node->child[0]);
			i64 rhs = eval_ast(pool, node->child[1]);
			switch (node->value.op) {
			case TOKEN_PLUS:          result = lhs +  rhs; break;
			case TOKEN_MINUS:         result = lhs -  rhs; break;
			case TOKEN_STAR:          result = lhs *  rhs; break;
			case TOKEN_SLASH:         result = lhs /  rhs; break;
			case TOKEN_PERCENT:       result = lhs %  rhs; break;
			case TOKEN_EQUAL_EQUAL:   result = lhs == rhs; break;
			case TOKEN_LESS:          result = lhs <  rhs; break;
			case TOKEN_GREATER:       result = lhs >  rhs; break;
			case TOKEN_RSHIFT:        result = lhs << rhs; break;
			case TOKEN_LSHIFT:        result = lhs >> rhs; break;
			case TOKEN_LESS_EQUAL:    result = lhs <= rhs; break;
			case TOKEN_GREATER_EQUAL: result = lhs >= rhs; break;
			case TOKEN_AMP:           result = lhs &  rhs; break;
			case TOKEN_BAR:           result = lhs |  rhs; break;
			case TOKEN_CARET:         result = lhs ^  rhs; break;
			case TOKEN_AMP_AMP:       result = lhs && rhs; break;
			case TOKEN_BAR_BAR:       result = lhs || rhs; break;
			default:                  ASSERT(!"Invalid operator");
			}
		} break;
	case AST_EXPR_UNARY:
		{
			switch (node->value.op) {
			case TOKEN_MINUS: result = -result; break;
			case TOKEN_BANG:  result = !result; break;
			case TOKEN_PLUS:
				break;
			default:
				ASSERT(!"Invalid operator");
			}
		} break;
	default:
		ASSERT(!"Invalid node");
	}

	return result;
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
	case AST_STMT_FOR2:
	case AST_STMT_FOR3:
	case AST_STMT_GOTO:
	case AST_STMT_IF1:
	case AST_STMT_IF2:
	case AST_STMT_LABEL:
	case AST_STMT_RETURN:
	case AST_STMT_SWITCH:
	case AST_STMT_WHILE:
		// NOTE: Types are already checked above
		break;
	case AST_STMT_FOR1:
		{
			ast_node *cond = ast_get(pool, node->child[1]);
			ast_node *post = ast_get(pool, cond->child[1]);
			ast_node *body = ast_get(pool, post->child[1]);
			ASSERT(cond->kind == AST_STMT_FOR2);
			ASSERT(post->kind == AST_STMT_FOR3);
			ASSERT(is_statement(body->kind));
		} break;
	case AST_INIT_LIST:
		{
			ASSERT(node->type->kind == TYPE_STRUCT || node->type->kind == TYPE_ARRAY);
			member *member = node->type->members;

			// TODO: Check the type of each field in the initializer
			if (node->type->kind == TYPE_STRUCT) {
				while (member && node_id.value != 0) {
					ast_node *list_node = ast_get(pool, node_id);
					ast_node *value = ast_get(pool, list_node->child[0]);
					if (value->kind == AST_INIT_LIST) {
						value->type = member->type;
					}

					check_type(pool, list_node->child[0], arena, error);
					// TODO: Type conversion
					if (!type_equals(member->type, value->type)) {
						//errorf(list_node->loc, "Invalid type");
					}

					member = member->next;
					node_id = list_node->child[1];
				}

				if (member == NULL && node_id.value != 0) {
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
	case AST_EXPR_MEMBER_PTR:
		{
			ast_node *operand = ast_get(pool, node->child[0]);
			type *operand_type = operand->type;
			if (node->kind == AST_EXPR_MEMBER_PTR) {
				if (operand_type->kind != TYPE_POINTER) {
					errorf(node->loc, "Left-hand side is not a pointer");
					*error = true;
				}

				operand_type = operand_type->children;
			}

			if (operand_type->kind != TYPE_STRUCT) {
				errorf(node->loc, "Left-hand side is not a struct");
				*error = true;
			}

			member *s = get_member(operand_type->members, node->value.s);
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

			member *param_member = called->type->members;
			ast_id param_id = node->child[1];
			while (param_member && param_id.value != 0) {
				ast_node *param_list = ast_get(pool, param_id);
				ast_node *param = ast_get(pool, param_list->child[0]);
				if (!type_equals(param_member->type, param->type)) {
					errorf(param->loc, "Invalid parameter type");
				}

				param_member = param_member->next;
				param_id = param_list->child[1];
			}

#if 0
			if (param_member && param_id.value == 0) {
				errorf(node->loc, "Too few arguments");
			} else if (!param_member && param_id.value != 0) {
				errorf(node->loc, "Too many arguments");
			}
#endif

			type *return_type = called->type->children;
			node->type = return_type;
		} break;
	case AST_EXPR_CAST:
		{
			ast_node *cast_node = ast_get(pool, node->child[0]);
			node->type = cast_node->type;

			// TODO: Ensure that this cast is valid.
		} break;
	case AST_EXPR_IDENT:
		{
			ASSERT(!"This node should have been eliminated by check_decls");
		} break;
	case AST_EXPR_FLOAT:
		{
			node->type = &type_double;
		} break;
	case AST_EXPR_INT:
		{
			node->type = &type_int;
		} break;
	case AST_EXPR_STRING:
		{
			node->type = &type_char_ptr;
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
			node->type->size = eval_ast(pool, node->child[0]);
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
	case AST_ENUMERATOR:
		{
			node->type = &type_int;
		} break;
	case AST_TYPE_ENUM:
		{
			node->type = &type_int;

			ast_id enum_id = node->child[0];
			i32 value = 0;
			while (enum_id.value != 0) {
				ast_node *enum_node = ast_get(pool, enum_id);
				if (enum_node->child[0].value != 0) {
					value = eval_ast(pool, enum_node->child[0]);
				}

				// TODO: Should we replace the whole enumerator or just the
				// underlying value. Modifying just the value would likely be
				// easier for error handling...
				enum_node->kind = AST_EXPR_INT;
				enum_node->value.i = value++;
				enum_node->type = &type_int;
				enum_id = enum_node->child[1];
			}
		} break;
	case AST_TYPE_STRUCT:
		{
			// TODO: add the struct tag to the scope and ensure that the
			// struct is only defined once.
			node->type = type_create(TYPE_STRUCT, arena);

			// TODO: Collect the members of the struct
			member **ptr = &node->type->members;
			ast_id decl_id = node->child[0];
			while (decl_id.value != 0) {
				ast_node *decl_node = ast_get(pool, decl_id);
				decl_id = decl_node->child[1];

				ASSERT(decl_node->kind == AST_DECL_LIST);
				*ptr = ALLOC(arena, 1, member);
				(*ptr)->name = ast_get(pool, decl_node->child[0])->value.s;
				(*ptr)->type = ast_get(pool, decl_node->child[0])->type;
				ptr = &(*ptr)->next;
			}
		} break;
	}
}

static void
add_global_symbols(ast_pool *pool, symbol_table symtab)
{
	for (isize i = 0; i < pool->size; i++) {
		ast_node *node = &pool->nodes[i];
		if (node->kind == AST_EXTERN_DEF) {
			ast_id node_id = {i};
			ASSERT(node->type);

			decl_symbol *sym = get_decl_symbol(symtab, node_id);
			sym->name = node->value.s;
			sym->type = node->type;
			sym->is_global = true;
			sym->is_function = (node->type->kind == TYPE_FUNCTION);
			sym->definition = node->child[1];

			sym->linkage = LINK_DEFAULT;
			if (node->flags & AST_EXTERN) {
				sym->linkage = LINK_EXTERN;
			} else if (node->flags & AST_STATIC) {
				sym->linkage = LINK_STATIC;
			}
		}
	}
}

static void
check_switch_stmt(ast_pool *pool, ast_id node_id, symbol_table symtab, ast_id switch_id, arena *perm)
{
	if (node_id.value == 0) {
		return;
	}

	ast_node *node = ast_get(pool, node_id);
	switch (node->kind) {
	case AST_STMT_DO_WHILE:
	case AST_STMT_FOR1:
	case AST_STMT_FOR2:
	case AST_STMT_FOR3:
	case AST_STMT_IF1:
	case AST_STMT_IF2:
	case AST_STMT_LIST:
	case AST_STMT_WHILE:
	case AST_DECL_LIST:
	case AST_EXTERN_DEF:
		check_switch_stmt(pool, node->child[0], symtab, switch_id, perm);
		check_switch_stmt(pool, node->child[1], symtab, switch_id, perm);
		break;
	case AST_STMT_SWITCH:
		check_switch_stmt(pool, node->child[1], symtab, node_id, perm);
		break;
	case AST_STMT_CASE:
		if (switch_id.value == 0) {
			errorf(node->loc, "case outside switch");
		} else {
			switch_symbol *switch_sym = get_switch_symbol(symtab, switch_id);
			case_symbol *case_sym = get_case_symbol(symtab, node_id);
			case_sym->case_id = node_id;

			if (switch_sym->last) {
				switch_sym->last = switch_sym->last->next = case_sym;
			} else {
				switch_sym->first = switch_sym->last = case_sym;
			}

			check_switch_stmt(pool, node->child[1], symtab, switch_id, perm);
		} break;
	case AST_STMT_DEFAULT:
		if (switch_id.value == 0) {
			errorf(node->loc, "default outside switch");
		} else {
			switch_symbol *switch_sym = get_switch_symbol(symtab, switch_id);
			if (switch_sym->default_case.value != 0) {
				errorf(node->loc, "Duplicate default label");
			}

			switch_sym->default_case = node_id;
			check_switch_stmt(pool, node->child[0], symtab, switch_id, perm);
		} break;
	default:
	}
}

static symbol_table
check(ast_pool *pool, arena *perm, b32 *error)
{
	symbol_table symtab = {0};
	symtab.symbols = ALLOC(perm, pool->size, symbol_id);
	symtab.kind = ALLOC(perm, pool->size, symbol_kind);
	symtab.switch_count = 1;
	symtab.decl_count = 1;
	symtab.case_count = 1;
	symtab.label_count = 1;
	symtab.string_count = 1;

	for (isize i = 0; i < pool->size; i++) {
		ast_node *node = &pool->nodes[i];
		switch (node->kind) {
		case AST_STMT_SWITCH:
			symtab.symbols[i].value = symtab.switch_count++;
			symtab.kind[i] = SYM_SWITCH;
			break;
		case AST_STMT_CASE:
			symtab.symbols[i].value = symtab.case_count++;
			symtab.kind[i] = SYM_CASE;
			break;
		case AST_STMT_LABEL:
			symtab.symbols[i].value = symtab.label_count++;
			symtab.kind[i] = SYM_LABEL;
			break;
		case AST_TYPE_STRUCT:
			if (node->value.s.at == NULL || node->child[0].value == 0) {
				break;
			}

			/* fallthrough */
		case AST_EXTERN_DEF:
		case AST_ENUMERATOR:
		case AST_DECL:
			symtab.symbols[i].value = symtab.decl_count++;
			symtab.kind[i] = SYM_DECL;
			break;
		case AST_EXPR_STRING:
			symtab.symbols[i].value = symtab.string_count++;
			symtab.kind[i] = SYM_STRING;
			break;
		default:
		}
	}

	symtab.labels   = ALLOC(perm, symtab.label_count, u32);
	symtab.strings  = ALLOC(perm, symtab.string_count, str);
	symtab.decls    = ALLOC(perm, symtab.decl_count, decl_symbol);
	symtab.cases    = ALLOC(perm, symtab.case_count, case_symbol);
	symtab.switches = ALLOC(perm, symtab.switch_count, switch_symbol);

	// NOTE: Check labels, ensure that no label is defined twice and merge
	// gotos with their corresponding label.
	if (symtab.label_count > 1) {
		arena_temp temp = arena_temp_begin(perm);
		ast_id *label_id = ALLOC(perm, symtab.label_count, ast_id);
		for (isize i = 1; i < pool->size; i++) {
			isize j = i;
			isize l = 0;
			for (; i < pool->size; i++) {
				ast_node *node = &pool->nodes[i];
				if (node->kind == AST_EXTERN_DEF) {
					break;
				} else if (node->kind == AST_STMT_LABEL) {
					label_id[l++].value = i;
					for (isize k = 0; k < l; k++) {
						ast_node *label_node = &pool->nodes[label_id[k].value];
						if (str_equals(label_node->value.s, node->value.s)) {
							errorf(node->loc, "Label was already defined");
						}
					}
				}
			}

			for (; j < i; j++) {
				ast_node *node = &pool->nodes[j];
				if (node->kind != AST_STMT_GOTO) {
					continue;
				}

				for (isize k = 0; k < l; k++) {
					ast_node *label_node = &pool->nodes[label_id[k].value];
					if (str_equals(label_node->value.s, node->value.s)) {
						symtab.symbols[j] = symtab.symbols[label_id[k].value];
						break;
					}
				}

				if (symtab.symbols[j].value == 0) {
					errorf(node->loc, "No label was found");
				}
			}
		}

		arena_temp_end(temp);
	}

	for (isize i = 1; i < pool->size; i++) {
		ast_node *node = &pool->nodes[i];
		if (node->kind == AST_EXPR_STRING) {
			str escaped = node->value.s;
			str unescaped = {0};
			unescaped.at = ALLOC(perm, escaped.length, char);
			for (isize i = 1; i < escaped.length - 1; i++) {
				char c = escaped.at[i];
				if (c == '\\') {
					c = escaped.at[++i];
					switch (c) {
					case '"':
						unescaped.at[unescaped.length++] = c;
						break;
					case 'n':
						unescaped.at[unescaped.length++] = '\n';
						break;
					case 't':
						unescaped.at[unescaped.length++] = '\t';
						break;
					case 'v':
						unescaped.at[unescaped.length++] = '\v';
						break;
					case 'r':
						unescaped.at[unescaped.length++] = '\r';
						break;
					case 'f':
						unescaped.at[unescaped.length++] = '\f';
						break;
					default:
						ASSERT(!"Invalid escape sequence");
					}
				} else {
					unescaped.at[unescaped.length++] = c;
				}
			}

			ast_id node_id = {i};
			str *sym = get_string_symbol(symtab, node_id);
			*sym = unescaped;
		}
	}

	check_switch_stmt(pool, pool->root, symtab, ast_id_nil, perm);

	scope s = new_scope(NULL);
	check_decls(pool, &pool->root, &s, perm, error);
	if (!*error) {
		check_type(pool, pool->root, perm, error);
		add_global_symbols(pool, symtab);
	}

	return symtab;
}
