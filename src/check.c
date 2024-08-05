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
	case TYPE_BITFIELD:
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

static b32
type_equals(type *lhs, type *rhs)
{
	member *l, *r;

	if (lhs == rhs) {
		return true;
	}

	if (lhs->kind == TYPE_VOID || rhs->kind == TYPE_VOID) {
		return false;
	}

	if (is_integer(lhs->kind) && is_integer(rhs->kind)) {
		return true;
	}

	if (lhs->kind == TYPE_OPAQUE) {
		lhs = lhs->children;
	}

	if (rhs->kind == TYPE_OPAQUE) {
		rhs = rhs->children;
	}

	switch (lhs->kind) {
	case TYPE_ARRAY:
	case TYPE_POINTER:
		if (rhs->kind != TYPE_ARRAY && rhs->kind != TYPE_POINTER) {
			return false;
		}

		if (rhs->children->kind == TYPE_VOID || lhs->children->kind == TYPE_VOID) {
			return true;
		}

		return type_equals(lhs->children, rhs->children);
	case TYPE_FUNCTION:
		if (rhs->kind != TYPE_FUNCTION) {
			return false;
		}

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
		if (lhs->kind != rhs->kind) {
			return false;
		}

		return true;
	}
}

static i64
eval_ast(ast_pool *pool, ast_id node_id)
{
	i64 result = 0;
	ast_node *node = get_node(pool, node_id);
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
			case TOKEN_TILDE: result = ~result; break;
			case TOKEN_PLUS:
				break;
			default:
				ASSERT(!"Invalid operator");
			}
		} break;
	case AST_EXPR_TERNARY1:
		{
			ast_node *node2 = get_node(pool, node->child[1]);
			if (eval_ast(pool, node->child[0])) {
				result = eval_ast(pool, node2->child[0]);
			} else {
				result = eval_ast(pool, node2->child[1]);
			}
		} break;
	case AST_EXPR_SIZEOF:
		{
			// TODO: Implement sizeof operator
			result = 8;
		} break;
	case AST_EXPR_CAST:
		{
			// TODO: Implement casting behavior
			result = eval_ast(pool, node->child[1]);
		} break;
	case AST_EXPR_IDENT:
		{
			result = eval_ast(pool, node->value.ref);
		} break;
	default:
		ASSERT(!"Invalid node");
	}

	return result;
}

static b32 is_ident_node(ast_node_kind kind)
{
	b32 result = (kind == AST_TYPE_IDENT || kind == AST_EXPR_IDENT);
	return result;
}

static type *
check_type(ast_pool *pool, ast_id node_id, arena *arena)
{
	if (pool->error) {
		return TYPE_NIL;
	}

	ast_node *node = get_node(pool, node_id);
	if (node->kind == AST_INIT_LIST) {
		if (node->type == NULL) {
			BREAK();
			return node->type;
		}
	} else if (node->type == NULL) {
		node->type = &type_nil;
	} else if (node->kind != AST_TYPE_STRUCT && node->kind != AST_TYPE_UNION) {
		// This node has already been type checked.
		return node->type;
	}

	switch (node->kind) {
	case AST_INVALID:
		pool->error = true;
		break;
	case AST_EXPR_LIST:
	case AST_STMT_LIST:
	case AST_DECL_LIST:
		while (node_id.value != 0) {
			ast_node *node = get_node(pool, node_id);
			check_type(pool, node->child[0], arena);
			node_id = node->child[1];
		}

		break;
	case AST_STMT_BREAK:
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
	case AST_EXPR_TERNARY2:
		if (node->child[0].value != 0) {
			check_type(pool, node->child[0], arena);
		}

		if (node->child[1].value != 0) {
			check_type(pool, node->child[1], arena);
		}
		break;
	case AST_STMT_FOR1:
		{
			if (node->child[0].value != 0) {
				check_type(pool, node->child[0], arena);
			}

			if (node->child[1].value != 0) {
				check_type(pool, node->child[1], arena);
			}

			ast_node *cond = get_node(pool, node->child[1]);
			ast_node *post = get_node(pool, cond->child[1]);
			ASSERT(cond->kind == AST_STMT_FOR2);
			ASSERT(post->kind == AST_STMT_FOR3);
		} break;
	case AST_INIT_LIST:
		{
			ASSERT(node->type != TYPE_NIL);
			ASSERT(is_compound_type(node->type->kind) || node->type->kind == TYPE_ARRAY);

			// TODO: Check the type of each field in the initializer
			if (is_compound_type(node->type->kind)) {
				if (node->type->kind == TYPE_OPAQUE) {
					node->type = node->type->children;
				}

				member *member = node->type->members;
				while (member && node_id.value != 0) {
					ast_node *list_node = get_node(pool, node_id);
					ast_node *value = get_node(pool, list_node->child[0]);
					if (value->kind == AST_INIT_LIST) {
						value->type = member->type;
					}

					check_type(pool, list_node->child[0], arena);
					// TODO: Type conversion
					if (!type_equals(member->type, value->type)) {
						//errorf(list_node->loc, "Invalid type");
					}

					member = member->next;
					node_id = list_node->child[1];
				}

				if (member == NULL && node_id.value != 0) {
#if 0
					errorf(node->loc, "Too many fields in the initializer");
#endif
				}
			} else if (node->type->kind == TYPE_ARRAY) {
				type *expected = node->type->children;
				while (node_id.value != 0) {
					ast_node *node = get_node(pool, node_id);
					ASSERT(node->kind == AST_INIT_LIST);

					ast_node *child = get_node(pool, node->child[0]);
					if (child->kind == AST_INIT_LIST) {
						child->type = expected;
					}

					type *found = check_type(pool, node->child[0], arena);
					if (child->kind == AST_EXPR_INT && child->value.i == 0) {
						// ignore
					} else if (!type_equals(found, expected)) {
						errorf(node->loc, "Invalid array member type");
					}

					node_id = node->child[1];
				}
			}
		} break;
	case AST_EXPR_MEMBER:
	case AST_EXPR_MEMBER_PTR:
		{
			type *operand_type = check_type(pool, node->child[0], arena);
			if (node->kind == AST_EXPR_MEMBER_PTR) {
				if (operand_type->kind != TYPE_POINTER) {
					errorf(node->loc, "Left-hand side is not a pointer");
					pool->error = true;
				}

				operand_type = operand_type->children;
			}

			if (operand_type->kind == TYPE_OPAQUE) {
				operand_type = operand_type->children;
			}

			if (operand_type->kind != TYPE_STRUCT && operand_type->kind != TYPE_UNION) {
				errorf(node->loc, "Left-hand side is not a struct");
				pool->error = true;
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
			type *lhs = check_type(pool, node->child[0], arena);
			type *rhs = check_type(pool, node->child[1], arena);
			node->type = lhs;

			u32 operator = node->value.i;
			if (operator == TOKEN_LBRACKET) {

				// NOTE: ensure that one operand is a pointer and the other one
				// is an integral type.
				if (is_pointer(lhs)) {
					node->type = lhs->children;
				} else if (is_pointer(rhs)) {
					node->type = rhs->children;
				} else {
					node->type = TYPE_NIL;
					pool->error = true;
					errorf(node->loc, "Incompatible types: %s, %s",
						type_get_name(lhs->kind),
						type_get_name(rhs->kind));
				}
			} else if (is_integer(lhs->kind) && is_integer(rhs->kind)) {
				// Apply integer promotion
				b32 same_sign = (lhs->kind & TYPE_UNSIGNED) == (rhs->kind & TYPE_UNSIGNED);
				if (lhs->kind == rhs->kind) {
					// do nothing
				} else if (same_sign) {
					if (lhs->kind < rhs->kind) {
						lhs->kind = rhs->kind;
					}
				} else {
					type *unsigned_type = (lhs->kind & TYPE_UNSIGNED ? lhs : rhs);
					type *signed_type = (lhs->kind & TYPE_UNSIGNED ? rhs : lhs);
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
			type *called = check_type(pool, node->child[0], arena);
			ast_node *called_node = get_node(pool, node->child[0]);
			str func_name = {0};
			if (called_node->kind == AST_EXPR_IDENT) {
				called_node = get_node(pool, called_node->value.ref);
				func_name = called_node->value.s;
			}

			if (starts_with(func_name, S("__builtin_")) || equals(func_name, S("asm"))) {
				// TODO: Implement type checking for builtins
			} else if (called->kind == TYPE_FUNCTION) {
				member *param_member = called->members;
				ast_id param_id = node->child[1];
				while (param_member && param_id.value != 0) {
					ast_node *param_list = get_node(pool, param_id);
					ast_node *param_node = get_node(pool, param_list->child[0]);
					type *param = check_type(pool, param_list->child[0], arena);
					if (!type_equals(param_member->type, param)) {
						errorf(param_node->loc, "Invalid parameter type");
					}

					param_member = param_member->next;
					param_id = param_list->child[1];
				}

				while (param_id.value != 0) {
					ast_node *param_list = get_node(pool, param_id);
					check_type(pool, param_list->child[0], arena);
					param_id = param_list->child[1];
				}

#if 0
				if (param_member && param_id.value == 0) {
					errorf(node->loc, "Too few arguments");
				} else if (!param_member && param_id.value != 0) {
					errorf(node->loc, "Too many arguments");
				}
#endif

				type *return_type = called->children;
				node->type = return_type;
			} else {
				pool->error = true;
				errorf(node->loc, "Not a function: %s", type_get_name(called->kind));
			}
		} break;
	case AST_EXPR_CAST:
		{
			node->type = check_type(pool, node->child[0], arena);
			check_type(pool, node->child[1], arena);
			// TODO: Ensure that this cast is valid.
		} break;
	case AST_EXPR_COMPOUND:
		{
			node->type = check_type(pool, node->child[0], arena);
			ast_node *init_list = get_node(pool, node->child[1]);
			init_list->type = node->type;
			check_type(pool, node->child[1], arena);
		} break;
	case AST_EXPR_IDENT:
		{
			if (equals(node->value.s, S("__builtin_popcount"))) {
				member *param = ALLOC(arena, 1, member);
				param->name = S("x");
				param->type = type_create(TYPE_INT, arena);

				node->type = type_create(TYPE_FUNCTION, arena);
				node->type->members = param;
				node->type->children = param->type;
			} else {
				node->type = get_node(pool, node->value.ref)->type;
			}
		} break;
	case AST_EXPR_FLOAT:
		{
			node->type = type_create(TYPE_DOUBLE, arena);
		} break;
	case AST_EXPR_CHAR:
		{
			node->type = type_create(TYPE_CHAR, arena);
		} break;
	case AST_EXPR_INT:
		{
			node->type = type_create(TYPE_INT, arena);
		} break;
	case AST_EXPR_STRING:
		{
			type *char_type = type_create(TYPE_CHAR, arena);
			node->type = type_create(TYPE_POINTER, arena);
			node->type->children = char_type;
		} break;
	case AST_EXPR_SIZEOF:
		{
			node->type = type_create(TYPE_INT, arena);
		} break;
	case AST_EXPR_POSTFIX:
	case AST_EXPR_UNARY:
		{
			check_type(pool, node->child[0], arena);

			ast_node *operand = get_node(pool, node->child[0]);
			switch (node->value.i) {
			case TOKEN_STAR:
				if (operand->type->kind == TYPE_POINTER) {
					node->type = operand->type->children;
				} else {
					node->type = TYPE_NIL;
					pool->error = true;
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
			case TOKEN_TILDE:
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
	case AST_EXPR_TERNARY1:
		{
			ast_node *node2 = get_node(pool, node->child[1]);

			check_type(pool, node->child[0], arena);
			ast_node *cond = get_node(pool, node->child[0]);
			if (!is_integer(cond->type->kind)) {
				errorf(cond->loc, "Not an integer expression");
			}

			check_type(pool, node2->child[0], arena);
			check_type(pool, node2->child[1], arena);
			ast_node *lhs = get_node(pool, node2->child[0]);
			ast_node *rhs = get_node(pool, node2->child[1]);
			if (!type_equals(lhs->type, rhs->type)) {
				errorf(lhs->loc, "Invalid type in ternary expression");
			}

			node->type = lhs->type;
		} break;
	case AST_DECL:
	case AST_EXTERN_DEF:
		{
			node->type = check_type(pool, node->child[0], arena);

			ASSERT(node->type);
			if (node->child[1].value != 0) {
				ast_node *initializer = get_node(pool, node->child[1]);
				if (initializer->kind == AST_INIT_LIST) {
					initializer->type = node->type;
				}

				check_type(pool, node->child[1], arena);
			}
		} break;
	case AST_TYPE_VOID:
		{
			node->type = &type_void;
		} break;
	case AST_TYPE_CHAR:
		{
			if (node->flags & AST_UNSIGNED) {
				node->type = type_create(TYPE_CHAR_UNSIGNED, arena);
			} else {
				node->type = type_create(TYPE_CHAR, arena);
			}
		} break;
	case AST_TYPE_FLOAT:
		{
			node->type = type_create(TYPE_FLOAT, arena);
		} break;
	case AST_TYPE_INT:
		{
			u32 int_flags = AST_LONG | AST_LLONG | AST_SHORT | AST_UNSIGNED;
			switch (node->flags & int_flags) {
			case AST_LLONG | AST_UNSIGNED:
				node->type = type_create(TYPE_LLONG_UNSIGNED, arena);
				break;
			case AST_LLONG:
				node->type = type_create(TYPE_LLONG, arena);
				break;
			case AST_LONG | AST_UNSIGNED:
				node->type = type_create(TYPE_LONG_UNSIGNED, arena);
				break;
			case AST_LONG:
				node->type = type_create(TYPE_LONG, arena);
				break;
			case AST_SHORT | AST_UNSIGNED:
				node->type = type_create(TYPE_SHORT_UNSIGNED, arena);
				break;
			case AST_SHORT:
				node->type = type_create(TYPE_SHORT, arena);
				break;
			case AST_UNSIGNED:
				node->type = type_create(TYPE_INT_UNSIGNED, arena);
				break;
			default:
				node->type = type_create(TYPE_INT, arena);
			}
		} break;
	case AST_TYPE_POINTER:
		{
			type *base_type = check_type(pool, node->child[1], arena);
			node->type = type_create(TYPE_POINTER, arena);
			node->type->children = base_type;
		} break;
	case AST_TYPE_ARRAY:
		{
			node->type = type_create(TYPE_ARRAY, arena);
			if (node->child[0].value != 0) {
				check_type(pool, node->child[0], arena);
				node->type->size = eval_ast(pool, node->child[0]);
			} else {
				// TODO: Evaluate the size based on the expression or error.
			}

			type *base_type = check_type(pool, node->child[1], arena);
			node->type->children = base_type;
		} break;
	case AST_TYPE_BITFIELD:
		{
			check_type(pool, node->child[1], arena);
			ast_node *type = get_node(pool, node->child[1]);
			node->type = type_create(TYPE_BITFIELD, arena);
			// TODO: Evaluate the expression
			node->type->size = 1;
			node->type->children = type->type;
		} break;
	case AST_TYPE_FUNC:
		{
			node->type = type_create(TYPE_FUNCTION, arena);
			member **m = &node->type->members;
			check_type(pool, node->child[1], arena);
			ast_node *return_type = get_node(pool, node->child[1]);

			if (node->child[0].value != 0) {
				check_type(pool, node->child[0], arena);
				ast_node *param_list = get_node(pool, node->child[0]);
				for (;;) {
					ast_node *param = get_node(pool, param_list->child[0]);
					*m = ALLOC(arena, 1, member);
					(*m)->name = param->value.s;
					(*m)->type = param->type;
					m = &(*m)->next;

					if (param_list->child[1].value == 0) {
						break;
					}

					param_list = get_node(pool, param_list->child[1]);
				}
			}

			node->type->children = return_type->type;
		} break;
	case AST_TYPE_IDENT:
		{
			ASSERT(node->value.ref.value != 0);
			node->type = get_node(pool, node->value.ref)->type;
		} break;
	case AST_ENUMERATOR:
		{
			node->type = type_create(TYPE_INT, arena);
		} break;
	case AST_TYPE_ENUM:
		{
			node->type = type_create(TYPE_INT, arena);

			ast_id enum_id = node->child[0];
			i32 value = 0;
			while (enum_id.value != 0) {
				ast_node *enum_node = get_node(pool, enum_id);
				if (enum_node->child[0].value != 0) {
					value = eval_ast(pool, enum_node->child[0]);
				}

				// TODO: Should we replace the whole enumerator or just the
				// underlying value. Modifying just the value would likely be
				// easier for error handling...
				enum_node->kind = AST_EXPR_INT;
				enum_node->value.i = value++;
				enum_node->type = type_create(TYPE_INT, arena);
				enum_id = enum_node->child[1];
			}
		} break;
	case AST_TYPE_STRUCT:
	case AST_TYPE_UNION:
		{
			// TODO: add the struct tag to the scope and ensure that the
			// struct is only defined once.
			if (node->child[0].value == 0) {
				ast_node *ref = get_node(pool, node->value.ref);
				if (node->flags & AST_OPAQUE) {
					ref->type = type_create(TYPE_STRUCT, arena);
					node->type = type_create(TYPE_OPAQUE, arena);
					node->type->children = ref->type;
				} else {
					node->type = get_node(pool, node->value.ref)->type;
					ASSERT(node->type->kind == TYPE_STRUCT
						|| node->type->kind == TYPE_UNION);
				}
			} else {
				if (node->type == TYPE_NIL) {
					node->type = type_create(TYPE_STRUCT, arena);
				}

				node->type->kind = TYPE_STRUCT;
				if (node->kind == AST_TYPE_UNION) {
					node->type->kind = TYPE_UNION;
				}

				// TODO: Collect the members of the struct
				ASSERT(node->type->members == NULL);
				member **ptr = &node->type->members;
				ast_id decl_id = node->child[0];
				while (decl_id.value != 0) {
					check_type(pool, decl_id, arena);
					ast_node *list_node = get_node(pool, decl_id);
					ASSERT(list_node->kind == AST_DECL_LIST);
					decl_id = list_node->child[1];

					ast_node *decl_node = get_node(pool, list_node->child[0]);
					*ptr = ALLOC(arena, 1, member);
					(*ptr)->name = decl_node->value.s;
					(*ptr)->type = decl_node->type;
					ptr = &(*ptr)->next;
				}

				ASSERT(node->value.s.at != NULL || node->type->members != NULL);
			}
		} break;
	}

	ASSERT(node->type != NULL);
	return node->type;
}

static void
check_switch_stmt(ast_pool *pool, ast_id node_id, symbol_table symtab, ast_id switch_id, arena *perm)
{
	if (node_id.value == 0) {
		return;
	}

	ast_node *node = get_node(pool, node_id);
	switch (node->kind) {
	case AST_STMT_DO_WHILE:
	case AST_STMT_FOR1:
	case AST_STMT_FOR2:
	case AST_STMT_FOR3:
	case AST_STMT_IF1:
	case AST_STMT_IF2:
	case AST_STMT_WHILE:
	case AST_EXTERN_DEF:
		check_switch_stmt(pool, node->child[0], symtab, switch_id, perm);
		check_switch_stmt(pool, node->child[1], symtab, switch_id, perm);
		break;
	case AST_STMT_LIST:
	case AST_DECL_LIST:
		while (node_id.value != 0) {
			ast_node *node = get_node(pool, node_id);
			check_switch_stmt(pool, node->child[0], symtab, switch_id, perm);
			node_id = node->child[1];
		}

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
		break;
	}
}

static symbol_table
check(ast_pool *pool, arena *perm)
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
			if (!(node->flags & AST_TYPEDEF)) {
				symtab.symbols[i].value = symtab.decl_count++;
				symtab.kind[i] = SYM_DECL;
			}

			break;
		case AST_EXPR_STRING:
			symtab.symbols[i].value = symtab.string_count++;
			symtab.kind[i] = SYM_STRING;
			break;
		default:
			break;
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
						if (label_node == node) {
							continue;
						}

						if (equals(label_node->value.s, node->value.s)) {
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
					if (equals(label_node->value.s, node->value.s)) {
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

	// NOTE: Collect all strings.
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
					case '\\':
						unescaped.at[unescaped.length++] = '\\';
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

	// NOTE: Collect all switch statements.
	check_switch_stmt(pool, pool->root, symtab, ast_id_nil, perm);
	check_type(pool, pool->root, perm);

	// Add global symbols to the symbol table
	for (isize i = 0; i < pool->size; i++) {
		ast_node *node = &pool->nodes[i];
		if (node->kind == AST_EXTERN_DEF && !(node->flags & AST_TYPEDEF)) {
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

	return symtab;
}
