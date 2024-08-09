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
		lhs = lhs->base_type;
	}

	if (rhs->kind == TYPE_OPAQUE) {
		rhs = rhs->base_type;
	}

	switch (lhs->kind) {
	case TYPE_ARRAY:
	case TYPE_POINTER:
		if (rhs->kind != TYPE_ARRAY && rhs->kind != TYPE_POINTER) {
			return false;
		}

		if (rhs->base_type->kind == TYPE_VOID || lhs->base_type->kind == TYPE_VOID) {
			return true;
		}

		return type_equals(lhs->base_type, rhs->base_type);
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
parse_i64(str input)
{
	i64 result = 0;

	while (input.length > 0 && is_digit(*input.at)) {
		result *= 10;
		result += *input.at - '0';

		input.at++;
		input.length--;
	}

	return result;
}

static i64
eval_ast(ast_pool *pool, ast_id node_id)
{
	i64 result = 0;
	ast_node *node = get_node(pool, node_id);
	switch (node->kind) {
	case AST_EXPR_LITERAL:
		{
			switch (node->token.kind) {
			case TOKEN_LITERAL_INT:
				result = parse_i64(node->token.value);
				break;
			default:
				ASSERT(!"TODO");
			}
		} break;
	case AST_EXPR_BINARY:
		{
			i64 lhs = eval_ast(pool, node->child[0]);
			i64 rhs = eval_ast(pool, node->child[1]);
			switch (node->token.kind) {
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
			switch (node->token.kind) {
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
			result = eval_ast(pool, node->child[0]);
		} break;
	default:
		ASSERT(!"Invalid node");
	}

	return result;
}

static type *
get_type(ast_pool *p, ast_id id)
{
	if (0 < id.value && id.value < p->size) {
		return p->types + id.value;
	}

	ASSERT(!"ID is out of bounds");
	return NULL;
}

static type *
check_type(ast_pool *pool, ast_id node_id, arena *arena)
{
	if (pool->error) {
		return &pool->types[0];
	}

	ast_node *node = get_node(pool, node_id);
	type *node_type = get_type(pool, node_id);
	if (node->kind != AST_INIT && node_type->kind != TYPE_UNKNOWN
		&& node->kind != AST_TYPE_STRUCT && node->kind != AST_TYPE_UNION)
	{
		// This node has already been type checked.
		return node_type;
	}

	switch (node->kind) {
	case AST_INVALID:
		pool->error = true;
		break;
	case AST_LIST:
		while (node_id.value != 0) {
			ast_node *node = get_node_of_kind(pool, node_id, AST_LIST);
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
	case AST_STMT_ASM:
		{
			ASSERT(node->token.kind == TOKEN_LITERAL_STRING);
		} break;
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
	case AST_INIT:
		{
			if (node_type->kind == TYPE_UNKNOWN) {
				return NULL;
			}

			ASSERT(is_compound_type(node_type->kind) || node_type->kind == TYPE_ARRAY);
			// TODO: Check the type of each field in the initializer
			if (is_compound_type(node_type->kind)) {
				if (node_type->kind == TYPE_OPAQUE) {
					memcpy(node_type, node_type->base_type, sizeof(*node_type));
				}

				member *member = node_type->members;
				node_id = node->child[0];
				while (member && node_id.value != 0) {
					ast_node *list_node = get_node_of_kind(pool, node_id, AST_LIST);
					ast_node *value = get_node(pool, list_node->child[0]);
					if (value->kind == AST_INIT) {
						type *value_type = get_type(pool, list_node->child[0]);
						memcpy(value_type, member->type, sizeof(*value_type));
					}

					type *value_type = check_type(pool, list_node->child[0], arena);
					// TODO: Type conversion
					if (!type_equals(member->type, value_type)) {
						//errorf(list_node->token.loc, "Invalid type");
					}

					member = member->next;
					node_id = list_node->child[1];
				}

				if (member == NULL && node_id.value != 0) {
#if 0
					errorf(node->token.loc, "Too many fields in the initializer");
#endif
				}
			} else if (node_type->kind == TYPE_ARRAY) {
				type *expected = node_type->base_type;
				node_id = node->child[0];
				while (node_id.value != 0) {
					ast_node *list_node = get_node_of_kind(pool, node_id, AST_LIST);
					ast_node *child = get_node(pool, list_node->child[0]);
					if (child->kind == AST_INIT) {
						type *child_type = get_type(pool, node->child[0]);
						memcpy(child_type, expected, sizeof(*expected));
					}

					type *found = check_type(pool, node->child[0], arena);
					if (!type_equals(found, expected)) {
						errorf(node->token.loc, "Invalid array member type");
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
					errorf(node->token.loc, "Left-hand side is not a pointer");
					pool->error = true;
				}

				operand_type = operand_type->base_type;
			}

			if (operand_type->kind == TYPE_OPAQUE) {
				operand_type = operand_type->base_type;
			}

			if (operand_type->kind != TYPE_STRUCT && operand_type->kind != TYPE_UNION) {
				errorf(node->token.loc, "Left-hand side is not a struct");
				pool->error = true;
			}

			member *s = get_member(operand_type->members, node->token.value);
			if (s) {
				memcpy(node_type, s->type, sizeof(*node_type));
			} else {
				errorf(node->token.loc, "Member does not exist");
			}
		} break;
	case AST_EXPR_BINARY:
		{
			type *lhs = check_type(pool, node->child[0], arena);
			type *rhs = check_type(pool, node->child[1], arena);
			memcpy(node_type, lhs, sizeof(*node_type));

			u32 operator = node->token.kind;
			if (operator == TOKEN_LBRACKET) {

				// NOTE: ensure that one operand is a pointer and the other one
				// is an integral type.
				if (is_pointer(lhs)) {
					memcpy(node_type, lhs->base_type, sizeof(*node_type));
				} else if (is_pointer(rhs)) {
					memcpy(node_type, rhs->base_type, sizeof(*node_type));
				} else {
					pool->error = true;
					errorf(node->token.loc, "Incompatible types: %s, %s",
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
				func_name = called_node->token.value;
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
						errorf(param_node->token.loc, "Invalid parameter type");
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
					errorf(node->token.loc, "Too few arguments");
				} else if (!param_member && param_id.value != 0) {
					errorf(node->token.loc, "Too many arguments");
				}
#endif

				type *return_type = called->base_type;
				memcpy(node_type, return_type, sizeof(*node_type));
			} else {
				pool->error = true;
				errorf(node->token.loc, "Not a function: %s", type_get_name(called->kind));
			}
		} break;
	case AST_EXPR_CAST:
		{
			type *cast_type = check_type(pool, node->child[0], arena);
			memcpy(node_type, cast_type, sizeof(*node_type));
			check_type(pool, node->child[1], arena);
			// TODO: Ensure that this cast is valid.
		} break;
	case AST_EXPR_COMPOUND:
		{
			type *expr_type = check_type(pool, node->child[0], arena);
			type *init_type = get_type(pool, node->child[1]);
			memcpy(node_type, expr_type, sizeof(*expr_type));
			memcpy(init_type, expr_type, sizeof(*expr_type));
			check_type(pool, node->child[1], arena);
		} break;
	case AST_EXPR_IDENT:
		{
			if (equals(node->token.value, S("__builtin_popcount"))) {
				member *param = ALLOC(arena, 1, member);
				param->name = S("x");
				param->type = type_create(TYPE_INT, arena);

				node_type->kind = TYPE_FUNCTION;
				node_type->members = param;
				node_type->base_type = param->type;
			} else {
				type *ref_type = get_type(pool, node->child[0]);
				memcpy(node_type, ref_type, sizeof(*node_type));
			}
		} break;
	case AST_EXPR_LITERAL:
		{
			switch (node->token.kind) {
			case TOKEN_LITERAL_INT:
				// TODO: If zero, set type to zero
				node_type->kind = TYPE_INT;
				break;
			case TOKEN_LITERAL_CHAR:
				node_type->kind = TYPE_CHAR;
				break;
			case TOKEN_LITERAL_FLOAT:
				node_type->kind = TYPE_FLOAT;
				break;
			case TOKEN_LITERAL_STRING:
				node_type->kind = TYPE_POINTER;
				node_type->base_type = type_create(TYPE_CHAR, arena);
				break;
			default:
				ASSERT(!"Invalid literal");
			}
		} break;
	case AST_EXPR_SIZEOF:
		{
			node_type->kind = TYPE_INT;
		} break;
	case AST_EXPR_POSTFIX:
	case AST_EXPR_UNARY:
		{
			check_type(pool, node->child[0], arena);

			type *operand_type = get_type(pool, node->child[0]);
			switch (node->token.kind) {
			case TOKEN_STAR:
				if (operand_type->kind == TYPE_POINTER) {
					node_type = operand_type->base_type;
				} else {
					pool->error = true;
					errorf(node->token.loc, "Expected pointer type");
				}
				break;
			case TOKEN_AMP:
				node_type = type_create(TYPE_POINTER, arena);
				node_type->base_type = operand_type;
				break;
			case TOKEN_BANG:
			case TOKEN_PLUS:
			case TOKEN_MINUS:
			case TOKEN_TILDE:
				// TODO: ensure that type is integer
				memcpy(node_type, operand_type, sizeof(*node_type));
				break;
			case TOKEN_PLUS_PLUS:
			case TOKEN_MINUS_MINUS:
				// TODO: ensure that type is integer or pointer
				memcpy(node_type, operand_type, sizeof(*node_type));
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
			type *cond_type = get_type(pool, node->child[0]);
			if (!is_integer(cond_type->kind)) {
				ast_node *cond = get_node(pool, node->child[0]);
				errorf(cond->token.loc, "Not an integer expression");
			}

			type *lhs_type = check_type(pool, node2->child[0], arena);
			type *rhs_type = check_type(pool, node2->child[1], arena);
			if (!type_equals(lhs_type, rhs_type)) {
				ast_node *lhs = get_node(pool, node2->child[0]);
				errorf(lhs->token.loc, "Invalid type in ternary expression");
			}

			memcpy(node_type, lhs_type, sizeof(*node_type));
		} break;
	case AST_DECL:
	case AST_EXTERN_DEF:
		{
			type *decl_type = check_type(pool, node->child[0], arena);
			memcpy(node_type, decl_type, sizeof(*node_type));

			ASSERT(node_type->kind != TYPE_UNKNOWN);
			if (node->child[1].value != 0) {
				ast_node *initializer = get_node(pool, node->child[1]);
				if (initializer->kind == AST_INIT) {
					type *initializer_type = get_type(pool, node->child[1]);
					memcpy(initializer_type, node_type, sizeof(*node_type));
				}

				check_type(pool, node->child[1], arena);
			}
		} break;
	case AST_TYPE_BASIC:
		switch (node->token.kind) {
		case TOKEN_VOID:
			node_type->kind = TYPE_VOID;
			break;
		case TOKEN_FLOAT:
		case TOKEN_DOUBLE:
			node_type->kind = TYPE_FLOAT;
			break;
		case TOKEN_CHAR:
			if (node->flags & AST_UNSIGNED) {
				node_type->kind = TYPE_CHAR_UNSIGNED;
			} else {
				node_type->kind = TYPE_CHAR;
			}

			break;
		case TOKEN_INT:
			switch (node->flags & (AST_LONG | AST_LLONG | AST_SHORT | AST_UNSIGNED)) {
			case AST_LLONG | AST_UNSIGNED:
				node_type->kind = TYPE_LLONG_UNSIGNED;
				break;
			case AST_LLONG:
				node_type->kind = TYPE_LLONG;
				break;
			case AST_LONG | AST_UNSIGNED:
				node_type->kind = TYPE_LONG_UNSIGNED;
				break;
			case AST_LONG:
				node_type->kind = TYPE_LONG;
				break;
			case AST_SHORT | AST_UNSIGNED:
				node_type->kind = TYPE_SHORT_UNSIGNED;
				break;
			case AST_SHORT:
				node_type->kind = TYPE_SHORT;
				break;
			case AST_UNSIGNED:
				node_type->kind = TYPE_INT_UNSIGNED;
				break;
			default:
				node_type->kind = TYPE_INT;
			}

			break;
		default:
			ASSERT(!"Invalid basic type");
		}

		break;
	case AST_TYPE_POINTER:
		{
			type *base_type = check_type(pool, node->child[1], arena);
			node_type->kind = TYPE_POINTER;
			node_type->base_type = base_type;
		} break;
	case AST_TYPE_ARRAY:
		{
			node_type->kind = TYPE_ARRAY;
			if (node->child[0].value != 0) {
				check_type(pool, node->child[0], arena);
				node_type->size = eval_ast(pool, node->child[0]);
			} else {
				// TODO: Evaluate the size based on the expression or error.
			}

			type *base_type = check_type(pool, node->child[1], arena);
			node_type->base_type = base_type;
		} break;
	case AST_TYPE_BITFIELD:
		{
			type *type = check_type(pool, node->child[1], arena);
			node_type->kind = TYPE_BITFIELD;
			// TODO: Evaluate the expression
			node_type->size = 1;
			node_type->base_type = type;
		} break;
	case AST_TYPE_FUNC:
		{
			node_type->kind = TYPE_FUNCTION;
			member **m = &node_type->members;
			type *return_type = check_type(pool, node->child[1], arena);

			if (node->child[0].value != 0) {
				check_type(pool, node->child[0], arena);
				ast_node *param_list = get_node(pool, node->child[0]);
				for (;;) {
					ast_node *param = get_node(pool, param_list->child[0]);
					type *param_type = get_type(pool, param_list->child[0]);
					*m = ALLOC(arena, 1, member);
					(*m)->name = param->token.value;
					(*m)->type = param_type;
					m = &(*m)->next;

					if (param_list->child[1].value == 0) {
						break;
					}

					param_list = get_node(pool, param_list->child[1]);
				}
			}

			node_type->base_type = return_type;
		} break;
	case AST_TYPE_IDENT:
		{
			ASSERT(node->child[0].value != 0);
			type *ref_type = get_type(pool, node->child[0]);
			memcpy(node_type, ref_type, sizeof(*node_type));
		} break;
	case AST_ENUMERATOR:
		{
			node_type->kind = TYPE_INT;
		} break;
	case AST_TYPE_ENUM:
		{
			node_type->kind = TYPE_INT;

			ast_id enum_id = node->child[0];
			i32 value = 0;
			while (enum_id.value != 0) {
				ast_node *enum_node = get_node(pool, enum_id);
				type *enum_type = get_type(pool, enum_id);
				if (enum_node->child[0].value != 0) {
					value = eval_ast(pool, enum_node->child[0]);
				}

				// TODO: Should we replace the whole enumerator or just the
				// underlying value. Modifying just the value would likely be
				// easier for error handling...
				enum_node->kind = AST_EXPR_LITERAL;
				// TODO: Set the value of the enumerator
				(void)value;
				enum_type->kind = TYPE_INT;
				enum_id = enum_node->child[1];
			}
		} break;
	case AST_TYPE_STRUCT:
	case AST_TYPE_UNION:
		{
			// TODO: add the struct tag to the scope and ensure that the
			// struct is only defined once.
			if (node->child[0].value == 0) {
				type *ref_type = get_type(pool, node->child[0]);
				if (node->flags & AST_OPAQUE) {
					ref_type->kind = TYPE_STRUCT;
					node_type->kind = TYPE_OPAQUE;
					node_type->base_type = ref_type;
				} else {
					memcpy(node_type, ref_type, sizeof(*node_type));
					ASSERT(node_type->kind == TYPE_STRUCT
						|| node_type->kind == TYPE_UNION);
				}
			} else {
				if (node_type->kind == TYPE_UNKNOWN) {
					node_type->kind = TYPE_STRUCT;
				}

				node_type->kind = TYPE_STRUCT;
				if (node->kind == AST_TYPE_UNION) {
					node_type->kind = TYPE_UNION;
				}

				// TODO: Collect the members of the struct
				ASSERT(node_type->members == NULL);
				member **ptr = &node_type->members;
				ast_id decl_id = node->child[0];
				while (decl_id.value != 0) {
					check_type(pool, decl_id, arena);
					ast_node *list_node = get_node_of_kind(pool, decl_id, AST_LIST);
					decl_id = list_node->child[1];

					ast_node *decl_node = get_node(pool, list_node->child[0]);
					type *decl_type = get_type(pool, list_node->child[0]);
					*ptr = ALLOC(arena, 1, member);
					(*ptr)->name = decl_node->token.value;
					(*ptr)->type = decl_type;
					ptr = &(*ptr)->next;
				}

				ASSERT(node->token.value.at != NULL || node_type->members != NULL);
			}
		} break;
	}

	return node_type;
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
	case AST_LIST:
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
			errorf(node->token.loc, "case outside switch");
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
			errorf(node->token.loc, "default outside switch");
		} else {
			switch_symbol *switch_sym = get_switch_symbol(symtab, switch_id);
			if (switch_sym->default_case.value != 0) {
				errorf(node->token.loc, "Duplicate default label");
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
			if (node->token.value.at == NULL || node->child[0].value == 0) {
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
		case AST_EXPR_LITERAL:
			if (node->token.kind == TOKEN_LITERAL_STRING) {
				symtab.symbols[i].value = symtab.string_count++;
				symtab.kind[i] = SYM_STRING;
			}
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

						if (equals(label_node->token.value, node->token.value)) {
							errorf(node->token.loc, "Label was already defined");
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
					if (equals(label_node->token.value, node->token.value)) {
						symtab.symbols[j] = symtab.symbols[label_id[k].value];
						break;
					}
				}

				if (symtab.symbols[j].value == 0) {
					errorf(node->token.loc, "No label was found");
				}
			}
		}

		arena_temp_end(temp);
	}

	// NOTE: Collect all strings.
	for (isize i = 1; i < pool->size; i++) {
		ast_node *node = &pool->nodes[i];
		if (node->token.kind == TOKEN_LITERAL_STRING) {
			str escaped = node->token.value;
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
			type *node_type = get_type(pool, node_id);
			ASSERT(node_type->kind != TYPE_UNKNOWN);

			decl_symbol *sym = get_decl_symbol(symtab, node_id);
			sym->name = node->token.value;
			sym->type = node_type;
			sym->is_global = true;
			sym->is_function = (node_type->kind == TYPE_FUNCTION);
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
