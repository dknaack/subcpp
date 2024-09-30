static b32
is_integer(type_id type)
{
	switch (type.value) {
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
equals_type(type *lhs, type *rhs)
{
	if (lhs == rhs) {
		return true;
	}

	if (lhs->kind != rhs->kind
		|| lhs->kind == TYPE_STRUCT
		|| lhs->base_type.value != rhs->base_type.value
		|| lhs->size != rhs->size) {
		return false;
	}

	member *l = lhs->members;
	member *r = rhs->members;
	while (l && r) {
		if (l->type.value != r->type.value) {
			return false;
		}
	}

	return l == NULL && r == NULL;
}

static b32
are_compatible(type_id lhs_id, type_id rhs_id, type_pool *pool)
{
	if (lhs_id.value == TYPE_VOID || rhs_id.value == TYPE_VOID) {
		return false;
	}

	if (lhs_id.value == rhs_id.value) {
		return true;
	}

	if (is_integer(lhs_id) && is_integer(rhs_id)) {
		return true;
	}

	type *lhs = get_type_data(pool, lhs_id);
	if (lhs->kind == TYPE_OPAQUE) {
		lhs_id = lhs->base_type;
		lhs = get_type_data(pool, lhs_id);
		ASSERT(lhs->kind != TYPE_OPAQUE);
	}

	type *rhs = get_type_data(pool, rhs_id);
	if (rhs->kind == TYPE_OPAQUE) {
		rhs_id = rhs->base_type;
		rhs = get_type_data(pool, rhs_id);
		ASSERT(rhs->kind != TYPE_OPAQUE);
	}

	member *l, *r;
	type *rhs_base, *lhs_base;
	switch (lhs->kind) {
	case TYPE_ARRAY:
	case TYPE_POINTER:
		if (rhs->kind != TYPE_ARRAY && rhs->kind != TYPE_POINTER) {
			return false;
		}

		lhs_base = get_type_data(pool, lhs->base_type);
		rhs_base = get_type_data(pool, rhs->base_type);
		if (rhs_base->kind == TYPE_VOID || lhs_base->kind == TYPE_VOID) {
			return true;
		}

		return are_compatible(lhs->base_type, rhs->base_type, pool);
	case TYPE_FUNCTION:
		if (rhs->kind != TYPE_FUNCTION) {
			return false;
		}

		l = lhs->members,
		r = rhs->members;
		while (l && r) {
			if (!are_compatible(l->type, r->type, pool)) {
				return false;
			}

			l = l->next;
			r = r->next;
		}

		return (!l && !r);
	case TYPE_STRUCT:
	case TYPE_UNION:
		return false;
	default:
		return (lhs->kind == rhs->kind);
	}
}

static type_id
intern_type(type_pool *p, type *src)
{
	// TODO: Use a hash table
	for (isize i = 1; i < p->size; i++) {
		type *dst = p->data + i;
		if (equals_type(dst, src)) {
			type_id id = {i};
			return id;
		} else if (dst->kind == TYPE_UNKNOWN) {
			break;
		}
	}

	if (p->size + 1 >= p->cap) {
		if (!p->cap) {
			p->cap = 1024;
			p->size = 1;
		}

		p->data = realloc(p->data, 2 * p->cap * sizeof(*p->data));
		memset(p->data + p->cap, 0, p->cap * sizeof(*p->data));
		p->cap *= 2;
	}

	type_id result = {p->size++};
	type *dst = &p->data[result.value];
	memcpy(dst, src, sizeof(*dst));
	return result;
}

static type_id
basic_type(type_kind kind, type_pool *pool)
{
	type type = {0};
	type.kind = kind;
	type_id result = intern_type(pool, &type);
	return result;
}

static type_id
pointer_type(type_id base_type, type_pool *pool)
{
	type type = {0};
	type.kind = TYPE_POINTER;
	type.base_type = base_type;
	type_id result = intern_type(pool, &type);
	return result;
}

static type_id
array_type(type_id base_type, i64 size, type_pool *pool)
{
	type type = {0};
	type.kind = TYPE_ARRAY;
	type.base_type = base_type;
	type.size = size;
	type_id result = intern_type(pool, &type);
	return result;
}

static type_id
bitfield_type(type_id base_type, i64 size, type_pool *pool)
{
	type type = {0};
	type.kind = TYPE_BITFIELD;
	type.base_type = base_type;
	type.size = size;
	type_id result = intern_type(pool, &type);
	return result;
}

static type_id
function_type(type_id return_type, member *params, type_pool *pool)
{
	type type = {0};
	type.kind = TYPE_FUNCTION;
	type.base_type = return_type;
	type.members = params;
	type_id result = intern_type(pool, &type);
	return result;
}

static type_id
compound_type(token_kind kind, member *members, type_pool *pool)
{
	type type = {0};
	switch (kind) {
	case TOKEN_STRUCT:
		type.kind = TYPE_STRUCT;
		break;
	case TOKEN_UNION:
		type.kind = TYPE_UNION;
		break;
	default:
		ASSERT(!"Invalid compound type");
	}

	type.members = members;
	type_id result = intern_type(pool, &type);
	return result;
}

static type_id
opaque_type(type_id ref_type, type_pool *pool)
{
	type type = {0};
	type.kind = TYPE_OPAQUE;
	type.base_type = ref_type;
	type_id result = intern_type(pool, &type);
	return result;
}

static linkage
get_linkage(ast_node_flags flags)
{
	linkage result = LINK_DEFAULT;
	if (flags & AST_EXTERN) {
		result = LINK_EXTERN;
	} else if (flags & AST_STATIC) {
		result = LINK_STATIC;
	}

	return result;
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
eval_ast(semantic_context ctx, ast_id node_id)
{
	type_pool *types = ctx.types;
	ast_pool *pool = ctx.ast;
	i64 result = 0;

	ast_id children[3] = {0};
	get_children(pool, node_id, children, LENGTH(children));

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
			i64 lhs = eval_ast(ctx, children[0]);
			i64 rhs = eval_ast(ctx, children[1]);
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
	case AST_EXPR_TERNARY:
		{
			ast_id cond = children[0];
			ast_id lhs = children[1];
			ast_id rhs = children[2];

			if (eval_ast(ctx, cond)) {
				result = eval_ast(ctx, lhs);
			} else {
				result = eval_ast(ctx, rhs);
			}
		} break;
	case AST_EXPR_SIZEOF:
		{
			type_id type = get_type_id(types, children[0]);
			result = type_sizeof(type, types);
		} break;
	case AST_EXPR_CAST:
		{
			// TODO: Implement casting behavior
			result = eval_ast(ctx, children[1]);
		} break;
	case AST_EXPR_IDENT:
		{
			result = eval_ast(ctx, children[0]);
		} break;
	default:
		ASSERT(!"Invalid node");
	}

	return result;
}

static type_id
check_type(semantic_context ctx, ast_id node_id)
{
	type_pool *types = ctx.types;
	ast_pool *pool = ctx.ast;
	arena *arena = ctx.arena;

	if (pool->error) {
		type_id nil = {0};
		return nil;
	}

	ast_node *node = get_node(pool, node_id);
	type_id node_type = get_type_id(types, node_id);
	b32 was_checked = (node->kind != AST_INIT && node_type.value != 0);
	if (was_checked) {
		return node_type;
	}

	ast_id children[3] = {0};
	get_children(pool, node_id, children, LENGTH(children));

	switch (node->kind) {
	case AST_INVALID:
		pool->error = true;
		break;
	case AST_NONE:
		break;
	case AST_BUILTIN:
		{
			ASSERT(!"TODO");
		} break;
	case AST_STMT_BREAK:
	case AST_STMT_CASE:
	case AST_STMT_COMPOUND:
	case AST_STMT_CONTINUE:
	case AST_STMT_DEFAULT:
	case AST_STMT_DO_WHILE:
	case AST_STMT_FOR:
	case AST_STMT_GOTO:
	case AST_STMT_IF:
	case AST_STMT_LABEL:
	case AST_STMT_RETURN:
	case AST_STMT_SWITCH:
	case AST_STMT_WHILE:
		{
			ast_id child_id = children[0];
			while (child_id.value != 0) {
				check_type(ctx, child_id);
				ast_node *child = get_node(pool, child_id);
				child_id = child->next;
			}
		} break;
	case AST_STMT_ASM:
		{
			ASSERT(node->token.kind == TOKEN_LITERAL_STRING);
		} break;
	case AST_INIT:
		{
			type *type_data = get_type_data(types, node_type);
			if (type_data->kind == TYPE_UNKNOWN) {
				type_id nil = {0};
				return nil;
			}

			ASSERT(is_compound_type(type_data->kind) || type_data->kind == TYPE_ARRAY);
			// TODO: Check the type of each field in the initializer
			if (is_compound_type(type_data->kind)) {
				if (type_data->kind == TYPE_OPAQUE) {
					type *type_data = get_type_data(types, node_type);
					node_type = type_data->base_type;
				}

				member *member = type_data->members;
				ast_id child = node->children;
				while (member && child.value != 0) {
					ast_node *value = get_node(pool, child);
					if (value->kind == AST_INIT) {
						set_type(types, node_id, member->type);
					}

					type_id value_type = check_type(ctx, child);
					// TODO: Type conversion
					if (!are_compatible(member->type, value_type, types)) {
						//errorf(list_node->token.loc, "Invalid type");
					}

					member = member->next;
					child = value->next;
				}

				if (member == NULL && node_id.value != 0) {
#if 0
					errorf(node->token.loc, "Too many fields in the initializer");
#endif
				}
			} else if (type_data->kind == TYPE_ARRAY) {
				type_id expected = type_data->base_type;
				node_id = node->children;
				while (node_id.value != 0) {
					ast_node *value = get_node(pool, node_id);
					if (value->kind == AST_INIT) {
						set_type(types, node_id, expected);
					}

					type_id found = check_type(ctx, node_id);
					if (!are_compatible(found, expected, types)) {
						errorf(node->token.loc, "Invalid array member type");
					}

					node_id = children[1];
				}
			}
		} break;
	case AST_EXPR_BINARY:
		{
			type_id lhs = check_type(ctx, children[0]);
			type_id rhs = check_type(ctx, children[1]);
			type *lhs_type = get_type_data(types, lhs);
			type *rhs_type = get_type_data(types, rhs);
			node_type = lhs;

			u32 operator = node->token.kind;
			if (operator == TOKEN_LBRACKET) {
				// NOTE: ensure that one operand is a pointer and the other one
				// is an integral type.
				if (is_pointer(lhs_type)) {
					node_type = lhs_type->base_type;
				} else if (is_pointer(rhs_type)) {
					node_type = rhs_type->base_type;
				} else {
					pool->error = true;
					errorf(node->token.loc, "Incompatible types: %s, %s",
						type_get_name(lhs_type->kind),
						type_get_name(rhs_type->kind));
				}
			} else if (is_integer(lhs) && is_integer(rhs)) {
				// Apply integer promotion
				b32 same_sign = (lhs_type->kind & TYPE_UNSIGNED) == (rhs_type->kind & TYPE_UNSIGNED);
				if (lhs_type->kind == rhs_type->kind) {
					// do nothing
				} else if (same_sign) {
					if (lhs_type->kind < rhs_type->kind) {
						lhs_type->kind = rhs_type->kind;
					}
				} else {
					type *unsigned_type = (lhs_type->kind & TYPE_UNSIGNED ? lhs_type : rhs_type);
					type *signed_type = (lhs_type->kind & TYPE_UNSIGNED ? rhs_type : lhs_type);
					if (unsigned_type->kind > signed_type->kind) {
						signed_type->kind = unsigned_type->kind;
					} else {
						signed_type->kind |= TYPE_UNSIGNED;
						unsigned_type->kind = signed_type->kind;
					}
				}
			} else {
				if (!are_compatible(lhs, rhs, types)) {
					errorf(node->token.loc, "Incompatible types");
				}
			}
		} break;
	case AST_EXPR_CALL:
		{
			type_id called_id = check_type(ctx, node->children);
			type *called = get_type_data(types, called_id);
			if (called->kind != TYPE_FUNCTION) {
				pool->error = true;
				errorf(node->token.loc, "Not a function: %s", type_get_name(called->kind));
			}

			member *param_member = called->members;
			ast_id param_id = children[1];
			while (param_member && param_id.value != 0) {
				ast_node *param_node = get_node(pool, param_id);
				type_id param = check_type(ctx, param_id);
				ASSERT(param_member->type.value != 0);
				if (!are_compatible(param_member->type, param, types)) {
					errorf(param_node->token.loc, "Invalid parameter type");
				}

				param_member = param_member->next;
				param_id = param_node->next;
			}

			while (param_id.value != 0) {
				check_type(ctx, param_id);

				ast_node *param_node = get_node(pool, param_id);
				param_id = param_node->next;
			}

#if 0
			if (param_member && param_id.value == 0) {
				errorf(node->token.loc, "Too few arguments");
			} else if (!param_member && param_id.value != 0) {
				errorf(node->token.loc, "Too many arguments");
			}
#endif

			type_id return_type = called->base_type;
			node_type = return_type;
		} break;
	case AST_EXPR_CAST:
		{
			type_id cast_type = check_type(ctx, node->children);
			node_type = cast_type;
			check_type(ctx, children[1]);
			// TODO: Ensure that this cast is valid.
		} break;
	case AST_EXPR_COMPOUND:
		{
			type_id expr_type = check_type(ctx, node->children);
			set_type(types, children[1], expr_type);
			check_type(ctx, children[1]);
			node_type = expr_type;
		} break;
	case AST_EXPR_IDENT:
		{
			type_id ref_type = get_type_id(types, node->children);
			node_type = ref_type;
		} break;
	case AST_EXPR_LITERAL:
		{
			type_id base_type = {0};
			switch (node->token.kind) {
			case TOKEN_LITERAL_INT:
				// TODO: If zero, set type to zero
				node_type = basic_type(TYPE_INT, types);
				break;
			case TOKEN_LITERAL_CHAR:
				node_type = basic_type(TYPE_CHAR, types);
				break;
			case TOKEN_LITERAL_FLOAT:
				node_type = basic_type(TYPE_FLOAT, types);
				break;
			case TOKEN_LITERAL_STRING:
				base_type = basic_type(TYPE_CHAR, types);
				node_type = pointer_type(base_type, types);
				break;
			default:
				ASSERT(!"Invalid literal");
			}
		} break;
	case AST_EXPR_MEMBER:
	case AST_EXPR_MEMBER_PTR:
		{
			type_id operand_id = check_type(ctx, node->children);
			type *operand = get_type_data(types, operand_id);
			if (node->kind == AST_EXPR_MEMBER_PTR) {
				if (operand->kind != TYPE_POINTER) {
					errorf(node->token.loc, "Left-hand side is not a pointer");
					pool->error = true;
				}

				operand = get_type_data(types, operand->base_type);
			}

			if (operand->kind == TYPE_OPAQUE) {
				operand = get_type_data(types, operand->base_type);
			}

			if (operand->kind != TYPE_STRUCT && operand->kind != TYPE_UNION) {
				errorf(node->token.loc, "Left-hand side is not a struct");
				pool->error = true;
			}

			str member_name = node->token.value;
			member *s = get_member(operand->members, member_name);
			if (s) {
				node_type = s->type;
			} else {
				errorf(node->token.loc, "Member '%.*s' does not exist",
					member_name.length, member_name.at);
			}
		} break;
	case AST_EXPR_SIZEOF:
		{
			node_type = basic_type(TYPE_INT, types);
		} break;
	case AST_EXPR_POSTFIX:
	case AST_EXPR_UNARY:
		{
			type_id operand_id = check_type(ctx, node->children);
			type *operand = get_type_data(types, operand_id);
			switch (node->token.kind) {
			case TOKEN_STAR:
				if (operand->kind == TYPE_POINTER) {
					node_type = operand->base_type;
				} else {
					pool->error = true;
					errorf(node->token.loc, "Expected pointer type");
				}
				break;
			case TOKEN_AMP:
				node_type = pointer_type(operand_id, types);
				break;
			case TOKEN_BANG:
			case TOKEN_PLUS:
			case TOKEN_MINUS:
			case TOKEN_TILDE:
				// TODO: ensure that type is integer
				node_type = operand_id;
				break;
			case TOKEN_PLUS_PLUS:
			case TOKEN_MINUS_MINUS:
				// TODO: ensure that type is integer or pointer
				node_type = operand_id;
				break;
			default:
				ASSERT(!"Invalid operator");
				break;
			}
		} break;
	case AST_EXPR_TERNARY:
		{
			type_id cond_id = check_type(ctx, children[0]);
			if (!is_integer(cond_id)) {
				ast_node *cond_node = get_node(pool, node->children);
				errorf(cond_node->token.loc, "Not an integer expression");
			}

			type_id lhs = check_type(ctx, children[1]);
			type_id rhs = check_type(ctx, children[2]);
			if (!are_compatible(lhs, rhs, types)) {
				ast_node *lhs_node = get_node(pool, children[1]);
				location loc = lhs_node->token.loc;
				errorf(loc, "Invalid type in ternary expression");
			}

			node_type = lhs;
		} break;
	case AST_DECL:
	case AST_EXTERN_DEF:
		{
			type_id decl_type = check_type(ctx, node->children);
			node_type = decl_type;

			ASSERT(node_type.value != 0);
			if (children[1].value != 0) {
				ast_node *initializer = get_node(pool, children[1]);
				if (initializer->kind == AST_INIT) {
					set_type(types, children[1], node_type);
				}

				check_type(ctx, children[1]);
			}
		} break;
	case AST_TYPE_BASIC:
		switch (node->token.kind) {
		case TOKEN_BUILTIN_VA_LIST:
			node_type = basic_type(TYPE_BUILTIN_VA_LIST, types);
			break;
		case TOKEN_VOID:
			node_type = basic_type(TYPE_VOID, types);
			break;
		case TOKEN_FLOAT:
		case TOKEN_DOUBLE:
			node_type = basic_type(TYPE_FLOAT, types);
			break;
		case TOKEN_CHAR:
			if (node->flags & AST_UNSIGNED) {
				node_type = basic_type(TYPE_CHAR_UNSIGNED, types);
			} else {
				node_type = basic_type(TYPE_CHAR, types);
			}

			break;
		case TOKEN_INT:
			switch (node->flags & (AST_LONG | AST_LLONG | AST_SHORT | AST_UNSIGNED)) {
			case AST_LLONG | AST_UNSIGNED:
				node_type = basic_type(TYPE_LLONG_UNSIGNED, types);
				break;
			case AST_LLONG:
				node_type = basic_type(TYPE_LLONG, types);
				break;
			case AST_LONG | AST_UNSIGNED:
				node_type = basic_type(TYPE_LONG_UNSIGNED, types);
				break;
			case AST_LONG:
				node_type = basic_type(TYPE_LONG, types);
				break;
			case AST_SHORT | AST_UNSIGNED:
				node_type = basic_type(TYPE_SHORT_UNSIGNED, types);
				break;
			case AST_SHORT:
				node_type = basic_type(TYPE_SHORT, types);
				break;
			case AST_UNSIGNED:
				node_type = basic_type(TYPE_INT_UNSIGNED, types);
				break;
			default:
				node_type = basic_type(TYPE_INT, types);
			}

			break;
		default:
			ASSERT(!"Invalid basic type");
		}

		break;
	case AST_TYPE_POINTER:
		{
			type_id base_type = check_type(ctx, children[0]);
			node_type = pointer_type(base_type, types);
		} break;
	case AST_TYPE_ARRAY:
		{
			i64 size = 0;
			if (children[1].value != 0) {
				check_type(ctx, children[1]);
				size = eval_ast(ctx, children[1]);
			} else {
				// TODO: Evaluate the size based on the expression or error.
			}

			type_id base_type = check_type(ctx, children[0]);
			if (base_type.value == TYPE_VOID) {
				errorf(node->token.loc, "array of voids");
			}

			node_type = array_type(base_type, size, types);
		} break;
	case AST_TYPE_BITFIELD:
		{
			type_id base_type = check_type(ctx, children[0]);
			// TODO: Evaluate the expression
			node_type = bitfield_type(base_type, 1, types);
		} break;
	case AST_TYPE_FUNC:
		{
			type_id return_type = check_type(ctx, children[0]);

			member *params = NULL;
			member **m = &params;
			ast_id param_id = children[1];
			while (param_id.value != 0) {
				ast_node *param = get_node(pool, param_id);
				type_id param_type = check_type(ctx, param_id);
				*m = ALLOC(arena, 1, member);
				(*m)->name = param->token.value;
				(*m)->type = param_type;
				ASSERT((*m)->type.value != 0);
				m = &(*m)->next;

				param_id = param->next;
			}

			node_type = function_type(return_type, params, types);
		} break;
	case AST_TYPE_IDENT:
		{
			ASSERT(node->children.value != 0);
			type_id ref_type = get_type_id(types, node->children);
			node_type = ref_type;
		} break;
	case AST_ENUMERATOR:
		{
			node_type = basic_type(TYPE_INT, types);
		} break;
	case AST_TYPE_ENUM:
		{
			node_type = basic_type(TYPE_INT, types);

			ast_id enum_id = node->children;
			i32 value = 0;
			while (enum_id.value != 0) {
				ast_node *enum_node = get_node(pool, enum_id);
				if (enum_node->children.value != 0) {
					value = eval_ast(ctx, enum_node->children);
				}

				// TODO: Should we replace the whole enumerator or just the
				// underlying value. Modifying just the value would likely be
				// easier for error handling...
				enum_node->kind = AST_EXPR_LITERAL;
				// TODO: Set the value of the enumerator
				(void)value;
				set_type(types, enum_id, basic_type(TYPE_INT, types));
				enum_id = enum_node->next;
			}
		} break;
	case AST_TYPE_TAG:
		{
			// TODO: Set this as an opaque type
			type_id base_type = check_type(ctx, children[0]);
			node_type = opaque_type(base_type, types);
		} break;
	case AST_TYPE_COMPOUND:
		{
			// NOTE: Collect the members of the struct
			member *members = NULL;
			member **ptr = &members;
			ast_id decl_id = node->children;
			while (decl_id.value != 0) {
				type_id decl_type = check_type(ctx, decl_id);
				ast_node *decl_node = get_node(pool, decl_id);

				*ptr = ALLOC(arena, 1, member);
				(*ptr)->name = decl_node->token.value;
				(*ptr)->type = decl_type;
				ptr = &(*ptr)->next;

				decl_id = decl_node->next;
			}

			ASSERT(node->token.value.at != NULL || members != NULL);
			node_type = compound_type(node->token.kind, members, types);
		} break;
	}

	set_type(types, node_id, node_type);
	return node_type;
}

static void
check_switch_stmt(ast_pool *pool, ast_id node_id, semantic_info info, ast_id switch_id, arena *perm)
{
	if (node_id.value == 0) {
		return;
	}

	ast_node *node = get_node(pool, node_id);
	switch (node->kind) {
	case AST_STMT_DO_WHILE:
	case AST_STMT_FOR:
	case AST_STMT_IF:
	case AST_STMT_WHILE:
	case AST_EXTERN_DEF:
	case AST_STMT_COMPOUND:
		while (node_id.value != 0) {
			ast_node *node = get_node(pool, node_id);
			check_switch_stmt(pool, node->children, info, switch_id, perm);
			node_id = node->next;
		}

		break;
	case AST_STMT_SWITCH:
		{
			ast_node *cond_node = get_node(pool, node->children);
			ast_id body = cond_node->next;
			check_switch_stmt(pool, body, info, node_id, perm);
		} break;
	case AST_STMT_CASE:
		if (switch_id.value == 0) {
			errorf(node->token.loc, "case outside switch");
		} else {
			switch_info *switch_sym = get_switch_info(info, switch_id);
			case_info *case_sym = get_case_info(info, node_id);
			case_sym->case_id = node_id;

			if (switch_sym->last) {
				switch_sym->last = switch_sym->last->next = case_sym;
			} else {
				switch_sym->first = switch_sym->last = case_sym;
			}

			ast_node *expr_node = get_node(pool, node->children);
			ast_id stmt = expr_node->next;
			check_switch_stmt(pool, stmt, info, switch_id, perm);
		} break;
	case AST_STMT_DEFAULT:
		if (switch_id.value == 0) {
			errorf(node->token.loc, "default outside switch");
		} else {
			switch_info *switch_sym = get_switch_info(info, switch_id);
			if (switch_sym->default_case.value != 0) {
				errorf(node->token.loc, "Duplicate default label");
			}

			switch_sym->default_case = node_id;
			check_switch_stmt(pool, node->children, info, switch_id, perm);
		} break;
	default:
		break;
	}
}

static semantic_info
check(ast_pool *pool, arena *perm)
{
	semantic_info info = {0};
	info.of = ALLOC(perm, pool->size, info_id);
	info.kind = ALLOC(perm, pool->size, info_kind);

	for (type_kind type = TYPE_VOID; type <= TYPE_DOUBLE; type++) {
		type_id type_id = basic_type(type, &info.types);
		ASSERT(type_id.value == (i32)type);
	}

	info.switch_count = 1;
	info.decl_count = 1;
	info.case_count = 1;
	info.label_count = 1;
	info.string_count = 1;

	for (isize i = 0; i < pool->size; i++) {
		ast_node *node = &pool->nodes[i];
		switch (node->kind) {
		case AST_STMT_SWITCH:
			info.of[i].value = info.switch_count++;
			info.kind[i] = INFO_SWITCH;
			break;
		case AST_STMT_CASE:
			info.of[i].value = info.case_count++;
			info.kind[i] = INFO_CASE;
			break;
		case AST_STMT_LABEL:
			info.of[i].value = info.label_count++;
			info.kind[i] = INFO_LABEL;
			break;
		case AST_TYPE_TAG:
			if (node->token.value.at == NULL || node->children.value == 0) {
				break;
			}

			/* fallthrough */
		case AST_EXTERN_DEF:
		case AST_ENUMERATOR:
		case AST_DECL:
			if (!(node->flags & AST_TYPEDEF)) {
				info.of[i].value = info.decl_count++;
				info.kind[i] = INFO_DECL;
			}

			break;
		case AST_EXPR_LITERAL:
			if (node->token.kind == TOKEN_LITERAL_STRING) {
				info.of[i].value = info.string_count++;
				info.kind[i] = INFO_STRING;
			}
			break;
		default:
			break;
		}
	}

	info.labels   = ALLOC(perm, info.label_count, u32);
	info.strings  = ALLOC(perm, info.string_count, str);
	info.decls    = ALLOC(perm, info.decl_count, decl_info);
	info.cases    = ALLOC(perm, info.case_count, case_info);
	info.switches = ALLOC(perm, info.switch_count, switch_info);

	// NOTE: Check labels, ensure that no label is defined twice and merge
	// gotos with their corresponding label.
	if (info.label_count > 1) {
		arena_temp temp = arena_temp_begin(perm);
		ast_id *label_id = ALLOC(perm, info.label_count, ast_id);
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
						info.of[j] = info.of[label_id[k].value];
						break;
					}
				}

				if (info.of[j].value == 0) {
					errorf(node->token.loc, "No label was found");
				}
			}
		}

		arena_temp_end(temp);
	}

	// NOTE: Gather symbol information
	symbol_table *symtab = &info.symtab;
	isize symbol_count = 1;
	for (isize i = 1; i < pool->size; i++) {
		ast_node *node = &pool->nodes[i];
		switch (node->kind) {
		case AST_EXTERN_DEF:
			symbol_count++;
			break;
		case AST_EXPR_LITERAL:
			if (node->token.kind == TOKEN_LITERAL_FLOAT
				|| node->token.kind == TOKEN_LITERAL_STRING)
			{
				symbol_count++;
			}

			break;
		default:
			break;
		}
	}

	symtab->symbol_count = symbol_count;
	symtab->symbols = ALLOC(perm, symbol_count, symbol);


	info.types.at = ALLOC(perm, pool->size, type_id);

	semantic_context ctx = {0};
	ctx.ast = pool;
	ctx.arena = perm;
	ctx.types = &info.types;

	ast_id node_id = pool->root;
	while (node_id.value != 0) {
		// NOTE: Collect all switch statements.
		check_switch_stmt(pool, node_id, info, ast_id_nil, perm);

		check_type(ctx, node_id);
		node_id = get_node(pool, node_id)->next;
	}

	// NOTE: Collect function definitions
	isize symbol_index = 1;
	symtab->text_offset = symbol_index;
	for (isize i = 1; i < pool->size; i++) {
		ast_node *node = &pool->nodes[i];
		ast_node *type = NULL;
		if (node->children.value != 0) {
			type = get_node(pool, node->children);
		}

		if (node->kind == AST_EXTERN_DEF
			&& !(node->flags & AST_TYPEDEF)
			&& (type && type->kind == AST_TYPE_FUNC))
		{
			info.of[i].value = symbol_index;
			symbol *sym = &symtab->symbols[symbol_index++];
			sym->linkage = get_linkage(node->flags);
			sym->name = node->token.value;
		}
	}

	// NOTE: Collect global initialized variables
	ASSERT(symbol_index > symtab->text_offset);
	symtab->data_offset = symbol_index;
	for (isize i = 1; i < pool->size; i++) {
		ast_node *node = &pool->nodes[i];
		type_id node_type = info.types.at[i];
		ast_node *type = NULL;
		if (node->children.value != 0) {
			type = get_node(pool, node->children);
		}

		if (node->kind == AST_EXTERN_DEF
			&& node->token.kind == TOKEN_IDENT
			&& !(node->flags & AST_TYPEDEF)
			&& !(type && type->kind == AST_TYPE_FUNC))
		{
			info.of[i].value = symbol_index;
			symbol *sym = &symtab->symbols[symbol_index++];
			sym->linkage = get_linkage(node->flags);
			sym->name = node->token.value;
			sym->size = type_sizeof(node_type, &info.types);
			ASSERT(sym->size > 0);

			ast_node *child = get_node(pool, node->children);
			if (child->next.value != 0) {
				sym->data = NULL; // TODO: Translate value into memory
			}
		}
	}

	// NOTE: Collect constants
	for (isize i = 1; i < pool->size; i++) {
		ast_node *node = &pool->nodes[i];
		if (node->token.kind == TOKEN_LITERAL_FLOAT) {
			ASSERT(!"TODO");
		} else if (node->token.kind == TOKEN_LITERAL_STRING) {
			str escaped = node->token.value;
			str unescaped = {0};
			unescaped.at = ALLOC(perm, escaped.length + 1, char);
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

			unescaped.at[unescaped.length++] = '\0';

			info.of[i].value = symbol_index;
			symbol *sym = &symtab->symbols[symbol_index++];
			sym->linkage = get_linkage(node->flags);
			sym->data = unescaped.at;
			sym->size = unescaped.length;
		}
	}

	// NOTE: Collect global uninitialized variables
	symtab->rodata_offset = symbol_index;
	for (isize i = 1; i < pool->size; i++) {
		ast_node *node = &pool->nodes[i];
		type_id node_type = info.types.at[i];
		ast_node *type = NULL;
		ast_node *child = NULL;
		if (node->children.value != 0) {
			type = get_node(pool, node->children);
			child = get_node(pool, node->children);
		}

		if (node->kind == AST_EXTERN_DEF
			&& !(node->flags & AST_TYPEDEF)
			&& !(type && type->kind == AST_TYPE_FUNC)
			&& !(child && child->next.value != 0))
		{
			info.of[i].value = symbol_index;
			symbol *sym = &symtab->symbols[symbol_index++];
			sym->linkage = get_linkage(node->flags);
			sym->name = node->token.value;
			sym->data = NULL; // TODO: Translate value into memory
			sym->size = type_sizeof(node_type, &info.types);
			ASSERT(sym->size > 0);
		}
	}

	symtab->bss_offset = symbol_index;

	// NOTE: Tag identifiers info with the referenced variable
	for (isize i = 1; i < pool->size; i++) {
		ast_node *node = &pool->nodes[i];
		if (node->kind == AST_EXPR_IDENT) {
			ast_id ref_node = node->children;
			info_id ref_info = info.of[ref_node.value];
			info.of[i] = ref_info;

			// NOTE: A referenced node should always exist, since we already
			// did name resolution during the parsing phase.
			ASSERT(ref_info.value != 0);
		}
	}

	return info;
}
