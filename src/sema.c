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
eval_ast(syntax_pool *pool, syntax_id node_id)
{
	i64 result = 0;

	syntax_id children[3] = {0};
	get_children(pool, node_id, children, LENGTH(children));

	syntax_node node = get_node(pool, node_id);
	switch (node.kind) {
	case SYNTAX_EXPR_LITERAL:
		{
			switch (node.token.kind) {
			case TOKEN_LITERAL_INT:
				result = parse_i64(node.token.value);
				break;
			default:
				ASSERT(!"TODO");
			}
		} break;
	case SYNTAX_EXPR_BINARY:
		{
			i64 lhs = eval_ast(pool, children[0]);
			i64 rhs = eval_ast(pool, children[1]);
			switch (node.token.kind) {
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
	case SYNTAX_EXPR_UNARY:
		{
			switch (node.token.kind) {
			case TOKEN_MINUS: result = -result; break;
			case TOKEN_BANG:  result = !result; break;
			case TOKEN_TILDE: result = ~result; break;
			case TOKEN_PLUS:
				break;
			default:
				ASSERT(!"Invalid operator");
			}
		} break;
	case SYNTAX_EXPR_TERNARY:
		{
			syntax_id cond = children[0];
			syntax_id lhs = children[1];
			syntax_id rhs = children[2];

			if (eval_ast(pool, cond)) {
				result = eval_ast(pool, lhs);
			} else {
				result = eval_ast(pool, rhs);
			}
		} break;
	case SYNTAX_EXPR_SIZEOF:
		{
			type_id type = find_type_id(pool, children[0]);
			result = get_node_size(pool, type);
		} break;
	case SYNTAX_EXPR_CAST:
		{
			// TODO: Implement casting behavior
			result = eval_ast(pool, children[1]);
		} break;
	case SYNTAX_EXPR_IDENT:
		{
			// TODO: Evaluate the constant definition for this identifier
			ASSERT(!"TODO");
		} break;
	default:
		ASSERT(!"Invalid node");
	}

	return result;
}

// Returns node with the same structure or creates it if `node_id == 0`
static syntax_id
intern_node(sema_context ctx, syntax_node node, syntax_id node_id)
{
	syntax_pool *pool = ctx.ast;
	scope scope = ctx.scope;

	syntax_id child_id = node.children;
	b32 is_ident = false;
	b32 is_tag = false;

	if (node_id.value != 0) {
		syntax_id intern_id = pool->nodes[node_id.value].info.ref;
		if (intern_id.value != 0) {
			return intern_id;
		}
	}

	// hash the node and intern its children
	u64 h = HASH_INIT;
	switch (node.kind) {
	case SYNTAX_TYPE_ARRAY:
		{
			hash(&h, &node.kind, sizeof(node.kind));
			hash(&h, &node.flags, sizeof(node.flags));

			syntax_id subtype_id = node.children;
			syntax_node subtype = get_node(pool, subtype_id);
			syntax_id intern_id = intern_node(ctx, subtype, subtype_id);
			hash(&h, &intern_id, sizeof(intern_id));

			syntax_id count_id = subtype.next;
			i64 count = eval_ast(pool, count_id);
			hash(&h, &count, sizeof(count));
		} break;
	case SYNTAX_TYPE_BITFIELD:
	case SYNTAX_TYPE_FUNC:
	case SYNTAX_TYPE_POINTER:
		hash(&h, &node.kind, sizeof(node.kind));
		hash(&h, &node.flags, sizeof(node.flags));
		while (child_id.value != 0) {
			syntax_node child = get_node(pool, child_id);
			syntax_id intern_id = intern_node(ctx, child, child_id);
			hash(&h, &intern_id, sizeof(intern_id));
			child_id = child.next;
		}

		break;
	case SYNTAX_STMT_LABEL:
		hash(&h, "label#", 6);
		hash(&h, node.token.value.at, node.token.value.length);
		break;
	case SYNTAX_TYPE_STRUCT:
	case SYNTAX_TYPE_UNION:
	case SYNTAX_TYPE_ENUM:
		is_tag = true;
		hash(&h, "tag#", 4);
		hash(&h, node.token.value.at, node.token.value.length);
		break;
	case SYNTAX_DECL:
	case SYNTAX_TYPE_IDENT:
	case SYNTAX_EXPR_IDENT:
		is_ident = true;
		hash(&h, node.token.value.at, node.token.value.length);
		break;
	case SYNTAX_TYPE_BASIC:
		hash(&h, &node.token.kind, sizeof(node.token.kind));
		break;
	default:
		ASSERT(!"Invalid node for interning");
		return syntax_nil;
	}

	isize dead_index = -1;
	for (isize j = 0; j < scope.max_size; j++) {
		// TODO: Use power of two size for hash scope
		isize i = (h + j) % scope.max_size;
		scope_entry entry = scope.at[i];

		b32 is_tombstone = (entry.depth > scope.depth || scope.ages[entry.depth - 1] > entry.age);
		if ((is_ident || is_tag) && is_tombstone) {
			dead_index = i;
			continue;
		}

		b32 is_empty = (entry.node_id.value == 0);
		if (is_empty) {
			if (node_id.value == 0) {
				node_id = new_node_with_flags(pool,
					node.kind, node.flags, node.token, node.children);
			}

			if (dead_index >= 0) {
				i = dead_index;
			}

			scope.size++;
			scope.at[i].node_id = node_id;
			scope.at[i].age = scope.ages[scope.depth - 1];
			scope.at[i].depth = scope.depth;
			break;
		}

		b32 are_equal = false;
		syntax_node intern_node = get_node(pool, entry.node_id);
		if (is_ident) {
			are_equal = (node.token.kind == intern_node.token.kind
				&& equals(node.token.value, intern_node.token.value));
		} else if (is_tag) {
			b32 is_compound_type = (intern_node.kind == SYNTAX_TYPE_STRUCT
				|| intern_node.kind == SYNTAX_TYPE_UNION);
			are_equal = (is_compound_type
				&& equals(node.token.value, intern_node.token.value));
		} else if (node.kind == SYNTAX_STMT_LABEL) {
			are_equal = (intern_node.kind == SYNTAX_STMT_LABEL
				&& equals(node.token.value, intern_node.token.value));
		} else {
			are_equal = (node.kind == intern_node.kind);

			syntax_id child_id = node.children;
			syntax_id intern_child_id = intern_node.children;
			while (are_equal && child_id.value != 0 && intern_child_id.value != 0) {
				type_id child_type = get_type_id(pool, intern_child_id);
				type_id intern_child_type = get_type_id(pool, child_id);
				if (child_type.value != intern_child_type.value) {
					are_equal = false;
					break;
				}

				intern_child_id = get_node(pool, intern_child_id).next;
				child_id = get_node(pool, child_id).next;
			}

			if (child_id.value != 0 || intern_child_id.value != 0) {
				are_equal = false;
			}
		}

		if (are_equal) {
			node_id = scope.at[i].node_id;
			break;
		}
	}

	return node_id;
}

static b32
are_compatible(sema_context ctx, type_id lhs_id, type_id rhs_id)
{
	syntax_pool *pool = ctx.ast;

	if (lhs_id.value == 0 || rhs_id.value == 0) {
		return false;
	}

	if (lhs_id.value == rhs_id.value) {
		return true;
	}

	syntax_node lhs = get_type(pool, lhs_id);
	syntax_node rhs = get_type(pool, rhs_id);
	ASSERT(is_type(lhs.kind) && is_type(rhs.kind));

	if (is_integer(lhs.token.kind) && is_integer(rhs.token.kind)) {
		return true;
	}

	switch (lhs.kind) {
	case SYNTAX_TYPE_ARRAY:
	case SYNTAX_TYPE_POINTER:
		{
			// arrays/pointers are only compatible with other arrays/pointers.
			if (rhs.kind != SYNTAX_TYPE_POINTER && rhs.kind != SYNTAX_TYPE_ARRAY) {
				return false;
			}

			// void pointers are always compatible with other pointers
			type_id lhs_base_id = get_type_id(pool, lhs.children);
			syntax_node lhs_base = get_type(pool, lhs_base_id);
			if (lhs_base.token.kind == TOKEN_VOID) {
				return true;
			}

			type_id rhs_base_id = get_type_id(pool, rhs.children);
			syntax_node rhs_base = get_type(pool, rhs_base_id);
			if (rhs_base.token.kind == TOKEN_VOID) {
				return true;
			}

			return are_compatible(ctx, lhs_base_id, rhs_base_id);
		} break;
	case SYNTAX_TYPE_FUNC:
		{
			if (rhs.kind != SYNTAX_TYPE_FUNC) {
				return false;
			}

			syntax_id l = lhs.children;
			syntax_id r = rhs.children;
			while (l.value != 0 && r.value != 0) {
				type_id l_type = get_type_id(pool, l);
				type_id r_type = get_type_id(pool, r);
				if (!are_compatible(ctx, l_type, r_type)) {
					return false;
				}

				l = get_node(pool, l).next;
				r = get_node(pool, r).next;
			}

			return (l.value == 0 && r.value == 0);
		} break;
	case SYNTAX_TYPE_STRUCT:
	case SYNTAX_TYPE_UNION:
		// compound pool are never compatible unless they are the exact same
		// type, which is checked by comparing the IDs.
		return false;
	default:
		return (lhs.kind == rhs.kind);
	}
}

static type_id
check_node(sema_context ctx, syntax_id node_id)
{
	syntax_pool *pool = ctx.ast;

	if (pool->error) {
		type_id nil = {0};
		return nil;
	}

	syntax_node node = get_node(pool, node_id);
	type_id node_type = node.info.type;

	syntax_id children[3] = {0};
	get_children(pool, node_id, children, LENGTH(children));

	switch (node.kind) {
	case SYNTAX_INVALID:
		pool->error = true;
		break;
	case SYNTAX_NONE:
		break;
	case SYNTAX_BUILTIN:
		{
			ASSERT(!"TODO");
		} break;
	case SYNTAX_STMT_BREAK:
	case SYNTAX_STMT_CONTINUE:
	case SYNTAX_STMT_DO_WHILE:
	case SYNTAX_STMT_FOR:
	case SYNTAX_STMT_IF:
	case SYNTAX_STMT_LABEL:
	case SYNTAX_STMT_RETURN:
	case SYNTAX_STMT_WHILE:
	case SYNTAX_STMT_DECL:
		{
			syntax_id child_id = children[0];
			while (child_id.value != 0) {
				check_node(ctx, child_id);
				syntax_node child = get_node(pool, child_id);
				child_id = child.next;
			}
		} break;
	case SYNTAX_STMT_ASM:
		{
			ASSERT(node.token.kind == TOKEN_LITERAL_STRING);
		} break;
	case SYNTAX_STMT_CASE:
		{
			if (ctx.switch_id.value == 0) {
				errorf(node.token.loc, "case outside switch");
			} else {
				// Prepend current case to list in switch statement
				syntax_node switch_node = get_node(pool, ctx.switch_id);
				pool->nodes[node_id.value].info.ref = switch_node.info.ref;
				pool->nodes[ctx.switch_id.value].info.ref = node_id;
				check_node(ctx, children[0]);
			}
		} break;
	case SYNTAX_STMT_COMPOUND:
		{
			ctx.scope.ages[ctx.scope.depth++]++;

			syntax_id child_id = children[0];
			while (child_id.value != 0) {
				check_node(ctx, child_id);
				syntax_node child = get_node(pool, child_id);
				child_id = child.next;
			}
		} break;
	case SYNTAX_STMT_DEFAULT:
		{
			if (ctx.switch_id.value == 0) {
				errorf(node.token.loc, "default outside switch");
			} else {
				// Append default statement to the list in switch statement
				syntax_id *case_id = &pool->nodes[ctx.switch_id.value].info.ref;
				syntax_id prev_id = {0};
				while (case_id->value != 0) {
					prev_id = *case_id;
					case_id = &pool->nodes[case_id->value].info.ref;
				}

				if (prev_id.value != 0) {
					syntax_node prev_case = get_node(pool, prev_id);
					if (prev_case.kind == SYNTAX_STMT_DEFAULT) {
						errorf(node.token.loc, "Duplicate default label");
					}
				}

				*case_id = node_id;
				check_node(ctx, children[0]);
			}
		} break;
	case SYNTAX_STMT_GOTO:
		{
			for (label_info *label = ctx.labels; label; label = label->next) {
				if (equals(label->name, node.token.value)) {
					pool->nodes[node_id.value].info.ref = label->label_id;
					break;
				}
			}
		} break;
	case SYNTAX_STMT_SWITCH:
		{
			check_node(ctx, children[0]);

			ctx.switch_id = node_id;
			check_node(ctx, children[1]);
		} break;
	case SYNTAX_INIT:
		{
			if (node_type.value == 0) {
				ASSERT(!"Always add type before analyzing initializers");
				return node_type;
			}

			syntax_node type = get_type(pool, node_type);
			// TODO: Check the type of each field in the initializer
			if (is_compound_type(type.kind)) {
				syntax_id member_id = type.children;
				syntax_id child_id = node.children;
				while (member_id.value != 0 && child_id.value != 0) {
					type_id member_type = get_type_id(pool, member_id);
					syntax_node child = get_node(pool, child_id);
					if (child.kind == SYNTAX_INIT) {
						set_type(pool, child_id, member_type);
					}

					type_id child_type = check_node(ctx, child_id);
					// TODO: Type conversion
					if (!are_compatible(ctx, member_type, child_type)) {
						errorf(child.token.loc, "Invalid type");
					}


					member_id = get_node(pool, member_id).next;
					child_id = child.next;
				}

				if (member_id.value != 0 || child_id.value != 0) {
					errorf(node.token.loc, "Too many fields in the initializer");
				}
			} else if (type.kind == SYNTAX_TYPE_ARRAY) {
				type_id base_type = get_type_id(pool, type.children);
				syntax_id child_id = node.children;
				while (child_id.value != 0) {
					syntax_node child = get_node(pool, node_id);
					if (child.kind == SYNTAX_INIT) {
						set_type(pool, child_id, base_type);
					}

					type_id child_type = check_node(ctx, child_id);
					if (!are_compatible(ctx, base_type, child_type)) {
						errorf(node.token.loc, "Invalid array member type");
					}

					child_id = child.next;
				}
			}
		} break;
	case SYNTAX_EXPR_BINARY:
		{
			type_id lhs_id = check_node(ctx, children[0]);
			type_id rhs_id = check_node(ctx, children[1]);
			syntax_node lhs = get_type(pool, lhs_id);
			syntax_node rhs = get_type(pool, rhs_id);

			u32 operator = node.token.kind;
			switch (operator) {
			case TOKEN_EQUAL_EQUAL:
			case TOKEN_BANG_EQUAL:
			case TOKEN_LESS:
			case TOKEN_GREATER:
			case TOKEN_LESS_EQUAL:
			case TOKEN_GREATER_EQUAL:
				{
					// NOTE: Comparison operators always return integers
					syntax_node int_type = {0};
					int_type.kind = SYNTAX_TYPE_BASIC;
					int_type.token.kind = TOKEN_INT;
					node_type.value = intern_node(ctx, int_type, syntax_nil).value;
				} break;
			default:
				node_type = lhs_id;
			}

			if (operator == TOKEN_LBRACKET) {
				// NOTE: ensure that one operand is a pointer and the other one
				// is an integral type.
				if (is_pointer(lhs.kind)) {
					node_type = get_type_id(pool, lhs.children);
				} else if (is_pointer(rhs.kind)) {
					node_type = get_type_id(pool, rhs.children);
				} else {
					pool->error = true;
					errorf(node.token.loc, "Incompatible pointer");
				}
			} else if (is_integer(lhs.token.kind) && is_integer(rhs.token.kind)) {
				// Apply integer promotion
#if 0
				b32 same_sign = (lhs.flags & SYNTAX_UNSIGNED) == (rhs.flags & SYNTAX_UNSIGNED);
				if (lhs.kind == rhs.kind) {
					// do nothing
				} else if (same_sign) {
					if (lhs_type->kind < rhs_type->kind) {
						lhs_type->kind = rhs_type->kind;
					}
				} else {
					syntax_node unsigned_type = (lhs_type->kind & SYNTAX_TYPE_UNSIGNED ? lhs_type : rhs_type);
					syntax_node signed_type = (lhs_type->kind & SYNTAX_TYPE_UNSIGNED ? rhs_type : lhs_type);
					if (unsigned_type->kind > signed_type->kind) {
						signed_type->kind = unsigned_type->kind;
					} else {
						signed_type->kind |= SYNTAX_TYPE_UNSIGNED;
						unsigned_type->kind = signed_type->kind;
					}
				}
#endif
			} else {
				if (!are_compatible(ctx, lhs_id, rhs_id)) {
					errorf(node.token.loc, "Incompatible types in binary expression");
				}
			}
		} break;
	case SYNTAX_EXPR_CALL:
		{
			type_id called_type_id = check_node(ctx, node.children);
			syntax_node called_type = get_type(pool, called_type_id);
			if (called_type.kind != SYNTAX_TYPE_FUNC) {
				pool->error = true;
#if 0
				errorf(node.token.loc, "Not a function: %s", type_get_name(called->kind));
#endif
			}

			syntax_id return_type_id = called_type.children;
			syntax_node return_type = get_node(pool, called_type.children);

			syntax_id param_id = return_type.next;
			syntax_id child_id = children[1];
			while (param_id.value != 0 && child_id.value != 0) {
				type_id child_type = check_node(ctx, child_id);
				type_id param_type = find_type_id(pool, param_id);
				if (!are_compatible(ctx, param_type, child_type)) {
					syntax_node child_node = get_node(pool, child_id);
					errorf(child_node.token.loc, "Invalid parameter type");
				}

				param_id = get_node(pool, param_id).next;
				child_id = get_node(pool, child_id).next;
			}

			while (param_id.value != 0) {
				check_node(ctx, param_id);

				syntax_node param_node = get_node(pool, param_id);
				param_id = param_node.next;
			}

#if 0
			if (param_member && param_id.value == 0) {
				errorf(node.token.loc, "Too few arguments");
			} else if (!param_member && param_id.value != 0) {
				errorf(node.token.loc, "Too many arguments");
			}
#endif

			node_type.value = return_type_id.value;
		} break;
	case SYNTAX_EXPR_CAST:
		{
			type_id csyntax_type = check_node(ctx, node.children);
			node_type = csyntax_type;
			check_node(ctx, children[1]);
			// TODO: Ensure that this cast is valid.
		} break;
	case SYNTAX_EXPR_COMPOUND:
		{
			type_id expr_type = check_node(ctx, children[0]);
			set_type(pool, children[1], expr_type);
			check_node(ctx, children[1]);
			node_type = expr_type;
		} break;
	case SYNTAX_EXPR_IDENT:
		{
			syntax_id origin = intern_node(ctx, node, node_id);
			if (origin.value == node_id.value) {
				errorf(node.token.loc, "Undefined variable: %.*s",
					(int)node.token.value.length, node.token.value.at);
			} else {
				pool->nodes[node_id.value].info.ref = origin;
				node_type = find_type_id(pool, origin);
				// TODO: Ensure that declaration is compatible with this node,
				// i.e. this is not a typedef declaration.
			}
		} break;
	case SYNTAX_EXPR_LITERAL:
		{
			syntax_node tmp = {0};
			tmp.kind = SYNTAX_TYPE_BASIC;
			switch (node.token.kind) {
			case TOKEN_LITERAL_INT:
				tmp.token.kind = TOKEN_INT;
				break;
			case TOKEN_LITERAL_CHAR:
			case TOKEN_LITERAL_STRING:
				tmp.token.kind = TOKEN_CHAR;
				break;
			case TOKEN_LITERAL_FLOAT:
				tmp.token.kind = TOKEN_FLOAT;
				break;
			default:
				ASSERT(!"Invalid literal");
			}

			syntax_id intern_id = intern_node(ctx, tmp, syntax_nil);
			if (node.token.kind == TOKEN_LITERAL_STRING) {
				tmp.kind = SYNTAX_TYPE_POINTER;
				tmp.token.kind = TOKEN_STAR;
				tmp.children = intern_id;
				intern_id = intern_node(ctx, tmp, syntax_nil);
			}

			node_type.value = intern_id.value;
		} break;
	case SYNTAX_EXPR_MEMBER:
	case SYNTAX_EXPR_MEMBER_PTR:
		{
			type_id operand_type_id = check_node(ctx, node.children);
			syntax_node operand_type = get_type(pool, operand_type_id);
			if (node.kind == SYNTAX_EXPR_MEMBER_PTR) {
				if (operand_type.kind != SYNTAX_TYPE_POINTER) {
					errorf(node.token.loc, "Left-hand side is not a pointer");
					pool->error = true;
				}

				operand_type = get_node(pool, operand_type.children);
			}

			// TODO: Handle opaque types

			if (!is_compound_type(operand_type.kind)) {
				errorf(node.token.loc, "Left-hand side is not a struct");
				pool->error = true;
			}

			str member_name = node.token.value;
			syntax_id member_id = get_member(pool, operand_type_id, member_name);
			if (member_id.value != 0) {
				node_type = find_type_id(pool, member_id);
			} else {
				errorf(node.token.loc, "Member '%.*s' does not exist",
					member_name.length, member_name.at);
			}
		} break;
	case SYNTAX_EXPR_SIZEOF:
		{
			ASSERT(!"TODO: Construct integer type");
		} break;
	case SYNTAX_EXPR_POSTFIX:
	case SYNTAX_EXPR_UNARY:
		{
			type_id operand_id = check_node(ctx, node.children);
			syntax_node operand = get_type(pool, operand_id);
			switch (node.token.kind) {
			case TOKEN_STAR:
				if (operand.kind == SYNTAX_TYPE_POINTER) {
					node_type = get_type_id(pool, operand.children);
				} else {
					pool->error = true;
					errorf(node.token.loc, "Expected pointer type");
				}
				break;
			case TOKEN_AMP:
				{
					syntax_node type = {0};
					type.kind = SYNTAX_TYPE_POINTER;
					type.children.value = operand_id.value;
					node_type.value = intern_node(ctx, type, syntax_nil).value;
				} break;
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
	case SYNTAX_EXPR_TERNARY:
		{
			type_id cond_type_id = check_node(ctx, children[0]);
			syntax_node cond_type = get_type(pool, cond_type_id);
			if (!is_integer(cond_type.token.kind)) {
				syntax_node cond_node = get_node(pool, node.children);
				errorf(cond_node.token.loc, "Not an integer expression");
			}

			type_id lhs = check_node(ctx, children[1]);
			type_id rhs = check_node(ctx, children[2]);
			if (!are_compatible(ctx, lhs, rhs)) {
				syntax_node lhs_node = get_node(pool, children[1]);
				location loc = lhs_node.token.loc;
				errorf(loc, "Invalid type in ternary expression");
			}

			node_type = lhs;
		} break;
	case SYNTAX_DECL:
		{
			b32 has_definition = (children[1].value != 0);
			node_type = check_node(ctx, children[0]);
			ASSERT(node_type.value != 0);

			syntax_id intern_id = intern_node(ctx, node, node_id);
			pool->nodes[node_id.value].info.ref = intern_id;
			if (intern_id.value != node_id.value) {
				// TODO: Ensure that the type of the old declaration is
				// compatible with the current declaration.
				syntax_id intern_children[2];
				get_children(pool, intern_id, intern_children, 2);
				if (has_definition && intern_children[1].value != 0) {
					errorf(node.token.loc, "Variables was previously defined");
				}
			}

			if (has_definition) {
				syntax_node init = get_node(pool, children[1]);
				if (init.kind == SYNTAX_INIT) {
					set_type(pool, children[1], node_type);
				}

				syntax_node type = get_node(pool, children[0]);
				if (type.kind == SYNTAX_TYPE_FUNC) {
					// Trick to reintroduce the scope of the parameters that
					// was introduced by checking the function type.
					ctx.scope.ages[ctx.scope.depth++]--;
				}

				check_node(ctx, children[1]);
			}

			// TODO: Unify the references nodes
		} break;
	case SYNTAX_TYPE_ARRAY:
	case SYNTAX_TYPE_BASIC:
	case SYNTAX_TYPE_BITFIELD:
	case SYNTAX_TYPE_IDENT:
	case SYNTAX_TYPE_POINTER:
		{
			if (children[0].value != 0) {
				check_node(ctx, node.children);
			}

			if (node.kind == SYNTAX_TYPE_ARRAY) {
				i64 length = eval_ast(pool, children[1]);
				pool->nodes[children[1].value].info.i = length;
			}

			syntax_id intern_id = intern_node(ctx, node, node_id);
			node_type.value = intern_id.value;
		} break;
	case SYNTAX_TYPE_FUNC:
	case SYNTAX_TYPE_STRUCT:
	case SYNTAX_TYPE_UNION:
		{
			ctx.scope.ages[ctx.scope.depth++]++;

			syntax_id child_id = node.children;
			while (child_id.value != 0) {
				check_node(ctx, child_id);
				child_id = get_node(pool, child_id).next;
			}

			// Close the previous scope, so the struct/union tag is inserted in
			// the correct scope and not with the members of the struct.
			ctx.scope.depth--;
			syntax_id intern_id = intern_node(ctx, node, node_id);
			node_type.value = intern_id.value;
		} break;
	case SYNTAX_TYPE_ENUM:
		{
			ASSERT(!"TODO: Construct integer type");
		} break;
	case SYNTAX_ENUMERATOR:
		{
			syntax_node tmp = {0};
			tmp.kind = SYNTAX_TYPE_BASIC;
			tmp.token.kind = TOKEN_INT;

			syntax_id int_id = intern_node(ctx, tmp, syntax_nil);
			node_type.value = int_id.value;

			syntax_id enum_id = node.children;
			i32 value = 0;
			while (enum_id.value != 0) {
				syntax_node enum_node = get_node(pool, enum_id);
				if (enum_node.children.value != 0) {
					value = eval_ast(pool, enum_node.children);
				}

				// TODO: Should we replace the whole enumerator or just the
				// underlying value. Modifying just the value would likely be
				// easier for error handling...
				enum_node.kind = SYNTAX_EXPR_LITERAL;
				// TODO: Set the value of the enumerator
				(void)value;
				set_type(pool, enum_id, node_type);
				enum_id = enum_node.next;
			}
		} break;
	}

	// Identifiers should reference their declaration, not their type
	if (node_type.value != 0 && has_type(&node))
	{
		set_type(pool, node_id, node_type);
	}

	return node_type;
}

static label_info *
get_labels(syntax_id node_id, syntax_pool *pool, label_info *labels, isize label_count)
{
	label_info *result = NULL;
	syntax_node node = get_node(pool, node_id);
	if (node.kind == SYNTAX_STMT_LABEL) {
		isize sym_id = pool->nodes[node_id.value].info.i;
		ASSERT(sym_id < label_count);

		result = &labels[sym_id];
		result->name = node.token.value;
		result->label_id = node_id;
	}

	syntax_id child_id = node.children;
	while (child_id.value != 0) {
		label_info *list = get_labels(child_id, pool, labels, label_count);

		// Merge result and list
		label_info dummy = {0};
		label_info *tail = &dummy;
		while (result && list) {
			if (compare(result->name, list->name) < 0) {
				tail->next = result;
				result->next = result;
			} else if (compare(result->name, list->name)) {
				tail->next = list;
				list->next = list;
			} else {
				ASSERT(!"Label defined twice!");
			}
		}

		tail->next = result ? result : list;
		result = dummy.next;

		syntax_node child = get_node(pool, child_id);
		child_id = child.next;
	}

	return result;
}

static isize
get_max_depth(syntax_pool *pool, syntax_id node_id)
{
	isize result = 0;

	if (node_id.value != 0) {
		syntax_id child_id = get_node(pool, node_id).children;
		while (child_id.value != 0) {
			isize depth = get_max_depth(pool, child_id);
			if (depth > result) {
				result = depth;
			}

			child_id = get_node(pool, child_id).next;
		}

		result += 1;
	}

	return result;
}

static void
check(syntax_pool *pool, arena *perm)
{
	arena_temp temp = arena_temp_begin(perm);
	sema_context ctx = {0};
	ctx.ast = pool;
	ctx.arena = perm;
	ctx.scope.depth = 1;
	ctx.scope.max_size = 2 * pool->size;
	ctx.scope.max_depth = get_max_depth(pool, pool->root);
	ctx.scope.at = ALLOC(perm, ctx.scope.max_size, scope_entry);
	ctx.scope.ages = ALLOC(perm, ctx.scope.max_depth, i32);

	syntax_id node_id = pool->root;
	while (node_id.value != 0) {
		isize label_count = 1;
		for (isize i = 0; i < pool->size; i++) {
			syntax_node node = pool->nodes[i];
			if (node.kind == SYNTAX_STMT_LABEL) {
				pool->nodes[i].info.i = label_count++;
			}
		}

		label_info *labels = ALLOC(perm, label_count, label_info);
		ctx.labels = get_labels(node_id, pool, labels, label_count);

		check_node(ctx, node_id);
		node_id = get_node(pool, node_id).next;
	}

	arena_temp_end(temp);
}
