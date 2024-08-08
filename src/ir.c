static ir_context
ir_push_context(ir_context *parent)
{
	ir_context ctx = {0};
	memcpy(&ctx, parent, sizeof(ctx));
	return ctx;
}

static u32
new_label(ir_context *ctx)
{
	u32 result = ctx->program->label_count++;
	return result;
}

static u32
ir_emit2_type(ir_context *ctx, ir_type type, ir_opcode opcode, u32 op0, u32 op1)
{
	ASSERT(type <= IR_F64);
	ASSERT(ctx->program->inst_count <= ctx->max_inst_count);
	ir_inst *inst = &ctx->program->insts[ctx->program->inst_count++];
	inst->opcode = opcode;
	inst->type = type;
	inst->op0 = op0;
	inst->op1 = op1;
	u32 result = ctx->program->register_count++;
	if (opcode == IR_STORE) ASSERT(op1 != 0);
	return result;
}

static u32
ir_emit2(ir_context *ctx, ir_opcode opcode, u32 op0, u32 op1)
{
	u32 result = ir_emit2_type(ctx, IR_VOID, opcode, op0, op1);
	return result;
}

static u32
ir_emit1_type(ir_context *ctx, ir_type type, ir_opcode opcode, u32 op0)
{
	ASSERT(type != 0);
	u32 result = ir_emit2_type(ctx, type, opcode, op0, 0);
	return result;
}

static u32
ir_emit1(ir_context *ctx, ir_opcode opcode, u32 op0)
{
	u32 result = ir_emit2(ctx, opcode, op0, 0);
	return result;
}

static u32
ir_emit0_type(ir_context *ctx, ir_type type, ir_opcode opcode)
{
	u32 result = ir_emit2_type(ctx, type, opcode, 0, 0);
	if (opcode == IR_VAR) ASSERT(type != IR_VOID);
	return result;
}

static u32
ir_emit_alloca(ir_context *ctx, u32 size)
{
	// TODO: alignment
	u32 result = ir_emit2_type(ctx, IR_I64, IR_ALLOC, size, ctx->stack_size);
	ctx->stack_size += size;
	return result;
}

static ir_function *
add_function(ir_context *ctx, str name, arena *perm)
{
	ir_function **ptr = &ctx->program->function_list;
	while (*ptr) {
		if (equals((*ptr)->name, name)) {
			return *ptr;
		}

		ptr = &(*ptr)->next;
	}

	if (!perm) {
		return NULL;
	}

	ctx->program->function_count++;
	*ptr = ALLOC(perm, 1, ir_function);
	(*ptr)->name = name;
	return *ptr;
}

static u32 translate_node(ir_context *ctx, ast_pool *pool, ast_id node_id, b32 is_lvalue);

static void
ir_memcpy(ir_context *ctx, u32 dst, u32 src, isize size)
{
	u32 continue_label = new_label(ctx);
	u32 break_label = new_label(ctx);

	u32 counter_reg = ir_emit0_type(ctx, IR_I64, IR_VAR);
	u32 zero = ir_emit1_type(ctx, IR_I64, IR_CONST, 0);
	u32 one = ir_emit1_type(ctx, IR_I64, IR_CONST, 1);
	ir_emit2(ctx, IR_MOV, counter_reg, zero);

	ir_emit1(ctx, IR_LABEL, continue_label);
	u32 size_reg = ir_emit1_type(ctx, IR_I64, IR_CONST, size);
	u32 comparison = ir_emit2(ctx, IR_LT, counter_reg, size_reg);
	ir_emit2(ctx, IR_JIZ, comparison, break_label);

	u32 dst_ptr = ir_emit2_type(ctx, IR_I64, IR_ADD, dst, counter_reg);
	u32 src_ptr = ir_emit2_type(ctx, IR_I64, IR_ADD, src, counter_reg);
	u32 src_byte = ir_emit1_type(ctx, IR_I8, IR_LOAD, src_ptr);
	ir_emit2_type(ctx, IR_I8, IR_STORE, dst_ptr, src_byte);

	u32 next = ir_emit2(ctx, IR_ADD, counter_reg, one);
	ir_emit2(ctx, IR_MOV, counter_reg, next);
	ir_emit1(ctx, IR_JMP, continue_label);
	ir_emit1(ctx, IR_LABEL, break_label);
}

static u32
ir_load(ir_context *ctx, u32 addr, type *type)
{
	u32 result = 0;

	if (is_compound_type(type->kind)) {
		isize size = type_sizeof(type);
		result = ir_emit_alloca(ctx, size);
		ir_memcpy(ctx, result, addr, size);
	} else {
		ir_type ir_type = ir_type_from(type);
		result = ir_emit1_type(ctx, ir_type, IR_LOAD, addr);
	}

	return result;
}

static void
ir_store(ir_context *ctx, u32 dst, u32 src, type *type)
{
	ASSERT(src != 0);
	if (is_compound_type(type->kind)) {
		isize size = type_sizeof(type);
		ir_memcpy(ctx, dst, src, size);
	} else {
		ir_type ir_type = ir_type_from(type);
		ir_emit2_type(ctx, ir_type, IR_STORE, dst, src);
	}
}

static void
ir_mov(ir_context *ctx, u32 dst, u32 src, type *type)
{
	if (is_compound_type(type->kind)) {
		isize size = type_sizeof(type);
		ir_memcpy(ctx, dst, src, size);
	} else {
		ir_type ir_type = ir_type_from(type);
		ir_emit2_type(ctx, ir_type, IR_MOV, dst, src);
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

static f64
parse_f64(str input)
{
	f64 result = 0;
	(void)input;
	return result;
}

static char
parse_char(str input)
{
	char result = 0;
	(void)input;
	return result;
}

// TODO: This only works for initializers with a correct set of braces,
// this does not work if there are no braces in the initializer, for example.
static void
translate_initializer(ir_context *ctx, ast_pool *pool, ast_id node_id, u32 result)
{
	ast_node *node = get_node(pool, node_id);
	switch (node->kind) {
	case AST_INIT:
		{
			usize offset = 0;

			node_id = node->child[0];
			while (node_id.value != 0) {
				ast_node *node = get_node(pool, node_id);
				ASSERT(node->kind == AST_LIST);

				type *child_type = get_type(pool, node->child[0]);
				isize child_align = type_alignof(child_type);
				offset = (offset + child_align - 1) & ~(child_align - 1);

				u32 offset_reg = ir_emit1_type(ctx, IR_I64, IR_CONST, offset);
				u32 addr = ir_emit2_type(ctx, IR_I64, IR_ADD, result, offset_reg);
				translate_initializer(ctx, pool, node->child[0], addr);

				isize child_size = type_sizeof(child_type);
				offset += child_size;
				node_id = node->child[1];
			}
		} break;
	default:
		{
			type *node_type = get_type(pool, node_id);
			u32 expr = translate_node(ctx, pool, node_id, false);
			ir_store(ctx, result, expr, node_type);
		} break;
	}
}

static u32
translate_node(ir_context *ctx, ast_pool *pool, ast_id node_id, b32 is_lvalue)
{
	u32 result = 0;

	ast_node *node = get_node(pool, node_id);
	switch (node->kind) {
	case AST_INVALID:
	case AST_INIT: // Should be handled in DECL
	case AST_STMT_IF2:
	case AST_STMT_FOR2:
	case AST_STMT_FOR3:
	case AST_EXPR_TERNARY2:
		{
			ASSERT(!"Invalid node");
		} break;
	case AST_EXPR_MEMBER:
	case AST_EXPR_MEMBER_PTR:
		{
			type *operand_type = get_type(pool, node->child[0]);
			if (node->kind == AST_EXPR_MEMBER_PTR) {
				operand_type = operand_type->base_type;
			}

			u32 offset = type_offsetof(operand_type, node->token.value);
			u32 offset_reg = ir_emit1_type(ctx, IR_I64, IR_CONST, offset);
			b32 base_is_lvalue = (node->kind == AST_EXPR_MEMBER_PTR);
			u32 base_reg = translate_node(ctx, pool, node->child[0], base_is_lvalue);
			result = ir_emit2(ctx, IR_ADD, base_reg, offset_reg);
			if (!is_lvalue) {
				type *node_type = get_type(pool, node_id);
				if (!is_compound_type(node_type->kind)) {
					ir_type type = ir_type_from(node_type);
					result = ir_emit1_type(ctx, type, IR_LOAD, result);
				} else {
					isize size = type_sizeof(node_type);
					u32 tmp = ir_emit_alloca(ctx, size);
					ir_memcpy(ctx, tmp, result, size);
					result = tmp;
				}
			}

			ASSERT(result != 0);
		} break;
	case AST_EXPR_BINARY:
		{
			token_kind operator = node->token.kind;
			ir_opcode opcode = IR_NOP;

			switch (operator) {
			case TOKEN_PLUS:          opcode = IR_ADD;   break;
			case TOKEN_MINUS:         opcode = IR_SUB;   break;
			case TOKEN_STAR:          opcode = IR_MUL;   break;
			case TOKEN_SLASH:         opcode = IR_DIV;   break;
			case TOKEN_PERCENT:       opcode = IR_MOD;   break;
			case TOKEN_PLUS_EQUAL:    opcode = IR_STORE; break;
			case TOKEN_MINUS_EQUAL:   opcode = IR_STORE; break;
			case TOKEN_STAR_EQUAL:    opcode = IR_STORE; break;
			case TOKEN_SLASH_EQUAL:   opcode = IR_STORE; break;
			case TOKEN_PERCENT_EQUAL: opcode = IR_STORE; break;
			case TOKEN_AMP_EQUAL:     opcode = IR_STORE; break;
			case TOKEN_BAR_EQUAL:     opcode = IR_STORE; break;
			case TOKEN_CARET_EQUAL:   opcode = IR_STORE; break;
			case TOKEN_EQUAL:         opcode = IR_STORE; break;
			case TOKEN_EQUAL_EQUAL:   opcode = IR_EQL;   break;
			case TOKEN_BANG_EQUAL:    opcode = IR_EQL;   break;
			case TOKEN_LESS:          opcode = IR_LT;    break;
			case TOKEN_GREATER:       opcode = IR_GT;    break;
			case TOKEN_RSHIFT:        opcode = IR_SHR;   break;
			case TOKEN_LSHIFT:        opcode = IR_SHL;   break;
			case TOKEN_LESS_EQUAL:    opcode = IR_LEQ;   break;
			case TOKEN_GREATER_EQUAL: opcode = IR_GEQ;   break;
			case TOKEN_LBRACKET:      opcode = IR_ADD;   break;
			case TOKEN_AMP:           opcode = IR_AND;   break;
			case TOKEN_BAR:           opcode = IR_OR;    break;
			case TOKEN_CARET:         opcode = IR_XOR;   break;
			case TOKEN_AMP_AMP:       opcode = IR_JIZ;   break;
			case TOKEN_BAR_BAR:       opcode = IR_JIZ;   break;
			default:                  ASSERT(!"Invalid operator");
			}

			type *node_type = get_type(pool, node_id);
			if (is_comparison_opcode(opcode) && (node_type->kind & TYPE_UNSIGNED)) {
				// TODO: Use switch instead of addition?
				opcode += IR_LTU - IR_LT;
			}

			if (operator == TOKEN_AMP_AMP) {
				// NOTE: Logical and operation
				u32 end_label = new_label(ctx);
				u32 zero_label = new_label(ctx);

				result = ir_emit0_type(ctx, IR_I32, IR_VAR);
				u32 lhs_reg = translate_node(ctx, pool, node->child[0], false);
				ir_emit2(ctx, IR_JIZ, lhs_reg, zero_label);

				u32 rhs_reg = translate_node(ctx, pool, node->child[1], false);
				ir_emit2(ctx, IR_JIZ, rhs_reg, zero_label);

				u32 one = ir_emit1_type(ctx, IR_I32, IR_CONST, 1);
				ir_emit2(ctx, IR_MOV, result, one);
				ir_emit1(ctx, IR_JMP, end_label);

				ir_emit1(ctx, IR_LABEL, zero_label);
				u32 zero = ir_emit1_type(ctx, IR_I32, IR_CONST, 0);
				ir_emit2(ctx, IR_MOV, result, zero);
				ir_emit1(ctx, IR_LABEL, end_label);
			} else if (operator == TOKEN_BAR_BAR) {
				// NOTE: Logical or operation
				u32 end_label = new_label(ctx);
				u32 one_label = new_label(ctx);

				result = ir_emit0_type(ctx, IR_I32, IR_VAR);
				u32 lhs_reg = translate_node(ctx, pool, node->child[0], false);
				ir_emit2(ctx, IR_JNZ, lhs_reg, one_label);

				u32 rhs_reg = translate_node(ctx, pool, node->child[1], false);
				ir_emit2(ctx, IR_JNZ, rhs_reg, one_label);

				u32 zero = ir_emit1_type(ctx, IR_I32, IR_CONST, 0);
				ir_emit2(ctx, IR_MOV, result, zero);
				ir_emit1(ctx, IR_JMP, end_label);

				ir_emit1(ctx, IR_LABEL, one_label);
				u32 one = ir_emit1_type(ctx, IR_I32, IR_CONST, 1);
				ir_emit2(ctx, IR_MOV, result, one);
				ir_emit1(ctx, IR_LABEL, end_label);
			} else if (opcode == IR_STORE) {
				// NOTE: Assignment operation
				switch (operator) {
				case TOKEN_PLUS_EQUAL:    opcode = IR_ADD; break;
				case TOKEN_MINUS_EQUAL:   opcode = IR_SUB; break;
				case TOKEN_STAR_EQUAL:    opcode = IR_MUL; break;
				case TOKEN_SLASH_EQUAL:   opcode = IR_DIV; break;
				case TOKEN_PERCENT_EQUAL: opcode = IR_MOD; break;
				case TOKEN_AMP_EQUAL:     opcode = IR_AND; break;
				case TOKEN_BAR_EQUAL:     opcode = IR_OR;  break;
				case TOKEN_CARET_EQUAL:   opcode = IR_XOR; break;
				case TOKEN_EQUAL:         opcode = IR_NOP; break;
				default:                  ASSERT(!"Invalid operator");
				}

				u32 lhs_reg = translate_node(ctx, pool, node->child[0], true);
				u32 rhs_reg = translate_node(ctx, pool, node->child[1], false);
				if (operator != TOKEN_EQUAL) {
					ir_type type = ir_type_from(node_type);
					u32 value = ir_emit1_type(ctx, type, IR_LOAD, lhs_reg);
					result = ir_emit2(ctx, opcode, value, rhs_reg);
					ir_emit2(ctx, IR_STORE, lhs_reg, result);
				} else {
					ir_store(ctx, lhs_reg, rhs_reg, node_type);
					if (!is_lvalue) {
						result = ir_load(ctx, lhs_reg, node_type);
					} else {
						result = lhs_reg;
					}
				}

				ASSERT(result != 0);
			} else if (operator == TOKEN_LBRACKET) {
				// NOTE: Array access
				u32 lhs_reg = translate_node(ctx, pool, node->child[0], false);
				u32 rhs_reg = translate_node(ctx, pool, node->child[1], false);
				isize size = type_sizeof(node_type);
				u32 size_reg = ir_emit1_type(ctx, IR_I64, IR_CONST, size);
				u32 offset = ir_emit2(ctx, IR_MUL, rhs_reg, size_reg);
				result = ir_emit2(ctx, IR_ADD, lhs_reg, offset);
				if (!is_lvalue) {
					result = ir_load(ctx, result, node_type);
				}
			} else {
				u32 lhs_reg = translate_node(ctx, pool, node->child[0], false);
				u32 rhs_reg = translate_node(ctx, pool, node->child[1], false);
				if (operator == TOKEN_BANG_EQUAL) {
					lhs_reg = ir_emit1(ctx, IR_NOT, lhs_reg);
				}

				result = ir_emit2(ctx, opcode, lhs_reg, rhs_reg);
			}

			ASSERT(result != 0);
		} break;
	case AST_EXPR_CALL:
		{
			ast_node *called = get_node(pool, node->child[0]);
			ir_opcode opcode = IR_CALL;
			u32 called_reg = translate_node(ctx, pool, node->child[0], false);

			str func_name = {0};
			if (called->kind == AST_EXPR_IDENT) {
				func_name = called->token.value;
			}

			if (starts_with(func_name, S("__builtin_")) || equals(func_name, S("asm"))) {
				// TODO: Implement translation for builtins
			} else {
				type *called_type = get_type(pool, node->child[0]);
				ASSERT(called_type->kind == TYPE_FUNCTION);

				i32 param_count = 0;
				u32 param_register[128] = {0};
				type *return_type = called_type->base_type;
				ir_type result_type = IR_I64;
				if (is_compound_type(return_type->kind)) {
					isize size = type_sizeof(return_type);
					param_register[param_count++] = ir_emit_alloca(ctx, size);
				} else {
					result_type = ir_type_from(return_type);
				}

				ast_id param_id = node->child[1];
				while (param_id.value != 0) {
					ASSERT(param_count < 128);
					ast_node *param_list = get_node(pool, param_id);
					u32 param_reg = translate_node(ctx, pool, param_list->child[0], false);
					type *param_type = get_type(pool, param_list->child[0]);
					if (is_compound_type(param_type->kind)) {
						isize size = type_sizeof(param_type);
						u32 copy = ir_emit_alloca(ctx, size);
						ir_memcpy(ctx, copy, param_reg, size);
						param_reg = copy;
					}

					param_register[param_count++ & 127] = param_reg;
					param_id = param_list->child[1];
				}

				for (i32 i = 0; i < param_count; i++) {
					ir_emit1(ctx, IR_PARAM, param_register[i]);
				}

				type *node_type = get_type(pool, node_id);
				if (node_type->kind != TYPE_VOID) {
					u32 return_reg = ir_emit2_type(ctx, result_type, opcode, called_reg, param_count);
					result = ir_emit0_type(ctx, result_type, IR_VAR);
					ir_emit2(ctx, IR_MOV, result, return_reg);
				}
			}
		} break;
	case AST_EXPR_CAST:
		{
			type *cast_type = get_type(pool, node->child[0]);
			type *expr_type = get_type(pool, node->child[0]);
			if (cast_type->kind != TYPE_VOID) {
				isize cast_size = type_sizeof(cast_type);
				isize expr_size = type_sizeof(expr_type);

				result = translate_node(ctx, pool, node->child[1], false);
				ir_type cast_ir_type = ir_type_from(cast_type);
				ir_type expr_ir_type = ir_type_from(expr_type);
				if (expr_type != cast_type) {
					if (cast_ir_type == IR_F32 || cast_ir_type == IR_F64
						|| expr_ir_type == IR_F32 || expr_ir_type == IR_F64)
					{
						// TODO: Unsigned conversion
						result = ir_emit1_type(ctx, cast_ir_type, IR_CAST, result);
					} else if (cast_size < expr_size) {
						result = ir_emit1_type(ctx, cast_ir_type, IR_TRUNC, result);
					} else if (cast_size > expr_size) {
						if (expr_type->kind & TYPE_UNSIGNED) {
							result = ir_emit1_type(ctx, cast_ir_type, IR_ZEXT, result);
						} else {
							result = ir_emit1_type(ctx, cast_ir_type, IR_SEXT, result);
						}
					}
				}

				ASSERT(result != 0);
			}
		} break;
	case AST_EXPR_COMPOUND:
		{
			type *node_type = get_type(pool, node_id);
			result = ir_emit_alloca(ctx, type_sizeof(node_type));
			translate_initializer(ctx, pool, node->child[1], result);
		} break;
	case AST_EXPR_IDENT:
		{
			if (equals(node->token.value, S("__builtin_popcount"))) {
				result = ir_emit1_type(ctx, IR_I64, IR_BUILTIN, BUILTIN_POPCOUNT);
			} else {
				result = translate_node(ctx, pool, node->child[0], is_lvalue);
			}

			ASSERT(result != 0);
		} break;
	case AST_EXPR_LITERAL:
		{
			switch (node->token.kind) {
			case TOKEN_LITERAL_STRING:
				{
					result = ir_emit1_type(ctx, IR_I64, IR_GLOBAL, node_id.value);
				} break;
			case TOKEN_LITERAL_INT:
				{
					i64 value = parse_i64(node->token.value);
					result = ir_emit1_type(ctx, IR_I32, IR_CONST, value);
				} break;
			case TOKEN_LITERAL_FLOAT:
				{
					f64 value = parse_f64(node->token.value);
					result = ir_emit1_type(ctx, IR_F32, IR_CONST, value);
				} break;
			case TOKEN_LITERAL_CHAR:
				{
					char value = parse_char(node->token.value);
					result = ir_emit1_type(ctx, IR_I8, IR_CONST, value);
				} break;
			default:
				ASSERT(!"Invalid literal");
				break;
			}
		} break;
	case AST_EXPR_SIZEOF:
		{
			// TODO: implement sizeof operator
			result = ir_emit1_type(ctx, IR_I64, IR_CONST, 8);
		} break;
	case AST_EXPR_UNARY:
		{
			token_kind operator = node->token.kind;
			switch (operator) {
			case TOKEN_AMP:
				{
					result = translate_node(ctx, pool, node->child[0], true);
				} break;
			case TOKEN_TILDE:
				{
					result = translate_node(ctx, pool, node->child[0], false);
					result = ir_emit1(ctx, IR_NOT, result);
				} break;
			case TOKEN_PLUS:
				{
					result = translate_node(ctx, pool, node->child[0], false);
				} break;
			case TOKEN_MINUS:
				{
					type *node_type = get_type(pool, node_id);
					ir_type type = ir_type_from(node_type);
					u32 zero = ir_emit1_type(ctx, type, IR_CONST, 0);
					result = translate_node(ctx, pool, node->child[0], false);
					result = ir_emit2(ctx, IR_SUB, zero, result);
				} break;
			case TOKEN_BANG:
				{
					type *node_type = get_type(pool, node_id);
					ir_type type = ir_type_from(node_type);
					u32 zero = ir_emit1_type(ctx, type, IR_CONST, 0);
					result = translate_node(ctx, pool, node->child[0], false);
					result = ir_emit2(ctx, IR_EQL, result, zero);
				} break;
			case TOKEN_STAR:
				{
					type *node_type = get_type(pool, node_id);
					result = translate_node(ctx, pool, node->child[0], false);
					if (!is_lvalue && node_type->kind != TYPE_FUNCTION) {
						result = ir_load(ctx, result, node_type);
					}
				} break;
			case TOKEN_PLUS_PLUS:
			case TOKEN_MINUS_MINUS:
				{
					ir_opcode opcode = IR_ADD;
					if (operator == TOKEN_MINUS_MINUS) {
						opcode = IR_SUB;
					}

					type *node_type = get_type(pool, node_id);
					ir_type type = ir_type_from(node_type);
					u32 addr = translate_node(ctx, pool, node->child[0], true);
					u32 value = ir_emit1_type(ctx, type, IR_LOAD, addr);
					u32 one = ir_emit1_type(ctx, type, IR_CONST, 1);
					result = ir_emit2_type(ctx, type, opcode, value, one);
					ir_emit2(ctx, IR_STORE, addr, result);
					if (is_lvalue) {
						result = addr;
					}
				} break;
			default:
				ASSERT(!"Invalid operator");
			}
		} break;
	case AST_EXPR_TERNARY1:
		{
			u32 endif_label = new_label(ctx);
			u32 else_label = new_label(ctx);

			ast_id cond = node->child[0];
			ast_node *branches = get_node(pool, node->child[1]);
			u32 cond_reg = translate_node(ctx, pool, cond, false);
			type *node_type = get_type(pool, node_id);
			if (is_compound_type(node_type->kind)) {
				result = ir_emit_alloca(ctx, type_sizeof(node_type));
			} else {
				result = ir_emit0_type(ctx, ir_type_from(node_type), IR_VAR);
			}
			ir_emit2(ctx, IR_JIZ, cond_reg, else_label);

			ast_id if_branch = branches->child[0];
			u32 if_reg = translate_node(ctx, pool, if_branch, false);
			ir_mov(ctx, result, if_reg, node_type);
			ir_emit1(ctx, IR_JMP, endif_label);

			ir_emit1(ctx, IR_LABEL, else_label);
			ast_id else_branch = branches->child[1];
			u32 else_reg = translate_node(ctx, pool, else_branch, false);
			ir_mov(ctx, result, else_reg, node_type);

			ir_emit1(ctx, IR_LABEL, endif_label);
		} break;
	case AST_EXPR_POSTFIX:
		{
			token_kind operator = node->token.kind;
			switch (operator) {
			case TOKEN_PLUS_PLUS:
			case TOKEN_MINUS_MINUS:
				{
					ir_opcode opcode = IR_ADD;
					if (operator == TOKEN_MINUS_MINUS) {
						opcode = IR_SUB;
					}

					type *node_type = get_type(pool, node_id);
					ir_type type = ir_type_from(node_type);
					u32 addr = translate_node(ctx, pool, node->child[0], true);
					result = ir_emit1_type(ctx, type, IR_LOAD, addr);
					u32 one = ir_emit1_type(ctx, type, IR_CONST, 1);
					u32 value = ir_emit2_type(ctx, type, opcode, result, one);
					ir_emit2(ctx, IR_STORE, addr, value);
					if (is_lvalue) {
						result = addr;
					}
				} break;
			default:
				ASSERT(!"Invalid postfix operator");
			}
		} break;
	case AST_STMT_BREAK:
		{
			ir_emit1(ctx, IR_JMP, ctx->break_label);
		} break;
	case AST_LIST:
		{
			ast_id child = node_id;
			while (child.value != 0) {
				ast_node *child_node = get_node(pool, child);
				translate_node(ctx, pool, child_node->child[0], false);
				child = child_node->child[1];
			}
		} break;
	case AST_STMT_CASE:
		{
			case_symbol *case_sym = get_case_symbol(*ctx->symtab, node_id);
			ir_emit1(ctx, IR_LABEL, case_sym->label);
			translate_node(ctx, pool, node->child[1], false);
		} break;
	case AST_STMT_CONTINUE:
		{
			ir_emit1(ctx, IR_JMP, ctx->continue_label);
		} break;
	case AST_STMT_DEFAULT:
		{
			// NOTE: Handled by switch statement
		} break;
	case AST_STMT_DO_WHILE:
		{
			ir_context new_ctx = ir_push_context(ctx);
			ctx = &new_ctx;

			ctx->break_label = new_label(ctx);
			ctx->continue_label = new_label(ctx);
			ast_id cond = node->child[0];
			ast_id body = node->child[1];

			ir_emit1(ctx, IR_LABEL, ctx->continue_label);
			translate_node(ctx, pool, body, false);

			u32 cond_reg = translate_node(ctx, pool, cond, false);
			ir_emit2(ctx, IR_JNZ, cond_reg, ctx->continue_label);
			ir_emit1(ctx, IR_LABEL, ctx->break_label);
		} break;
	case AST_DECL:
	case AST_EXTERN_DEF:
		{
			// do not generate code for typedef declarations
			if (node->flags & AST_TYPEDEF) {
				break;
			}

			decl_symbol *sym = get_decl_symbol(*ctx->symtab, node_id);
			symbol_id sym_id = ctx->symtab->symbols[node_id.value];
			result = ctx->symbol_registers[sym_id.value];
			b32 is_initialized = (result != 0);
			if (!is_initialized) {
				b32 is_builtin = false;
				if (sym->is_global) {
					result = ir_emit1_type(ctx, IR_I64, IR_GLOBAL, node_id.value);
					ctx->symbol_registers[sym_id.value] = sym_id.value;

					type *node_type = get_type(pool, node_id);
					if (node_type->kind == TYPE_FUNCTION && node->child[1].value != 0) {
						ast_id body = node->child[1];
						ast_node *type_node = get_node(pool, node->child[0]);
						ASSERT(type_node->kind == AST_TYPE_FUNC);

						u32 prev_label_count = ctx->program->label_count;
						u32 prev_register_count = ctx->program->register_count;
						ctx->program->label_count = 1;
						ctx->program->register_count = 0;
						ctx->stack_size = 0;

						ir_function *func = add_function(ctx, node->token.value, ctx->arena);
						func->inst_index = ctx->program->inst_count;
						ir_emit1(ctx, IR_LABEL, new_label(ctx));

						i32 param_count = 0;
						type *return_type = node_type->base_type;
						if (is_compound_type(return_type->kind)) {
							isize size = type_sizeof(return_type);
							ir_emit_alloca(ctx, size);
							param_count++;
						}

						ast_id param_id = type_node->child[0];
						while (param_id.value != 0) {
							ast_node *param_list = get_node(pool, param_id);
							symbol_id param_symbol = ctx->symtab->symbols[param_list->child[0].value];
							ASSERT(ctx->symbol_registers[param_symbol.value] == 0);
							translate_node(ctx, pool, param_list->child[0], false);

							param_id = param_list->child[1];
							param_count++;
						}

						func->parameter_count = param_count;
						if (body.value != 0) {
							translate_node(ctx, pool, body, false);
						}

						func->stack_size = ctx->stack_size;
						func->label_count = ctx->program->label_count;
						func->inst_count = ctx->program->inst_count - func->inst_index;

						u32 curr_register_count = ctx->program->register_count;
						ctx->program->label_count = MAX(func->label_count, prev_label_count);
						ctx->program->register_count = MAX(curr_register_count, prev_register_count);
					} else {
						// TODO: Initialize the variable
					}
				} else if (!is_builtin) {
					type *node_type = get_type(pool, node_id);
					isize size = type_sizeof(node_type);
					result = ir_emit_alloca(ctx, size);

					if (node_type->kind == TYPE_ARRAY) {
						u32 addr = ir_emit_alloca(ctx, 8);
						ir_emit2(ctx, IR_STORE, addr, result);
						result = addr;
					}

					ctx->symbol_registers[sym_id.value] = result;
					if (node->child[1].value != 0) {
						ast_node *expr = get_node(pool, node->child[1]);
						if (expr->kind == AST_INIT) {
							translate_initializer(ctx, pool, node->child[1], result);
						} else if (is_compound_type(node_type->kind)) {
							u32 value = translate_node(ctx, pool, node->child[1], false);
							ir_memcpy(ctx, result, value, size);
						} else {
							u32 value = translate_node(ctx, pool, node->child[1], false);
							ir_type type = ir_type_from(node_type);
							ir_emit2_type(ctx, type, IR_STORE, result, value);
						}
					}
				} else {
					result = ir_emit_alloca(ctx, 32);
				}
			}

			if (sym->is_global) {
				result = ir_emit1_type(ctx, IR_I64, IR_GLOBAL, node_id.value);
			}

			type *node_type = get_type(pool, node_id);
			b32 is_builtin = false;
			if (!is_lvalue && !is_builtin
				&& node_type->kind != TYPE_FUNCTION
				&& !is_compound_type(node_type->kind))
			{
				ir_type type = ir_type_from(node_type);
				result = ir_emit1_type(ctx, type, IR_LOAD, result);
			}

			ASSERT(result != 0);
		} break;
	case AST_STMT_EMPTY:
		break;
	case AST_STMT_FOR1:
		{
			ir_context new_ctx = ir_push_context(ctx);
			ctx = &new_ctx;

			ctx->break_label = new_label(ctx);
			ctx->continue_label = new_label(ctx);
			u32 cond_label = new_label(ctx);

			translate_node(ctx, pool, node->child[0], false);
			ir_emit1(ctx, IR_LABEL, cond_label);

			ast_node *cond = get_node(pool, node->child[1]);
			u32 cond_reg = translate_node(ctx, pool, cond->child[0], false);
			ir_emit2(ctx, IR_JIZ, cond_reg, ctx->break_label);

			ast_node *post = get_node(pool, cond->child[1]);
			ast_id body = post->child[1];
			translate_node(ctx, pool, body, false);
			ir_emit1(ctx, IR_LABEL, ctx->continue_label);

			translate_node(ctx, pool, post->child[0], false);
			ir_emit1(ctx, IR_JMP, cond_label);
			ir_emit1(ctx, IR_LABEL, ctx->break_label);
		} break;
	case AST_STMT_GOTO:
	case AST_STMT_LABEL:
		{
			ir_opcode opcode = node->kind == AST_STMT_LABEL ? IR_LABEL : IR_JMP;
			symbol_id sym_id = ctx->symtab->symbols[node_id.value];
			u32 *label = &ctx->symtab->labels[sym_id.value];
			if (!*label) {
				*label = new_label(ctx);
			}

			ir_emit1(ctx, opcode, *label);
			if (node->child[0].value != 0) {
				translate_node(ctx, pool, node->child[0], false);
			}
		} break;
	case AST_STMT_IF1:
		{
			u32 endif_label = new_label(ctx);
			u32 else_label = new_label(ctx);

			ast_id cond = node->child[0];
			ast_node *if_else = get_node(pool, node->child[1]);
			u32 cond_reg = translate_node(ctx, pool, cond, false);
			ir_emit2(ctx, IR_JIZ, cond_reg, else_label);

			ast_id if_branch = if_else->child[0];
			translate_node(ctx, pool, if_branch, false);
			ir_emit1(ctx, IR_JMP, endif_label);

			ir_emit1(ctx, IR_LABEL, else_label);
			ast_id else_branch = if_else->child[1];
			if (else_branch.value != 0) {
				translate_node(ctx, pool, else_branch, false);
			}

			ir_emit1(ctx, IR_LABEL, endif_label);
		} break;
	case AST_STMT_WHILE:
		{
			ir_context new_ctx = ir_push_context(ctx);
			ctx = &new_ctx;

			ctx->break_label = new_label(ctx);
			ctx->continue_label = new_label(ctx);

			ir_emit1(ctx, IR_LABEL, ctx->continue_label);
			ast_id cond = node->child[0];
			u32 cond_reg = translate_node(ctx, pool, cond, false);
			ir_emit2(ctx, IR_JIZ, cond_reg, ctx->break_label);

			ast_id body = node->child[1];
			translate_node(ctx, pool, body, false);
			ir_emit1(ctx, IR_JMP, ctx->continue_label);
			ir_emit1(ctx, IR_LABEL, ctx->break_label);
		} break;
	case AST_STMT_RETURN:
		{
			u32 value = 0;
			b32 returns_struct = false;
			isize struct_size = 0;
			if (node->child[0].value != 0) {
				value = translate_node(ctx, pool, node->child[0], false);
				type *value_type = get_type(pool, node->child[0]);
				returns_struct = is_compound_type(value_type->kind);
				struct_size = type_sizeof(value_type);
			}

			if (returns_struct) {
				ir_memcpy(ctx, 1, value, struct_size);
			}

			ir_emit1(ctx, IR_RET, value);
			// NOTE: For dead code elimination
			ir_emit1(ctx, IR_LABEL, new_label(ctx));
		} break;
	case AST_STMT_SWITCH:
		{
			ir_context new_ctx = ir_push_context(ctx);
			ctx = &new_ctx;

			u32 switch_reg = translate_node(ctx, pool, node->child[0], false);
			ctx->break_label = new_label(ctx);

			switch_symbol *switch_sym = get_switch_symbol(*ctx->symtab, node_id);
			for (case_symbol *case_sym = switch_sym->first; case_sym; case_sym = case_sym->next) {
				case_sym->label = new_label(ctx);

				ast_node *case_node = get_node(pool, case_sym->case_id);
				u32 case_reg = translate_node(ctx, pool, case_node->child[0], false);
				u32 cond_reg = ir_emit2(ctx, IR_EQL, switch_reg, case_reg);
				ir_emit2(ctx, IR_JNZ, cond_reg, case_sym->label);
			}

			if (switch_sym->default_case.value != 0) {
				ast_node *default_node = get_node(pool, switch_sym->default_case);
				translate_node(ctx, pool, default_node->child[0], false);
			}

			ir_emit1(ctx, IR_JMP, ctx->break_label);
			translate_node(ctx, pool, node->child[1], false);
			ir_emit1(ctx, IR_LABEL, ctx->break_label);
		} break;
	case AST_ENUMERATOR:
	case AST_TYPE_VOID:
	case AST_TYPE_CHAR:
	case AST_TYPE_INT:
	case AST_TYPE_ENUM:
	case AST_TYPE_STRUCT:
	case AST_TYPE_UNION:
	case AST_TYPE_FLOAT:
	case AST_TYPE_FUNC:
	case AST_TYPE_IDENT:
	case AST_TYPE_ARRAY:
	case AST_TYPE_POINTER:
	case AST_TYPE_BITFIELD:
		ASSERT(false);
		break;
	}

	return result;
}

static ir_opcode_info
get_opcode_info(ir_opcode opcode)
{
	ir_opcode_info info = {0};
	switch (opcode) {
	case IR_JMP:
		info.op0 = IR_OPERAND_LABEL;
		break;
	case IR_CAST:
	case IR_CASTU:
	case IR_PARAM:
	case IR_RET:
	case IR_LOAD:
	case IR_COPY:
	case IR_TRUNC:
	case IR_SEXT:
	case IR_ZEXT:
	case IR_NOT:
		info.op0 = IR_OPERAND_REG_SRC;
		break;
	case IR_MOV:
	case IR_STORE:
		info.op0 = IR_OPERAND_REG_DST;
		info.op1 = IR_OPERAND_REG_SRC;
		break;
	case IR_ADD:
	case IR_AND:
	case IR_SUB:
	case IR_MUL:
	case IR_DIV:
	case IR_MOD:
	case IR_EQL:
	case IR_LT:
	case IR_GT:
	case IR_LEQ:
	case IR_GEQ:
	case IR_LTU:
	case IR_GTU:
	case IR_LEQU:
	case IR_GEQU:
	case IR_OR:
	case IR_SHL:
	case IR_SHR:
	case IR_XOR:
		info.op0 = IR_OPERAND_REG_SRC;
		info.op1 = IR_OPERAND_REG_SRC;
		break;
	case IR_JIZ:
	case IR_JNZ:
		info.op0 = IR_OPERAND_REG_SRC;
		info.op1 = IR_OPERAND_LABEL;
		break;
	case IR_ALLOC:
		info.op0 = IR_OPERAND_CONST;
		info.op1 = IR_OPERAND_CONST;
		break;
	case IR_CONST:
	case IR_BUILTIN:
		info.op0 = IR_OPERAND_CONST;
		break;
	case IR_CALL:
		info.op0 = IR_OPERAND_REG_SRC;
		info.op1 = IR_OPERAND_CONST;
		break;
	case IR_GLOBAL:
		info.op0 = IR_OPERAND_GLOBAL;
		break;
	case IR_LABEL:
		info.op0 = IR_OPERAND_LABEL;
		break;
	case IR_NOP:
	case IR_VAR:
		break;
	}

	return info;
}

static b32
is_register_operand(ir_operand_type operand)
{
	b32 result = operand == IR_OPERAND_REG_SRC || operand == IR_OPERAND_REG_DST;
	return result;
}

static b8 *
get_toplevel_instructions(ir_function *func, ir_inst *insts, arena *arena)
{
	b8 *is_toplevel = ALLOC(arena, func->inst_count, b8);
	for (u32 i = 0; i < func->inst_count; i++) {
		is_toplevel[i] = true;
	}

	for (u32 i = 0; i < func->inst_count; i++) {
		ir_opcode_info info = get_opcode_info(insts[i].opcode);
		if (insts[i].opcode == IR_GLOBAL) {
			is_toplevel[i] = false;
		}

		if (is_register_operand(info.op0)) {
			is_toplevel[insts[i].op0] = false;
		}

		if (is_register_operand(info.op1)) {
			is_toplevel[insts[i].op1] = false;
		}
	}

	return is_toplevel;
}

static ir_program
translate(ast_pool *pool, symbol_table *symtab, arena *arena)
{
	isize max_inst_count = 1024 * 1024;
	ir_program program = {0};
	program.insts = ALLOC(arena, max_inst_count, ir_inst);
	program.register_count++;
	program.label_count++;

	ir_context ctx = {0};
	ctx.program = &program;
	ctx.max_inst_count = max_inst_count;
	ctx.arena = arena;
	ctx.symbol_registers = ALLOC(arena, symtab->decl_count, u32);
	ctx.symtab = symtab;

	translate_node(&ctx, pool, pool->root, false);

	for (ir_function *func = program.function_list; func; func = func->next) {
		ir_inst *inst = program.insts + func->inst_index;
		for (u32 i = 0; i < func->inst_count; i++) {
			if (inst[i].type != IR_VOID || inst[i].opcode == IR_CAST
				|| inst[i].opcode == IR_CASTU)
			{
				continue;
			}

			u32 op0 = inst[i].op0;
			u32 op1 = inst[i].op1;

			ir_opcode_info info = get_opcode_info(inst[i].opcode);
			if (is_register_operand(info.op0)) {
				inst[i].type = inst[op0].type;
			} else if (is_register_operand(info.op1)) {
				inst[i].type = inst[op1].type;
			}
		}
	}

	return program;
}
