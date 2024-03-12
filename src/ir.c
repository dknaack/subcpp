static u32
new_label(ir_context *ctx)
{
	u32 result = ctx->program.label_count++;
	return result;
}

static u32
ir_emit2_size(ir_context *ctx, u32 size, ir_opcode opcode, u32 op0, u32 op1)
{
	ASSERT(ctx->program.instr_count <= ctx->max_instr_count);
	ir_instr *instr = &ctx->program.instrs[ctx->program.instr_count++];
	instr->opcode = opcode;
	instr->size = size;
	instr->op0 = op0;
	instr->op1 = op1;
	u32 result = ctx->program.register_count++;
	return result;
}

static u32
ir_emit2(ir_context *ctx, ir_opcode opcode, u32 op0, u32 op1)
{
	u32 result = ir_emit2_size(ctx, 0, opcode, op0, op1);
	return result;
}

static u32
ir_emit1_size(ir_context *ctx, u32 size, ir_opcode opcode, u32 op0)
{
	ASSERT(size != 0);
	u32 result = ir_emit2_size(ctx, size, opcode, op0, 0);
	return result;
}

static u32
ir_emit1(ir_context *ctx, ir_opcode opcode, u32 op0)
{
	u32 result = ir_emit2(ctx, opcode, op0, 0);
	return result;
}

static u32
ir_emit0_size(ir_context *ctx, u32 size, ir_opcode opcode)
{
	u32 result = ir_emit2_size(ctx, size, opcode, 0, 0);
	return result;
}

static u32
ir_emit_alloca(ir_context *ctx, u32 size)
{
	// TODO: alignment
	u32 result = ir_emit2_size(ctx, 8, IR_ALLOC, size, ctx->stack_size);
	ctx->stack_size += size;
	return result;
}

static ir_function *
add_function(ir_context *ctx, str name, arena *perm)
{
	ir_function **ptr = &ctx->program.function_list;
	while (*ptr) {
		if (str_equals((*ptr)->name, name)) {
			return *ptr;
		}

		ptr = &(*ptr)->next;
	}

	if (!perm) {
		return NULL;
	}

	ctx->program.function_count++;
	*ptr = ALLOC(perm, 1, ir_function);
	(*ptr)->name = name;
	return *ptr;
}

static u32
get_function(ir_context *ctx, str ident)
{
	u32 i = 0;

	for (ir_function *f = ctx->program.function_list; f; f = f->next) {
		if (str_equals(f->name, ident)) {
			return i;
		}

		i++;
	}

	ASSERT(!"Function not defined");
	return i;
}

static u32 translate_node(ir_context *ctx, ast_node *node, b32 is_lvalue);

// TODO: This only works for initializers with a correct set of braces,
// this does not work if there are no braces in the initializer, for example.
static void
translate_initializer(ir_context *ctx, ast_node *node, u32 result)
{
	switch (node->kind) {
	case AST_INIT_LIST:
		{
			usize offset = 0;

			while (node != AST_NIL) {
				isize child_align = type_alignof(node->child[0]->type);
				offset = (offset + child_align - 1) & ~(child_align - 1);

				u32 offset_reg = ir_emit1_size(ctx, 8, IR_INT, offset);
				u32 addr = ir_emit2_size(ctx, 8, IR_SUB, result, offset_reg);
				translate_initializer(ctx, node->child[0], addr);

				isize child_size = type_sizeof(node->child[0]->type);
				offset += child_size;
				node = node->child[1];
			}
		} break;
	default:
		{
			u32 expr = translate_node(ctx, node, false);
			u32 size = type_sizeof(node->type);
			ir_emit2_size(ctx, size, IR_STORE, result, expr);
		} break;
	}
}

static u32
get_register(ir_context *ctx, ast_node *node)
{
	ASSERT(node->kind == AST_DECL || node->kind == AST_EXTERN_DEF);

	symbol_id symbol_id = node->symbol_id;
	symbol *sym = &ctx->symbol_table->symbols[symbol_id.value];
	u32 result = ctx->symbol_registers[node->symbol_id.value];
	b32 is_initialized = (result != 0);
	if (!is_initialized) {
		if (sym->is_global) {
			result = ir_emit1_size(ctx, 8, IR_GLOBAL, symbol_id.value);
			ctx->symbol_registers[node->symbol_id.value] = symbol_id.value;

			if (node->type->kind == TYPE_FUNCTION) {
				ast_node *body = node->child[1];
				ast_node *type = node->child[0];
				ASSERT(type->kind == AST_TYPE_FUNC);
				ast_node *param_list = type->child[0];

				ctx->stack_size = 0;

				u32 prev_label_count = ctx->program.label_count;
				u32 prev_register_count = ctx->program.register_count;
				ctx->program.label_count = 1;
				ctx->program.register_count = 0;

				ir_function *func = add_function(ctx, node->value.s, ctx->arena);
				func->instr_index = ctx->program.instr_count;
				ir_emit1(ctx, IR_LABEL, new_label(ctx));

				i32 param_count = 0;
				while (param_list != AST_NIL) {
					ASSERT(ctx->symbol_registers[param_list->child[0]->symbol_id.value] == 0);
					get_register(ctx, param_list->child[0]);
					param_list = param_list->child[1];
					param_count++;
				}

				func->parameter_count = param_count;
				translate_node(ctx, body, false);
				func->stack_size = ctx->stack_size;
				func->label_count = ctx->program.label_count;
				func->instr_count = ctx->program.instr_count - func->instr_index;

				u32 curr_register_count = ctx->program.register_count;
				ctx->program.label_count = MAX(func->label_count, prev_label_count);
				ctx->program.register_count = MAX(curr_register_count, prev_register_count);
			} else {
				// TODO: Initialize the variable
			}
		} else {
			isize size = type_sizeof(node->type);
			result = ir_emit_alloca(ctx, size);
			ctx->symbol_registers[node->symbol_id.value] = result;

			if (node->type->kind == TYPE_ARRAY) {
				u32 addr = ir_emit_alloca(ctx, 8);
				ir_emit2(ctx, IR_STORE, addr, result);
				result = addr;
			}

			ast_node *expr = node->child[1];
			if (expr != AST_NIL) {
				if (expr->kind == AST_INIT_LIST) {
					translate_initializer(ctx, expr, result);
				} else {
					u32 value = translate_node(ctx, expr, false);
					ir_emit2_size(ctx, size, IR_STORE, result, value);
				}
			}

		}
	}

	if (sym->is_global) {
		result = ir_emit1_size(ctx, 8, IR_GLOBAL, symbol_id.value);
	}

	ASSERT(result != 4);
	return result;
}

static u32
translate_node(ir_context *ctx, ast_node *node, b32 is_lvalue)
{
	u32 result = 0;

	switch (node->kind) {
	case AST_INVALID:
	case AST_INIT_LIST: // Should be handled by get_register
	case AST_STMT_IF_ELSE:
	case AST_STMT_FOR_COND:
	case AST_STMT_FOR_POST:
	case AST_EXPR_LIST:
		{
			ASSERT(!"Invalid node");
		} break;
	case AST_EXPR_MEMBER:
		{
			ast_node *operand = node->child[0];
			u32 size = type_sizeof(node->type);
			u32 offset = type_offsetof(operand->type, node->value.s);
			u32 offset_reg = ir_emit1_size(ctx, 8, IR_INT, offset);
			u32 base_reg = translate_node(ctx, operand, true);
			u32 addr = ir_emit2_size(ctx, 8, IR_ADD, base_reg, offset_reg);
			result = ir_emit1_size(ctx, size, IR_LOAD, addr);
		} break;
	case AST_EXPR_BINARY:
		{
			token_kind operator = node->value.op;
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
			default:
				ASSERT(!"Invalid operator");
				break;
			}

			if (is_comparison_opcode(opcode) && (node->type->kind & TYPE_UNSIGNED)) {
				// TODO: Use switch instead of addition?
				opcode += IR_LTU - IR_LT;
			}

			ast_node *lhs = node->child[0];
			ast_node *rhs = node->child[1];
			u32 lhs_reg = 0;
			if (opcode == IR_STORE) {
				lhs_reg = translate_node(ctx, lhs, true);
			} else {
				lhs_reg = translate_node(ctx, lhs, false);
			}

			u32 size = type_sizeof(node->type);
			if (operator == TOKEN_AMP_AMP) {
				u32 end_label = new_label(ctx);
				u32 zero_label = new_label(ctx);

				result = ir_emit0_size(ctx, 4, IR_VAR);
				u32 lhs_reg = translate_node(ctx, lhs, false);
				ir_emit2(ctx, IR_JIZ, lhs_reg, zero_label);

				u32 rhs_reg = translate_node(ctx, rhs, false);
				ir_emit2(ctx, IR_JIZ, rhs_reg, zero_label);

				u32 one = ir_emit1_size(ctx, 4, IR_INT, 1);
				ir_emit2(ctx, IR_MOV, result, one);
				ir_emit1(ctx, IR_JMP, end_label);

				ir_emit1(ctx, IR_LABEL, zero_label);
				u32 zero = ir_emit1_size(ctx, 4, IR_INT, 0);
				ir_emit2(ctx, IR_MOV, result, zero);
				ir_emit1(ctx, IR_LABEL, end_label);
			} else if (operator == TOKEN_BAR_BAR) {
				u32 end_label = new_label(ctx);
				u32 one_label = new_label(ctx);

				result = ir_emit0_size(ctx, 4, IR_VAR);
				u32 lhs_reg = translate_node(ctx, lhs, false);
				ir_emit2(ctx, IR_JNZ, lhs_reg, one_label);

				u32 rhs_reg = translate_node(ctx, rhs, false);
				ir_emit2(ctx, IR_JNZ, rhs_reg, one_label);

				u32 zero = ir_emit1_size(ctx, 4, IR_INT, 0);
				ir_emit2(ctx, IR_MOV, result, zero);
				ir_emit1(ctx, IR_JMP, end_label);

				ir_emit1(ctx, IR_LABEL, one_label);
				u32 one = ir_emit1_size(ctx, 4, IR_INT, 1);
				ir_emit2(ctx, IR_MOV, result, one);
				ir_emit1(ctx, IR_LABEL, end_label);
			} else if (opcode == IR_STORE) {
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
				default:
					ASSERT(!"Invalid operator");
				}

				u32 rhs_reg = translate_node(ctx, rhs, false);
				if (operator != TOKEN_EQUAL) {
					u32 value = ir_emit1_size(ctx, size, IR_LOAD, lhs_reg);
					result = ir_emit2_size(ctx, size, opcode, value, rhs_reg);
					ir_emit2_size(ctx, size, IR_STORE, lhs_reg, result);
				} else {
					ir_emit2_size(ctx, size, IR_STORE, lhs_reg, rhs_reg);
				}
			} else if (operator == TOKEN_LBRACKET) {
				u32 rhs_reg = translate_node(ctx, rhs, false);
				u32 size_reg = ir_emit1_size(ctx, 8, IR_INT, size);
				rhs_reg = ir_emit2_size(ctx, 8, IR_MUL, rhs_reg, size_reg);
				result = ir_emit2_size(ctx, 8, IR_ADD, lhs_reg, rhs_reg);
				result = ir_emit1_size(ctx, size, IR_LOAD, result);
			} else {
				u32 rhs_reg = translate_node(ctx, rhs, false);
				result = ir_emit2_size(ctx, size, opcode, lhs_reg, rhs_reg);
			}
		} break;
	case AST_EXPR_CALL:
		{
			ast_node *called = node->child[0];
			u32 called_reg = translate_node(ctx, node->child[0], false);

			ast_node *param_list = node->child[1];
			i32 param_count = 0;
			u32 param_register[128] = {0};
			while (param_list != AST_NIL) {
				ASSERT(param_count < 128);
				param_register[param_count++ & 127] = translate_node(ctx, param_list->child[0], false);
				param_list = param_list->child[1];
			}

			param_list = node->child[1];
			for (i32 i = 0; i < param_count; i++) {
				u32 param_size = type_sizeof(param_list->child[0]->type);
				ir_emit1_size(ctx, param_size, IR_PARAM, param_register[i]);
				param_list = param_list->child[1];
			}

			type *return_type = called->type->children;
			u32 result_size = type_sizeof(return_type);
			result = ir_emit2_size(ctx, result_size, IR_CALL, called_reg, param_count);
		} break;
	case AST_EXPR_IDENT:
		{
			ASSERT(!"Should have been removed by merge_identifiers");
		} break;
	case AST_EXPR_INT:
		{
			result = ir_emit1_size(ctx, 4, IR_INT, node->value.i);
		} break;
	case AST_EXPR_UNARY:
		{
			u32 operator = node->value.op;
			switch (operator) {
			case TOKEN_AMP:
				{
					result = translate_node(ctx, node->child[0], true);
				} break;
			case TOKEN_PLUS:
				{
					result = translate_node(ctx, node->child[0], false);
				} break;
			case TOKEN_MINUS:
				{
					u32 size = type_sizeof(node->type);
					u32 zero = ir_emit1_size(ctx, size, IR_INT, 0);
					result = translate_node(ctx, node->child[0], false);
					result = ir_emit2_size(ctx, size, IR_SUB, zero, result);
				} break;
			case TOKEN_BANG:
				{
					u32 size = type_sizeof(node->type);
					u32 zero = ir_emit1_size(ctx, size, IR_INT, 0);
					result = translate_node(ctx, node->child[0], false);
					result = ir_emit2_size(ctx, size, IR_EQL, result, zero);
				} break;
			case TOKEN_STAR:
				{
					result = translate_node(ctx, node->child[0], false);
					if (!is_lvalue && node->type->kind != TYPE_FUNCTION) {
						u32 size = type_sizeof(node->type);
						result = ir_emit1_size(ctx, size, IR_LOAD, result);
					}
				} break;
			case TOKEN_PLUS_PLUS:
			case TOKEN_MINUS_MINUS:
				{
					ir_opcode add_or_sub = IR_ADD;
					if (operator == TOKEN_MINUS_MINUS) {
						add_or_sub = IR_SUB;
					}

					u32 size = type_sizeof(node->type);
					u32 addr = translate_node(ctx, node->child[0], true);
					u32 value = ir_emit1_size(ctx, size, IR_LOAD, addr);
					u32 one = ir_emit1_size(ctx, size, IR_INT, 1);
					result = ir_emit2_size(ctx, size, add_or_sub, value, one);
					ir_emit2(ctx, IR_STORE, addr, result);
					if (is_lvalue) {
						result = addr;
					}
				} break;
			default:
				ASSERT(!"Invalid operator");
			}
		} break;
	case AST_EXPR_POSTFIX:
		{
			u32 operator = node->value.op;
			switch (operator) {
			case TOKEN_PLUS_PLUS:
			case TOKEN_MINUS_MINUS:
				{
					ir_opcode add_or_sub = IR_ADD;
					if (operator == TOKEN_MINUS_MINUS) {
						add_or_sub = IR_SUB;
					}

					u32 size = type_sizeof(node->type);
					u32 addr = translate_node(ctx, node->child[0], true);
					result = ir_emit1_size(ctx, size, IR_LOAD, addr);
					u32 one = ir_emit1_size(ctx, size, IR_INT, 1);
					u32 value = ir_emit2_size(ctx, size, add_or_sub, result, one);
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
	case AST_DECL_LIST:
	case AST_STMT_LIST:
		{
			while (node != AST_NIL) {
				translate_node(ctx, node->child[0], false);
				node = node->child[1];
			}
		} break;
	case AST_STMT_CONTINUE:
		{
			ir_emit1(ctx, IR_JMP, ctx->continue_label);
		} break;
	case AST_STMT_DO_WHILE:
		{
			ctx->break_label = new_label(ctx);
			ctx->continue_label = new_label(ctx);
			ast_node *cond = node->child[0];
			ast_node *body = node->child[1];

			ir_emit1(ctx, IR_LABEL, ctx->continue_label);
			translate_node(ctx, body, false);

			u32 cond_reg = translate_node(ctx, cond, false);
			u32 cond_size = type_sizeof(cond->type);
			ir_emit2_size(ctx, cond_size, IR_JNZ, cond_reg, ctx->continue_label);
			ir_emit1(ctx, IR_LABEL, ctx->break_label);
		} break;
	case AST_DECL:
	case AST_EXTERN_DEF:
		{
			result = get_register(ctx, node);
			if (!is_lvalue && node->type->kind != TYPE_FUNCTION) {
				u32 size = type_sizeof(node->type);
				result = ir_emit1_size(ctx, size, IR_LOAD, result);
			}
		} break;
	case AST_STMT_EMPTY:
		break;
	case AST_STMT_FOR_INIT:
		{
			ctx->break_label = new_label(ctx);
			ctx->continue_label = new_label(ctx);
			u32 cond_label = new_label(ctx);

			translate_node(ctx, node->child[0], false);
			ir_emit1(ctx, IR_LABEL, cond_label);

			ast_node *cond = node->child[1];
			u32 cond_reg = translate_node(ctx, cond->child[0], false);
			u32 cond_size = type_sizeof(cond->child[0]->type);
			ir_emit2_size(ctx, cond_size, IR_JIZ, cond_reg, ctx->break_label);

			ast_node *post = cond->child[1];
			ast_node *body = post->child[1];
			translate_node(ctx, body, false);
			ir_emit1(ctx, IR_LABEL, ctx->continue_label);

			translate_node(ctx, post->child[0], false);
			ir_emit1(ctx, IR_JMP, cond_label);
			ir_emit1(ctx, IR_LABEL, ctx->break_label);
		} break;
	case AST_STMT_IF_COND:
		{
			u32 endif_label = new_label(ctx);
			u32 else_label = new_label(ctx);

			ast_node *cond = node->child[0];
			ast_node *if_else = node->child[1];
			u32 cond_reg = translate_node(ctx, cond, false);
			u32 cond_size = type_sizeof(cond->type);
			ir_emit2_size(ctx, cond_size, IR_JIZ, cond_reg, else_label);

			ast_node *if_branch = if_else->child[0];
			translate_node(ctx, if_branch, false);
			ir_emit1(ctx, IR_JMP, endif_label);

			ir_emit1(ctx, IR_LABEL, else_label);
			ast_node *else_branch = if_branch->child[1];
			if (else_branch != AST_NIL) {
				translate_node(ctx, else_branch, false);
			}

			ir_emit1(ctx, IR_LABEL, endif_label);
		} break;
	case AST_STMT_WHILE:
		{
			ctx->break_label = new_label(ctx);
			ctx->continue_label = new_label(ctx);

			ir_emit1(ctx, IR_LABEL, ctx->continue_label);
			ast_node *cond = node->child[0];
			u32 cond_reg = translate_node(ctx, cond, false);
			u32 cond_size = type_sizeof(cond->type);
			ir_emit2_size(ctx, cond_size, IR_JIZ, cond_reg, ctx->break_label);

			ast_node *body = node->child[1];
			translate_node(ctx, body, false);
			ir_emit1(ctx, IR_JMP, ctx->continue_label);
			ir_emit1(ctx, IR_LABEL, ctx->break_label);
		} break;
	case AST_STMT_RETURN:
		{
			u32 size = 0;
			u32 value = 0;
			if (node->child[0]) {
				size = type_sizeof(node->child[0]->type);
				value = translate_node(ctx, node->child[0], false);
			}

			ir_emit1_size(ctx, size, IR_RET, value);
			// NOTE: For dead code elimination
			ir_emit1(ctx, IR_LABEL, new_label(ctx));
		} break;
	case AST_STMT_PRINT:
		{
			u32 size = type_sizeof(node->child[0]->type);
			u32 value = translate_node(ctx, node->child[0], false);
			ir_emit1_size(ctx, size, IR_PRINT, value);
		} break;
	case AST_TYPE_VOID:
	case AST_TYPE_CHAR:
	case AST_TYPE_INT:
	case AST_TYPE_STRUCT:
	case AST_TYPE_STRUCT_DEF:
	case AST_TYPE_FLOAT:
	case AST_TYPE_FUNC:
	case AST_TYPE_ARRAY:
	case AST_TYPE_POINTER:
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
	case IR_PRINT:
	case IR_PARAM:
	case IR_RET:
	case IR_FLOAD:
	case IR_LOAD:
	case IR_COPY:
		info.op0 = IR_OPERAND_REG_SRC;
		break;
	case IR_MOV:
	case IR_FSTORE:
	case IR_STORE:
		info.op0 = IR_OPERAND_REG_DST;
		info.op1 = IR_OPERAND_REG_SRC;
		break;
	case IR_FADD:
	case IR_FSUB:
	case IR_FMUL:
	case IR_FDIV:
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
	case IR_INT:
	case IR_FLOAT:
		info.op0 = IR_OPERAND_CONST;
		break;
	case IR_CALL:
		info.op0 = IR_OPERAND_REG_DST;
		info.op1 = IR_OPERAND_FUNC;
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
get_toplevel_instructions(ir_function *func, ir_instr *instrs, arena *arena)
{
	b8 *is_toplevel = ALLOC(arena, func->instr_count, b8);
	for (u32 i = 0; i < func->instr_count; i++) {
		is_toplevel[i] = true;
	}

	for (u32 i = 0; i < func->instr_count; i++) {
		ir_opcode_info info = get_opcode_info(instrs[i].opcode);
		if (instrs[i].opcode == IR_GLOBAL) {
			is_toplevel[i] = false;
		}

		if (is_register_operand(info.op0)) {
			is_toplevel[instrs[i].op0] = false;
		}

		if (is_register_operand(info.op1)) {
			is_toplevel[instrs[i].op1] = false;
		}
	}

	return is_toplevel;
}

static ir_program
translate(ast_node *root, symbol_table *symbol_table, arena *arena)
{
	ir_context ctx = {0};
	ctx.program.instrs = ALLOC(arena, 1024, ir_instr);
	ctx.max_instr_count = 1024;
	ctx.program.register_count++;
	ctx.program.label_count++;
	ctx.arena = arena;
	ctx.symbol_registers = ALLOC(arena, symbol_table->count, u32);
	ctx.symbol_table = symbol_table;

	if (root->kind == AST_INVALID) {
		return ctx.program;
	}

	translate_node(&ctx, root, false);
	return ctx.program;
}
