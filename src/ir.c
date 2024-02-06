static ir_context
ir_context_init(arena *arena)
{
	ir_context ctx = {0};
	ctx.program.instrs = ALLOC(arena, 1024, ir_instr);
	ctx.max_instr_count = 1024;
	ctx.variable_table = ALLOC(arena, 1024, variable);
	ctx.variable_table_size = 1024;
	ctx.program.register_count++;
	ctx.program.label_count++;
	return ctx;
}

static u32
new_label(ir_context *ctx)
{
	u32 result = ctx->program.label_count++;
	return result;
}

static u32
emit2_size(ir_context *ctx, ir_opcode opcode, u32 size, u32 op0, u32 op1)
{
	ASSERT(ctx->program.instr_count <= ctx->max_instr_count);
	ir_instr *instr = &ctx->program.instrs[ctx->program.instr_count++];
	instr->opcode = opcode;
	instr->size = size;
	instr->op0 = op0;
	instr->op1 = op1;
	ctx->program.register_count++;
	return ctx->program.instr_count - 1;
}

static u32
emit2(ir_context *ctx, ir_opcode opcode, u32 op0, u32 op1)
{
	u32 result = emit2_size(ctx, opcode, 0, op0, op1);
	return result;
}

static u32
emit1_size(ir_context *ctx, ir_opcode opcode, u32 size, u32 op0)
{
	u32 result = emit2_size(ctx, opcode, size, op0, 0);
	return result;
}

static u32
emit1(ir_context *ctx, ir_opcode opcode, u32 op0)
{
	u32 result = emit2(ctx, opcode, op0, 0);
	return result;
}

static u32
emit0(ir_context *ctx, ir_opcode opcode)
{
	u32 result = emit2(ctx, opcode, 0, 0);
	return result;
}

static u32
emit_alloca(ir_context *ctx, u32 size)
{
	ctx->stack_size += size;
	u32 result = emit2(ctx, IR_ALLOC, size, ctx->stack_size);
	return result;
}

static u32
new_register(ir_context *ctx, str ident, u32 size)
{
	variable *variable_table = ctx->variable_table;
	u32 h = hash(ident);
	for (u32 j = 0; j < ctx->variable_table_size; j++) {
		u32 i = (h + j) & (ctx->variable_table_size - 1);
		if (i == 0) {
			continue;
		}

		if (!variable_table[i].name.at) {
			variable_table[i].name = ident;
			variable_table[i].vreg = emit_alloca(ctx, size);
			return variable_table[i].vreg;
		}
	}

	ASSERT(!"OOM");
	return 0;
}

static u32
get_function(ir_context *ctx, str ident)
{
	for (u32 i = 0; i < ctx->program.function_count; i++) {
		ir_function *func = &ctx->program.functions[i];
		if (str_equals(func->name, ident)) {
			return i;
		}
	}

	ASSERT(!"Function not defined");
	return ctx->program.function_count;
}

static u32
get_register(ir_context *ctx, str ident)
{
	variable *variable_table = ctx->variable_table;
	u32 h = hash(ident);
	for (u32 j = 0; j < ctx->variable_table_size; j++) {
		u32 i = (h + j) & (ctx->variable_table_size - 1);
		if (i == 0) {
			continue;
		}

		if (str_equals(variable_table[i].name, ident)) {
			return variable_table[i].vreg;
		}
	}

	ASSERT(!"Variable not declared");
	return 0;
}

static u32 translate_node(ir_context *ctx, ast_node *node);

static u32
translate_lvalue(ir_context *ctx, ast_node *node)
{
	u32 result = 0;

	switch (node->kind) {
	case AST_EXPR_IDENT:
		{
			result = get_register(ctx, node->value.s);
		} break;
	case AST_EXPR_BINARY:
		{
			ir_opcode opcode = IR_NOP;
			u32 operator = node->value.i;
			switch (operator) {
			case TOKEN_PLUS:     opcode = IR_ADD; break;
			case TOKEN_MINUS:    opcode = IR_SUB; break;
			case TOKEN_LBRACKET: opcode = IR_ADD; break;
			case TOKEN_DOT:      opcode = IR_ADD; break;
			default:
				ASSERT(!"Not an lvalue");
			}

			ast_node *lhs = node->children;
			ast_node *rhs = lhs->next;

			u32 size = type_sizeof(node->type);
			if (operator != TOKEN_DOT) {
				u32 lhs_reg = translate_lvalue(ctx, lhs);
				u32 rhs_reg = translate_lvalue(ctx, rhs);
				result = emit2_size(ctx, opcode, size, lhs_reg, rhs_reg);
			} else {
				u32 offset = type_offsetof(lhs->type, rhs->value.s);
				u32 offset_reg = emit1(ctx, IR_CONST, offset);
				u32 base_reg = translate_lvalue(ctx, lhs);
				result = emit2_size(ctx, IR_ADD, size, base_reg, offset_reg);
			}
		} break;
	case AST_EXPR_UNARY:
		{
			u32 operator = node->value.i;
			switch (operator) {
			case TOKEN_STAR:
			case TOKEN_PLUS:
				result = translate_node(ctx, node->children);
				break;
			case TOKEN_MINUS:
				{
					u32 zero = emit1(ctx, IR_CONST, 0);
					result = translate_node(ctx, node->children);
					result = emit2(ctx, IR_SUB, zero, result);
				} break;
			default:
				ASSERT(!"Not an lvalue");
			}
		} break;
	case AST_EXPR_INT:
		{
			result = emit1_size(ctx, IR_CONST, 4, node->value.i);
		} break;
	default:
		ASSERT(!"Not an lvalue");
	}

	return result;
}

static u32
translate_node(ir_context *ctx, ast_node *node)
{
	u32 result = 0;

	switch (node->kind) {
	case AST_INVALID:
		{
			ASSERT(!"Invalid node");
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
			case TOKEN_DOT:           opcode = IR_LOAD;  break;
			default:
				ASSERT(!"Invalid operator");
				break;
			}

			ast_node *lhs = node->children;
			u32 lhs_reg = 0;
			if (opcode == IR_STORE) {
				lhs_reg = translate_lvalue(ctx, lhs);
			} else {
				lhs_reg = translate_node(ctx, lhs);
			}

			ast_node *rhs = lhs->next;
			u32 size = type_sizeof(node->type);
			if (operator == TOKEN_DOT) {
				u32 offset = type_offsetof(lhs->type, rhs->value.s);
				u32 offset_reg = emit1(ctx, IR_CONST, offset);
				u32 base_reg = translate_lvalue(ctx, lhs);
				u32 addr = emit2(ctx, IR_ADD, base_reg, offset_reg);
				result = emit2_size(ctx, IR_LOAD, size, addr, addr);
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

				u32 rhs_reg = translate_node(ctx, rhs);
				if (operator != TOKEN_EQUAL) {
					u32 value = emit1_size(ctx, IR_LOAD, size, lhs_reg);
					result = emit2_size(ctx, opcode, size, value, rhs_reg);
					emit2_size(ctx, IR_STORE, size, lhs_reg, result);
				} else {
					emit2_size(ctx, IR_STORE, size, lhs_reg, rhs_reg);
				}
			} else if (operator == TOKEN_LBRACKET) {
				u32 rhs_reg = translate_node(ctx, rhs);
				result = emit2_size(ctx, IR_ADD, 8, lhs_reg, rhs_reg);
				result = emit1_size(ctx, IR_LOAD, size, result);
			} else {
				u32 rhs_reg = translate_node(ctx, rhs);
				result = emit2_size(ctx, opcode, size, lhs_reg, rhs_reg);
			}
		} break;
	case AST_EXPR_CALL:
		{
			ast_node *called = node->children;
			if (called->kind == AST_EXPR_IDENT) {
				u32 label = get_function(ctx, called->value.s);
				ast_node *param = called->next;
				i32 param_count = 0;
				u32 param_register[128] = {0};
				while (param != AST_NIL) {
					ASSERT(param_count < 128);
					param_register[param_count++ & 127] = translate_node(ctx, param);
					param = param->next;
				}

				param = called->next;
				for (i32 i = 0; i < param_count; i++) {
					u32 param_size = type_sizeof(param->type);
					emit1_size(ctx, IR_PARAM, param_size, param_register[i]);
					param = param->next;
				}

				type *return_type = called->type->children;
				u32 result_size = type_sizeof(return_type);
				result = emit2_size(ctx, IR_CALL, result_size, label, param_count);
			}
		} break;
	case AST_EXPR_IDENT:
		{
			ASSERT(node->type != NULL);
			u32 size = type_sizeof(node->type);
			u32 addr = get_register(ctx, node->value.s);
			result = emit1_size(ctx, IR_LOAD, size, addr);
		} break;
	case AST_EXPR_INT:
		{
			result = emit1_size(ctx, IR_CONST, 4, node->value.i);
		} break;
	case AST_EXPR_UNARY:
		{
			u32 operator = node->value.op;
			switch (operator) {
			case TOKEN_AMP:
			case TOKEN_PLUS:
				{
					result = translate_lvalue(ctx, node->children);
				} break;
			case TOKEN_MINUS:
				{
					u32 zero = emit1(ctx, IR_CONST, 0);
					result = translate_node(ctx, node->children);
					result = emit2(ctx, IR_SUB, zero, result);
				} break;
			case TOKEN_STAR:
				{
					result = translate_node(ctx, node->children);
					u32 size = type_sizeof(node->type);
					result = emit1_size(ctx, IR_LOAD, size, result);
				} break;
			case TOKEN_PLUS_PLUS:
			case TOKEN_MINUS_MINUS:
				{
					ir_opcode add_or_sub = IR_ADD;
					if (operator == TOKEN_MINUS_MINUS) {
						add_or_sub = IR_SUB;
					}

					u32 var = translate_lvalue(ctx, node->children);
					u32 value = emit1(ctx, IR_LOAD, var);
					u32 one = emit1(ctx, IR_CONST, 1);
					result = emit2(ctx, add_or_sub, value, one);
					emit2(ctx, IR_STORE, var, result);
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

					u32 var = translate_lvalue(ctx, node->children);
					result = emit1(ctx, IR_LOAD, var);
					u32 one = emit1(ctx, IR_CONST, 1);
					u32 value = emit2(ctx, add_or_sub, result, one);
					emit2(ctx, IR_STORE, var, value);
				} break;
			default:
				ASSERT(!"Invalid postfix operator");
			}
		} break;
	case AST_STMT_BREAK:
		{
			emit1(ctx, IR_JMP, ctx->break_label);
		} break;
	case AST_ROOT:
	case AST_STMT_COMPOUND:
		{
			for (ast_node *child = node->children; child != AST_NIL; child = child->next) {
				translate_node(ctx, child);
			}
		} break;
	case AST_STMT_CONTINUE:
		{
			emit1(ctx, IR_JMP, ctx->continue_label);
		} break;
	case AST_STMT_DO_WHILE:
		{
			ctx->break_label = new_label(ctx);
			ctx->continue_label = new_label(ctx);
			ast_node *cond = node->children;
			ast_node *body = cond->next;

			emit1(ctx, IR_LABEL, ctx->continue_label);
			translate_node(ctx, body);

			u32 cond_reg = translate_node(ctx, cond);
			u32 cond_size = type_sizeof(cond->type);
			emit2_size(ctx, IR_JNZ, cond_size, cond_reg, ctx->continue_label);
			emit1(ctx, IR_LABEL, ctx->break_label);
		} break;
	case AST_DECL:
		{
			for (ast_node *child = node->children; child != AST_NIL; child = child->next) {
				translate_node(ctx, child);
			}
		} break;
	case AST_DECL_INIT:
		{
			result = translate_node(ctx, node->children);
			if (node->children->next != AST_NIL) {
				u32 expr = translate_node(ctx, node->children->next);
				emit2(ctx, IR_STORE, result, expr);
			}
		} break;
	case AST_DECL_POINTER:
	case AST_DECL_ARRAY:
		{
			result = translate_node(ctx, node->children);
		} break;
	case AST_DECL_FUNC:
		{
			if (node->children->kind != AST_DECL_IDENT) {
				result = translate_node(ctx, node->children);
			}
		} break;
	case AST_DECL_IDENT:
		{
			usize size = type_sizeof(node->type);
			if (node->type->kind == TYPE_ARRAY) {
				result = new_register(ctx, node->value.s, 8);
				u32 addr = emit_alloca(ctx, size);
				emit2(ctx, IR_STORE, result, addr);
			} else {
				result = new_register(ctx, node->value.s, size);
			}
		} break;
	case AST_STMT_EMPTY:
		break;
	case AST_STMT_FOR:
		{
			ctx->break_label = new_label(ctx);
			ctx->continue_label = new_label(ctx);
			u32 cond_label = new_label(ctx);

			ast_node *init = node->children;
			translate_node(ctx, init);
			emit1(ctx, IR_LABEL, cond_label);

			ast_node *cond = init->next;
			u32 cond_reg = translate_node(ctx, cond);
			u32 cond_size = type_sizeof(cond->type);
			emit2_size(ctx, IR_JIZ, cond_size, cond_reg, ctx->break_label);

			ast_node *post = cond->next;
			ast_node *body = post->next;
			translate_node(ctx, body);
			emit1(ctx, IR_LABEL, ctx->continue_label);

			translate_node(ctx, post);
			emit1(ctx, IR_JMP, cond_label);
			emit1(ctx, IR_LABEL, ctx->break_label);
		} break;
	case AST_STMT_IF:
		{
			u32 endif_label = new_label(ctx);
			u32 else_label = new_label(ctx);

			ast_node *cond = node->children;
			u32 cond_reg = translate_node(ctx, cond);
			u32 cond_size = type_sizeof(cond->type);
			emit2_size(ctx, IR_JIZ, cond_size, cond_reg, else_label);

			ast_node *if_branch = cond->next;
			translate_node(ctx, if_branch);
			emit1(ctx, IR_JMP, endif_label);

			emit1(ctx, IR_LABEL, else_label);
			ast_node *else_branch = if_branch->next;
			if (else_branch != AST_NIL) {
				translate_node(ctx, else_branch);
			}

			emit1(ctx, IR_LABEL, endif_label);
		} break;
	case AST_STMT_WHILE:
		{
			ctx->break_label = new_label(ctx);
			ctx->continue_label = new_label(ctx);

			emit1(ctx, IR_LABEL, ctx->continue_label);
			ast_node *cond = node->children;
			u32 cond_reg = translate_node(ctx, cond);
			u32 cond_size = type_sizeof(cond->type);
			emit2_size(ctx, IR_JIZ, cond_size, cond_reg, ctx->break_label);

			ast_node *body = cond->next;
			translate_node(ctx, body);
			emit1(ctx, IR_JMP, ctx->continue_label);
			emit1(ctx, IR_LABEL, ctx->break_label);
		} break;
	case AST_STMT_RETURN:
		{
			u32 size = 0;
			u32 value = 0;
			if (node->children) {
				size = type_sizeof(node->children->type);
				value = translate_node(ctx, node->children);
			}

			emit1_size(ctx, IR_RET, size, value);
		} break;
	case AST_STMT_PRINT:
		{
			u32 size = type_sizeof(node->children->type);
			u32 value = translate_node(ctx, node->children);
			emit1_size(ctx, IR_PRINT, size, value);
		} break;
	case AST_FUNCTION:
		{
			ast_node *decl = node->children;
			ast_node *body = decl->next;
			ast_node *declarator = decl->children->next;
			ast_node *param = declarator->children->next;

			u32 function_label = new_label(ctx);
			emit1(ctx, IR_LABEL, function_label);

			ctx->stack_size = 0;
			// TODO: find some better mechanism to reset the variable table
			memset(ctx->variable_table, 0, ctx->variable_table_size * sizeof(variable));
			ir_function *ir_function = &ctx->program.functions[ctx->program.function_count++];
			ir_function->name = node->value.s;
			ir_function->block_index = function_label;
			ir_function->instr_index = ctx->program.instr_count;

			i32 param_count = 0;
			while (param != AST_NIL) {
				translate_node(ctx, param);
				param = param->next;
				param_count++;
			}

			ir_function->parameter_count = param_count;
			translate_node(ctx, body);
			ir_function->stack_size = ctx->stack_size;
		} break;
	case AST_TYPE_VOID:
	case AST_TYPE_CHAR:
	case AST_TYPE_INT:
	case AST_TYPE_STRUCT:
	case AST_TYPE_STRUCT_DEF:
		break;
	}

	return result;
}

static b32
is_block_start(ir_instr *instrs, u32 i)
{
	b32 result = (i == 0 || instrs[i].opcode == IR_LABEL);
	if (!result) {
		u32 prev = i - 1;
		result = (instrs[prev].opcode == IR_JMP
		    || instrs[prev].opcode == IR_JIZ
		    || instrs[prev].opcode == IR_JNZ
		    || instrs[prev].opcode == IR_RET);
	}

	return result;
}

static void
construct_cfg(ir_program *program, arena *arena)
{
	u32 block_count = 0;
	for (u32 i = 0; i < program->instr_count; i++) {
		if (is_block_start(program->instrs, i)) {
			block_count++;
		}
	}

	program->blocks = ALLOC(arena, block_count, ir_block);
	program->block_count = block_count;

	arena_temp temp = arena_temp_begin(arena);
	ir_instr *instrs = program->instrs;
	u32 instr_count = program->instr_count;

	/* replace labels with block indices */
	u32 *block_indices = ALLOC(arena, program->label_count, u32);
	u32 block_index = 0;
	for (u32 i = 0; i < instr_count; i++) {
		if (is_block_start(program->instrs, i)) {
			u32 opcode = instrs[i].opcode;
			if (opcode == IR_LABEL) {
				u32 label = program->instrs[i].op0;
				block_indices[label] = i;
				instrs[i].op0 = block_index;
			}

			program->blocks[block_index++].start = i;
		}
	}

	for (u32 i = 0; i < program->instr_count; i++) {
		ir_instr *instr = &program->instrs[i];
		ir_opcode opcode = instr->opcode;
		if (opcode == IR_JMP) {
			u32 block = block_indices[instr->op0];
			instr->op0 = block;
			ASSERT(block > 0);
		} else if (opcode == IR_JIZ || opcode == IR_JNZ) {
			u32 block = block_indices[instr->op1];
			instr->op1 = block;
			ASSERT(block > 0);
		}
	}

	for (u32 i = 0; i < program->function_count; i++) {
		ir_function *curr = &program->functions[i];
		u32 label = curr->block_index;
		u32 block_index = block_indices[label];
		block_index = instrs[block_index].op0;
		curr->block_index = block_index;
		ASSERT(block_index < program->block_count);

		b32 is_first_function = (i == 0);
		if (!is_first_function) {
			ir_function *prev = &program->functions[i - 1];
			prev->block_count = block_index - prev->block_index;
			ASSERT(prev->block_count > 0);
		}
	}

	if (program->function_count > 0) {
		ir_function *last_function = &program->functions[program->function_count - 1];
		last_function->block_count = program->block_count - last_function->block_index;
	}

	arena_temp_end(temp);

	/* calculate size of each block */
	ir_block *blocks = program->blocks;
	for (u32 i = 0; i < program->block_count; i++) {
		b32 is_last_block = (i + 1 >= program->block_count);
		if (is_last_block) {
			blocks[i].size = program->instr_count - blocks[i].start;
		} else {
			blocks[i].size = blocks[i+1].start - blocks[i].start;
		}

		ASSERT(blocks[i].size > 0);
	}

	/* determine the next block */
	for (u32 i = 0; i < program->block_count; i++) {
		u32 block_end = blocks[i].start + blocks[i].size - 1;
		switch (instrs[block_end].opcode) {
		case IR_JMP:
			blocks[i].next[0] = instrs[block_end].op0;
			blocks[i].next[1] = instrs[block_end].op0;
			break;
		case IR_JIZ:
		case IR_JNZ:
			blocks[i].next[0] = i + 1;
			blocks[i].next[1] = instrs[block_end].op1;
			break;
		case IR_RET:
			blocks[i].next[0] = program->instr_count;
			blocks[i].next[1] = program->instr_count;
			break;
		default:
			blocks[i].next[0] = i + 1;
			blocks[i].next[1] = i + 1;
			break;
		}
	}
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
	case IR_LOAD:
	case IR_COPY:
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
		info.op0 = IR_OPERAND_CONST;
		break;
	case IR_CALL:
		info.op0 = IR_OPERAND_REG_DST;
		info.op1 = IR_OPERAND_FUNC;
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
get_toplevel_instructions(ir_program program, arena *arena)
{
	b8 *is_toplevel = ALLOC(arena, program.instr_count, b8);
	for (u32 i = 0; i < program.instr_count; i++) {
		is_toplevel[i] = true;
	}

	ir_instr *instrs = program.instrs;
	for (u32 i = 0; i < program.instr_count; i++) {
		ir_opcode_info info = get_opcode_info(instrs[i].opcode);
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
translate(ast_node *root, symbol_table *symbols, arena *arena)
{
	ir_context ctx = ir_context_init(arena);
	if (root->kind == AST_INVALID || symbols->error) {
		return ctx.program;
	}

	u32 function_count = 0;
	for (ast_node *node = root->children; node != AST_NIL; node = node->next) {
		function_count += (node->kind == AST_FUNCTION);
	}

	ctx.program.functions = ALLOC(arena, function_count, ir_function);
	translate_node(&ctx, root);
	construct_cfg(&ctx.program, arena);
	return ctx.program;
}
