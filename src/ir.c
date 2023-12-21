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
hash(string str)
{
	u32 h = 0x811c9dc5;

	while (str.length-- > 0) {
		h *= 0x01000193;
		h ^= *str.at++;
	}

	return h;
}

static u32
emit2_sized(ir_context *ctx, ir_opcode opcode, u32 size, u32 op0, u32 op1)
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
	u32 result = emit2_sized(ctx, opcode, 0, op0, op1);
	return result;
}

static u32
emit1_sized(ir_context *ctx, ir_opcode opcode, u32 size, u32 op0)
{
	u32 result = emit2_sized(ctx, opcode, size, op0, 0);
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
new_register(ir_context *ctx, string ident)
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
			variable_table[i].vreg = emit0(ctx, IR_ALLOC);
			return variable_table[i].vreg;
		}
	}

	ASSERT(!"OOM");
	return 0;
}

static u32
get_function(ir_context *ctx, string ident)
{
	for (u32 i = 0; i < ctx->program.function_count; i++) {
		ir_function *func = &ctx->program.functions[i];
		if (string_equals(func->name, ident)) {
			return i;
		}
	}

	ASSERT(!"Function not defined");
	return ctx->program.function_count;
}

static u32
get_register(ir_context *ctx, string ident)
{
	variable *variable_table = ctx->variable_table;
	u32 h = hash(ident);
	for (u32 j = 0; j < ctx->variable_table_size; j++) {
		u32 i = (h + j) & (ctx->variable_table_size - 1);
		if (i == 0) {
			continue;
		}

		if (string_equals(variable_table[i].name, ident)) {
			return variable_table[i].vreg;
		}
	}

	ASSERT(!"Variable not declared");
	return 0;
}

static u32
generate_lvalue(ir_context *ctx, ast_node *node)
{
	u32 result = 0;

	switch (node->kind) {
	case AST_EXPR_IDENT:
		{
			result = get_register(ctx, node->u.ident);
		} break;
	case AST_EXPR_BINARY:
		{
			ir_opcode opcode = IR_NOP;
			u32 operator = node->u.bin_expr.op;
			switch (operator) {
			case TOKEN_ADD: opcode = IR_ADD; break;
			case TOKEN_SUB: opcode = IR_SUB; break;
			default:
				ASSERT(!"Not an lvalue");
			}

			ast_node *lhs = node->u.bin_expr.lhs;
			ast_node *rhs = node->u.bin_expr.rhs;
			u32 size = type_sizeof(node->type);
			u32 lhs_reg = generate_lvalue(ctx, lhs);
			u32 rhs_reg = generate_lvalue(ctx, rhs);
			result = emit2_sized(ctx, opcode, size, lhs_reg, rhs_reg);
		} break;
	case AST_EXPR_INT:
		{
			result = emit1_sized(ctx, IR_CONST, 4, node->u.ival);
		} break;
	default:
		ASSERT(!"Not an lvalue");
	}

	return result;
}

static u32
generate(ir_context *ctx, ast_node *node)
{
	u32 endif_label, else_label, cond_label, function_label;
	u32 label, param_register[128];
	u32 param_count, result = 0;
	usize result_size;
	ast_node *called, *param;
	ir_function *ir_function;
	type *return_type;
	ir_opcode opcode;

	switch (node->kind) {
	case AST_INVALID:
		ASSERT(!"Invalid node");
		break;
	case AST_EXPR_BINARY:
		{
			u32 operator = node->u.bin_expr.op;
			switch (operator) {
			case TOKEN_ADD:    opcode = IR_ADD;   break;
			case TOKEN_SUB:    opcode = IR_SUB;   break;
			case TOKEN_MUL:    opcode = IR_MUL;   break;
			case TOKEN_DIV:    opcode = IR_DIV;   break;
			case TOKEN_MOD:    opcode = IR_MOD;   break;
			case TOKEN_ASSIGN: opcode = IR_STORE; break;
			case TOKEN_EQUALS: opcode = IR_EQL;   break;
			case TOKEN_LT:     opcode = IR_LT;    break;
			case TOKEN_GT:     opcode = IR_GT;    break;
			case TOKEN_LEQ:    opcode = IR_LEQ;   break;
			case TOKEN_GEQ:    opcode = IR_GEQ;   break;
			default:
				ASSERT(!"Invalid operator");
				break;
			}

			ast_node *lhs = node->u.bin_expr.lhs;
			u32 lhs_reg = 0;
			if (opcode == IR_STORE) {
				lhs_reg = generate_lvalue(ctx, lhs);
			} else {
				lhs_reg = generate(ctx, lhs);
			}

			ast_node *rhs = node->u.bin_expr.rhs;
			u32 rhs_reg = generate(ctx, rhs);

			u32 size = type_sizeof(node->type);
			result = emit2_sized(ctx, opcode, size, lhs_reg, rhs_reg);
		} break;
	case AST_EXPR_CALL:
		called = node->u.call_expr.called;
		if (called->kind == AST_EXPR_IDENT) {
			label = get_function(ctx, called->u.ident);
			param = node->u.call_expr.params;
			param_count = 0;
			while (param) {
				ASSERT(param_count < 128);
				param_register[param_count++] = generate(ctx, param);
				param = param->next;
			}

			param = node->u.call_expr.params;
			for (u32 i = 0; i < param_count; i++) {
				u32 param_size = type_sizeof(param->type);
				emit1_sized(ctx, IR_PARAM, param_size, param_register[i]);
				param = param->next;
			}

			return_type = called->type->u.function.return_type;
			result_size = type_sizeof(return_type);
			result = emit2_sized(ctx, IR_CALL, result_size, label, param_count);
		}
		break;
	case AST_EXPR_IDENT:
		{
			u32 size = type_sizeof(node->type);
			u32 addr = get_register(ctx, node->u.ident);
			result = emit1_sized(ctx, IR_LOAD, size, addr);
		} break;
	case AST_EXPR_INT:
		result = emit1_sized(ctx, IR_CONST, 4, node->u.ival);
		break;
	case AST_EXPR_UNARY:
		{
			switch (node->u.unary_expr.op) {
			case TOKEN_AMPERSAND:
				{
					result = generate_lvalue(ctx, node->u.unary_expr.operand);
				} break;
			case TOKEN_MUL:
				{
					result = generate(ctx, node->u.unary_expr.operand);
					result = emit1(ctx, IR_LOAD, result);
				} break;
			default:
				ASSERT(!"Invalid operator");
			}
		} break;
	case AST_STMT_BREAK:
		emit1(ctx, IR_JMP, ctx->break_label);
		break;
	case AST_ROOT:
	case AST_STMT_COMPOUND:
	case AST_STMT_DECL:
		for (node = node->u.children; node; node = node->next) {
			generate(ctx, node);
		}
		break;
	case AST_STMT_CONTINUE:
		emit1(ctx, IR_JMP, ctx->continue_label);
		break;
	case AST_DECL:
		result = new_register(ctx, node->u.decl.name);
		if (node->u.decl.expr) {
			u32 expr = generate(ctx, node->u.decl.expr);
			emit2(ctx, IR_STORE, result, expr);
		}

		break;
	case AST_STMT_EMPTY:
		break;
	case AST_STMT_FOR:
		ctx->break_label = new_label(ctx);
		ctx->continue_label = new_label(ctx);
		cond_label = new_label(ctx);

		generate(ctx, node->u.for_stmt.init);
		emit1(ctx, IR_LABEL, cond_label);
		result = generate(ctx, node->u.for_stmt.cond);
		emit2(ctx, IR_JIZ, result, ctx->break_label);
		generate(ctx, node->u.for_stmt.body);
		emit1(ctx, IR_LABEL, ctx->continue_label);
		generate(ctx, node->u.for_stmt.post);
		emit1(ctx, IR_JMP, cond_label);
		emit1(ctx, IR_LABEL, ctx->break_label);
		break;
	case AST_STMT_IF:
		endif_label = new_label(ctx);
		else_label = new_label(ctx);

		result = generate(ctx, node->u.if_stmt.cond);
		emit2(ctx, IR_JIZ, result, else_label);
		generate(ctx, node->u.if_stmt.then);
		emit1(ctx, IR_JMP, endif_label);
		emit1(ctx, IR_LABEL, else_label);
		if (node->u.if_stmt.otherwise) {
			generate(ctx, node->u.if_stmt.otherwise);
		}

		emit1(ctx, IR_LABEL, endif_label);
		break;
	case AST_STMT_WHILE:
		ctx->break_label = new_label(ctx);
		ctx->continue_label = new_label(ctx);

		emit1(ctx, IR_LABEL, ctx->continue_label);
		result = generate(ctx, node->u.while_stmt.cond);
		emit2(ctx, IR_JIZ, result, ctx->break_label);
		generate(ctx, node->u.while_stmt.body);
		emit1(ctx, IR_JMP, ctx->continue_label);
		emit1(ctx, IR_LABEL, ctx->break_label);
		break;
	case AST_STMT_RETURN:
		if (node->u.children) {
			result = generate(ctx, node->u.children);
		}
		emit1(ctx, IR_RET, result);
		break;
	case AST_STMT_PRINT:
		result = generate(ctx, node->u.children);
		emit1(ctx, IR_PRINT, result);
		break;
	case AST_FUNCTION:
		function_label = new_label(ctx);
		emit1(ctx, IR_LABEL, function_label);
		param = node->u.function.params;
		param_count = 0;

		// TODO: find some better mechanism to reset the variable table
		memset(ctx->variable_table, 0, ctx->variable_table_size * sizeof(variable));
		ir_function = &ctx->program.functions[ctx->program.function_count++];
		ir_function->name = node->u.function.name;
		ir_function->block_index = function_label;
		ir_function->instr_index = ctx->program.instr_count;

		while (param) {
			new_register(ctx, param->u.decl.name);
			param_count++;
			param = param->next;
		}

		ir_function->parameter_count = param_count;

		for (ast_node *stmt = node->u.function.body; stmt; stmt = stmt->next) {
			generate(ctx, stmt);
		}
		break;
	case AST_TYPE_POINTER:
	case AST_TYPE_VOID:
	case AST_TYPE_CHAR:
	case AST_TYPE_INT:
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
		} else if (opcode == IR_JIZ) {
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

	ir_function *last_function = &program->functions[program->function_count - 1];
	last_function->block_count = program->block_count - last_function->block_index;

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
		info.op0 = IR_OPERAND_REG_SRC;
		break;
	case IR_MOV:
	case IR_LOAD:
	case IR_STORE:
		info.op0 = IR_OPERAND_REG_DST;
		info.op1 = IR_OPERAND_REG_SRC;
		break;
	case IR_ADD:
	case IR_SUB:
	case IR_MUL:
	case IR_DIV:
	case IR_MOD:
	case IR_EQL:
	case IR_LT:
	case IR_GT:
	case IR_LEQ:
	case IR_GEQ:
		info.op0 = IR_OPERAND_REG_SRC;
		info.op1 = IR_OPERAND_REG_SRC;
		break;
	case IR_JIZ:
		info.op0 = IR_OPERAND_REG_SRC;
		info.op1 = IR_OPERAND_LABEL;
		break;
	case IR_ALLOC:
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
		break;
	}

	return info;
}

static u32 *
get_usage_count(ir_program program, arena *arena)
{
	ir_instr *instrs = program.instrs;
	u32 *usage_count = ALLOC(arena, program.register_count, u32);

	for (u32 i = 0; i < program.register_count; i++) {
		ir_opcode_info info = get_opcode_info(instrs[i].opcode);
		if (info.op0 == IR_OPERAND_REG_SRC || info.op0 == IR_OPERAND_LABEL) {
			usage_count[instrs[i].op0]++;
		}

		if (info.op1 == IR_OPERAND_REG_SRC || info.op1 == IR_OPERAND_LABEL) {
			usage_count[instrs[i].op1]++;
		}
	}

	return usage_count;
}

static ir_program
ir_generate(ast_node *root, arena *arena)
{
	ir_context ctx = ir_context_init(arena);

	u32 function_count = 0;
	for (ast_node *node = root->u.children; node; node = node->next) {
		function_count += (node->kind == AST_FUNCTION);
	}

	ctx.program.functions = ALLOC(arena, function_count, ir_function);
	generate(&ctx, root);
	construct_cfg(&ctx.program, arena);
	return ctx.program;
}
