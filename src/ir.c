static char *
get_opcode_name(ir_opcode opcode)
{
	switch (opcode) {
	case IR_NOP:   return "nop";
	case IR_LABEL: return "label";
	case IR_CONST: return "const";
	case IR_MOV:   return "mov";
	case IR_ADD:   return "add";
	case IR_SUB:   return "sub";
	case IR_MUL:   return "mul";
	case IR_DIV:   return "div";
	case IR_MOD:   return "mod";
	case IR_EQL:   return "eql";
	case IR_LT:    return "lt";
	case IR_GT:    return "gt";
	case IR_LEQ:   return "leq";
	case IR_GEQ:   return "geq";
	case IR_JMP:   return "jmp";
	case IR_JIZ:   return "jiz";
	case IR_RET:   return "ret";
	case IR_CALL:  return "call";
	case IR_PARAM: return "param";
	case IR_ALLOC: return "alloc";
	case IR_PRINT: return "print";
	case IR_LOAD:  return "load";
	case IR_STORE: return "store";
	}

	return "(invalid)";
}

static ir_generator
ir_generator_init(arena *arena)
{
	ir_generator state = {0};
	state.program.instrs = ALLOC(arena, 1024, ir_instr);
	state.max_instr_count = 1024;
	state.variable_table = ALLOC(arena, 1024, variable);
	state.variable_table_size = 1024;
	state.program.register_count++;
	state.program.label_count++;
	return state;
}

static u32
new_label(ir_generator *state)
{
	u32 result = state->program.label_count++;
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
emit2_sized(ir_generator *state, ir_opcode opcode, u32 size,
	u32 op0, u32 op1)
{
	ASSERT(state->program.instr_count <= state->max_instr_count);
	ir_instr *instr = &state->program.instrs[state->program.instr_count++];
	instr->opcode = opcode;
	instr->size = size;
	instr->op0 = op0;
	instr->op1 = op1;
	state->program.register_count++;
	return state->program.instr_count - 1;
}

static u32
emit2(ir_generator *state, ir_opcode opcode, u32 op0, u32 op1)
{
	u32 result = emit2_sized(state, opcode, 0, op0, op1);
	return result;
}

static u32
emit1_sized(ir_generator *state, ir_opcode opcode, u32 size, u32 op0)
{
	u32 result = emit2_sized(state, opcode, size, op0, 0);
	return result;
}

static u32
emit1(ir_generator *state, ir_opcode opcode, u32 op0)
{
	u32 result = emit2(state, opcode, op0, 0);
	return result;
}

static u32
emit0(ir_generator *state, ir_opcode opcode)
{
	u32 result = emit2(state, opcode, 0, 0);
	return result;
}

static u32
new_register(ir_generator *state, string ident)
{
	variable *variable_table = state->variable_table;
	u32 h = hash(ident);
	for (u32 j = 0; j < state->variable_table_size; j++) {
		u32 i = (h + j) & (state->variable_table_size - 1);
		if (i == 0) {
			continue;
		}

		if (!variable_table[i].name.at) {
			variable_table[i].name = ident;
			variable_table[i].vreg = emit0(state, IR_ALLOC);
			return variable_table[i].vreg;
		}
	}

	ASSERT(!"OOM");
	return 0;
}

static u32
get_function(ir_generator *state, string ident)
{
	for (u32 i = 0; i < state->program.function_count; i++) {
		ir_function *func = &state->program.functions[i];
		if (string_equals(func->name, ident)) {
			return i;
		}
	}

	ASSERT(!"Function not defined");
	return state->program.function_count;
}

static u32
get_register(ir_generator *state, string ident)
{
	variable *variable_table = state->variable_table;
	u32 h = hash(ident);
	for (u32 j = 0; j < state->variable_table_size; j++) {
		u32 i = (h + j) & (state->variable_table_size - 1);
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
generate_lvalue(ir_generator *state, ast_node *node)
{
	u32 result = 0;

	switch (node->kind) {
	case AST_EXPR_IDENT:
		{
			result = get_register(state, node->u.ident);
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
			u32 lhs_reg = generate_lvalue(state, lhs);
			u32 rhs_reg = generate_lvalue(state, rhs);
			result = emit2_sized(state, opcode, size, lhs_reg, rhs_reg);
		} break;
	case AST_EXPR_INT:
		{
			result = emit1_sized(state, IR_CONST, 4, node->u.ival);
		} break;
	default:
		ASSERT(!"Not an lvalue");
	}

	return result;
}

static u32
generate(ir_generator *state, ast_node *node)
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
				lhs_reg = generate_lvalue(state, lhs);
			} else {
				lhs_reg = generate(state, lhs);
			}

			ast_node *rhs = node->u.bin_expr.rhs;
			u32 rhs_reg = generate(state, rhs);

			u32 size = type_sizeof(node->type);
			result = emit2_sized(state, opcode, size, lhs_reg, rhs_reg);
		} break;
	case AST_EXPR_CALL:
		called = node->u.call_expr.called;
		if (called->kind == AST_EXPR_IDENT) {
			label = get_function(state, called->u.ident);
			param = node->u.call_expr.params;
			param_count = 0;
			while (param) {
				ASSERT(param_count < 128);
				param_register[param_count++] = generate(state, param);
				param = param->next;
			}

			param = node->u.call_expr.params;
			for (u32 i = 0; i < param_count; i++) {
				u32 param_size = type_sizeof(param->type);
				emit1_sized(state, IR_PARAM, param_size, param_register[i]);
				param = param->next;
			}

			return_type = called->type->u.function.return_type;
			result_size = type_sizeof(return_type);
			result = emit2_sized(state, IR_CALL, result_size, label, param_count);
		}
		break;
	case AST_EXPR_IDENT:
		{
			u32 size = type_sizeof(node->type);
			u32 addr = get_register(state, node->u.ident);
			result = emit1_sized(state, IR_LOAD, size, addr);
		} break;
	case AST_EXPR_INT:
		result = emit1_sized(state, IR_CONST, 4, node->u.ival);
		break;
	case AST_EXPR_UNARY:
		{
			switch (node->u.unary_expr.op) {
			case TOKEN_AMPERSAND:
				{
					result = generate_lvalue(state, node->u.unary_expr.operand);
				} break;
			case TOKEN_MUL:
				{
					result = generate(state, node->u.unary_expr.operand);
					result = emit1(state, IR_LOAD, result);
				} break;
			default:
				ASSERT(!"Invalid operator");
			}
		} break;
	case AST_STMT_BREAK:
		emit1(state, IR_JMP, state->break_label);
		break;
	case AST_ROOT:
	case AST_STMT_COMPOUND:
	case AST_STMT_DECL:
		for (node = node->u.children; node; node = node->next) {
			generate(state, node);
		}
		break;
	case AST_STMT_CONTINUE:
		emit1(state, IR_JMP, state->continue_label);
		break;
	case AST_DECL:
		result = new_register(state, node->u.decl.name);
		if (node->u.decl.expr) {
			u32 expr = generate(state, node->u.decl.expr);
			emit2(state, IR_STORE, result, expr);
		}

		break;
	case AST_STMT_EMPTY:
		break;
	case AST_STMT_FOR:
		state->break_label = new_label(state);
		state->continue_label = new_label(state);
		cond_label = new_label(state);

		generate(state, node->u.for_stmt.init);
		emit1(state, IR_LABEL, cond_label);
		result = generate(state, node->u.for_stmt.cond);
		emit2(state, IR_JIZ, result, state->break_label);
		generate(state, node->u.for_stmt.body);
		emit1(state, IR_LABEL, state->continue_label);
		generate(state, node->u.for_stmt.post);
		emit1(state, IR_JMP, cond_label);
		emit1(state, IR_LABEL, state->break_label);
		break;
	case AST_STMT_IF:
		endif_label = new_label(state);
		else_label = new_label(state);

		result = generate(state, node->u.if_stmt.cond);
		emit2(state, IR_JIZ, result, else_label);
		generate(state, node->u.if_stmt.then);
		emit1(state, IR_JMP, endif_label);
		emit1(state, IR_LABEL, else_label);
		if (node->u.if_stmt.otherwise) {
			generate(state, node->u.if_stmt.otherwise);
		}

		emit1(state, IR_LABEL, endif_label);
		break;
	case AST_STMT_WHILE:
		state->break_label = new_label(state);
		state->continue_label = new_label(state);

		emit1(state, IR_LABEL, state->continue_label);
		result = generate(state, node->u.while_stmt.cond);
		emit2(state, IR_JIZ, result, state->break_label);
		generate(state, node->u.while_stmt.body);
		emit1(state, IR_JMP, state->continue_label);
		emit1(state, IR_LABEL, state->break_label);
		break;
	case AST_STMT_RETURN:
		if (node->u.children) {
			result = generate(state, node->u.children);
		}
		emit1(state, IR_RET, result);
		break;
	case AST_STMT_PRINT:
		result = generate(state, node->u.children);
		emit1(state, IR_PRINT, result);
		break;
	case AST_FUNCTION:
		function_label = new_label(state);
		emit1(state, IR_LABEL, function_label);
		param = node->u.function.params;
		param_count = 0;

		ir_function = &state->program.functions[state->program.function_count++];
		ir_function->name = node->u.function.name;
		ir_function->block_index = function_label;
		ir_function->instr_index = state->program.instr_count;

		while (param) {
			new_register(state, param->u.decl.name);
			param_count++;
			param = param->next;
		}

		ir_function->parameter_count = param_count;

		for (ast_node *stmt = node->u.function.body; stmt; stmt = stmt->next) {
			generate(state, stmt);
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
is_block_start(ir_program *program, u32 i)
{
	u32 curr = program->toplevel_instr_indices[i];
	b32 result = (i == 0 || program->instrs[curr].opcode == IR_LABEL);
	if (!result) {
		u32 prev = program->toplevel_instr_indices[i-1];
		result = (program->instrs[prev].opcode == IR_JMP
		    || program->instrs[prev].opcode == IR_JIZ
		    || program->instrs[prev].opcode == IR_RET);
	}

	return result;
}

static void
construct_cfg(ir_program *program, arena *arena)
{
	u32 block_count = 0;
	for (u32 i = 0; i < program->toplevel_count; i++) {
		if (is_block_start(program, i)) {
			block_count++;
		}
	}

	program->blocks = ALLOC(arena, block_count, ir_block);
	program->block_count = block_count;

	arena_temp temp = arena_temp_begin(arena);

	/* replace labels with block indices */
	u32 *block_indices = ALLOC(arena, program->label_count, u32);
	u32 block_index = 0;
	for (u32 j = 0; j < program->toplevel_count; j++) {
		if (is_block_start(program, j)) {
			u32 i = program->toplevel_instr_indices[j];
			u32 opcode = program->instrs[i].opcode;
			if (opcode == IR_LABEL) {
				u32 label = program->instrs[i].op0;
				block_indices[label] = i;
				program->instrs[i].op0 = block_index;
			}

			program->blocks[block_index++].start = j;
		}
	}

	for (u32 j = 0; j < program->toplevel_count; j++) {
		u32 i = program->toplevel_instr_indices[j];
		ir_instr *instr = &program->instrs[i];
		ir_opcode opcode = instr->opcode;
		if (opcode == IR_JMP) {
			u32 block = block_indices[instr->op0];
			ASSERT(block > 0);
			instr->op0 = block;
		} else if (opcode == IR_JIZ) {
			u32 block = block_indices[instr->op1];
			ASSERT(block > 0);
			instr->op1 = block;
		}
	}

	for (u32 i = 0; i < program->function_count; i++) {
		u32 label = program->functions[i].block_index;
		u32 block_index = block_indices[label];
		block_index = program->instrs[block_index].op0;
		program->functions[i].block_index = block_index;
		ASSERT(block_index < program->block_count);
		if (i > 0) {
			ir_function *prev_function = &program->functions[i - 1];
			prev_function->block_count = block_index - prev_function->block_index;
		}
	}

	ir_function *last_function = &program->functions[program->function_count - 1];
	last_function->block_count = program->block_count - last_function->block_index;

	arena_temp_end(temp);

	/* calculate size of each block */
	ir_block *blocks = program->blocks;
	for (u32 i = 0; i < program->block_count; i++) {
		if (i + 1 < program->block_count) {
			blocks[i].size = blocks[i+1].start - blocks[i].start;
		} else {
			blocks[i].size = program->toplevel_count - blocks[i].start;
		}

		ASSERT(blocks[i].size > 0);
	}

	/* determine the next block */
	ir_instr *instrs = program->instrs;
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

static void
mark_toplevel_instructions(ir_program *program, arena *arena)
{
	arena_temp temp = arena_temp_begin(arena);
	u32 *toplevel_instrs = ALLOC(arena, program->instr_count, u32);
	u32 *usage_count = get_usage_count(*program, arena);
	ir_instr *instrs = program->instrs;
	u32 toplevel_count = 0;
	for (u32 i = 0; i < program->instr_count; i++) {
		ir_opcode opcode = instrs[i].opcode;
		b32 is_toplevel_instr = (usage_count[i] == 0 || opcode == IR_LABEL);
		if (is_toplevel_instr) {
			toplevel_instrs[toplevel_count++] = i;
		}
	}

	arena_temp_end(temp);
	program->toplevel_instr_indices = ALLOC(arena, toplevel_count, u32);
	program->toplevel_count = toplevel_count;
}

static ir_program
ir_generate(ast_node *root, arena *arena)
{
	ir_generator generator = ir_generator_init(arena);

	u32 function_count = 0;
	for (ast_node *node = root->u.children; node; node = node->next) {
		function_count += (node->kind == AST_FUNCTION);
	}

	generator.program.functions = ALLOC(arena, function_count, ir_function);
	generate(&generator, root);
	mark_toplevel_instructions(&generator.program, arena);
	construct_cfg(&generator.program, arena);
	return generator.program;
}
