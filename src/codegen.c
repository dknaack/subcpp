static struct generator
generator_init(struct arena *arena)
{
	struct generator state = {0};
	state.program.instructions = ALLOC(arena, 1024, struct ir_instruction);
	state.max_instruction_count = 1024;
	state.variable_table = ALLOC(arena, 1024, struct variable);
	state.variable_table_size = 1024;
	return state;
}

static uint32_t
new_label(struct generator *state)
{
	uint32_t result = state->program.label_count++;
	return result;
}

static uint32_t
hash(struct string str)
{
	uint32_t h = 0x811c9dc5;

	while (str.length-- > 0) {
		h *= 0x01000193;
		h ^= *str.at++;
	}

	return h;
}

static uint32_t
new_temp_register(struct generator *state)
{
	uint32_t result = state->program.register_count++;
	return result;
}

static uint32_t
new_register(struct generator *state, struct string ident)
{
	struct variable *variable_table = state->variable_table;
	uint32_t h = hash(ident);
	for (uint32_t j = 0; j < state->variable_table_size; j++) {
		uint32_t i = (h + j) & (state->variable_table_size - 1);
		if (i == 0) {
			continue;
		}

		if (!variable_table[i].name.at) {
			variable_table[i].name = ident;
			variable_table[i]._register = new_temp_register(state);
			return variable_table[i]._register;
		}
	}

	ASSERT(!"OOM");
	return 0;
}

static uint32_t
get_function(struct generator *state, struct string ident)
{
	for (uint32_t i = 0; i < state->program.function_count; i++) {
		struct ir_function *func = &state->program.functions[i];
		if (string_equals(func->name, ident)) {
			return i;
		}
	}

	ASSERT(!"Function not defined");
	return state->program.function_count;
}

static uint32_t
get_register(struct generator *state, struct string ident)
{
	struct variable *variable_table = state->variable_table;
	uint32_t h = hash(ident);
	for (uint32_t j = 0; j < state->variable_table_size; j++) {
		uint32_t i = (h + j) & (state->variable_table_size - 1);
		if (i == 0) {
			continue;
		}

		if (string_equals(variable_table[i].name, ident)) {
			return variable_table[i]._register;
		}
	}

	ASSERT(!"Variable not declared");
	return 0;
}

static void
emit3(struct generator *state, enum ir_opcode opcode,
    uint32_t dst, uint32_t op0, uint32_t op1)
{
	ASSERT(state->program.instruction_count <= state->max_instruction_count);
	struct ir_instruction *instruction = &state->program.instructions[state->program.instruction_count++];
	instruction->opcode = opcode;
	instruction->op0 = op0;
	instruction->op1 = op1;
	instruction->dst = dst;
}

static void
emit2(struct generator *state, enum ir_opcode opcode,
    uint32_t dst, uint32_t op0)
{
	emit3(state, opcode, dst, op0, 0);
}

static void
emit1(struct generator *state, enum ir_opcode opcode, uint32_t dst)
{
	emit3(state, opcode, dst, 0, 0);
}

static void
emit_label(struct generator *state, uint32_t label)
{
	emit3(state, IR_LABEL, 0, label, 0);
}

static uint32_t
generate(struct generator *state, struct ast_node *node)
{
	uint32_t endif_label, else_label, cond_label, function_label;
	uint32_t lhs, rhs, label, parameter_register, result = 0;
	struct ast_node *called, *parameter;
	enum ir_opcode opcode;

	switch (node->kind) {
	case AST_INVALID:
		ASSERT(!"Invalid node");
		break;
	case AST_BINARY:
		lhs = generate(state, node->u.bin_expr.lhs);
		rhs = generate(state, node->u.bin_expr.rhs);
		switch (node->u.bin_expr.op) {
		case TOKEN_ADD:    opcode = IR_ADD; break;
		case TOKEN_SUB:    opcode = IR_SUB; break;
		case TOKEN_MUL:    opcode = IR_MUL; break;
		case TOKEN_DIV:    opcode = IR_DIV; break;
		case TOKEN_MOD:    opcode = IR_MOD; break;
		case TOKEN_ASSIGN: opcode = IR_MOV; break;
		default:
			ASSERT(!"Invalid node");
			break;
		}

		if (opcode == IR_MOV) {
			result = lhs;
			emit3(state, IR_MOV, result, rhs, 0);
		} else {
			result = new_temp_register(state);
			emit3(state, opcode, result, lhs, rhs);
		}
		break;
	case AST_CALL:
		called = node->u.call_expr.called;
		if (called->kind == AST_IDENT) {
			label = get_function(state, called->u.ident);
			result = new_temp_register(state);
			parameter = node->u.call_expr.parameter;
			if (parameter) {
				parameter_register = generate(state, parameter);
				emit1(state, IR_PARAM, parameter_register);
			}
			emit2(state, IR_CALL, result, label);
		}
		break;
	case AST_IDENT:
		result = get_register(state, node->u.ident);
		break;
	case AST_INT:
		result = new_temp_register(state);
		emit2(state, IR_SET, result, node->u.ival);
		break;
	case AST_BREAK:
		emit1(state, IR_JMP, state->break_label);
		break;
	case AST_COMPOUND:
		for (node = node->u.children; node; node = node->next) {
			generate(state, node);
		}
		break;
	case AST_CONTINUE:
		emit1(state, IR_JMP, state->continue_label);
		break;
	case AST_DECL:
		result = new_register(state, node->u.decl.name);
		if (node->u.decl.expr) {
			uint32_t expr = generate(state, node->u.decl.expr);
			emit3(state, IR_MOV, result, expr, 0);
		}

		break;
	case AST_EMPTY:
		break;
	case AST_FOR:
		state->break_label = new_label(state);
		state->continue_label = new_label(state);
		cond_label = new_label(state);

		generate(state, node->u.for_stmt.init);
		emit_label(state, cond_label);
		result = generate(state, node->u.for_stmt.cond);
		emit2(state, IR_JIZ, state->break_label, result);
		generate(state, node->u.for_stmt.body);
		emit_label(state, state->continue_label);
		generate(state, node->u.for_stmt.post);
		emit1(state, IR_JMP, cond_label);
		emit_label(state, state->break_label);
		break;
	case AST_IF:
		endif_label = new_label(state);
		else_label = new_label(state);

		cond_label = generate(state, node->u.if_stmt.cond);
		emit2(state, IR_JIZ, else_label, cond_label);
		generate(state, node->u.if_stmt.then);
		emit1(state, IR_JMP, endif_label);
		emit_label(state, else_label);
		if (node->u.if_stmt.otherwise) {
			generate(state, node->u.if_stmt.otherwise);
		}

		emit_label(state, endif_label);
		break;
	case AST_WHILE:
		state->break_label = new_label(state);
		state->continue_label = new_label(state);

		emit_label(state, state->continue_label);
		result = generate(state, node->u.while_stmt.cond);
		emit2(state, IR_JIZ, state->break_label, result);
		generate(state, node->u.while_stmt.body);
		emit1(state, IR_JMP, state->continue_label);
		emit_label(state, state->break_label);
		break;
	case AST_RETURN:
		if (node->u.children) {
			result = generate(state, node->u.children);
		}
		emit3(state, IR_RET, 0, result, 0);
		break;
	case AST_PRINT:
		result = generate(state, node->u.children);
		emit3(state, IR_PRINT, 0, result, 0);
		break;
	case AST_FUNCTION:
		function_label = new_label(state);
		emit_label(state, function_label);
		parameter = node->u.function.parameter;
		if (parameter) {
			new_register(state, parameter->u.decl.name);
		}

		struct ir_function *ir_function = &state->program.functions[state->program.function_count++];
		ir_function->name = node->u.function.name;
		ir_function->block_index = function_label;

		for (struct ast_node *stmt = node->u.function.body; stmt; stmt = stmt->next) {
			generate(state, stmt);
		}
		break;
	}

	return result;
}

static bool
is_block_start(struct ir_instruction *instructions, uint32_t i)
{
	bool result = (i == 0 ||
	    instructions[i].opcode == IR_LABEL ||
	    instructions[i-1].opcode == IR_JMP ||
	    instructions[i-1].opcode == IR_JIZ ||
	    instructions[i-1].opcode == IR_RET);
	return result;
}

static void
construct_cfg(struct ir_program *program, struct arena *arena)
{
	uint32_t block_count = 0;
	for (uint32_t i = 0; i < program->instruction_count; i++) {
		if (is_block_start(program->instructions, i)) {
			block_count++;
		}
	}

	program->blocks = ALLOC(arena, block_count, struct ir_block);
	program->block_count = block_count;

	struct arena_temp temp = arena_temp_begin(arena);

	/* replace labels with block indices */
	uint32_t *block_indices = ALLOC(arena, program->label_count, uint32_t);
	uint32_t block_index = 0;
	for (uint32_t i = 0; i < program->instruction_count; i++) {
		if (is_block_start(program->instructions, i)) {
			uint32_t opcode = program->instructions[i].opcode;
			if (opcode == IR_LABEL) {
				uint32_t label = program->instructions[i].op0;
				block_indices[label] = block_index;
				program->instructions[i].op0 = block_indices[label];
			}

			program->blocks[block_index++].start = i;
		}
	}

	for (uint32_t i = 0; i < program->instruction_count; i++) {
		struct ir_instruction *instruction = &program->instructions[i];
		enum ir_opcode opcode = instruction->opcode;
		if (opcode == IR_JMP || opcode == IR_JIZ) {
			uint32_t block = block_indices[instruction->dst];
			ASSERT(block > 0);
			instruction->dst = block;
		}
	}

	for (uint32_t i = 0; i < program->function_count; i++) {
		uint32_t label = program->functions[i].block_index;
		uint32_t block_index = block_indices[label];
		program->functions[i].block_index = block_index;
		ASSERT(block_index < program->block_count);
		if (i > 0) {
			struct ir_function *prev_function = &program->functions[i - 1];
			prev_function->block_count = block_index - prev_function->block_index;
		}
	}

	struct ir_function *last_function = &program->functions[program->function_count - 1];
	last_function->block_count = program->block_count - last_function->block_index;

	arena_temp_end(temp);

	/* calculate size of each block */
	struct ir_block *blocks = program->blocks;
	for (uint32_t i = 0; i < program->block_count; i++) {
		if (i + 1 < program->block_count) {
			blocks[i].size = blocks[i+1].start - blocks[i].start;
		} else {
			blocks[i].size = program->instruction_count - blocks[i].start;
		}

		ASSERT(blocks[i].size > 0);
	}

	/* determine the next block */
	struct ir_instruction *instructions = program->instructions;
	for (uint32_t i = 0; i < program->block_count; i++) {
		uint32_t block_end = blocks[i].start + blocks[i].size - 1;
		switch (instructions[block_end].opcode) {
		case IR_JMP:
			blocks[i].next[0] = instructions[block_end].dst;
			blocks[i].next[1] = instructions[block_end].dst;
			break;
		case IR_JIZ:
			blocks[i].next[0] = i + 1;
			blocks[i].next[1] = instructions[block_end].dst;
			break;
		case IR_RET:
			blocks[i].next[0] = program->instruction_count;
			blocks[i].next[1] = program->instruction_count;
			break;
		default:
			blocks[i].next[0] = i + 1;
			blocks[i].next[1] = i + 1;
			break;
		}
	}
}

static struct ir_program
ir_generate(struct ast_node *root, struct arena *arena)
{
	struct generator generator = generator_init(arena);

	uint32_t function_count = 0;
	for (struct ast_node *node = root; node; node = node->next) {
		function_count += (node->kind == AST_FUNCTION);
	}

	generator.program.functions = ALLOC(arena, function_count, struct ir_function);
	for (struct ast_node *node = root; node; node = node->next) {
		generate(&generator, node);
	}

	construct_cfg(&generator.program, arena);
	return generator.program;
}
