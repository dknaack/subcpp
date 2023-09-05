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
new_register(struct generator *state, struct string identifier)
{
	struct variable *variable_table = state->variable_table;
	uint32_t h = hash(identifier);
	for (uint32_t j = 0; j < state->variable_table_size; j++) {
		uint32_t i = (h + j) & (state->variable_table_size - 1);
		if (i == 0) {
			continue;
		}

		if (!variable_table[i].name.at) {
			variable_table[i].name = identifier;
			variable_table[i]._register = new_temp_register(state);
			return variable_table[i]._register;
		}
	}

	ASSERT(!"OOM");
	return 0;
}

static uint32_t
get_function(struct generator *state, struct string identifier)
{
	for (uint32_t i = 0; i < state->program.function_count; i++) {
		struct ir_function *func = &state->program.functions[i];
		if (string_equals(func->name, identifier)) {
			return i;
		}
	}

	ASSERT(!"Function not defined");
	return state->program.function_count;
}

static uint32_t
get_register(struct generator *state, struct string identifier)
{
	struct variable *variable_table = state->variable_table;
	uint32_t h = hash(identifier);
	for (uint32_t j = 0; j < state->variable_table_size; j++) {
		uint32_t i = (h + j) & (state->variable_table_size - 1);
		if (i == 0) {
			continue;
		}

		if (string_equals(variable_table[i].name, identifier)) {
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
generate_label(struct generator *state, uint32_t label)
{
	emit3(state, IR_LABEL, 0, label, 0);
}

static uint32_t
generate_expr(struct generator *state, struct expr *expr)
{
	uint32_t lhs, rhs, label, result = 0;
	enum ir_opcode opcode;
	struct expr *called;

	switch (expr->kind) {
	case EXPR_BINARY:
		lhs = generate_expr(state, expr->u.binary.lhs);
		rhs = generate_expr(state, expr->u.binary.rhs);
		switch (expr->u.binary.op) {
		case TOKEN_ADD:    opcode = IR_ADD; break;
		case TOKEN_SUB:    opcode = IR_SUB; break;
		case TOKEN_MUL:    opcode = IR_MUL; break;
		case TOKEN_DIV:    opcode = IR_DIV; break;
		case TOKEN_MOD:    opcode = IR_MOD; break;
		case TOKEN_ASSIGN: opcode = IR_MOV; break;
		default:
			ASSERT(!"Invalid expr");
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
	case EXPR_CALL:
		called = expr->u.call.called;
		if (called->kind == EXPR_IDENTIFIER) {
			label = get_function(state, called->u.identifier);
			result = new_temp_register(state);
			emit2(state, IR_CALL, result, label);
		}
		break;
	case EXPR_IDENTIFIER:
		result = get_register(state, expr->u.identifier);
		break;
	case EXPR_INT:
		result = new_temp_register(state);
		emit2(state, IR_SET, result, expr->u.ival);
		break;
	}

	return result;
}

static void
generate_decl(struct generator *state, struct decl *decl)
{
	while (decl) {
		uint32_t _register = new_register(state, decl->name);
		if (decl->expr) {
			uint32_t expr = generate_expr(state, decl->expr);
			emit3(state, IR_MOV, _register, expr, 0);
		}

		decl = decl->next;
	}
}

static void
generate_stmt(struct generator *state, struct stmt *stmt)
{
	uint32_t endif_label, else_label, condition, result = 0;

	switch (stmt->kind) {
	case STMT_BREAK:
		emit2(state, IR_JMP, 0, state->break_label);
		break;
	case STMT_COMPOUND:
		for (stmt = stmt->u.compound; stmt; stmt = stmt->next) {
			generate_stmt(state, stmt);
		}
		break;
	case STMT_CONTINUE:
		emit2(state, IR_JMP, 0, state->continue_label);
		break;
	case STMT_DECL:
		generate_decl(state, stmt->u.decl);
		break;
	case STMT_EMPTY:
		break;
	case STMT_EXPR:
		generate_expr(state, stmt->u.expr);
		break;
	case STMT_FOR_EXPR:
	case STMT_FOR_DECL:
		state->break_label = new_label(state);
		state->continue_label = new_label(state);
		condition = new_label(state);

		if (stmt->kind == STMT_FOR_EXPR) {
			generate_expr(state, stmt->u._for.init.expr);
		} else {
			generate_decl(state, stmt->u._for.init.decl);
		}
		generate_label(state, condition);
		result = generate_expr(state, stmt->u._for.condition);
		emit3(state, IR_JIZ, 0, result, state->break_label);
		generate_stmt(state, stmt->u._for.body);
		generate_label(state, state->continue_label);
		generate_expr(state, stmt->u._for.post);
		emit3(state, IR_JMP, 0, condition, 0);
		generate_label(state, state->break_label);
		break;
	case STMT_IF:
		endif_label = new_label(state);
		else_label = new_label(state);

		condition = generate_expr(state, stmt->u._if.condition);
		emit3(state, IR_JIZ, 0, condition, else_label);
		generate_stmt(state, stmt->u._if.then);
		emit3(state, IR_JMP, 0, endif_label, 0);
		generate_label(state, else_label);
		if (stmt->u._if.otherwise) {
			generate_stmt(state, stmt->u._if.otherwise);
		}

		generate_label(state, endif_label);
		break;
	case STMT_WHILE:
		state->break_label = new_label(state);
		state->continue_label = new_label(state);

		generate_label(state, state->continue_label);
		result = generate_expr(state, stmt->u._while.condition);
		emit3(state, IR_JIZ, 0, result, state->break_label);
		generate_stmt(state, stmt->u._while.body);
		emit3(state, IR_JMP, 0, state->continue_label, 0);
		generate_label(state, state->break_label);
		break;
	case STMT_RETURN:
		if (stmt->u.expr) {
			result = generate_expr(state, stmt->u.expr);
		}
		emit3(state, IR_RET, 0, result, 0);
		break;
	case STMT_PRINT:
		result = generate_expr(state, stmt->u.expr);
		emit3(state, IR_PRINT, 0, result, 0);
		break;
	}
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
		if (instruction->opcode == IR_JMP) {
			uint32_t block = block_indices[instruction->op0];
			ASSERT(block > 0);
			instruction->op0 = block;
		} else if (instruction->opcode == IR_JIZ) {
			uint32_t block = block_indices[instruction->op1];
			ASSERT(block > 0);
			instruction->op1 = block;
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
			blocks[i].next[0] = instructions[block_end].op0;
			blocks[i].next[1] = instructions[block_end].op0;
			break;
		case IR_JIZ:
			blocks[i].next[0] = i + 1;
			blocks[i].next[1] = instructions[block_end].op1;
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
ir_generate(struct function *function, struct arena *arena)
{
	struct generator generator = generator_init(arena);

	for (struct function *f = function; f; f = f->next) {
		generator.program.function_count++;
	}

	generator.program.functions = ALLOC(arena,
	    generator.program.function_count, struct ir_function);

	uint32_t function_count = 0;
	for (; function; function = function->next) {
		uint32_t function_label = new_label(&generator);
		generate_label(&generator, function_label);

		struct ir_function *ir_function = &generator.program.functions[function_count++];
		ir_function->name = function->name;
		ir_function->block_index = function_label;

		for (struct stmt *stmt = function->body; stmt; stmt = stmt->next) {
			generate_stmt(&generator, stmt);
		}
	}

	construct_cfg(&generator.program, arena);
	return generator.program;
}
