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
new_register(struct generator *state)
{
	uint32_t result = state->program.register_count++;
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
get_register(struct generator *state, struct string identifier)
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
			variable_table[i]._register = new_register(state);
			return variable_table[i]._register;
		} else if (string_equals(variable_table[i].name, identifier)) {
			return variable_table[i]._register;
		}
	}

	ASSERT(!"Out of memory");
	return 0;
}

static void
emit(struct generator *state, enum ir_opcode opcode,
    uint32_t op0, uint32_t op1, uint32_t dst)
{
	ASSERT(state->program.instruction_count <= state->max_instruction_count);
	struct ir_instruction *instruction = &state->program.instructions[state->program.instruction_count++];
	instruction->opcode = opcode;
	instruction->op0 = op0;
	instruction->op1 = op1;
	instruction->dst = dst;
}

static uint32_t
emit2(struct generator *state, enum ir_opcode opcode, uint32_t op0, uint32_t op1)
{
	uint32_t result = new_register(state);
	emit(state, opcode, op0, op1, result);
	return result;
}

static uint32_t
emit1(struct generator *state, enum ir_opcode opcode, uint32_t op0)
{
	uint32_t result = emit2(state, opcode, op0, 0);
	return result;
}

static void
generate_label(struct generator *state, uint32_t label)
{
	emit(state, IR_LABEL, label, 0, 0);
}

static uint32_t
generate_expr(struct generator *state, struct expr *expr)
{
	enum ir_opcode opcode;
	uint32_t result = 0;
	uint32_t lhs, rhs;

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
			emit(state, IR_MOV, rhs, 0, result);
		} else {
			result = emit2(state, opcode, lhs, rhs);
		}
		break;
	case EXPR_IDENTIFIER:
		result = get_register(state, expr->u.identifier);
		break;
	case EXPR_INT:
		result = emit1(state, IR_SET, expr->u.ival);
		break;
	}

	return result;
}

static void
generate_stmt(struct generator *state, struct stmt *stmt)
{
	uint32_t endif_label, else_label, condition, result = 0;

	switch (stmt->kind) {
	case STMT_BREAK:
		emit(state, IR_JMP, state->break_label, 0, 0);
		break;
	case STMT_COMPOUND:
		for (stmt = stmt->u.compound; stmt; stmt = stmt->next) {
			generate_stmt(state, stmt);
		}
		break;
	case STMT_CONTINUE:
		emit(state, IR_JMP, state->continue_label, 0, 0);
		break;
	case STMT_EMPTY:
		break;
	case STMT_EXPR:
		generate_expr(state, stmt->u.expr);
		break;
	case STMT_FOR:
		state->break_label = new_label(state);
		state->continue_label = new_label(state);
		condition = new_label(state);

		generate_expr(state, stmt->u._for.init);
		generate_label(state, condition);
		result = generate_expr(state, stmt->u._for.condition);
		emit(state, IR_JIZ, result, state->break_label, 0);
		generate_stmt(state, stmt->u._for.body);
		generate_label(state, state->continue_label);
		generate_expr(state, stmt->u._for.post);
		emit(state, IR_JMP, condition, 0, 0);
		generate_label(state, state->break_label);
		break;
	case STMT_IF:
		endif_label = new_label(state);
		else_label = new_label(state);

		condition = generate_expr(state, stmt->u._if.condition);
		emit(state, IR_JIZ, condition, else_label, 0);
		generate_stmt(state, stmt->u._if.then);
		emit(state, IR_JMP, endif_label, 0, 0);
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
		emit(state, IR_JIZ, result, state->break_label, 0);
		generate_stmt(state, stmt->u._while.body);
		emit(state, IR_JMP, state->continue_label, 0, 0);
		generate_label(state, state->break_label);
		break;
	case STMT_RETURN:
		if (stmt->u.expr) {
			result = generate_expr(state, stmt->u.expr);
		}
		emit(state, IR_RET, result, 0, 0);
		break;
	case STMT_PRINT:
		result = generate_expr(state, stmt->u.expr);
		emit(state, IR_PRINT, result, 0, 0);
		break;
	}
}

static void
construct_cfg(struct ir_program *program, struct arena *arena)
{
	program->blocks = ALLOC(arena, 1, struct ir_block);
	program->blocks[0].start = 0;
	uint32_t prev_block = 0;
	for (uint32_t i = 0; i < program->instruction_count; i++) {
		uint32_t opcode = program->instructions[i].opcode;
		if (opcode == IR_JMP || opcode == IR_JIZ || opcode == IR_RET) {
			ALLOC(arena, 1, struct ir_block)->start = prev_block = i+1;
		} else if (prev_block != i && opcode == IR_LABEL) {
			ALLOC(arena, 1, struct ir_block)->start = i;
		}
	}

	struct arena_temp temp = arena_temp_begin(arena);

	/* replace labels with block indices */
	uint32_t block_count = 1;
	prev_block = 0;
	uint32_t *block_indices = ALLOC(arena, program->label_count, uint32_t);
	for (uint32_t i = 0; i < program->instruction_count; i++) {
		uint32_t opcode = program->instructions[i].opcode;
		if (opcode == IR_JMP || opcode == IR_JIZ || opcode == IR_RET) {
			block_count++;
			prev_block = i+1;
		} else if (prev_block != i && opcode == IR_LABEL) {
			uint32_t label = program->instructions[i].op0;
			block_indices[label] = ++block_count;
			program->instructions[i].op0 = block_indices[label];
		}
	}

	for (uint32_t i = 0; i < program->instruction_count; i++) {
		struct ir_instruction *instruction = &program->instructions[i];
		if (instruction->opcode == IR_JMP) {
			instruction->op0 = block_indices[instruction->op0];
		} else if (instruction->opcode == IR_JIZ) {
			instruction->op1 = block_indices[instruction->op1];
		}
	}

	arena_temp_end(temp);

	/* calculate size of each block */
	struct ir_block *blocks = program->blocks;
	for (uint32_t i = 0; i < block_count; i++) {
		if (i + 1 < block_count) {
			blocks[i].size = blocks[i+1].start - blocks[i].start;
		} else {
			blocks[i].size = program->instruction_count - blocks[i].start;
		}
	}

	/* determine the next block */
	struct ir_instruction *instructions = program->instructions;
	for (uint32_t i = 0; i < block_count; i++) {
		uint32_t block_end = blocks[i].start + blocks[i].size - 1;
		switch ((uint32_t)instructions[block_end].opcode) {
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
		}
	}
}

static struct ir_program
ir_generate(struct function *function, struct arena *arena)
{
	struct generator generator = generator_init(arena);
	for (struct stmt *stmt = function->body; stmt; stmt = stmt->next) {
		generate_stmt(&generator, stmt);
	}

	construct_cfg(&generator.program, arena);
	return generator.program;
}
