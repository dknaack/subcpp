static uint32_t
add(uint32_t a, uint32_t b)
{
	uint32_t result = a + b;
	return result;
}

static uint32_t
sub(uint32_t a, uint32_t b)
{
	uint32_t result = a - b;
	return result;
}

static uint32_t
multiply(uint32_t a, uint32_t b)
{
	uint32_t result = a * b;
	return result;
}

static void
optimize(struct ir_program program, struct arena *arena)
{
	(void)arena;
	struct ir_instr *instrs = program.instrs;
	for (uint32_t i = 0; i < program.instr_count; i++) {
		uint32_t op0 = instrs[i].op0;
		uint32_t op1 = instrs[i].op1;

		switch (instrs[i].opcode) {
		case IR_ADD:
			if (instrs[op0].opcode == IR_CONST
				&& instrs[op1].opcode == IR_CONST)
			{
				instrs[i].opcode = IR_ADD;
				instrs[op1].opcode = IR_NOP;
				instrs[op0].opcode = IR_NOP;
				instrs[i].op0 = add(
					instrs[op0].op0,
					instrs[op1].op0);
			} else if (instrs[op0].opcode == IR_CONST
				&& instrs[op0].op0 == 0)
			{
				instrs[op0].opcode = IR_NOP;
				instrs[i].opcode = IR_MOV;
				instrs[i].op0 = i;
			} else if (instrs[op1].opcode == IR_CONST
				&& instrs[op1].op0 == 0) {
				instrs[op1].opcode = IR_NOP;
				instrs[i].opcode = IR_MOV;
				instrs[i].op1 = op0;
				instrs[i].op0 = i;
			}
			break;
		case IR_SUB:
			if (instrs[op0].opcode == IR_CONST
				&& instrs[op1].opcode == IR_CONST)
			{
				instrs[i].opcode = IR_CONST;
				instrs[op1].opcode = IR_NOP;
				instrs[op0].opcode = IR_NOP;
				instrs[i].op0 = sub(
					instrs[op0].op0,
					instrs[op1].op0);
			} else if (instrs[op1].opcode == IR_CONST
				&& instrs[op1].op0 == 0)
			{
				instrs[op1].opcode = IR_NOP;
				instrs[i].opcode = IR_MOV;
				instrs[i].op1 = op0;
				instrs[i].op0 = i;
			}
			break;
		case IR_MUL:
			if (instrs[op0].opcode == IR_CONST
				&& instrs[op1].opcode == IR_CONST) {
				instrs[i].opcode = IR_CONST;
				instrs[op1].opcode = IR_NOP;
				instrs[op0].opcode = IR_NOP;
				/* TODO: evaluate depending on the target architecture */
				instrs[i].op0 = multiply(
					instrs[op0].op0,
					instrs[op1].op0);
			} else if (instrs[op0].opcode == IR_CONST
				&& instrs[op0].op0 == 1)
			{
				instrs[i].opcode = IR_MOV;
				instrs[i].op0 = i;
			} else if (instrs[op1].opcode == IR_CONST
				&& instrs[op1].op0 == 1)
			{
				instrs[i].opcode = IR_MOV;
				instrs[i].op1 = op0;
				instrs[i].op0 = i;
			}
			break;
		case IR_JIZ:
			if (instrs[op0].opcode == IR_CONST
				&& instrs[op0].op0 == 0)
			{
				instrs[op0].opcode = IR_NOP;
				instrs[i].opcode = IR_JMP;
				instrs[i].op0 = instrs[i].op1;
			}
			break;
		default:
			break;
		}
	}

	for (uint32_t i = 0; i < program.instr_count; i++) {
		uint32_t op0 = instrs[i].op0;
		uint32_t op1 = instrs[i].op1;
		(void)op0;
		(void)op1;

		while (instrs[op1].opcode == IR_MOV
			&& instrs[op1].op0 == op1) {
			instrs[op1].opcode = IR_NOP;
			instrs[i].op1 = instrs[op1].op1;
		}
	}
}
