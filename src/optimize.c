static u32
add(u32 a, u32 b)
{
	u32 result = a + b;
	return result;
}

static u32
sub(u32 a, u32 b)
{
	u32 result = a - b;
	return result;
}

static u32
multiply(u32 a, u32 b)
{
	u32 result = a * b;
	return result;
}

static void
optimize(ir_program program, arena *arena)
{
	ir_instr *instrs = program.instrs;

	b32 *addr_used = ALLOC(arena, program.instr_count, b32);
	for (u32 i = 0; i < program.instr_count; i++) {
		u32 op0 = instrs[i].op0;
		u32 op1 = instrs[i].op1;

		ir_opcode_info info = get_opcode_info(instrs[i].opcode);
		if (info.op0 == IR_OPERAND_REG_SRC && instrs[op0].opcode == IR_ALLOC) {
			addr_used[op0] |= true;
		}

		if (info.op1 == IR_OPERAND_REG_SRC && instrs[op1].opcode == IR_ALLOC) {
			addr_used[op1] |= true;
		}
	}

	for (u32 i = 0; i < program.instr_count; i++) {
		if (instrs[i].opcode == IR_LOAD) {
			u32 op0 = instrs[i].op0;
			if (instrs[op0].opcode == IR_ALLOC && !addr_used[op0]) {
				instrs[i].opcode = IR_MOV;
				instrs[i].op0 = i;
				instrs[i].op1 = op0;
			}
		} else if (instrs[i].opcode == IR_STORE) {
			u32 op0 = instrs[i].op0;
			if (instrs[op0].opcode == IR_ALLOC && !addr_used[op0]) {
				instrs[i].opcode = IR_MOV;
			}
		}
	}

	for (u32 i = 0; i < program.instr_count; i++) {
		if (instrs[i].opcode == IR_ALLOC) {
			if (!addr_used[i]) {
				instrs[i].opcode = IR_NOP;
			}
		}
	}

	for (u32 i = 0; i < program.instr_count; i++) {
		u32 op0 = instrs[i].op0;
		u32 op1 = instrs[i].op1;

		switch (instrs[i].opcode) {
		case IR_ADD:
			if (instrs[op0].opcode == IR_CONST
				&& instrs[op1].opcode == IR_CONST)
			{
				instrs[i].opcode = IR_ADD;
				instrs[op1].opcode = IR_NOP;
				instrs[op0].opcode = IR_NOP;
				instrs[i].op0 = add(instrs[op0].op0, instrs[op1].op0);
			} else if (instrs[op0].opcode == IR_CONST
				&& instrs[op0].op0 == 0)
			{
				instrs[op0].opcode = IR_NOP;
				instrs[i].opcode = IR_MOV;
				instrs[i].op0 = i;
			} else if (instrs[op1].opcode == IR_CONST
				&& instrs[op1].op0 == 0)
			{
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
				instrs[i].op0 = sub(instrs[op0].op0, instrs[op1].op0);
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
				instrs[i].op0 = multiply(instrs[op0].op0, instrs[op1].op0);
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
}
