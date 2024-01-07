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
promote_stack_variables(ir_program program, arena *arena)
{
	ir_instr *instrs = program.instrs;
	arena_temp temp = arena_temp_begin(arena);

	// Mark all stack variables which address is used by a different
	// instruction than load/store.
	b32 *addr_used = ALLOC(arena, program.instr_count, b32);
	for (u32 i = 0; i < program.instr_count; i++) {
		u32 opcode = instrs[i].opcode;
		u32 op0 = instrs[i].op0;
		u32 op1 = instrs[i].op1;

		ir_opcode_info info = get_opcode_info(opcode);
		if (opcode != IR_LOAD && info.op0 == IR_OPERAND_REG_SRC
			&& instrs[op0].opcode == IR_ALLOC)
		{
			addr_used[op0] = true;
		}

		if (info.op1 == IR_OPERAND_REG_SRC && instrs[op1].opcode == IR_ALLOC) {
			addr_used[op1] = true;
		}
	}

	// Promote stack variables to registers
	for (u32 i = 0; i < program.instr_count; i++) {
		if (instrs[i].opcode == IR_LOAD) {
			u32 op0 = instrs[i].op0;
			if (instrs[op0].opcode == IR_ALLOC && !addr_used[op0]) {
				instrs[i].opcode = IR_COPY;
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
				instrs[i].opcode = IR_VAR;
			}
		}
	}

	// Reallocate all stack allocations and fix the stack size for each function
	for (u32 i = 0; i < program.function_count; i++) {
		ir_function *function = &program.functions[i];
		u32 stack_size = 0;

		u32 first_instr = function->instr_index;
		u32 last_block_index = function->block_index + function->block_count - 1;
		ir_block *last_block = &program.blocks[last_block_index];
		u32 last_instr = last_block->start + last_block->size;

		for (u32 j = first_instr; j < last_instr; j++) {
			ir_instr *instr = &instrs[j];
			if (instr->opcode == IR_ALLOC) {
				instr->op1 = stack_size;
				stack_size += instr->op0;
			}
		}

		function->stack_size = stack_size;
	}

	arena_temp_end(temp);
}

static void
remove_unused_registers(ir_program program, arena *arena)
{
	ir_instr *instrs = program.instrs;
	arena_temp temp = arena_temp_begin(arena);

	// Remove unused registers
	b32 *used = ALLOC(arena, program.instr_count, b32);
	for (u32 j = 0; j < program.instr_count; j++) {
		u32 i = program.instr_count - 1 - j;
		ir_instr instr = program.instrs[i];
		ir_opcode_info info = get_opcode_info(instr.opcode);

		switch (instr.opcode) {
		case IR_STORE:
		case IR_PARAM:
		case IR_CALL:
		case IR_RET:
		case IR_MOV:
		case IR_PRINT:
		case IR_JIZ:
		case IR_JMP:
		case IR_LABEL: // TODO: Removal of unused labels
			used[i] = true;
		default:
			break;
		}

		if (!used[i]) {
			continue;
		}

		if (info.op0 == IR_OPERAND_REG_SRC || info.op0 == IR_OPERAND_REG_DST) {
			used[instr.op0] = true;
		}

		if (info.op1 == IR_OPERAND_REG_SRC || info.op1 == IR_OPERAND_REG_DST) {
			used[instr.op1] = true;
		}
	}

	for (u32 i = 0; i < program.instr_count; i++) {
		if (!used[i]) {
			instrs[i].opcode = IR_NOP;
		}
	}

	arena_temp_end(temp);
}

static void
optimize(ir_program program, arena *arena)
{
	ir_instr *instrs = program.instrs;

	promote_stack_variables(program, arena);

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
				instrs[i].opcode = IR_COPY;
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
				instrs[i].opcode = IR_COPY;
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

	// Remove register copies from the IR tree
	for (u32 i = 0; i < program.instr_count; i++) {
		ir_opcode_info info = get_opcode_info(instrs[i].opcode);

		u32 op0 = instrs[i].op0;
		if (is_register_operand(info.op0) && instrs[op0].opcode == IR_COPY) {
			instrs[i].op0 = instrs[op0].op0;
		}

		u32 op1 = instrs[i].op1;
		if (is_register_operand(info.op1) && instrs[op1].opcode == IR_COPY) {
			instrs[i].op1 = instrs[op1].op0;
		}
	}

	remove_unused_registers(program, arena);
}
