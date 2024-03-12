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
	for (ir_function *func = program.function_list; func; func = func->next) {
		// Mark all stack variables which address is used by a different
		// instruction than load/store.
		ir_instr *instrs = program.instrs + func->instr_index;
		arena_temp temp = arena_temp_begin(arena);
		b32 *addr_used = ALLOC(arena, func->instr_count, b32);
		for (u32 i = 0; i < func->instr_count; i++) {
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
		for (u32 i = 0; i < func->instr_count; i++) {
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

		for (u32 i = 0; i < func->instr_count; i++) {
			if (instrs[i].opcode == IR_ALLOC) {
				if (!addr_used[i]) {
					instrs[i].opcode = IR_VAR;
					instrs[i].size = instrs[i].op0;
					// Can only turn scalars into registers, not arrays or structs
					ASSERT(instrs[i].op0 <= 8);
				}
			}
		}

		// Reallocate all stack allocations and fix the stack size for each function
		u32 stack_size = 0;
		for (u32 j = 0; j < func->instr_count; j++) {
			ir_instr *instr = &instrs[j];
			if (instr->opcode == IR_ALLOC) {
				// TODO: Alignment
				instr->op1 = stack_size;
				stack_size += instr->op0;
			}
		}

		func->stack_size = stack_size;

		arena_temp_end(temp);
	}
}

static void
remove_unused_registers(ir_program program, arena *arena)
{
	// Remove unused registers
	for (ir_function *func = program.function_list; func; func = func->next) {
		arena_temp temp = arena_temp_begin(arena);
		ir_instr *instrs = program.instrs + func->instr_index;

		b32 *used = ALLOC(arena, func->instr_count, b32);
		for (u32 j = 0; j < func->instr_count; j++) {
			u32 i = func->instr_count - 1 - j;
			ir_instr instr = instrs[i];
			ir_opcode_info info = get_opcode_info(instr.opcode);

			switch (instr.opcode) {
			case IR_STORE:
			case IR_PARAM:
			case IR_CALL:
			case IR_RET:
			case IR_MOV:
			case IR_PRINT:
			case IR_JIZ:
			case IR_JNZ:
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

		for (u32 i = func->parameter_count; i < func->instr_count; i++) {
			if (!used[i]) {
				instrs[i].opcode = IR_NOP;
			}
		}

		arena_temp_end(temp);
	}
}

static void
optimize(ir_program program, arena *arena)
{
	promote_stack_variables(program, arena);

	for (ir_function *func = program.function_list; func; func = func->next) {
		ir_instr *instrs = program.instrs + func->instr_index;
		for (u32 i = 0; i < func->instr_count; i++) {
			u32 op0 = instrs[i].op0;
			u32 op1 = instrs[i].op1;

			switch (instrs[i].opcode) {
			case IR_ADD:
				if (instrs[op0].opcode == IR_INT
					&& instrs[op1].opcode == IR_INT)
				{
					instrs[i].opcode = IR_ADD;
					instrs[op1].opcode = IR_NOP;
					instrs[op0].opcode = IR_NOP;
					instrs[i].op0 = add(instrs[op0].op0, instrs[op1].op0);
				} else if (instrs[op0].opcode == IR_INT
					&& instrs[op0].op0 == 0)
				{
					instrs[op0].opcode = IR_NOP;
					instrs[i].opcode = IR_MOV;
					instrs[i].op0 = i;
				} else if (instrs[op1].opcode == IR_INT
					&& instrs[op1].op0 == 0)
				{
					instrs[op1].opcode = IR_NOP;
					instrs[i].opcode = IR_COPY;
				}
				break;
			case IR_SUB:
				if (instrs[op0].opcode == IR_INT
					&& instrs[op1].opcode == IR_INT)
				{
					instrs[i].opcode = IR_INT;
					instrs[op1].opcode = IR_NOP;
					instrs[op0].opcode = IR_NOP;
					instrs[i].op0 = sub(instrs[op0].op0, instrs[op1].op0);
				} else if (instrs[op1].opcode == IR_INT
					&& instrs[op1].op0 == 0)
				{
					instrs[op1].opcode = IR_NOP;
					instrs[i].opcode = IR_COPY;
				}
				break;
			case IR_MUL:
				if (instrs[op0].opcode == IR_INT
					&& instrs[op1].opcode == IR_INT) {
					instrs[i].opcode = IR_INT;
					instrs[op1].opcode = IR_NOP;
					instrs[op0].opcode = IR_NOP;
					/* TODO: evaluate depending on the target architecture */
					instrs[i].op0 = multiply(instrs[op0].op0, instrs[op1].op0);
				} else if (instrs[op0].opcode == IR_INT
					&& instrs[op0].op0 == 1)
				{
					instrs[i].opcode = IR_MOV;
					instrs[i].op0 = i;
				} else if (instrs[op1].opcode == IR_INT
					&& instrs[op1].op0 == 1)
				{
					instrs[i].opcode = IR_MOV;
					instrs[i].op1 = op0;
					instrs[i].op0 = i;
				}
				break;
			case IR_JIZ:
				if (instrs[op0].opcode == IR_INT
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

	// Eliminate dead code
	{
		b8 *reachable = ALLOC(arena, program.label_count, b8);
		u32 *stack = ALLOC(arena, program.label_count, u32);
		u32 *label_addresses = ALLOC(arena, program.label_count, u32);
		for (ir_function *func = program.function_list; func; func = func->next) {
			memset(reachable, 0, program.label_count * sizeof(*reachable));
			ir_instr *instr = program.instrs + func->instr_index;

			// Get the address of each label
			for (u32 i = 0; i < func->instr_count; i++) {
				if (instr[i].opcode == IR_LABEL) {
					label_addresses[instr[i].op0] = i;
				}
			}

			// Do a depth first search on the labels, start with the first
			// instruction in the function.
			i32 stack_pos = 0;
			stack[stack_pos++] = 1;
			reachable[1] = true;
next_block:
			while (stack_pos > 0) {
				u32 label = stack[--stack_pos];

				// NOTE: The first instruction is the label itself
				u32 start = label_addresses[label] + 1;
				for (u32 i = start; i < func->instr_count; i++) {
					u32 new_label = 0;
					switch (instr[i].opcode) {
					case IR_JMP:
					case IR_LABEL:
						new_label = instr[i].op0;
						if (!reachable[new_label]) {
							stack[stack_pos++] = new_label;
							reachable[new_label] = true;
						}
						goto next_block;
					case IR_RET:
						goto next_block;
					case IR_JIZ:
					case IR_JNZ:
						// TODO: Check if the condition is zero or not.
						new_label = instr[i].op1;
						if (!reachable[new_label]) {
							stack[stack_pos++] = new_label;
							reachable[new_label] = true;
						}
						break;
					default:
						break;
					}
				}
			}

			for (u32 label = 1; label < func->label_count; label++) {
				if (reachable[label]) {
					continue;
				}

				u32 start = label_addresses[label];
				instr[start].opcode = IR_NOP;
				for (u32 i = start; i < func->instr_count; i++) {
					if (instr[i].opcode == IR_LABEL) {
						break;
					}

					instr[i].opcode = IR_NOP;
				}
			}
		}
	}

	// Remove register copies from the IR tree
	for (ir_function *func = program.function_list; func; func = func->next) {
		ir_instr *instrs = program.instrs + func->instr_index;
		for (u32 i = 0; i < func->instr_count; i++) {
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
	}

	remove_unused_registers(program, arena);
}
