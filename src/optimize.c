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
	for (isize f = 0; f < program.function_count; f++) {
		ir_function *func = &program.functions[f];
		// Mark all stack variables which address is used by a different
		// instruction than load/store.
		ir_inst *insts = program.insts + func->inst_index;
		arena_temp temp = arena_temp_begin(arena);
		b32 *addr_used = ALLOC(arena, func->inst_count, b32);
		ir_type *types = ALLOC(arena, func->inst_count, b32);
		for (isize i = 0; i < func->inst_count; i++) {
			u32 opcode = insts[i].opcode;
			u32 op0 = insts[i].op0;
			u32 op1 = insts[i].op1;

			// Can only turn scalars into registers, not arrays or structs
			if (insts[i].opcode == IR_ALLOC && insts[i].op0 > 8) {
				addr_used[i] = true;
			}

			ir_opcode_info info = get_opcode_info(opcode);
			if (opcode != IR_LOAD && info.op0 == IR_OPERAND_REG_SRC
				&& insts[op0].opcode == IR_ALLOC)
			{
				addr_used[op0] = true;
			}

			if (info.op1 == IR_OPERAND_REG_SRC && insts[op1].opcode == IR_ALLOC) {
				addr_used[op1] = true;
			}
		}

		// Promote stack variables to registers
		for (isize i = 0; i < func->inst_count; i++) {
			if (insts[i].opcode == IR_LOAD) {
				u32 op0 = insts[i].op0;
				if (insts[op0].opcode == IR_ALLOC && !addr_used[op0]) {
					types[op0] = insts[i].type;
					insts[i].opcode = IR_COPY;
				}
			} else if (insts[i].opcode == IR_STORE) {
				u32 op0 = insts[i].op0;
				if (insts[op0].opcode == IR_ALLOC && !addr_used[op0]) {
					u32 op1 = insts[i].op1;
					types[op0] = insts[op1].type;
					insts[i].opcode = IR_MOV;
				}
			}
		}

		for (isize i = 0; i < func->inst_count; i++) {
			if (insts[i].opcode == IR_ALLOC) {
				if (!addr_used[i]) {
					insts[i].opcode = IR_VAR;
					insts[i].type = types[i];
					ASSERT(insts[i].type != IR_VOID);
					// Can only turn scalars into registers, not arrays or structs
					ASSERT(insts[i].op0 <= 8);
				}
			}
		}

		// Reallocate all stack allocations and fix the stack size for each function
		u32 stack_size = 0;
		for (isize j = 0; j < func->inst_count; j++) {
			ir_inst *inst = &insts[j];
			if (inst->opcode == IR_ALLOC) {
				// TODO: Alignment
				inst->op1 = stack_size;
				stack_size += inst->op0;
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
	for (isize i = 0; i < program.function_count; i++) {
		ir_function *func = &program.functions[i];
		arena_temp temp = arena_temp_begin(arena);
		ir_inst *insts = program.insts + func->inst_index;

		b32 *used = ALLOC(arena, func->inst_count, b32);
		for (isize j = 0; j < func->inst_count; j++) {
			isize i = func->inst_count - 1 - j;
			ir_inst inst = insts[i];
			switch (inst.opcode) {
			case IR_STORE:
			case IR_PARAM:
			case IR_CALL:
			case IR_RET:
			case IR_MOV:
			case IR_JIZ:
			case IR_JNZ:
			case IR_JMP:
			case IR_LABEL: // TODO: Removal of unused labels
				used[i] = true;
				break;
			default:
				if (!used[i]) {
					continue;
				}
			}

			ir_opcode_info info = get_opcode_info(inst.opcode);
			if (info.op0 == IR_OPERAND_REG_SRC || info.op0 == IR_OPERAND_REG_DST) {
				used[inst.op0] = true;
			}

			if (info.op1 == IR_OPERAND_REG_SRC || info.op1 == IR_OPERAND_REG_DST) {
				used[inst.op1] = true;
			}
		}

		for (isize i = func->param_count; i < func->inst_count; i++) {
			if (!used[i]) {
				insts[i].opcode = IR_NOP;
			}
		}

		arena_temp_end(temp);
	}
}

static void
optimize(ir_program program, arena *arena)
{
	promote_stack_variables(program, arena);

	for (isize i = 0; i < program.function_count; i++) {
		ir_function *func = &program.functions[i];
		ir_inst *insts = program.insts + func->inst_index;
		for (isize i = 0; i < func->inst_count; i++) {
			u32 op0 = insts[i].op0;
			u32 op1 = insts[i].op1;

			switch (insts[i].opcode) {
			case IR_ADD:
				if (insts[op0].opcode == IR_CONST
					&& insts[op1].opcode == IR_CONST)
				{
					insts[i].opcode = IR_CONST;
					insts[op1].opcode = IR_NOP;
					insts[op0].opcode = IR_NOP;
					insts[i].op0 = add(insts[op0].op0, insts[op1].op0);
				} else if (insts[op0].opcode == IR_CONST
					&& insts[op0].op0 == 0)
				{
					insts[op0].opcode = IR_NOP;
					insts[i].opcode = IR_MOV;
					insts[i].op0 = i;
				} else if (insts[op1].opcode == IR_CONST
					&& insts[op1].op0 == 0)
				{
					insts[op1].opcode = IR_NOP;
					insts[i].opcode = IR_COPY;
				}
				break;
			case IR_SUB:
				if (insts[op0].opcode == IR_CONST
					&& insts[op1].opcode == IR_CONST)
				{
					insts[i].opcode = IR_CONST;
					insts[op1].opcode = IR_NOP;
					insts[op0].opcode = IR_NOP;
					insts[i].op0 = sub(insts[op0].op0, insts[op1].op0);
				} else if (insts[op1].opcode == IR_CONST
					&& insts[op1].op0 == 0)
				{
					insts[op1].opcode = IR_NOP;
					insts[i].opcode = IR_COPY;
				}
				break;
			case IR_MUL:
				if (insts[op0].opcode == IR_CONST
					&& insts[op1].opcode == IR_CONST) {
					insts[i].opcode = IR_CONST;
					insts[op1].opcode = IR_NOP;
					insts[op0].opcode = IR_NOP;
					/* TODO: evaluate depending on the target architecture */
					insts[i].op0 = multiply(insts[op0].op0, insts[op1].op0);
				} else if (insts[op0].opcode == IR_CONST
					&& insts[op0].op0 == 1)
				{
					insts[i].opcode = IR_MOV;
					insts[i].op0 = i;
				} else if (insts[op1].opcode == IR_CONST
					&& insts[op1].op0 == 1)
				{
					insts[i].opcode = IR_MOV;
					insts[i].op1 = op0;
					insts[i].op0 = i;
				}
				break;
			case IR_JIZ:
				if (insts[op0].opcode == IR_CONST
					&& insts[op0].op0 == 0)
				{
					insts[op0].opcode = IR_NOP;
					insts[i].opcode = IR_JMP;
					insts[i].op0 = insts[i].op1;
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
		for (isize i = 0; i < program.function_count; i++) {
			ir_function *func = &program.functions[i];
			memset(reachable, 0, program.label_count * sizeof(*reachable));
			ir_inst *inst = program.insts + func->inst_index;

			// Get the address of each label
			for (isize i = 0; i < func->inst_count; i++) {
				if (inst[i].opcode == IR_LABEL) {
					label_addresses[inst[i].op0] = i;
				}
			}

			// Do a depth first search on the labels, start with the first
			// instruction in the function.
			i32 stack_pos = 0;
			stack[stack_pos++] = 1;
			reachable[1] = true;
next_block:
			while (stack_pos > 0) {
				isize label = stack[--stack_pos];

				// NOTE: The first instruction is the label itself
				u32 start = label_addresses[label] + 1;
				for (isize i = start; i < func->inst_count; i++) {
					u32 new_label = 0;
					switch (inst[i].opcode) {
					case IR_JMP:
					case IR_LABEL:
						new_label = inst[i].op0;
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
						new_label = inst[i].op1;
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

			for (isize label = 1; label < func->label_count; label++) {
				if (reachable[label]) {
					continue;
				}

				u32 start = label_addresses[label];
				inst[start].opcode = IR_NOP;
				for (isize i = start; i < func->inst_count; i++) {
					if (inst[i].opcode == IR_LABEL) {
						break;
					}

					inst[i].opcode = IR_NOP;
				}
			}
		}
	}

	// Remove register copies from the IR tree
	for (isize i = 0; i < program.function_count; i++) {
		ir_function *func = &program.functions[i];
		ir_inst *insts = program.insts + func->inst_index;
		for (isize i = 0; i < func->inst_count; i++) {
			ir_opcode_info info = get_opcode_info(insts[i].opcode);

			u32 op0 = insts[i].op0;
			if (is_register_operand(info.op0) && insts[op0].opcode == IR_COPY) {
				insts[i].op0 = insts[op0].op0;
			}

			u32 op1 = insts[i].op1;
			if (is_register_operand(info.op1) && insts[op1].opcode == IR_COPY) {
				insts[i].op1 = insts[op1].op0;
			}
		}
	}

	remove_unused_registers(program, arena);
}
