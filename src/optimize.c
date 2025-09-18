typedef struct {
	i8 *rank;
	i32 *parent;
	i32 *points_to;
	isize register_count;
} pointer_info;

typedef struct {
	i32 start;
	i32 size;
	i32 succ[2];
	i32 *pred;
	i32 pred_count;
} ir_block;

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

static isize
find_pointer_set(pointer_info *info, isize value)
{
	i32 *parent = info->parent;
	if (parent[value] != value) {
		parent[value] = find_pointer_set(info, parent[value]);
	}

	isize result = parent[value];
	return result;
}

static void
join_pointer_sets(pointer_info *info, isize dst, isize src)
{
	if (!dst || !src || dst == src) {
		return;
	}

	i8 *rank = info->rank;
	i32 *parent = info->parent;
	i32 *points_to = info->points_to;

	dst = find_pointer_set(info, dst);
	src = find_pointer_set(info, src);
	if (dst != src) {
		if (rank[dst] < rank[src]) {
			parent[dst] = src;
			rank[src] += 1;
		} else {
			parent[src] = dst;
			rank[dst] += 1;
		}
	}

	i32 pa = points_to[dst];
	i32 pb = points_to[src];
	if (pb != 0) {
		points_to[src] = points_to[dst];
	} else if (pa != 0) {
		points_to[dst] = points_to[src];
	} else {
		join_pointer_sets(info, pa, pb);
	}
}

static void
optimize(ir_program program, arena *arena)
{
	// Build a CFG
	isize block_count = 0;
	for (isize i = 0; i < program.inst_count; i++) {
		ir_opcode opcode = program.insts[i].opcode;
		if (opcode == IR_JMP || opcode == IR_JIZ || opcode == IR_JNZ) {
			block_count++;
		}
	}

	ir_block *blocks = ALLOC(arena, block_count, ir_block);
	isize block_index = 0;
	for (isize i = 0; i < program.inst_count; i++) {
		ir_opcode opcode = program.insts[i].opcode;
		if (opcode == IR_JMP || opcode == IR_JIZ || opcode == IR_JNZ) {
			blocks[block_index].size = i + 1 - blocks[block_index].start;
			block_index++;
			blocks[block_index].start = i + 1;
		}

		if (opcode == IR_JMP) {
			i32 arg0 = program.insts[i].args[0];
			blocks[arg0].pred_count++;
			blocks[block_index - 1].succ[0] = arg0;
			blocks[block_index - 1].succ[1] = arg0;
		} else if (opcode == IR_JIZ || opcode == IR_JNZ) {
			i32 arg1 = program.insts[i].args[1];
			blocks[arg1].pred_count++;
			blocks[block_index].pred_count++;
			blocks[block_index - 1].succ[0] = block_index;
			blocks[block_index - 1].succ[1] = arg1;
		}
	}

	i32 pred_offset = 0;
	i32 *block_preds = ALLOC(arena, block_count, i32);
	for (isize i = 0; i < block_count; i++) {
		pred_offset += blocks[i].pred_count;
		blocks[i].pred = block_preds + pred_offset;
	}

	block_index = 0;
	for (isize i = 0; i < program.inst_count; i++) {
		ir_opcode opcode = program.insts[i].opcode;
		if (opcode == IR_JMP) {
			i32 arg0 = program.insts[i].args[0];
			*--blocks[arg0].pred = block_index;
			block_index++;
		} else if (opcode == IR_JIZ || opcode == IR_JNZ) {
			i32 arg1 = program.insts[i].args[1];
			*--blocks[arg1].pred = block_index;
			*--blocks[block_index + 1].pred = block_index;
			block_index++;
		}
	}


	// Promote stack variables
	for (isize f = 0; f < program.func_count; f++) {
		ir_function *func = &program.funcs[f];
		// Mark all stack variables which address is used by a different
		// instruction than load/store.
		ir_inst *insts = program.insts + func->inst_index;

		// Determine what each variable can point to
		arena_temp temp = arena_temp_begin(arena);
		pointer_info pointer_info = {0};
		pointer_info.register_count = func->inst_count;
		pointer_info.rank = ALLOC(arena, pointer_info.register_count, i8);
		pointer_info.parent = ALLOC(arena, pointer_info.register_count, i32);
		pointer_info.points_to = ALLOC(arena, pointer_info.register_count, i32);

		// initialize the sets
		for (isize i = 0; i < func->inst_count; i++) {
			pointer_info.parent[i] = i;
		}

		for (isize i = 0; i < func->inst_count; i++) {
			ir_opcode opcode = insts[i].opcode;
			ir_opcode_info info = get_opcode_info(opcode);
			i32 arg0 = insts[i].args[0];
			i32 arg1 = insts[i].args[1];
			i32 dst = i;

			if (opcode == IR_STORE) {
				join_pointer_sets(&pointer_info, pointer_info.points_to[arg0], arg1);
			} else if (insts[i].opcode == IR_LOAD) {
				join_pointer_sets(&pointer_info, dst, pointer_info.points_to[arg0]);
			} else {
				if (info.args[0] == IR_ARG_REG_SRC) {
					join_pointer_sets(&pointer_info, dst, arg0);
				}

				if (info.args[1] == IR_ARG_REG_SRC) {
					join_pointer_sets(&pointer_info, dst, arg1);
				}
			}
		}

		// Mark all sets based on the points-to analysis
		b8 *has_escaped = ALLOC(arena, func->inst_count, b8);
		for (isize i = 0; i < func->inst_count; i++) {
			i32 set = 0;
			i32 arg0 = insts[i].args[0];
			i32 dst = i;
			if (insts[i].opcode == IR_CALL || insts[i].opcode == IR_RET) {
				set = find_pointer_set(&pointer_info, arg0);
			} else if (insts[i].opcode == IR_PARAM) {
				set = find_pointer_set(&pointer_info, dst);
			}

			if (set != 0) {
				has_escaped[set] = true;
			}
		}

		// Mark all registers that escaped
		for (isize i = 0; i < func->inst_count; i++) {
			isize set = find_pointer_set(&pointer_info, i);
			has_escaped[i] = has_escaped[set];
		}

		// Assign each allocation instruction a stack slot
		for (isize i = 0; i < func->inst_count; i++) {
			if (has_escaped[i] && insts[i].opcode == IR_ALLOC) {
				printf("%zd has escaped\n", i);
			}
		}

		for (isize i = 0; i < func->inst_count; i++) {
			if (insts[i].opcode != IR_ALLOC || has_escaped[i]) {
				continue;
			}
		}

		arena_temp_end(temp);
	}

	// Constant-folding
	for (isize i = 0; i < program.func_count; i++) {
		ir_function *func = &program.funcs[i];
		ir_inst *insts = program.insts + func->inst_index;
		for (isize i = 0; i < func->inst_count; i++) {
			u32 arg0 = insts[i].args[0];
			u32 arg1 = insts[i].args[1];

			switch (insts[i].opcode) {
			case IR_ADD:
				if (insts[arg0].opcode == IR_CONST
					&& insts[arg1].opcode == IR_CONST)
				{
					insts[i].opcode = IR_CONST;
					insts[arg1].opcode = IR_NOP;
					insts[arg0].opcode = IR_NOP;
					insts[i].args[0] = add(insts[arg0].args[0], insts[arg1].args[0]);
#if 0
				} else if (insts[arg0].opcode == IR_CONST
					&& insts[arg0].args[0] == 0)
				{
					insts[arg0].opcode = IR_NOP;
					insts[i].opcode = IR_MOV;
					insts[i].args[0] = i;
#endif
				} else if (insts[arg1].opcode == IR_CONST
					&& insts[arg1].args[0] == 0)
				{
					insts[arg1].opcode = IR_NOP;
					insts[i].opcode = IR_COPY;
				}
				break;
			case IR_SUB:
				if (insts[arg0].opcode == IR_CONST
					&& insts[arg1].opcode == IR_CONST)
				{
					insts[i].opcode = IR_CONST;
					insts[arg1].opcode = IR_NOP;
					insts[arg0].opcode = IR_NOP;
					insts[i].args[0] = sub(insts[arg0].args[0], insts[arg1].args[0]);
				} else if (insts[arg1].opcode == IR_CONST
					&& insts[arg1].args[0] == 0)
				{
					insts[arg1].opcode = IR_NOP;
					insts[i].opcode = IR_COPY;
				}
				break;
			case IR_MUL:
				if (insts[arg0].opcode == IR_CONST
					&& insts[arg1].opcode == IR_CONST) {
					insts[i].opcode = IR_CONST;
					insts[arg1].opcode = IR_NOP;
					insts[arg0].opcode = IR_NOP;
					/* TODO: evaluate depending on the target architecture */
					insts[i].args[0] = multiply(insts[arg0].args[0], insts[arg1].args[0]);
#if 0
				} else if (insts[arg0].opcode == IR_CONST
					&& insts[arg0].args[0] == 1)
				{
					insts[i].opcode = IR_MOV;
					insts[i].args[0] = i;
				} else if (insts[arg1].opcode == IR_CONST
					&& insts[arg1].args[0] == 1)
				{
					insts[i].opcode = IR_MOV;
					insts[i].args[1] = arg0;
					insts[i].args[0] = i;
#endif
				}
				break;
			case IR_JIZ:
				if (insts[arg0].opcode == IR_CONST
					&& insts[arg0].args[0] == 0)
				{
					insts[arg0].opcode = IR_NOP;
					insts[i].opcode = IR_JMP;
					insts[i].args[0] = insts[i].args[1];
				}
				break;
			default:
				break;
			}
		}
	}

	// Eliminate dead code
	{
		b8 *reachable = ALLOC(arena, program.max_label_count, b8);
		u32 *stack = ALLOC(arena, program.max_label_count, u32);
		u32 *label_addresses = ALLOC(arena, program.max_label_count, u32);
		for (isize i = 0; i < program.func_count; i++) {
			ir_function *func = &program.funcs[i];
			memset(reachable, 0, program.max_label_count * sizeof(*reachable));
			ir_inst *inst = program.insts + func->inst_index;

			// Get the address of each label
			for (isize j = 0; j < func->inst_count; j++) {
				u32 j = inst[i].args[0];
				if (inst[j].opcode == IR_LABEL) {
					label_addresses[inst[j].args[0]] = i;
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
				u32 start = label_addresses[label];
				for (isize j = start; j < func->inst_count; j++) {
					u32 i = inst[j].args[0];
					u32 new_label = 0;
					switch (inst[i].opcode) {
					case IR_JMP:
					case IR_LABEL:
						new_label = inst[i].args[0];
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
						new_label = inst[i].args[1];
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

			for (isize label = 1; label < program.max_label_count; label++) {
				if (reachable[label]) {
					continue;
				}

				u32 start = label_addresses[label];
				// Remove the label
				inst[inst[start].args[0]].opcode = IR_NOP;
				for (isize j = start; j; j = inst[j].args[1]) {
					u32 i = inst[j].args[0];
					if (inst[i].opcode == IR_LABEL) {
						break;
					}

					inst[i].opcode = IR_NOP;
				}
			}
		}
	}

	// Remove register copies from the IR tree
	for (isize i = 0; i < program.func_count; i++) {
		ir_function *func = &program.funcs[i];
		ir_inst *insts = program.insts + func->inst_index;
		for (isize i = 0; i < func->inst_count; i++) {
			ir_opcode_info info = get_opcode_info(insts[i].opcode);

			u32 arg0 = insts[i].args[0];
			if (is_register_operand(info.args[0]) && insts[arg0].opcode == IR_COPY) {
				insts[i].args[0] = insts[arg0].args[0];
			}

			u32 arg1 = insts[i].args[1];
			if (is_register_operand(info.args[1]) && insts[arg1].opcode == IR_COPY) {
				insts[i].args[1] = insts[arg1].args[0];
			}
		}
	}

	// Remove unused registers
	for (isize i = 0; i < program.func_count; i++) {
		ir_function *func = &program.funcs[i];
		arena_temp temp = arena_temp_begin(arena);
		ir_inst *inst = program.insts + func->inst_index;

		i32 *ref_count = get_ref_count(inst, func->inst_count, arena);
		for (isize j = 0; j < func->inst_count; j++) {
			b32 has_side_effect;
			ir_opcode opcode = inst[j].opcode;
			switch (opcode) {
			case IR_ALLOC:
			case IR_LABEL:
			case IR_PARAM:
			case IR_LOAD:
			case IR_STORE:
			case IR_CALL:
			case IR_JIZ:
			case IR_JMP:
			case IR_JNZ:
			case IR_RET:
				has_side_effect = true;
				break;
			default:
				has_side_effect = false;
				break;
			}
			if (!(ref_count[j] >= 1 || has_side_effect)) {
				inst[j].opcode = IR_NOP;
			}
		}

		arena_temp_end(temp);
	}
}
