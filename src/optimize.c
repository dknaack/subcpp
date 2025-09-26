typedef struct {
	i8 *rank;
	i32 *parent;
	i32 *points_to;
	isize register_count;
} pointer_info;

typedef struct {
	ir_inst *insts;
	ir_block *blocks;
	i32 *incomplete_phis;
	i32 *current_def;
	b8 *sealed_blocks;

	isize max_inst_count;
	isize inst_count;
	isize block_count;
	isize var_count;
} ssa_context;

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

static i32
new_phi(ssa_context *ctx, i32 block_id)
{
	i32 result = ctx->inst_count;

	ASSERT(block_id < ctx->block_count);
	i32 inst_count = ctx->blocks[block_id].pred_count;
	while (ctx->inst_count + inst_count < ctx->max_inst_count) {
		ctx->max_inst_count *= 2;
		ctx->insts = realloc(ctx->insts, ctx->max_inst_count * sizeof(*ctx->insts));
	}

	for (isize j = 0; j < inst_count; j++) {
		isize i = result + j;
		ctx->insts[i].opcode = IR_PHI;
		if (j + 1 < inst_count) {
			ctx->insts[i].args[1] = i + 1;
			ctx->insts[i].args[0] = 0;
		}
	}

	ctx->inst_count += inst_count;
	return result;
}

static i32
read_var(ssa_context *ctx, isize var_id, isize block_id)
{
	i32 value = 0;
	isize offset = block_id * ctx->var_count + var_id;

	if (ctx->current_def[offset]) {
		value = ctx->current_def[offset];
		return value;
	}

	if (!ctx->sealed_blocks[block_id]) {
		value = new_phi(ctx, block_id);
		ctx->incomplete_phis[offset] = value;
	} else if (ctx->blocks[block_id].pred_count == 1) {
		isize pred_id = ctx->blocks[block_id].pred[0];
		value = read_var(ctx, var_id, pred_id);
	} else {
		value = new_phi(ctx, block_id);
		ctx->current_def[offset] = value;

		isize pred_count = ctx->blocks[block_id].pred_count;
		while (pred_count-- > 0) {
			isize pred_id = ctx->blocks[block_id].pred[pred_count];
			i32 pred_value = read_var(ctx, var_id, pred_id);
			ctx->insts[value].args[0] = pred_value;
		}
	}

	ctx->current_def[offset] = value;
	return value;
}

static void
remove_copy_insts(ir_inst *insts, isize inst_count)
{
	for (isize i = 0; i < inst_count; i++) {
		ir_opcode_info info = get_opcode_info(insts[i].opcode);

		i32 arg0 = insts[i].args[0];
		if (info.usage[0] != 0 && insts[arg0].opcode == IR_COPY) {
			insts[i].args[0] = insts[arg0].args[0];
			ASSERT(insts[insts[i].args[0]].opcode != IR_NOP);
		}

		i32 arg1 = insts[i].args[1];
		if (info.usage[1] != 0 && insts[arg1].opcode == IR_COPY) {
			insts[i].args[1] = insts[arg1].args[0];
			ASSERT(insts[insts[i].args[1]].opcode != IR_NOP);
		}
	}

	for (isize i = 0; i < inst_count; i++) {
		if (insts[i].opcode == IR_COPY) {
			insts[i].opcode = IR_NOP;
		}
	}
}

static void
optimize(ir_program program, arena *arena)
{
	for (isize func_id = 0; func_id < program.func_count; func_id++) {
		ir_function *func = &program.funcs[func_id];
		ir_inst *insts = func->insts;
		arena_temp temp = arena_temp_begin(arena);

		//
		// CFG Construction
		//

		isize block_count = 0;
		for (isize i = 0; i < func->inst_count; i++) {
			ir_opcode opcode = insts[i].opcode;
			if (opcode == IR_LABEL) {
				block_count++;
			}
		}

		ir_opcode prev_opcode = IR_NOP;
		ir_block *blocks = ALLOC(arena, block_count, ir_block);
		isize block_index = 0;
		for (isize i = 0; i < func->inst_count; i++) {
			ir_opcode opcode = insts[i].opcode;
			if (opcode == IR_LABEL) {
				block_index = insts[i].args[0];
				if (i > 0) {
					blocks[block_index].end = i;
					if (prev_opcode == IR_JIZ || prev_opcode == IR_JNZ) {
						blocks[block_index].succ[1] = block_index;
						blocks[block_index].pred_count++;
					} else if (prev_opcode != IR_JMP) {
						blocks[block_index].succ[0] = block_index;
						blocks[block_index].succ[1] = block_index;
						blocks[block_index].pred_count++;
					}
				}

				blocks[block_index].begin = i;
			} else if (opcode == IR_JMP) {
				i32 arg0 = insts[i].args[0];
				i32 target = insts[arg0].args[0];
				ASSERT(insts[arg0].opcode == IR_LABEL);
				ASSERT(target < block_count);
				blocks[target].pred_count++;
				blocks[block_index].succ[0] = target;
				blocks[block_index].succ[1] = target;
			} else if (opcode == IR_JIZ || opcode == IR_JNZ) {
				i32 arg1 = insts[i].args[1];
				i32 target = insts[arg1].args[0];
				ASSERT(target < block_count);
				blocks[target].pred_count++;
				blocks[block_index].succ[0] = target;
			}

			prev_opcode = opcode;
		}

		i32 pred_offset = 0;
		i32 *block_preds = ALLOC(arena, block_count, i32);
		for (isize i = 0; i < block_count; i++) {
			pred_offset += blocks[i].pred_count;
			blocks[i].pred = block_preds + pred_offset;
		}

		block_index = 0;
		for (isize i = 0; i < func->inst_count; i++) {
			ir_opcode opcode = insts[i].opcode;
			if (opcode == IR_LABEL) {
				block_index = insts[i].args[0];
				if (insts[i-1].opcode == IR_JIZ || insts[i-1].opcode == IR_JNZ) {
				}
			} else if (opcode == IR_JMP) {
				i32 arg0 = insts[i].args[0];
				*--blocks[arg0].pred = block_index;
			} else if (opcode == IR_JIZ || opcode == IR_JNZ) {
				i32 arg1 = insts[i].args[1];
				*--blocks[arg1].pred = block_index;
				*--blocks[block_index + 1].pred = block_index;
			}
		}

		//
		// Promote stack variables
		//

		// Determine what each variable can point to
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
				if (info.usage[0] == IR_USE) {
					join_pointer_sets(&pointer_info, dst, arg0);
				}

				if (info.usage[1] == IR_USE) {
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
		isize var_count = 0;
		for (isize i = 0; i < func->inst_count; i++) {
			isize set = find_pointer_set(&pointer_info, i);
			has_escaped[i] = has_escaped[set];
			var_count += has_escaped[i];
		}

		isize matrix_size = block_count * var_count;
		ssa_context ssa_ctx = {0};
		ssa_ctx.blocks = blocks;
		ssa_ctx.sealed_blocks = ALLOC(arena, block_count, b8);
		ssa_ctx.current_def = ALLOC(arena, matrix_size, i32);
		ssa_ctx.incomplete_phis = ALLOC(arena, matrix_size, i32);
		ssa_ctx.insts = insts;
		ssa_ctx.var_count = var_count;
		ssa_ctx.block_count = block_count;
		ssa_ctx.max_inst_count = func->inst_count;
		ssa_ctx.inst_count = func->inst_count;

		isize block_id = -1;
		for (isize i = 0; i < func->inst_count; i++) {
			switch (insts[i].opcode) {
			case IR_LABEL:
				{
					if (block_id > 0) {
						ssa_ctx.sealed_blocks[block_id] = true;
					}

					block_id = insts[i].args[0];
				} break;
			case IR_STORE:
				{
					i32 var_id = insts[i].args[0];
					i32 value = insts[i].args[1];
					isize offset = block_id * ssa_ctx.var_count + var_id;
					ssa_ctx.current_def[offset] = value;
					insts[i].opcode = IR_COPY;
					insts[i].args[0] = value;
					insts[i].args[1] = 0;
				} break;
			case IR_LOAD:
				{
					i32 var_id = insts[i].args[0];
					insts[i].opcode = IR_COPY;
					insts[i].args[0] = read_var(&ssa_ctx, var_id, block_id);
					ASSERT(insts[i].args[0] != 0);
				} break;
			case IR_ALLOC:
				{
					/* TODO */
				} break;
			default:
				{

				} break;
			}
		}

		func->insts = ssa_ctx.insts;
		func->inst_count = ssa_ctx.inst_count;

		remove_copy_insts(func->insts, func->inst_count);

		//
		// Constant folding
		//

		block_id = 0;
		for (isize i = 0; i < func->inst_count; i++) {
			i32 arg0 = insts[i].args[0];
			i32 arg1 = insts[i].args[1];

			switch (insts[i].opcode) {
			case IR_LABEL:
				block_id = insts[i].args[0];
				break;
			case IR_ADD:
				if (insts[arg0].opcode == IR_CONST
					&& insts[arg1].opcode == IR_CONST)
				{
					insts[i].opcode = IR_CONST;
					insts[i].args[0] = add(insts[arg0].args[0], insts[arg1].args[0]);
				} else if (insts[arg0].opcode == IR_CONST
					&& insts[arg0].args[0] == 0)
				{
					insts[i].opcode = IR_COPY;
					insts[i].args[0] = i;
				} else if (insts[arg1].opcode == IR_CONST
					&& insts[arg1].args[0] == 0)
				{
					insts[i].opcode = IR_COPY;
				}
				break;
			case IR_SUB:
				if (insts[arg0].opcode == IR_CONST
					&& insts[arg1].opcode == IR_CONST)
				{
					insts[i].opcode = IR_CONST;
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
					&& insts[arg1].opcode == IR_CONST)
				{
					insts[i].opcode = IR_CONST;
					/* TODO: evaluate depending on the target architecture */
					insts[i].args[0] = multiply(insts[arg0].args[0], insts[arg1].args[0]);
				} else if (insts[arg0].opcode == IR_CONST
					&& insts[arg0].args[0] == 1)
				{
					insts[i].opcode = IR_COPY;
					insts[i].args[0] = i;
				} else if (insts[arg1].opcode == IR_CONST
					&& insts[arg1].args[0] == 1)
				{
					insts[i].opcode = IR_COPY;
					insts[i].args[0] = arg1;
				}
				break;
			case IR_DIV:
				if (insts[arg0].opcode == IR_CONST
					&& insts[arg1].opcode == IR_CONST)
				{
					// TODO: Handle divide-by-zero error
					ASSERT(insts[arg1].args[0] != 0);

					insts[i].opcode = IR_CONST;
					insts[i].args[0] = insts[arg0].args[0] / insts[arg1].args[0];
				} else if (insts[arg1].opcode == IR_CONST
					&& insts[arg1].args[0] == 1)
				{
					insts[i].opcode = IR_COPY;
					insts[i].args[0] = arg0;
				}
				break;
			case IR_MOD:
				if (insts[arg0].opcode == IR_CONST
					&& insts[arg1].opcode == IR_CONST)
				{
					// TODO: Handle divide-by-zero error
					ASSERT(insts[arg1].args[0] != 0);

					insts[i].opcode = IR_CONST;
					insts[i].args[0] = insts[arg0].args[0] % insts[arg1].args[0];
				}
				break;
			case IR_EQ:
				if (insts[arg0].opcode == IR_CONST
					&& insts[arg1].opcode == IR_CONST)
				{
					insts[i].opcode = IR_CONST;
					insts[i].args[0] = (insts[arg0].args[0] == insts[arg1].args[0]);
				}
				break;
			case IR_JIZ:
				if (insts[arg0].opcode == IR_CONST
					&& insts[arg0].args[0] == 0)
				{
					insts[i].opcode = IR_JMP;
					insts[i].args[0] = insts[i].args[1];
				}
				else if (insts[arg0].opcode == IR_CONST
					&& insts[arg0].args[0] != 0)
				{
					insts[i].opcode = IR_JMP;
					insts[i].args[0] = block_id + 1;
				}
				break;
			case IR_JNZ:
				if (insts[arg0].opcode == IR_CONST
					&& insts[arg0].args[0] != 0)
				{
					insts[i].opcode = IR_JMP;
					insts[i].args[0] = insts[i].args[1];
				}
				else if (insts[arg0].opcode == IR_CONST
					&& insts[arg0].args[0] == 0)
				{
					insts[i].opcode = IR_JMP;
					insts[i].args[0] = block_id + 1;
				}
				break;
			default:
				break;
			}
		}

		remove_copy_insts(func->insts, func->inst_count);

		// Remove unused registers
		i32 *ref_count = get_ref_count(insts, func->inst_count, arena);
		for (isize j = 0; j < func->inst_count; j++) {
			b32 has_side_effect;
			ir_opcode opcode = insts[j].opcode;
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
				insts[j].opcode = IR_NOP;
			}
		}

		arena_temp_end(temp);
	}

	// Eliminate dead code
#if 0
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

			// Do a depth first search on the labels, begin with the first
			// instruction in the function.
			i32 stack_pos = 0;
			stack[stack_pos++] = 1;
			reachable[1] = true;
next_block:
			while (stack_pos > 0) {
				isize label = stack[--stack_pos];

				// NOTE: The first instruction is the label itself
				u32 begin = label_addresses[label];
				for (isize j = begin; j < func->inst_count; j++) {
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

				u32 begin = label_addresses[label];
				// Remove the label
				inst[inst[begin].args[0]].opcode = IR_NOP;
				for (isize j = begin; j; j = inst[j].args[1]) {
					u32 i = inst[j].args[0];
					if (inst[i].opcode == IR_LABEL) {
						break;
					}

					inst[i].opcode = IR_NOP;
				}
			}
		}
	}
#endif
}
