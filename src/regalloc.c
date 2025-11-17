/*
 * For each instruction, the register allocator ensures that all virtual
 * register operands are stored in machine registers and not on the stack. It
 * does so by inserting mov/spill/reload before the current instruction when
 * the specific register is not available. The code generator can then use a
 * peephole optimizer to combine loads/stores into one instruction, if
 * possible.
 */
static mach_location *
regalloc(inst *insts, isize inst_count, block *blocks, isize block_count,
	mach_info mach, arena *perm)
{
	mach_location *result = NULL;

	bitset *live_in = ALLOC(perm, block_count, bitset);
	for (isize i = 0; i < block_count; i++) {
		live_in[i] = new_bitset(inst_count, perm);
	}

	b8 *visited = ALLOC(perm, block_count, b8);
	i32 *stack = ALLOC(perm, block_count, i32);
	i32 top = 0;

	stack[top++] = 0;
	while (top > 0) {
		isize block_id = stack[--top];
		if (visited[block_id]) {
			continue;
		}

		isize succ0 = blocks[block_id].succ[0];
		isize succ1 = blocks[block_id].succ[1];
		b32 is_cond_jump = (succ0 != succ1);

		visited[block_id] = true;
		stack[top++] = succ0;
		if (is_cond_jump) {
			stack[top++] = succ1;
		}

		// TODO: Implement the allocation part
	}

	return result;
}
