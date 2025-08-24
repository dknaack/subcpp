/*
 * For each instruction, the register allocator ensures that all virtual
 * register operands are stored in machine registers and not on the stack. It
 * does so by inserting mov/spill/reload before the current instruction when
 * the specific register is not available. The code generator can then use a
 * peephole optimizer to combine loads/stores into one instruction, if
 * possible.
 */
static u32 *
regalloc(mach_token *tokens, isize token_count,
	basic_block *blocks, isize block_count, mach_info mach, arena *arena)
{
	isize mreg_count = mach.mreg_count;
	isize reg_count = mreg_count + mach.vreg_count;
	u32 *result = ALLOC(arena, reg_count, u32);
	for (isize i = 0; i < mreg_count; i++) {
		result[i] = i;
	}

	arena_temp temp = arena_temp_begin(arena);

	// Allocate the bitsets
	bitset *gen = ALLOC(arena, block_count, bitset);
	bitset *kill = ALLOC(arena, block_count, bitset);
	bitset *live_in = ALLOC(arena, block_count, bitset);
	bitset *live_out = ALLOC(arena, block_count, bitset);
	for (isize i = 0; i < block_count; i++) {
		gen[i] = new_bitset(reg_count, arena);
		kill[i] = new_bitset(reg_count, arena);
		live_in[i] = new_bitset(reg_count, arena);
		live_out[i] = new_bitset(reg_count, arena);
	}

	// Initialize the gen and kill bitsets
	for (isize i = 1; i < block_count; i++) {
		for (isize j = 0; j < blocks[i].size; j++) {
			mach_token token = tokens[blocks[i].offset + j];

			// Gen stores all registers which have been used before any def. We
			// track use first, since a register with both flags would count as
			// usage and then definition.
			if ((token.flags & MACH_USE) && !get_bit(kill[i], token.value)) {
				set_bit(gen[i], token.value, 1);
			}

			if (token.flags & MACH_DEF) {
				set_bit(kill[i], token.value, 1);
			}
		}
	}

	// Do the live variable analysis
	b32 has_changed = true;
	while (has_changed) {
		has_changed = false;

		for (isize i = block_count - 1; i >= 0; i--) {
			// Update the live_out matrix: Union all live_in successors
			for (isize j = 0; j < live_out[i].size; j++) {
				b32 bit = 0;

				for (isize k = 0; k < 2; k++) {
					isize succ = blocks[i].succ[k];
					if (succ > 0) {
						bit |= get_bit(live_out[succ], j);
					}
				}

				set_bit(live_out[i], j, bit);
			}

			// Update the live_in matrix
			for (isize j = 0; j < live_in[i].size; j++) {
				b32 o = get_bit(live_out[i], j);
				b32 k = get_bit(kill[i], j);
				b32 g = get_bit(gen[i], j);

				b32 old = get_bit(live_in[i], j);
				b32 new = g || (o && !k);

				set_bit(live_in[i], j, new);
				if (old != new) {
					has_changed = true;
				}
			}
		}
	}

	// Compute the interfering machine registers for each virtual register by
	// checking which preallocated registers are live at the same time.
	bitset *blocked_regs = ALLOC(arena, reg_count, bitset);
	for (isize i = 0; i < reg_count; i++) {
		blocked_regs[i] = new_bitset(mreg_count, arena);
	}

	for (isize i = 1; i < block_count; i++) {
		arena_temp loop_temp = arena_temp_begin(arena);
		bitset live = clone_bitset(live_out[i], arena);

		isize offset = blocks[i].offset;
		isize j = blocks[i].size;
		while (j-- > 0) {
			mach_token token = tokens[offset + j];
			u32 reg = token.value;

			if (token.flags & MACH_DEF && get_bit(live, reg) != 0) {
				for (isize j = 0; j < mreg_count; j++) {
					if (reg != j && get_bit(live, j)) {
						set_bit(blocked_regs[reg], j, 1);
					}
				}

				set_bit(live, reg, 0);
			}

			if (token.flags & MACH_USE) {
				set_bit(live, reg, 1);
			}
		}

		arena_temp_end(loop_temp);
	}

	// Find all floating-point registers
	bitset float_regs = new_bitset(reg_count, arena);
	for (isize i = 1; i < block_count; i++) {
		isize offset = blocks[i].offset;
		isize j = blocks[i].size;
		while (j-- > 0) {
			mach_token token = tokens[offset + j];
			u32 reg = token.value;

			b32 is_vreg = (token.flags & (MACH_USE | MACH_DEF));
			b32 is_float = (token.flags & MACH_FLOAT);
			if (is_vreg && is_float) {
				set_bit(float_regs, reg, 1);
			}
		}
	}


	// NOTE: Calculate the live intervals of the virtual registers
	live_interval *intervals = ALLOC(arena, reg_count, live_interval);
	for (isize i = 0; i < reg_count; i++) {
		isize start = token_count;
		isize end = 0;

		// Find first live_in
		for (isize j = 0; j < block_count; j++) {
			isize block_start = blocks[j].offset;
			if (get_bit(live_in[j], i) && block_start < start) {
				start = block_start;
			}
		}

		// Find last live_out
		for (isize j = 0; j < block_count; j++) {
			isize block_end = blocks[j].offset + blocks[j].size;
			if (get_bit(live_out[j], i) && block_end > end) {
				end = block_end;
			}
		}

		if (end < start) {
			// Find first def
			for (isize j = 0; j < start; j++) {
				if ((tokens[j].flags & MACH_DEF) && tokens[j].value == i) {
					start = j;
				}
			}

			// Find last use
			for (isize j = token_count - 1; j >= end; j--) {
				if (tokens[j].flags & MACH_USE && tokens[j].value == i) {
					end = j;
				}
			}
		}

		intervals[i].vreg = i;
		intervals[i].start = start;
		intervals[i].end = end;
	}

	// NOTE: Sort the intervals by their start
	// TODO: Replace with a more efficient sorting algorithm
	for (isize i = mreg_count; i < reg_count; i++) {
		for (isize j = mreg_count; j < reg_count - i - 1; j++) {
			live_interval tmp = intervals[j];
			intervals[j] = intervals[j + 1];
			intervals[j + 1] = tmp;
		}
	}

	b32 *is_active = ALLOC(arena, mreg_count, b32);
	isize active_start = 0;
	for (isize i = mreg_count; i < reg_count; i++) {
		u32 curr_reg = intervals[i].vreg;
		u32 curr_start = intervals[i].start;
		u32 curr_end = intervals[i].end;
		ASSERT(curr_reg >= mreg_count);

		// Expire old intervals
		for (isize j = active_start; j < i; j++) {
			b32 has_expired = intervals[j].end < curr_start;
			if (!has_expired) {
				continue;
			}

			u32 vreg = intervals[j].vreg;
			if (vreg >= mreg_count) {
				u32 mreg = result[vreg];
				is_active[mreg] = false;
			}

			// Swap the expired interval with the one at the start.
			intervals[j] = intervals[active_start++];
		}

		// Ignore empty or preallocated registers
		b32 is_empty = (curr_start > curr_end);
		b32 is_preallocated = (result[curr_reg] > 0);
		if (is_empty || is_preallocated) {
			continue;
		}

		// Find a valid machine register that doesn't overlap with curr_reg
		u32 assigned_mreg = 0;
		b32 is_float = get_bit(float_regs, i);
		if (is_float) {
			for (isize j = 0; j < mach.float_mreg_count; j++) {
				u32 mreg = mach.float_mregs[j];
				b32 is_blocked = get_bit(blocked_regs[i], mreg);
				if (!is_blocked && !is_active[mreg]) {
					assigned_mreg = mreg;
					break;
				}
			}
		} else {
			for (isize j = 0; j < mach.int_mreg_count; j++) {
				u32 mreg = mach.int_mregs[j];
				b32 is_blocked = get_bit(blocked_regs[i], mreg);
				if (!is_blocked && !is_active[mreg]) {
					assigned_mreg = mreg;
					break;
				}
			}
		}

		if (assigned_mreg == 0) {
			result[curr_reg] = 0;
		} else {
			result[curr_reg] = assigned_mreg;
			is_active[assigned_mreg] = true;
			BREAK_IF(assigned_mreg == X86_XMM5);
		}
	}

	arena_temp_end(temp);
	return result;
}
