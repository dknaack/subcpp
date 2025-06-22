static u32 *
regalloc(mach_token *tokens, isize token_count,
	basic_block *blocks, isize block_count, mach_info mach, arena *arena)
{
	u32 *result = ALLOC(arena, mach.vreg_count, u32);
	arena_temp temp = arena_temp_begin(arena);
	isize reg_count = mach.mreg_count + mach.vreg_count;

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
	for (isize i = mach.mreg_count; i < reg_count; i++) {
		for (isize j = mach.mreg_count; j < reg_count - i - 1; j++) {
			live_interval tmp = intervals[j];
			intervals[j] = intervals[j + 1];
			intervals[j + 1] = tmp;
		}
	}

	// Initialize the register pool
	b32 *is_active = ALLOC(arena, mach.mreg_count, b32);

	/*
	 * NOTE: the register pool is only valid after active_count. In the active
	 * part of the array, there can be multiple registers with the same value.
	 */
	isize active_start = 0;
	for (isize i = mach.mreg_count; i < reg_count; i++) {
		u32 curr_reg = intervals[i].vreg;
		u32 curr_start = intervals[i].start;
		u32 curr_end = intervals[i].end;
		ASSERT(curr_reg >= mach.mreg_count);

		// Expire old intervals
		for (isize j = active_start; j < i; j++) {
			b32 has_expired = intervals[j].end < curr_start;
			if (!has_expired) {
				continue;
			}

			u32 reg = intervals[j].vreg;
			if (reg >= mach.mreg_count) {
				reg = result[reg];
			}

			if (reg > 0) {
				is_active[reg] = false;
			}

			intervals[j] = intervals[active_start++];
		}

		b32 is_empty = (curr_start > curr_end);
		b32 is_preallocated = (result[curr_reg] > 0);
		if (is_empty || is_preallocated) {
			continue;
		}

		// Find a valid machine register that doesn't overlap with curr_reg
		ASSERT(mach.mreg_count <= curr_reg && curr_reg < reg_count);
		b32 should_spill = (active_start == i);
		u32 mreg = 0;
		if (!should_spill) {
			for (isize j = 1; j < mach.mreg_count; j++) {
				if (!is_active[j]) {
					mreg = j;
					break;
				}
			}

			should_spill = (mreg == 0);
		}

		if (should_spill) {
			isize spill_index = -1;
			u32 end = 0;
			for (isize j = active_start; j < i; j++) {
				if (intervals[j].end > end) {
					end = intervals[j].end;
					spill_index = j;
				}
			}

			b32 swap_with_spill = (end > intervals[curr_reg].end);
			// TODO: currently we always spill the current register, see below.
			if (false && swap_with_spill) {
				// TODO: check that the machine register of spill doesn't
				// overlap with the interval of the current register. Otherwise
				// we cannot use the register here.
				u32 spilled_vreg = intervals[spill_index].vreg;
				result[curr_reg] = result[spilled_vreg];
				result[spilled_vreg] = -1;
				intervals[spill_index] = intervals[active_start++];
			} else {
				result[curr_reg] = -1;
			}
		} else {
			ASSERT(mreg < mach.mreg_count);
			result[curr_reg] = mreg;
		}
	}

	arena_temp_end(temp);
	return result;
}
