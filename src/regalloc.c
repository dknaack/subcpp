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

		int i = 0;
		(void)i;
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

		intervals[i].start = start;
		intervals[i].end = end;
	}

	// NOTE: Sort the intervals by their start
	// TODO: Replace with a more efficient sorting algorithm
	u32 *sorted = ALLOC(arena, reg_count, u32);
	for (u32 i = 0; i < reg_count; i++) {
		sorted[i] = i;
	}

	for (u32 i = mach.mreg_count; i < reg_count; i++) {
		u32 j = i;
		while (j > mach.mreg_count && intervals[sorted[j - 1]].start > intervals[sorted[j]].start) {
			swap_u32(sorted, j, j - 1);
			j--;
		}
	}

	// Initialize the register pool
	u32 *pool = mach.pool;

	// Preallocate registers with only one valid machine register
	for (u32 vreg = mach.mreg_count; vreg < reg_count; vreg++) {
		result[vreg] = -1;
		for (isize i = 0; i < mach.pool_size; i++) {
			ASSERT(mach.vreg_class[vreg] > 0);

			u32 mreg = pool[i];
			if ((mach.vreg_class[vreg] & ~mach.mreg_class[mreg]) == 0) {
				printf("prealloc(%%%d, %s)\n", vreg, x86_get_register_name(mreg, 8));
				result[vreg] = mreg;
				break;
			}
		}
	}

	/*
	 * NOTE: the register pool is only valid after active_count. In the active
	 * part of the array, there can be multiple registers with the same value.
	 */
	isize active_start = 0;
	isize active_count = 0;
	mach_token *mreg_map = ALLOC(arena, reg_count, mach_token);
	for (u32 i = mach.mreg_count; i < reg_count; i++) {
		u32 curr_reg = sorted[i];
		ASSERT(curr_reg >= mach.mreg_count);

		u32 curr_start = intervals[curr_reg].start;
		u32 curr_end = intervals[curr_reg].end;
		b32 is_empty = (curr_start > curr_end);

		// Free all registers whose interval has ended
		isize active_end = active_start + active_count;
		for (isize j = active_start; j < active_end; j++) {
			u32 inactive_reg = sorted[j];
			u32 end = intervals[inactive_reg].end;
			b32 is_active = (end >= curr_start);
			if (is_active) {
				continue;
			}

			/* Free the register again */
			active_count--;
			b32 is_mreg = (inactive_reg < mach.mreg_count);
			if (is_mreg) {
				u32 mreg = inactive_reg;
				pool[active_count] = mreg;
				ASSERT(pool[active_count] < mach.mreg_count);
			} else if (mreg_map[inactive_reg].value < mach.mreg_count) {
				u32 mreg = mreg_map[inactive_reg].value;
				pool[active_count] = mreg;
				ASSERT(pool[active_count] < mach.mreg_count);
			}

			sorted[j] = sorted[active_start];
			sorted[active_start++] = inactive_reg;
		}

		if (is_empty || (i32)result[curr_reg] >= 0) {
			continue;
		}

		// Find a valid machine register that doesn't overlap with curr_reg
		ASSERT(mach.mreg_count <= curr_reg && curr_reg < reg_count);
		b32 should_spill = (active_count >= mach.mreg_count);
		b32 found_mreg = false;
		if (!should_spill) {
			for (u32 i = active_count; i < mach.mreg_count; i++) {
				u32 mreg = pool[i];
				u32 mreg_class = mach.mreg_class[mreg];
				u32 vreg_class = mach.vreg_class[curr_reg];
				b32 has_valid_class = (vreg_class & mreg_class) == mreg_class;
				if (!has_valid_class) {
					continue;
				}

				u32 mreg_start = intervals[mreg].start;
				u32 mreg_end = intervals[mreg].end;
				b32 mreg_overlaps = (curr_end >= mreg_start && mreg_end >= curr_start);
				if (!mreg_overlaps) {
					swap_u32(pool, active_count, i);
					found_mreg = true;
					break;
				}
			}

			should_spill = !found_mreg;
		}

		if (should_spill) {
			u32 spill = sorted[active_start];
			u32 spill_index = active_start;
			u32 end = 0;
			for (u32 j = active_start + 1; j < active_end; j++) {
				u32 reg = sorted[j];
				if (intervals[reg].end > end) {
					end = intervals[reg].end;
					spill_index = j;
					spill = reg;
				}
			}

			b32 swap_with_spill = (intervals[spill].end > intervals[curr_reg].end);
			// TODO: currently we always spill the current register, see below.
			if (false && swap_with_spill) {
				// TODO: check that the machine register of spill doesn't
				// overlap with the interval of the current register. Otherwise
				// we cannot use the register here.
				result[curr_reg] = result[spill];
				result[spill] = -1;
				sorted[spill_index] = sorted[active_start];
				sorted[active_start] = spill;
				active_start++;
			} else {
				result[curr_reg] = -1;
			}
		} else {
			u32 mreg = pool[active_count++];
			ASSERT(mreg < mach.mreg_count);

			u32 mreg_class = mach.mreg_class[mreg];
			u32 vreg_class = mach.vreg_class[curr_reg];
			ASSERT((vreg_class & mreg_class) == mreg_class);

			result[curr_reg] = mreg;
		}
	}

	arena_temp_end(temp);
	return result;
}
