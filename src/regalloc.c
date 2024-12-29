#include <string.h>

// NOTE: width is the number of bits
static bit_matrix
bit_matrix_init(u32 width, u32 height, arena *arena)
{
	bit_matrix matrix = {0};
	matrix.bits = ALLOC_NOZERO(arena, width * height, b8);
	memset(matrix.bits, 0, width * height * sizeof(b8));
	matrix.width = width;
	matrix.height = height;
	return matrix;
}

static void
set_bit(bit_matrix matrix, u32 y, u32 x, b32 value)
{
	ASSERT(x < matrix.width);
	ASSERT(y < matrix.height);
	u32 i = y * matrix.width + x;
	matrix.bits[i] = value;
}

static void
clear_row(bit_matrix matrix, u32 y)
{
	ASSERT(y < matrix.height);
	for (u32 x = 0; x < matrix.width; x++) {
		matrix.bits[y * matrix.width + x] = 0;
	}
}

static void
union_rows(bit_matrix matrix, u32 dst_y, u32 src_y)
{
	ASSERT(dst_y < matrix.height);
	ASSERT(src_y < matrix.height);
	for (u32 x = 0; x < matrix.width; x++) {
		b32 src_bit = matrix.bits[src_y * matrix.width + x];
		matrix.bits[dst_y * matrix.width + x] |= src_bit;
	}
}

static void
swap_u32(u32 *a, isize i, isize j)
{
	u32 tmp = a[i];
	a[i] = a[j];
	a[j] = tmp;
}

static regalloc_info
regalloc(mach_token *tokens, isize token_count, regalloc_hints hints, arena *arena)
{
	regalloc_info info = {0};
	info.used = ALLOC(arena, hints.mreg_count, b32);
	arena_temp temp = arena_temp_begin(arena);

	// NOTE: Compute the instruction index of each label
	isize *label_offset = ALLOC(arena, hints.label_count, isize);
	for (isize i = 0; i < token_count; i++) {
		if (tokens[i].kind == MACH_INST
			&& tokens[i].value == X86_LABEL
			&& i + 1 < token_count)
		{
			// A label should only have one token: The index of the label.
			ASSERT(tokens[i + 1].kind == MACH_CONST);
			isize label_index = tokens[i + 1].value;
			ASSERT(label_index < hints.label_count);
			label_offset[label_index] = i;
		}
	}

	// NOTE; Compute the live matrix. Each row corresponds to one instruction,
	// each column corresponds to a register. If a register is live at a
	// certain instruction then the matrix has a one at the corresponding
	// column and row.
	isize reg_count = hints.mreg_count + hints.vreg_count;
	bit_matrix live_matrix = bit_matrix_init(reg_count, token_count, arena);
	{
		// TODO: use a bitset as matrix instead of a boolean matrix.
		arena_temp temp = arena_temp_begin(arena);
		bit_matrix prev_live_matrix =
			bit_matrix_init(reg_count, token_count, arena);

		b32 has_matrix_changed = false;
		do {
			isize i = token_count;
			while (i-- > 0) {
				clear_row(live_matrix, i);
				/* TODO: successor of jump instructions */
				if (i + 1 != token_count) {
					union_rows(live_matrix, i, i + 1);
				}

				mach_token token = tokens[i];
				u32 value = token.value;
				switch (token.kind) {
				case MACH_INVALID:
					{
						ASSERT(!"Invalid token");
					} break;
				case MACH_LABEL:
					{
						isize inst_index = label_offset[token.value];
						ASSERT(inst_index < token_count);
						union_rows(live_matrix, i, inst_index);
					} break;
				default:
					break;
				}

				if (token.flags & MACH_CALL) {
					for (u32 k = 0; k < hints.tmp_mreg_count; k++) {
						u32 mreg = hints.tmp_mregs[k];
						set_bit(live_matrix, i, mreg, 1);
					}
				}

				if (token.flags & MACH_DEF) {
					set_bit(live_matrix, i, value, 1);
				}

				if (token.flags & MACH_USE) {
					set_bit(live_matrix, i, value, 1);
				}
			}

			isize matrix_size = live_matrix.width * live_matrix.height * sizeof(b32);
			has_matrix_changed = memcmp(live_matrix.bits, prev_live_matrix.bits, matrix_size);
			memcpy(prev_live_matrix.bits, live_matrix.bits, matrix_size);
		} while (has_matrix_changed);

		arena_temp_end(temp);
	}

	// NOTE: Calculate the live intervals of the virtual registers
	live_interval *intervals = ALLOC(arena, reg_count, live_interval);
	for (u32 i = 0; i < reg_count; i++) {
		intervals[i].start = live_matrix.height;
		for (u32 j = 0; j < live_matrix.height; j++) {
			if (live_matrix.bits[j * live_matrix.width + i]) {
				intervals[i].start = j;
				break;
			}
		}

		intervals[i].end = 0;
		for (u32 j = live_matrix.height; j-- > 0;) {
			if (live_matrix.bits[j * live_matrix.width + i]) {
				intervals[i].end = j + 1;
				break;
			}
		}
	}

	// NOTE: Sort the intervals by their start
	// TODO: Replace with a more efficient sorting algorithm
	u32 *sorted = ALLOC(arena, reg_count, u32);
	for (u32 i = hints.mreg_count; i < reg_count; i++) {
		sorted[i] = i;
	}

	for (u32 i = hints.mreg_count; i < reg_count; i++) {
		u32 j = i;
		while (j > 0 && intervals[sorted[j - 1]].start > intervals[sorted[j]].start) {
			swap_u32(sorted, j, j - 1);
			j--;
		}
	}

	// Initialize the register pool
	u32 *pool = ALLOC(arena, hints.mreg_count, u32);
	for (u32 i = 0; i < hints.mreg_count; i++) {
		pool[i] = i;
	}

	/*
	 * NOTE: the register pool is only valid after active_count. In the active
	 * part of the array, there can be multiple registers with the same value.
	 */
	u32 active_start = 0;
	u32 active_count = 0;
	mach_token *mreg_map = ALLOC(arena, reg_count, mach_token);
	for (u32 i = hints.mreg_count; i < reg_count; i++) {
		u32 curr_reg = sorted[i];
		u32 curr_start = intervals[curr_reg].start;
		u32 curr_end = intervals[curr_reg].end;
		b32 is_empty = (curr_start > curr_end);

		// Free all registers whose interval has ended
		u32 active_end = active_start + active_count;
		for (u32 j = active_start; j < active_end; j++) {
			u32 inactive_reg = sorted[j];
			u32 end = intervals[inactive_reg].end;
			b32 is_active = (end >= curr_start);
			if (is_active) {
				continue;
			}

			/* Free the register again */
			active_count--;
			b32 is_mreg = (inactive_reg < hints.mreg_count);
			if (is_mreg) {
				u32 mreg = inactive_reg;
				pool[active_count] = mreg;
				ASSERT(pool[active_count] < hints.mreg_count);
			} else if (mreg_map[inactive_reg].value < hints.mreg_count) {
				u32 mreg = mreg_map[inactive_reg].value;
				pool[active_count] = mreg;
				ASSERT(pool[active_count] < hints.mreg_count);
			}

			sorted[j] = sorted[active_start];
			sorted[active_start++] = inactive_reg;
		}

		// Find a valid machine register that doesn't overlap with curr_reg
		ASSERT(hints.mreg_count <= curr_reg && curr_reg < reg_count);
		b32 should_spill = (active_count >= hints.mreg_count);
		b32 found_mreg = false;
		if (!should_spill) {
			for (u32 i = active_count; i < hints.mreg_count; i++) {
				u32 mreg = pool[i];
				b32 is_float_mreg = pool[i] >= hints.int_mreg_count;
				if (is_float_mreg != hints.is_float[curr_reg]) {
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
				mreg_map[curr_reg] = mreg_map[spill];
				mreg_map[spill] = make_spill(8 * info.spill_count++);
				sorted[spill_index] = sorted[active_start];
				sorted[active_start] = spill;
				active_start++;
			} else {
				mreg_map[curr_reg] = make_spill(8 * info.spill_count++);
			}
		} else {
			u32 mreg = pool[active_count++];
			ASSERT(mreg < hints.mreg_count);
			info.used[mreg] |= !is_empty;
			mreg_map[curr_reg] = make_mach_token(MACH_REG, mreg, 0);
		}
	}

	// NOTE: Replace the virtual registers with the allocated machine registers
	for (u32 i = 0; i < token_count; i++) {
		if (tokens[i].kind == MACH_REG && tokens[i].value > hints.mreg_count) {
			u32 vreg = tokens[i].value;
			ASSERT(vreg < hints.vreg_count);
			tokens[i].kind = mreg_map[vreg].kind;
			tokens[i].value = mreg_map[vreg].value;
			ASSERT(tokens[i].kind == MACH_REG || tokens[i].kind == MACH_SPILL);
		}
	}

	arena_temp_end(temp);
	return info;
}
