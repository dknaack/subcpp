#include <string.h>

typedef struct {
	u32 start;
	u32 end;
} live_interval;

typedef struct {
	b32 *bits;
	u32 width;
	u32 height;
} bit_matrix;

// NOTE: width is the number of bits
static bit_matrix
bit_matrix_init(u32 width, u32 height, arena *arena)
{
	bit_matrix matrix = {0};
	matrix.bits = ALLOC(arena, width * height, b32);
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

typedef struct {
	b32 *used;
	u32 spill_count;
} allocation_info;

static allocation_info
allocate_function_registers(machine_function func, void *code,
	machine_register_info reg_info, arena *arena)
{
	allocation_info info = {0};
	info.used = ALLOC(arena, reg_info.register_count, b32);
	arena_temp temp = arena_temp_begin(arena);

	// NOTE; Compute the live matrix. Each row corresponds to one instruction,
	// each column corresponds to a register. If a register is live at a
	// certain instruction then the matrix has a one at the corresponding
	// column and row.
	u32 reg_count = reg_info.register_count + func.register_count;
	bit_matrix live_matrix = bit_matrix_init(reg_count, func.inst_count, arena);
	{
		// TODO: use a bitset as matrix instead of a boolean matrix.
		arena_temp temp = arena_temp_begin(arena);
		bit_matrix prev_live_matrix = bit_matrix_init(reg_count, func.inst_count, arena);

		b32 has_matrix_changed = false;
		do {
			u32 i = func.inst_count;
			while (i-- > 0) {
				clear_row(live_matrix, i);
				/* TODO: successor of jump instructions */
				if (i + 1 != func.inst_count) {
					union_rows(live_matrix, i, i + 1);
				}

				machine_inst *inst = get_inst(code, func.inst_offsets, i);
				machine_operand *operands = (machine_operand *)(inst + 1);
				for (u32 j = 0; j < inst->operand_count; j++) {
					if (operands[j].kind == MOP_LABEL) {
						u32 inst_index = operands[j].value;
						ASSERT(inst_index < func.inst_count);
						union_rows(live_matrix, i, inst_index);
					}
				}

				for (u32 j = 0; j < inst->operand_count; j++) {
					if (operands[j].kind != MOP_FUNC) {
						continue;
					}

					for (u32 k = 0; k < reg_info.volatile_register_count; k++) {
						u32 mreg = reg_info.volatile_registers[k];
						set_bit(live_matrix, i, live_matrix.width - 1 - mreg, 1);
					}
				}

				for (u32 j = 0; j < inst->operand_count; j++) {
					u32 value = operands[j].value;
					switch (operands[j].kind) {
					case MOP_VREG:
						if (operands[j].flags & MOP_DEF) {
							set_bit(live_matrix, i, value, 1);
						}

						if (operands[j].flags & MOP_USE) {
							set_bit(live_matrix, i, value, 1);
						}
						break;
					case MOP_MREG:
						if (operands[j].flags & MOP_DEF) {
							set_bit(live_matrix, i, live_matrix.width - 1 - value, 1);
						}

						if (operands[j].flags & MOP_USE) {
							set_bit(live_matrix, i, live_matrix.width - 1 - value, 1);
						}
						break;
					case MOP_SPILL:
					case MOP_LABEL:
					case MOP_IMMEDIATE:
					case MOP_FLOAT:
					case MOP_FUNC:
					case MOP_GLOBAL:
						break;
					default:
						ASSERT(!"Invalid operand type");
					}
				}
			}

			usize matrix_size = live_matrix.width * live_matrix.height * sizeof(b32);
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
			}
		}

		intervals[i].end = 0;
		for (u32 j = live_matrix.height; j-- > 0;) {
			if (live_matrix.bits[j * live_matrix.width + i]) {
				intervals[i].end = j + 1;
			}
		}
	}

	// NOTE: Sort the intervals by their start
	// TODO: Replace with a more efficient sorting algorithm
	u32 *sorted = ALLOC(arena, reg_count, u32);
	for (u32 i = 0; i < reg_count; i++) {
		sorted[i] = i;
	}

	for (u32 i = 1; i < reg_count; i++) {
		u32 j = i;
		while (j > 0 && intervals[sorted[j - 1]].start > intervals[sorted[j]].start) {
			swap_u32(sorted, j, j - 1);
			j--;
		}
	}

	// Determine floating-pointer registers
	b32 *is_float_vreg = ALLOC(arena, func.register_count, b32);
	for (u32 i = 0; i < func.inst_count; i++) {
		machine_inst *inst = get_inst(code, func.inst_offsets, i);
		u32 operand_count = inst->operand_count;

		machine_operand *operands = (machine_operand *)(inst + 1);
		for (u32 j = 0; j < operand_count; j++) {
			b32 is_vreg = (operands[j].kind == MOP_VREG);
			if (is_vreg && (operands[j].flags & MOP_ISFLOAT)) {
				u32 reg = operands[j].value;
				is_float_vreg[reg] = true;
			}
		}
	}

	// Initialize the register pool
	u32 *pool = ALLOC(arena, reg_info.register_count, u32);
	for (u32 i = 0; i < reg_info.register_count; i++) {
		pool[i] = i;
	}

	/*
	 * NOTE: the register pool is only valid after active_count. In the active
	 * part of the array, there can be multiple registers with the same value.
	 */
	u32 active_start = 0;
	u32 active_count = 0;
	machine_operand *mreg_map = ALLOC(arena, func.register_count, machine_operand);
	for (u32 i = 0; i < func.register_count; i++) {
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
			b32 is_mreg = (inactive_reg >= func.register_count);
			if (is_mreg) {
				u32 mreg = reg_count - 1 - inactive_reg;
				pool[active_count] = mreg;
				ASSERT(pool[active_count] < reg_info.register_count);
			} else if (mreg_map[inactive_reg].kind == MOP_MREG) {
				u32 mreg = mreg_map[inactive_reg].value;
				pool[active_count] = mreg;
				ASSERT(pool[active_count] < reg_info.register_count);
			}

			sorted[j] = sorted[active_start];
			sorted[active_start++] = inactive_reg;
		}

		// Find a valid machine register that doesn't overlap with curr_reg
		ASSERT(curr_reg < func.register_count);
		b32 should_spill = (active_count >= reg_info.register_count);
		b32 found_mreg = false;
		if (!should_spill) {
			for (u32 i = active_count; i < reg_info.register_count; i++) {
				u32 mreg = pool[i];
				b32 is_float_mreg = pool[i] >= reg_info.int_register_count;
				if (is_float_mreg != is_float_vreg[curr_reg]) {
					continue;
				}

				u32 mreg_start = intervals[reg_count - 1 - mreg].start;
				u32 mreg_end = intervals[reg_count - 1 - mreg].end;
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
			ASSERT(mreg < reg_info.register_count);
			info.used[mreg] |= !is_empty;
			mreg_map[curr_reg] = make_operand(MOP_MREG, mreg, 0);
		}
	}

	// NOTE: Replace the virtual registers with the allocated machine registers
	for (u32 i = 0; i < func.inst_count; i++) {
		machine_inst *inst = get_inst(code, func.inst_offsets, i);
		u32 operand_count = inst->operand_count;

		machine_operand *operands = (machine_operand *)(inst + 1);
		for (u32 i = 0; i < operand_count; i++) {
			if (operands[i].kind == MOP_VREG) {
				u32 reg = operands[i].value;
				ASSERT(reg < func.register_count);
				ASSERT(mreg_map[reg].kind != MOP_INVALID);
				operands[i].kind = mreg_map[reg].kind;
				operands[i].value = mreg_map[reg].value;
			}
		}
	}

	arena_temp_end(temp);
	return info;
}

static allocation_info *
allocate_registers(machine_program program, arena *arena)
{
	allocation_info *info = ALLOC(arena, program.function_count, allocation_info);
	for (u32 i = 0; i < program.function_count; i++) {
		info[i] = allocate_function_registers(program.functions[i],
			program.code, program.register_info, arena);
	}

	return info;
}
