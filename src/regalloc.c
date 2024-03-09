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
set_bit(bit_matrix matrix, u32 y, u32 x)
{
	ASSERT(x < matrix.width);
	ASSERT(y < matrix.height);
	u32 i = y * matrix.width + x;
	matrix.bits[i] = 1;
}

static void
clear_bit(bit_matrix matrix, u32 y, u32 x)
{
	ASSERT(x < matrix.width);
	ASSERT(y < matrix.height);
	u32 i = y * matrix.width + x;
	matrix.bits[i] = 0;
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
print_row(bit_matrix matrix, u32 y)
{
	printf("{");
	b32 first = true;
	for (u32 x = 0; x < matrix.width; x++) {
		if (matrix.bits[y * matrix.width + x]) {
			if (first) {
				first = false;
			} else {
				printf(", ");
			}

			u32 mreg = matrix.width - 1 - x;
			if (mreg < X86_REGISTER_COUNT) {
				printf("%s", x86_get_register_name(mreg, 8));
			} else {
				printf("r%d", x);
			}
		}
	}

	printf("}\n");
}

static void
print_matrix(bit_matrix matrix)
{
	for (u32 y = 0; y < matrix.height; y++) {
		printf("live[%d] = ", y);
		print_row(matrix, y);
	}
}

static bit_matrix
get_live_matrix(void *code, machine_function func, u32 mreg_count,
	u32 *temp_mregs, u32 temp_mreg_count, arena *arena)
{
	// TODO: use a bitset as matrix instead of a b32ean matrix.
	u32 instr_count = func.instr_count;
	u32 register_count = func.register_count + mreg_count;
	bit_matrix live_matrix = bit_matrix_init(register_count, instr_count, arena);
	arena_temp temp = arena_temp_begin(arena);
	bit_matrix prev_live_matrix = bit_matrix_init(register_count, instr_count, arena);

	b32 has_matrix_changed = false;
	do {
		u32 i = instr_count;
		while (i-- > 0) {
			machine_instr *instr = get_instr(code, func.instr_offsets, i);

			clear_row(live_matrix, i);
			/* TODO: successor of jump instructions */
			if (i + 1 != instr_count) {
				union_rows(live_matrix, i, i + 1);
			}

			machine_operand *operands = (machine_operand *)(instr + 1);
			for (u32 j = 0; j < instr->operand_count; j++) {
				if (operands[j].kind == MOP_LABEL) {
					u32 instr_index = operands[j].value;
					ASSERT(instr_index < instr_count);
					union_rows(live_matrix, i, instr_index);
				}
			}

			for (u32 j = 0; j < instr->operand_count; j++) {
				if (operands[j].kind != MOP_FUNC) {
					continue;
				}

				for (u32 k = 0; k < temp_mreg_count; k++) {
					u32 mreg = temp_mregs[k];
					set_bit(live_matrix, i, live_matrix.width - 1 - mreg);
				}
			}

			for (u32 j = 0; j < instr->operand_count; j++) {
				switch (operands[j].kind) {
				case MOP_VREG:
					if (operands[j].flags & MOP_DEF) {
						clear_bit(live_matrix, i, operands[j].value);
					}

					if (operands[j].flags & MOP_USE) {
						set_bit(live_matrix, i, operands[j].value);
					}
					break;
				case MOP_MREG:
					if (operands[j].flags & MOP_DEF) {
						clear_bit(live_matrix, i, live_matrix.width - 1 - operands[j].value);
					}

					if (operands[j].flags & MOP_USE) {
						set_bit(live_matrix, i, live_matrix.width - 1 - operands[j].value);
					}
					break;
				case MOP_SPILL:
				case MOP_LABEL:
				case MOP_IMMEDIATE:
				case MOP_FUNC:
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
	return live_matrix;
}

static u32
get_interval_start(bit_matrix live_matrix, u32 reg)
{
	for (u32 i = 0; i < live_matrix.height; i++) {
		if (live_matrix.bits[i * live_matrix.width + reg]) {
			return i;
		}
	}


	return live_matrix.height;
}

static u32
get_interval_end(bit_matrix live_matrix, u32 reg)
{
	for (u32 i = live_matrix.height; i-- > 0;) {
		if (live_matrix.bits[i * live_matrix.width + reg]) {
			return i + 1;
		}
	}

	return 0;
}

// TODO: replace with counting-sort?
static u32 *
sort_intervals_by_start(live_interval *intervals,
    u32 interval_count, arena *arena)
{
	u32 *index = ALLOC(arena, interval_count, u32);
	for (u32 i = 0; i < interval_count; i++) {
		index[i] = i;
	}

	for (u32 i = 1; i < interval_count; i++) {
		u32 j = i;
		while (j > 0 && intervals[index[j - 1]].start > intervals[index[j]].start) {
			u32 tmp = index[j];
			index[j] = index[j-1];
			index[j-1] = tmp;
			j--;
		}
	}

	return index;
}

static void
swap_u32(u32 *a, u32 *b)
{
	u32 tmp = *a;
	*a = *b;
	*b = tmp;
}

static b32
overlaps(live_interval a, live_interval b)
{
	b32 result = !(b.end < a.start || a.end < b.start);
	return result;
}

typedef struct {
	b32 *used;
	u32 spill_count;
} allocation_info;

static allocation_info
allocate_function_registers(machine_function function, void *code,
	u32 mreg_count, u32 *temp_mregs, u32 temp_mreg_count, arena *arena)
{
	allocation_info info = {0};
	info.used = ALLOC(arena, mreg_count, b32);
	arena_temp temp = arena_temp_begin(arena);

	bit_matrix live_matrix = get_live_matrix(code, function,
		mreg_count, temp_mregs, temp_mreg_count, arena);

	u32 reg_count = mreg_count + function.register_count;
	live_interval *intervals = ALLOC(arena, reg_count, live_interval);
	for (u32 i = 0; i < reg_count; i++) {
		intervals[i].start = get_interval_start(live_matrix, i);
		intervals[i].end   = get_interval_end(live_matrix, i);
	}

	u32 *sorted_by_start = sort_intervals_by_start(intervals, function.register_count, arena);

	u32 *register_pool = ALLOC(arena, mreg_count, u32);
	for (u32 i = 0; i < mreg_count; i++) {
		register_pool[i] = i;
	}

	machine_operand *vreg = ALLOC(arena, reg_count, machine_operand);

	b32 *force_mreg_for = ALLOC(arena, reg_count, b32);
	/* TODO: Mark registers where the force flag is set */

	/* NOTE: the register pool is only valid after active_count. In the
	 * active part of the array, there be multiple registers with the same
	 * value. */
	u32 active_start = 0;
	u32 active_count = 0;
	for (u32 i = 0; i < function.register_count; i++) {
		u32 current_register = sorted_by_start[i];
		u32 current_start = intervals[current_register].start;
		u32 current_end = intervals[current_register].end;
		b32 is_empty = (current_start > current_end);

		u32 active_end = active_start + active_count;
		for (u32 j = active_start; j < active_end; j++) {
			u32 inactive_register = sorted_by_start[j];
			u32 end = intervals[inactive_register].end;
			b32 is_active = (end >= current_start);
			if (is_active) {
				continue;
			}

			/* Free the register again */
			active_count--;
			b32 is_mreg = (inactive_register >= function.register_count);
			if (is_mreg) {
				u32 mreg = reg_count - 1 - inactive_register;
				register_pool[active_count] = mreg;
				ASSERT(register_pool[active_count] < mreg_count);
			} else if (vreg[inactive_register].kind == MOP_MREG) {
				u32 mreg = vreg[inactive_register].value;
				register_pool[active_count] = mreg;
				ASSERT(register_pool[active_count] < mreg_count);
			}

			sorted_by_start[j] = sorted_by_start[active_start];
			sorted_by_start[active_start++] = inactive_register;
		}

		ASSERT(current_register < function.register_count);
		b32 should_spill = (active_count >= mreg_count);
		b32 found_mreg = false;
		if (!should_spill) {
			for (u32 i = active_count; i < mreg_count; i++) {
				u32 j = register_pool[i];
				live_interval mreg_interval = intervals[reg_count - 1 - j];
				if (!overlaps(mreg_interval, intervals[current_register])) {
					swap_u32(register_pool + active_count, register_pool + i);
					found_mreg = true;
					break;
				}
			}

			should_spill = !found_mreg;
		}

		if (should_spill) {
			u32 spill = sorted_by_start[active_start];
			u32 spill_index = active_start;
			u32 end = 0;
			for (u32 j = active_start + 1; j < active_end; j++) {
				u32 reg = sorted_by_start[j];
				if (force_mreg_for[reg]) {
					continue;
				}
				if (intervals[reg].end > end) {
					end = intervals[reg].end;
					spill_index = j;
					spill = reg;
				}
			}

			b32 swap_spill_register_with_current_register =
				(intervals[spill].end > intervals[current_register].end);
			// TODO: currently we always spill the current register, see below.
			if (false && swap_spill_register_with_current_register) {
				ASSERT(!force_mreg_for[spill]);
				// TODO: check that the machine register of spill doesn't
				// overlap with the interval of the current register. Otherwise
				// we cannot use the register here.
				vreg[current_register] = vreg[spill];
				vreg[spill] = make_spill(8 * info.spill_count++);
				sorted_by_start[spill_index] = sorted_by_start[active_start];
				sorted_by_start[active_start] = spill;
				active_start++;
			} else {
				ASSERT(!force_mreg_for[current_register]);
				vreg[current_register] = make_spill(8 * info.spill_count++);
			}
		} else if (!is_empty) {
			u32 mreg = register_pool[active_count++];
			ASSERT(mreg < mreg_count);
			info.used[mreg] |= !is_empty;
			vreg[current_register] = make_mreg(mreg, 0);
		}
	}

	// NOTE: Replace the virtual registers with the allocated machine registers
	for (u32 i = 0; i < function.instr_count; i++) {
		machine_instr *instr = get_instr(code, function.instr_offsets, i);
		u32 operand_count = instr->operand_count;

		machine_operand *operands = (machine_operand *)(instr + 1);
		for (u32 i = 0; i < operand_count; i++) {
			if (operands[i].kind == MOP_VREG) {
				u32 reg = operands[i].value;
				ASSERT(reg < function.register_count);
				operands[i].kind = vreg[reg].kind;
				operands[i].value = vreg[reg].value;
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
		info[i] = allocate_function_registers(program.functions[i], program.code,
			program.mreg_count, program.temp_mregs, program.temp_mreg_count, arena);
	}

	return info;
}
