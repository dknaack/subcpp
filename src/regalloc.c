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
	matrix.bits = ZALLOC(arena, width * height, b32);
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
get_live_matrix(machine_program program, u32 first_instr,
    u32 last_instr, u32 reg_count, arena *arena)
{
	// TODO: use a bitset as matrix instead of a b32ean matrix.
	u32 instr_count = last_instr - first_instr;
	bit_matrix live_matrix = bit_matrix_init(reg_count, instr_count, arena);
	arena_temp temp = arena_temp_begin(arena);
	bit_matrix prev_live_matrix = bit_matrix_init(reg_count, instr_count, arena);

	b32 has_matrix_changed = false;
	do {
		u32 i = instr_count;
		while (i-- > 0) {
			machine_instr *instr = get_instr(program, first_instr + i);

			clear_row(live_matrix, i);
			/* TODO: successor of jump instructions */
			if (i + 1 != instr_count) {
				union_rows(live_matrix, i, i + 1);
			}

			machine_operand *operands = (machine_operand *)(instr + 1);
			for (u32 j = 0; j < instr->operand_count; j++) {
				if (operands[j].kind != MOP_LABEL) {
					continue;
				}

				u32 block_index = operands[j].value;
				ASSERT(block_index < program.block_count);

				machine_block block = program.blocks[block_index];
				u32 instr_index = block.instr_index - first_instr;
				union_rows(live_matrix, i, instr_index);
			}

			for (u32 j = 0; j < instr->operand_count; j++) {
				if (operands[j].kind != MOP_FUNC) {
					continue;
				}

				for (u32 k = 0; k < program.temp_mreg_count; k++) {
					u32 mreg = program.temp_mregs[k];
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


	return live_matrix.height - 1;
}

static u32
get_interval_end(bit_matrix live_matrix, u32 reg)
{
	for (u32 i = live_matrix.height; i-- > 0;) {
		if (live_matrix.bits[i * live_matrix.width + reg]) {
			return i;
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

typedef struct {
	b32 *used;
	u32 spill_count;
} allocation_info;

static allocation_info
allocate_function_registers(machine_program program,
    u32 function_index, arena *arena)
{
	allocation_info info = {0};
	u32 mreg_count = program.mreg_count;
	u32 reg_count = program.mreg_count + program.vreg_count;
	info.used = ZALLOC(arena, program.mreg_count, b32);

	arena_temp temp = arena_temp_begin(arena);

	machine_function function = program.functions[function_index];
	u32 first_instr = function.instr_index;
	u32 last_instr = program.instr_count;
	if (function_index + 1 < program.function_count) {
		machine_function next_function = program.functions[function_index+1];
		last_instr = next_function.instr_index;
	}

	bit_matrix live_matrix = get_live_matrix(program, first_instr, last_instr, reg_count, arena);
	live_interval *intervals = ALLOC(arena, reg_count, live_interval);
	for (u32 i = 0; i < reg_count; i++) {
		intervals[i].start = get_interval_start(live_matrix, i);
		intervals[i].end   = get_interval_end(live_matrix, i);
	}

	u32 *sorted_by_start = sort_intervals_by_start(intervals, reg_count, arena);

	u32 *register_pool = ALLOC(arena, mreg_count, u32);
	for (u32 i = 0; i < mreg_count; i++) {
		register_pool[i] = i;
	}

	machine_operand *vreg = ALLOC(arena, reg_count, machine_operand);

	b32 *force_mreg_for = ZALLOC(arena, reg_count, b32);
	/* TODO: Mark registers where the force flag is set */

	/* NOTE: the register pool is only valid after active_count. In the
	 * active part of the array, there be multiple registers with the same
	 * value. */
	u32 active_start = 0;
	u32 active_count = 0;
	for (u32 i = 0; i < reg_count; i++) {
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
			b32 is_mreg = (inactive_register >= program.vreg_count);
			if (is_mreg) {
				u32 mreg = reg_count - 1 - inactive_register;
				register_pool[active_count] = mreg;
				ASSERT(register_pool[active_count] < program.mreg_count);
			} else if (vreg[inactive_register].kind == MOP_MREG) {
				register_pool[active_count] = vreg[inactive_register].value;
				ASSERT(register_pool[active_count] < program.mreg_count);
			}

			sorted_by_start[j] = sorted_by_start[active_start];
			sorted_by_start[active_start++] = inactive_register;
		}

		b32 is_mreg = (current_register >= program.vreg_count);
		if (is_mreg) {
			machine_operand mreg = make_mreg(reg_count - 1 - current_register);
			b32 should_reallocate = false;
			for (u32 j = active_start; j < active_end; j++) {
				u32 previous_register = sorted_by_start[j];
				if (machine_operand_equals(mreg, vreg[previous_register])) {
					/* Reallocate the previous register */
					current_register = previous_register;
					should_reallocate = true;
					/* There can only be one such register */
					break;
				}
			}

			/* Remove the register from the pool if it exists */
			for (u32 j = active_count; j < program.mreg_count; j++) {
				if (register_pool[j] == mreg.value) {
					u32 tmp = register_pool[j];
					ASSERT(tmp < program.mreg_count);
					register_pool[j] = register_pool[active_count];
					register_pool[active_count++] = tmp;
					break;
				}
			}

			if (!should_reallocate) {
				continue;
			}
		}

		ASSERT(current_register < program.vreg_count);
		if (active_count >= mreg_count) {
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

			if (intervals[spill].end > intervals[current_register].end) {
				ASSERT(!force_mreg_for[spill]);
				vreg[current_register] = vreg[spill];
				vreg[spill] = make_spill(info.spill_count++);
				sorted_by_start[spill_index] = sorted_by_start[active_start];
				sorted_by_start[active_start] = spill;
				active_start++;
			} else {
				ASSERT(!force_mreg_for[current_register]);
				vreg[current_register] = make_spill(info.spill_count++);
			}
		} else {
			u32 mreg = register_pool[active_count++];
			ASSERT(mreg < mreg_count);
			info.used[mreg] |= !is_empty;
			vreg[current_register] = make_mreg(mreg);
		}
	}

	for (u32 i = first_instr; i < last_instr; i++) {
		machine_instr *instr = get_instr(program, i);
		u32 operand_count = instr->operand_count;

		machine_operand *operands = (machine_operand *)(instr + 1);
		for (u32 i = 0; i < operand_count; i++) {
			if (operands[i].kind == MOP_VREG) {
				u32 reg = operands[i].value;
				ASSERT(reg < program.vreg_count);
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
		info[i] = allocate_function_registers(program, i, arena);
	}

	return info;
}
