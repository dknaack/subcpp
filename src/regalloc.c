#include <string.h>

struct live_interval {
	uint32_t start;
	uint32_t end;
};

struct bit_matrix {
	bool *bits;
	uint32_t width;
	uint32_t height;
};

// NOTE: width is the number of bits
static struct bit_matrix
bit_matrix_init(uint32_t width, uint32_t height, struct arena *arena)
{
	struct bit_matrix matrix = {0};
	matrix.bits = ZALLOC(arena, width * height, bool);
	matrix.width = width;
	matrix.height = height;
	return matrix;
}

static void
set_bit(struct bit_matrix matrix, uint32_t y, uint32_t x)
{
	ASSERT(x < matrix.width);
	ASSERT(y < matrix.height);
	uint32_t i = y * matrix.width + x;
	matrix.bits[i] = 1;
}

static void
clear_bit(struct bit_matrix matrix, uint32_t y, uint32_t x)
{
	ASSERT(x < matrix.width);
	ASSERT(y < matrix.height);
	uint32_t i = y * matrix.width + x;
	matrix.bits[i] = 0;
}

static void
clear_row(struct bit_matrix matrix, uint32_t y)
{
	ASSERT(y < matrix.height);
	for (uint32_t x = 0; x < matrix.width; x++) {
		matrix.bits[y * matrix.width + x] = 0;
	}
}

static void
union_rows(struct bit_matrix matrix, uint32_t dst_y, uint32_t src_y)
{
	ASSERT(dst_y < matrix.height);
	ASSERT(src_y < matrix.height);
	for (uint32_t x = 0; x < matrix.width; x++) {
		bool src_bit = matrix.bits[src_y * matrix.width + x];
		matrix.bits[dst_y * matrix.width + x] |= src_bit;
	}
}

static void
print_row(struct bit_matrix matrix, uint32_t y)
{
	printf("{");
	bool first = true;
	for (uint32_t x = 0; x < matrix.width; x++) {
		if (matrix.bits[y * matrix.width + x]) {
			if (first) {
				first = false;
			} else {
				printf(", ");
			}

			uint32_t mreg = matrix.width - 1 - x;
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
print_matrix(struct bit_matrix matrix)
{
	for (uint32_t y = 0; y < matrix.height; y++) {
		printf("live[%d] = ", y);
		print_row(matrix, y);
	}
}

static struct bit_matrix
get_live_matrix(struct machine_program program, uint32_t first_instr,
    uint32_t last_instr, uint32_t reg_count, struct arena *arena)
{
	// TODO: use a bitset as matrix instead of a boolean matrix.
	uint32_t instr_count = last_instr - first_instr;
	struct bit_matrix live_matrix = bit_matrix_init(reg_count, instr_count, arena);
	struct arena_temp temp = arena_temp_begin(arena);
	struct bit_matrix prev_live_matrix = bit_matrix_init(reg_count, instr_count, arena);

	bool has_matrix_changed = false;
	do {
		uint32_t i = instr_count;
		while (i-- > 0) {
			struct machine_instr *instr = get_instr(program, first_instr + i);

			clear_row(live_matrix, i);
			/* TODO: successor of jump instructions */
			if (i + 1 != instr_count) {
				union_rows(live_matrix, i, i + 1);
			}

			struct machine_operand *operands = (struct machine_operand *)(instr + 1);
			for (uint32_t j = 0; j < instr->operand_count; j++) {
				if (operands[j].kind != MOP_LABEL) {
					continue;
				}

				uint32_t block_index = operands[j].value;
				ASSERT(block_index < program.block_count);

				struct machine_block block = program.blocks[block_index];
				uint32_t instr_index = block.instr_index - first_instr;
				union_rows(live_matrix, i, instr_index);
			}

			for (uint32_t j = 0; j < instr->operand_count; j++) {
				if (operands[j].kind != MOP_FUNC) {
					continue;
				}

				for (uint32_t k = 0; k < program.temp_mreg_count; k++) {
					uint32_t mreg = program.temp_mregs[k];
					set_bit(live_matrix, i, live_matrix.width - 1 - mreg);
				}
			}

			for (uint32_t j = 0; j < instr->operand_count; j++) {
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

		size_t matrix_size = live_matrix.width * live_matrix.height * sizeof(bool);
		has_matrix_changed = memcmp(live_matrix.bits, prev_live_matrix.bits, matrix_size);
		memcpy(prev_live_matrix.bits, live_matrix.bits, matrix_size);
	} while (has_matrix_changed);

	arena_temp_end(temp);
	return live_matrix;
}

static uint32_t
get_interval_start(struct bit_matrix live_matrix, uint32_t reg)
{
	for (uint32_t i = 0; i < live_matrix.height; i++) {
		if (live_matrix.bits[i * live_matrix.width + reg]) {
			return i;
		}
	}


	return live_matrix.height - 1;
}

static uint32_t
get_interval_end(struct bit_matrix live_matrix, uint32_t reg)
{
	for (uint32_t i = live_matrix.height; i-- > 0;) {
		if (live_matrix.bits[i * live_matrix.width + reg]) {
			return i;
		}
	}

	return 0;
}

// TODO: replace with counting-sort?
static uint32_t *
sort_intervals_by_start(struct live_interval *intervals,
    uint32_t interval_count, struct arena *arena)
{
	uint32_t *index = ALLOC(arena, interval_count, uint32_t);
	for (uint32_t i = 0; i < interval_count; i++) {
		index[i] = i;
	}

	for (uint32_t i = 1; i < interval_count; i++) {
		uint32_t j = i;
		while (j > 0 && intervals[index[j - 1]].start > intervals[index[j]].start) {
			uint32_t tmp = index[j];
			index[j] = index[j-1];
			index[j-1] = tmp;
			j--;
		}
	}

	return index;
}

struct allocation_info {
	bool *used;
	uint32_t spill_count;
};

static struct allocation_info
allocate_function_registers(struct machine_program program,
    uint32_t function_index, struct arena *arena)
{
	struct allocation_info info = {0};
	uint32_t mreg_count = program.mreg_count;
	uint32_t reg_count = program.mreg_count + program.vreg_count;
	info.used = ZALLOC(arena, program.mreg_count, bool);

	struct arena_temp temp = arena_temp_begin(arena);

	struct machine_function function = program.functions[function_index];
	uint32_t first_instr = function.instr_index;
	uint32_t last_instr = program.instr_count;
	if (function_index + 1 < program.function_count) {
		struct machine_function next_function = program.functions[function_index+1];
		last_instr = next_function.instr_index;
	}

	struct bit_matrix live_matrix = get_live_matrix(program, first_instr, last_instr, reg_count, arena);
	struct live_interval *intervals = ALLOC(arena, reg_count, struct live_interval);
	for (uint32_t i = 0; i < reg_count; i++) {
		intervals[i].start = get_interval_start(live_matrix, i);
		intervals[i].end   = get_interval_end(live_matrix, i);
	}

	uint32_t *sorted_by_start = sort_intervals_by_start(intervals, reg_count, arena);

	uint32_t *register_pool = ALLOC(arena, mreg_count, uint32_t);
	for (uint32_t i = 0; i < mreg_count; i++) {
		register_pool[i] = i;
	}

	struct machine_operand *vreg = ALLOC(arena, reg_count, struct machine_operand);

	bool *force_mreg_for = ZALLOC(arena, reg_count, bool);
	/* TODO: Mark registers where the force flag is set */

	/* NOTE: the register pool is only valid after active_count. In the
	 * active part of the array, there be multiple registers with the same
	 * value. */
	uint32_t active_start = 0;
	uint32_t active_count = 0;
	for (uint32_t i = 0; i < reg_count; i++) {
		uint32_t current_register = sorted_by_start[i];
		uint32_t current_start = intervals[current_register].start;
		uint32_t current_end = intervals[current_register].end;
		bool is_empty = (current_start > current_end);

		uint32_t active_end = active_start + active_count;
		for (uint32_t j = active_start; j < active_end; j++) {
			uint32_t inactive_register = sorted_by_start[j];
			uint32_t end = intervals[inactive_register].end;
			bool is_active = (end >= current_start);
			if (is_active) {
				continue;
			}

			/* Free the register again */
			active_count--;
			bool is_mreg = (inactive_register >= program.vreg_count);
			if (is_mreg) {
				uint32_t mreg = reg_count - 1 - inactive_register;
				register_pool[active_count] = mreg;
				ASSERT(register_pool[active_count] < program.mreg_count);
			} else if (vreg[inactive_register].kind == MOP_MREG) {
				register_pool[active_count] = vreg[inactive_register].value;
				ASSERT(register_pool[active_count] < program.mreg_count);
			}

			sorted_by_start[j] = sorted_by_start[active_start];
			sorted_by_start[active_start++] = inactive_register;
		}

		bool is_mreg = (current_register >= program.vreg_count);
		if (is_mreg) {
			struct machine_operand mreg = make_mreg(reg_count - 1 - current_register);
			bool should_reallocate = false;
			for (uint32_t j = active_start; j < active_end; j++) {
				uint32_t previous_register = sorted_by_start[j];
				if (machine_operand_equals(mreg, vreg[previous_register])) {
					/* Reallocate the previous register */
					current_register = previous_register;
					should_reallocate = true;
					/* There can only be one such register */
					break;
				}
			}

			/* Remove the register from the pool if it exists */
			for (uint32_t j = active_count; j < program.mreg_count; j++) {
				if (register_pool[j] == mreg.value) {
					uint32_t tmp = register_pool[j];
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
			uint32_t spill = sorted_by_start[active_start];
			uint32_t spill_index = active_start;
			uint32_t end = 0;
			for (uint32_t j = active_start + 1; j < active_end; j++) {
				uint32_t reg = sorted_by_start[j];
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
			uint32_t mreg = register_pool[active_count++];
			ASSERT(mreg < mreg_count);
			info.used[mreg] |= !is_empty;
			vreg[current_register] = make_mreg(mreg);
		}
	}

	for (uint32_t i = first_instr; i < last_instr; i++) {
		struct machine_instr *instr = get_instr(program, i);
		uint32_t operand_count = instr->operand_count;

		struct machine_operand *operands = (struct machine_operand *)(instr + 1);
		for (uint32_t i = 0; i < operand_count; i++) {
			if (operands[i].kind == MOP_VREG) {
				uint32_t reg = operands[i].value;
				ASSERT(reg < program.vreg_count);
				operands[i].kind = vreg[reg].kind;
				operands[i].value = vreg[reg].value;
			}
		}
	}

	arena_temp_end(temp);
	return info;
}

static struct allocation_info *
allocate_registers(struct machine_program program, struct arena *arena)
{
	struct allocation_info *info = ALLOC(arena, program.function_count,
	    struct allocation_info);
	for (uint32_t i = 0; i < program.function_count; i++) {
		info[i] = allocate_function_registers(program, i, arena);
	}

	return info;
}
