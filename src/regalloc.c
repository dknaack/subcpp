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

			printf("r%d", x);
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
get_live_matrix(struct machine_program program, uint32_t *instr_offsets,
    uint32_t instr_count, struct arena *arena)
{
	// TODO: use a bitset as matrix instead of a boolean matrix.
	struct bit_matrix live_matrix = bit_matrix_init(
	    program.vreg_count + program.mreg_count, instr_count, arena);
	struct arena_temp temp = arena_temp_begin(arena);
	struct bit_matrix prev_live_matrix = bit_matrix_init(
	    program.vreg_count, instr_count, arena);

	char *code = program.code;
	bool has_matrix_changed = false;
	do {
		uint32_t i = instr_count;
		while (i-- > 0) {
			uint32_t offset = instr_offsets[i];
			struct machine_instr *instr = (struct machine_instr *)(code + offset);

			clear_row(live_matrix, i);
			/* TODO: successor of jump instructions */
			if (i + 1 != instr_count) {
				union_rows(live_matrix, i, i + 1);
			}

			struct machine_operand *operands = (struct machine_operand *)(instr + 1);
			for (uint32_t j = 0; j < instr->operand_count; j++) {
				switch (operands[j].kind) {
				case MOP_MREG:
					if (operands[j].flags & MOP_DEF) {
						clear_bit(live_matrix, i, live_matrix.width - operands[j].value - 1);
					}

					if (operands[j].flags & MOP_USE) {
						set_bit(live_matrix, i, live_matrix.width - operands[j].value - 1);
					}
					break;
				case MOP_VREG:
					if (operands[j].flags & MOP_DEF) {
						clear_bit(live_matrix, i, operands[j].value);
					}

					if (operands[j].flags & MOP_USE) {
						set_bit(live_matrix, i, operands[j].value);
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

static void
allocate_registers(struct machine_program program,
    uint32_t mreg_count, struct arena *arena)
{
	struct arena_temp temp = arena_temp_begin(arena);
	struct live_interval *intervals = ALLOC(arena, program.vreg_count, struct live_interval);

	uint32_t instr_count = 0;
	char *start = (char *)program.code;
	char *end = start + program.size;
	char *code = start;
	while (code < end) {
		struct machine_instr *instr = (struct machine_instr *)code;
		code += sizeof(*instr);
		code += instr->operand_count * sizeof(struct machine_operand);
		instr_count++;
	}

	uint32_t *instr_offsets = ALLOC(arena, instr_count, uint32_t);
	uint32_t instr_index = 0;
	code = start;
	while (code < end) {
		struct machine_instr *instr = (struct machine_instr *)code;
		code += sizeof(*instr);
		code += instr->operand_count * sizeof(struct machine_operand);

		instr_offsets[instr_index++] = code - start;
	}

	struct bit_matrix live_matrix = get_live_matrix(program, instr_offsets, instr_count, arena);
	uint32_t reg_count = program.mreg_count + program.vreg_count;
	for (uint32_t i = 0; i < reg_count; i++) {
		intervals[i].start = get_interval_start(live_matrix, i);
		intervals[i].end   = get_interval_end(live_matrix, i);
	}

	uint32_t *sorted_by_start = sort_intervals_by_start(intervals, reg_count, arena);

	uint32_t *register_pool = ALLOC(arena, mreg_count, uint32_t);
	for (uint32_t i = 0; i < mreg_count; i++) {
		register_pool[i] = i;
	}

	struct machine_operand *vreg = ALLOC(arena,
	    program.vreg_count, struct machine_operand);

	uint32_t spill_count = 0;
	uint32_t active_start = 0;
	uint32_t active_count = 0;
	for (uint32_t i = 0; i < reg_count; i++) {
		uint32_t current_register = sorted_by_start[i];
		uint32_t current_start = intervals[current_register].start;

		uint32_t active_end = active_start + active_count;
		for (uint32_t j = active_start; j < active_end; j++) {
			uint32_t inactive_register = sorted_by_start[j];
			uint32_t end = intervals[inactive_register].end;
			bool is_active = (end >= current_start);
			if (is_active) {
				continue;
			}

			active_count--;
			register_pool[active_count] = vreg[inactive_register].value;

			sorted_by_start[j] = sorted_by_start[active_start];
			sorted_by_start[active_start++] = inactive_register;
		}

		if (current_register < mreg_count && false) {
			/* TODO: ensure that register is not used */
			vreg[current_register] = make_mreg(current_register);
			active_count++;
		} else if (active_count == mreg_count) {
			uint32_t spill = sorted_by_start[active_start];
			uint32_t end = 0;
			for (uint32_t j = active_start; j < active_end; j++) {
				uint32_t reg = sorted_by_start[j];
				if (intervals[reg].end > end) {
					end = intervals[reg].end;
					spill = reg;
				}
			}

			if (intervals[spill].end > intervals[current_register].end) {
				vreg[current_register] = vreg[spill];
				vreg[spill] = make_spill(spill_count++);
				active_start++;
				active_count++;
			} else {
				vreg[spill] = make_spill(spill_count++);
			}
		} else {
			vreg[current_register] = make_mreg(register_pool[active_count++]);
		}
	}

	code = start;
	while (code < end) {
		struct machine_instr *instr = (struct machine_instr *)code;
		uint32_t operand_count = instr->operand_count;
		code += sizeof(struct machine_instr);

		struct machine_operand *operands = (struct machine_operand *)code;
		code += operand_count * sizeof(struct machine_operand);

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
}
