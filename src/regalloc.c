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
set_bit(struct bit_matrix matrix, uint32_t x, uint32_t y)
{
	ASSERT(x < matrix.width);
	ASSERT(y < matrix.height);
	uint32_t i = y * matrix.width + x;
	matrix.bits[i] = 1;
}

static void
clear_bit(struct bit_matrix matrix, uint32_t x, uint32_t y)
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
get_live_matrix(struct ir_program program, struct arena *arena)
{
	// TODO: use a bitset as matrix instead of a boolean matrix.
	struct bit_matrix live_matrix = bit_matrix_init(
	    program.register_count, program.instruction_count, arena);
	struct bit_matrix prev_live_matrix = bit_matrix_init(
	    program.register_count, program.instruction_count, arena);
	struct ir_instruction *instructions = program.instructions;
	uint32_t *label_addresses = program.label_addresses;

	bool has_matrix_changed;
	do {
		has_matrix_changed = false;
		uint32_t i = program.instruction_count;
		while (i-- > 0) {
			uint32_t kill, use0, use1, address;
			kill = use0 = use1 = program.register_count;

			clear_row(live_matrix, i);
			switch (instructions[i].opcode) {
			case IR_JMP:
				address = label_addresses[instructions[i].op0];
				union_rows(live_matrix, i, address);
				break;
			case IR_JIZ:
				address = label_addresses[instructions[i].op1];
				union_rows(live_matrix, i, address);
				/* fallthrough */
			default:
				if (i + 1 != program.instruction_count) {
					union_rows(live_matrix, i, i + 1);
				}
			}

			switch (instructions[i].opcode) {
			case IR_SET:
				kill = instructions[i].dst;
				break;
			case IR_MOV:
				kill = instructions[i].dst;
				use0 = instructions[i].op0;
				break;
			case IR_ADD:
			case IR_SUB:
			case IR_MUL:
			case IR_DIV:
			case IR_MOD:
				kill = instructions[i].dst;
				use0 = instructions[i].op0;
				use1 = instructions[i].op1;
				break;
			case IR_JIZ:
			case IR_RET:
				use0 = instructions[i].op0;
				break;
			default:
				break;
			}

			if (kill < program.register_count) {
				clear_bit(live_matrix, kill, i);
			}

			if (use0 < program.register_count) {
				set_bit(live_matrix, use0, i);
			}

			if (use1 < program.register_count) {
				set_bit(live_matrix, use1, i);
			}
		}

		size_t matrix_size = live_matrix.width * live_matrix.height * sizeof(bool);
		has_matrix_changed = memcmp(live_matrix.bits, prev_live_matrix.bits, matrix_size);
		memcpy(prev_live_matrix.bits, live_matrix.bits, matrix_size);
	} while (has_matrix_changed);

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

enum location_type {
	LOCATION_REGISTER,
	LOCATION_STACK,
	LOCATION_CONST,
	LOCATION_LABEL,
};

struct location {
	enum location_type type;
	uint32_t address;
};

static struct location
new_location(enum location_type type, uint32_t address)
{
	struct location location;
	location.type = type;
	location.address = address;
	return location;
}

static struct location
label_location(uint32_t value)
{
	return new_location(LOCATION_LABEL, value);
}

static struct location
const_location(uint32_t value)
{
	return new_location(LOCATION_CONST, value);
}

static struct location
stack_location(uint32_t address)
{
	return new_location(LOCATION_STACK, address);
}

static struct location
register_location(uint32_t reg)
{
	return new_location(LOCATION_REGISTER, reg);
}

static struct location *
allocate_registers(struct ir_program program, uint32_t target_register_count,
    struct arena *arena)
{
	struct location *location = ZALLOC(arena,
	    program.register_count, struct location);

	struct arena_temp temp = arena_temp_begin(arena);
	struct live_interval *intervals = ALLOC(arena, program.register_count, struct live_interval);
	struct bit_matrix live_matrix = get_live_matrix(program, arena);

	for (uint32_t i = 0; i < program.register_count; i++) {
		intervals[i].start = get_interval_start(live_matrix, i);
		intervals[i].end   = get_interval_end(live_matrix, i);
	}

	uint32_t *sorted_by_start = sort_intervals_by_start(intervals, program.register_count, arena);

	uint32_t *register_pool = ALLOC(arena, target_register_count, uint32_t);
	for (uint32_t i = 0; i < target_register_count; i++) {
		register_pool[i] = i;
	}

	uint32_t spill_count = 0;
	uint32_t active_start = 0;
	uint32_t active_count = 0;
	for (uint32_t i = 0; i < program.register_count; i++) {
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
			register_pool[active_count] = location[inactive_register].address;

			sorted_by_start[j] = sorted_by_start[active_start];
			sorted_by_start[active_start++] = inactive_register;
		}

		if (active_count == target_register_count) {
			uint32_t spill = sorted_by_start[active_start];
			uint32_t end = 0;
			for (uint32_t j = active_start; j < active_end; j++) {
				uint32_t _register = sorted_by_start[j];
				if (intervals[_register].end > end) {
					end = intervals[_register].end;
					spill = _register;
				}
			}

			if (intervals[spill].end > intervals[current_register].end) {
				location[current_register] = location[spill];
				location[spill] = stack_location(spill_count++);
				active_start++;
				active_count++;
			} else {
				location[spill] = stack_location(spill_count++);
			}
		} else {
			location[current_register] = register_location(register_pool[active_count++]);
		}
	}

	arena_temp_end(temp);
	return location;
}
