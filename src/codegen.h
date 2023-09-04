#define LOC_PAIR(a, b) ((a) * LOC_TYPE_COUNT + (b))

enum ir_opcode {
	IR_NOP,
	IR_LABEL,
	IR_SET,
	IR_MOV,
	IR_ADD,
	IR_SUB,
	IR_MUL,
	IR_DIV,
	IR_MOD,
	IR_JMP,
	IR_JIZ,
	IR_RET,
};

struct ir_instruction {
	enum ir_opcode opcode;
	uint32_t op0;
	uint32_t op1;
	uint32_t dst;
};

struct ir_program {
	struct ir_instruction *instructions;
	uint32_t *block_start;

	uint32_t block_count;
	uint32_t register_count;
	uint32_t instruction_count;
	uint32_t label_count;
};

enum location_type {
	LOC_REGISTER,
	LOC_STACK,
	LOC_CONST,
	LOC_LABEL,
	LOC_TYPE_COUNT
};

struct location {
	enum location_type type;
	uint32_t address;
};

struct variable {
	struct variable *next;
	struct string name;
	uint32_t _register;
};

struct generator {
	struct ir_program program;

	uint32_t *label_addresses;
	uint32_t max_instruction_count;

	struct variable *variable_table;
	uint32_t variable_table_size;

	uint32_t continue_label;
	uint32_t break_label;
};
