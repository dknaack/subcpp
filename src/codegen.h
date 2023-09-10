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
	IR_CALL,
	IR_PARAM,
	IR_VAR,
	IR_PRINT,
};

struct ir_instr {
	enum ir_opcode opcode;
	uint32_t op0;
	uint32_t op1;
};

struct ir_block {
	uint32_t start;
	uint32_t size;
	uint32_t next[2];
};

struct ir_function {
	struct string name;
	uint32_t block_index;
	uint32_t block_count;
};

struct ir_program {
	struct ir_instr *instrs;
	struct ir_block *blocks;
	struct ir_function *functions;

	uint32_t block_count;
	uint32_t register_count;
	uint32_t function_count;
	uint32_t instr_count;
	uint32_t label_count;
};

enum location_type {
	LOC_INVALID,
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
	uint32_t max_instr_count;

	struct variable *variable_table;
	uint32_t variable_table_size;

	uint32_t continue_label;
	uint32_t break_label;
};
