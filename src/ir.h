typedef enum {
	IR_NOP,
	IR_LABEL,
	IR_CONST,
	IR_MOV,
	IR_ADD,
	IR_SUB,
	IR_MUL,
	IR_DIV,
	IR_MOD,
	IR_EQL,
	IR_LT,
	IR_GT,
	IR_LEQ,
	IR_GEQ,
	IR_JMP,
	IR_JIZ,
	IR_RET,
	IR_CALL,
	IR_PARAM,
	IR_ALLOC,
	IR_PRINT,
	IR_LOAD,
	IR_STORE,
} ir_opcode;

typedef enum {
	IR_OPERAND_NONE,
	IR_OPERAND_REG_SRC,
	IR_OPERAND_REG_DST,
	IR_OPERAND_CONST,
	IR_OPERAND_LABEL,
	IR_OPERAND_FUNC,
	IR_OPERAND_COUNT
} ir_operand_type;

typedef struct {
	ir_operand_type op0, op1;
} ir_opcode_info;

typedef struct {
	ir_opcode opcode:24;
	uint32_t size:8;
	uint32_t op0;
	uint32_t op1;
} ir_instr;

typedef struct {
	uint32_t start;
	uint32_t size;
	uint32_t next[2];
} ir_block;

typedef struct {
	string name;
	uint32_t parameter_count;
	uint32_t instr_index;
	uint32_t block_index;
	uint32_t block_count;
} ir_function;

typedef struct {
	ir_instr *instrs;
	ir_block *blocks;
	ir_function *functions;
	uint32_t *toplevel_instr_indices;

	uint32_t block_count;
	uint32_t register_count;
	uint32_t function_count;
	uint32_t toplevel_count;
	uint32_t instr_count;
	uint32_t label_count;
} ir_program;

typedef struct variable variable;
struct variable {
	variable *next;
	string name;
	uint32_t vreg;
};

typedef struct {
	ir_program program;

	uint32_t *label_addresses;
	uint32_t max_instr_count;

	variable *variable_table;
	uint32_t variable_table_size;

	uint32_t continue_label;
	uint32_t break_label;
} ir_generator;
