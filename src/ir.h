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
	u32 size:8;
	u32 op0;
	u32 op1;
} ir_instr;

typedef struct {
	u32 start;
	u32 size;
	u32 next[2];
} ir_block;

typedef struct {
	string name;
	u32 parameter_count;
	u32 instr_index;
	u32 block_index;
	u32 block_count;
} ir_function;

typedef struct {
	ir_instr *instrs;
	ir_block *blocks;
	ir_function *functions;
	u32 *toplevel_instr_indices;

	u32 block_count;
	u32 register_count;
	u32 function_count;
	u32 toplevel_count;
	u32 instr_count;
	u32 label_count;
} ir_program;

typedef struct variable variable;
struct variable {
	variable *next;
	string name;
	u32 vreg;
};

typedef struct {
	ir_program program;

	u32 *label_addresses;
	u32 max_instr_count;

	variable *variable_table;
	u32 variable_table_size;

	u32 continue_label;
	u32 break_label;
} ir_context;
