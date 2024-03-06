typedef enum {
	IR_NOP,
	IR_LABEL,
	IR_ALLOC,
	IR_JMP,

	// Integer instructions
	IR_INT,
	IR_VAR,
	IR_COPY,
	IR_MOV,
	IR_ADD,
	IR_AND,
	IR_SUB,
	IR_MUL,
	IR_DIV,
	IR_MOD,
	IR_EQL,
	IR_LT,
	IR_GT,
	IR_LEQ,
	IR_GEQ,
	IR_OR,
	IR_SHL,
	IR_SHR,
	IR_XOR,
	IR_JIZ,
	IR_JNZ,
	IR_RET,
	IR_CALL,
	IR_PARAM,
	IR_PRINT,
	IR_LOAD,
	IR_STORE,

	// Unsigned integer instructions
	IR_LTU,
	IR_GTU,
	IR_LEQU,
	IR_GEQU,

	// Floating point instructions
	IR_FLOAT,
	IR_FLOAD,
	IR_FSTORE,
	IR_FADD,
	IR_FSUB,
	IR_FMUL,
	IR_FDIV,
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

typedef struct ir_function ir_function;
struct ir_function {
	ir_function *next;

	str name;
	u32 parameter_count;
	u32 instr_index;
	u32 instr_count;
	u32 label_count;
	u32 stack_size;
};

typedef struct symbol symbol;
struct symbol {
	symbol *next;
	symbol *child[4];

	b32 function;
	b32 global;
	str name;
	i64 addr;
	i64 size;
};

typedef struct {
	ir_instr *instrs;
	ir_function *function_list;
	symbol *symbols;

	u32 register_count;
	u32 function_count;
	u32 instr_count;
	u32 label_count;
} ir_program;

typedef struct variable variable;
struct variable {
	variable *next;
	str name;
	u32 vreg;
};

typedef struct {
	ir_program program;
	arena *arena;

	u32 *label_addresses;
	u32 max_instr_count;

	variable *variable_table;
	u32 variable_table_size;
	u32 stack_size;

	u32 continue_label;
	u32 break_label;
} ir_context;

static b32
is_comparison_opcode(ir_opcode ir_opcode)
{
	switch (ir_opcode) {
	case IR_EQL:
	case IR_LT:
	case IR_GT:
	case IR_LEQ:
	case IR_GEQ:
	case IR_LTU:
	case IR_GTU:
	case IR_LEQU:
	case IR_GEQU:
		return true;
	default:
		return false;
	}
}
