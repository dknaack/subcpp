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

typedef struct {
	u32 start;
	u32 size;
	u32 next[2];
} ir_block;

typedef struct {
	str name;
	u32 parameter_count;
	u32 instr_index;
	u32 block_index;
	u32 block_count;
	u32 stack_size;
} ir_function;

typedef struct {
	ir_instr *instrs;
	ir_block *blocks;
	ir_function *functions;

	u32 block_count;
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

	u32 *label_addresses;
	u32 max_instr_count;

	variable *variable_table;
	u32 variable_table_size;
	u32 stack_size;

	u32 continue_label;
	u32 break_label;
} ir_context;

static char *
get_opcode_name(ir_opcode opcode)
{
	switch (opcode) {
	case IR_NOP:   return "nop";
	case IR_LABEL: return "label";
	case IR_VAR:   return "var";
	case IR_INT:   return "int";
	case IR_COPY:  return "copy";
	case IR_MOV:   return "mov";
	case IR_ADD:   return "add";
	case IR_AND:   return "and";
	case IR_SUB:   return "sub";
	case IR_MUL:   return "mul";
	case IR_DIV:   return "div";
	case IR_MOD:   return "mod";
	case IR_EQL:   return "eql";
	case IR_LT:    return "lt";
	case IR_GT:    return "gt";
	case IR_LEQ:   return "leq";
	case IR_GEQ:   return "geq";
	case IR_LTU:   return "ltu";
	case IR_GTU:   return "gtu";
	case IR_LEQU:  return "lequ";
	case IR_GEQU:  return "gequ";
	case IR_OR:    return "or";
	case IR_XOR:   return "xor";
	case IR_SHL:   return "shl";
	case IR_SHR:   return "shr";
	case IR_JMP:   return "jmp";
	case IR_JIZ:   return "jiz";
	case IR_JNZ:   return "jnz";
	case IR_RET:   return "ret";
	case IR_CALL:  return "call";
	case IR_PARAM: return "param";
	case IR_ALLOC: return "alloc";
	case IR_PRINT: return "print";
	case IR_LOAD:  return "load";
	case IR_STORE: return "store";
	case IR_FLOAT: return "float";
	case IR_FLOAD: return "fload";
	case IR_FSTORE: return "fstore";
	case IR_FADD:   return "fadd";
	case IR_FSUB:   return "fsub";
	case IR_FMUL:   return "fmul";
	case IR_FDIV:   return "fdiv";
	}

	return "(invalid)";
}

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
