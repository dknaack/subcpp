typedef enum {
	IR_NOP,
	IR_ADD,
	IR_ALLOC,
	IR_AND,
	IR_CALL,
	IR_CALL_BUILTIN,
	IR_CAST,
	IR_CASTU,
	IR_CONST,
	IR_COPY,
	IR_DIV,
	IR_EQL,
	IR_GEQ,
	IR_GEQU,
	IR_GLOBAL,
	IR_GT,
	IR_GTU,
	IR_JIZ,
	IR_JMP,
	IR_JNZ,
	IR_LABEL,
	IR_LEQ,
	IR_LEQU,
	IR_LOAD,
	IR_LT,
	IR_LTU,
	IR_MOD,
	IR_MOV,
	IR_MUL,
	IR_NOT,
	IR_OR,
	IR_PARAM,
	IR_RET,
	IR_SEXT,
	IR_SHL,
	IR_SHR,
	IR_STORE,
	IR_SUB,
	IR_TRUNC,
	IR_VAR,
	IR_XOR,
	IR_ZEXT,
} ir_opcode;

typedef enum {
	IR_VOID,
	IR_I8,
	IR_I16,
	IR_I32,
	IR_I64,
	IR_F32,
	IR_F64,
} ir_type;

typedef enum {
	IR_OPERAND_NONE,
	IR_OPERAND_REG_SRC,
	IR_OPERAND_REG_DST,
	IR_OPERAND_GLOBAL,
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
	ir_type type:8;
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

typedef struct {
	ir_instr *instrs;
	ir_function *function_list;

	u32 register_count;
	u32 function_count;
	u32 instr_count;
	u32 label_count;
} ir_program;

typedef enum {
	BUILTIN_POPCOUNT,
	BUILTIN_VA_START,
	BUILTIN_VA_ARG,
	BUILTIN_VA_END,
} ir_builtin;

typedef struct {
	ir_program *program;
	symbol_table *symtab;
	arena *arena;

	u32 *symbol_registers;
	u32 max_instr_count;
	u32 stack_size;
	u32 continue_label;
	u32 break_label;
	u32 case_label;
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

static isize
ir_sizeof(ir_type type)
{
	switch (type) {
	case IR_I8:  return 1;
	case IR_I16: return 2;
	case IR_I32: return 4;
	case IR_I64: return 8;
	case IR_F32: return 4;
	case IR_F64: return 8;
	case IR_VOID:
	}

	return 0;
}

static ir_type
ir_typeof(isize size, b32 is_float)
{
	ir_type type = IR_VOID;

	if (is_float) {
		if (size == 4) {
			type = IR_F32;
		} else if (size == 8) {
			type = IR_F64;
		}
	} else {
		if (size == 1) {
			type = IR_I8;
		} else if (size == 2) {
			type = IR_I16;
		} else if (size == 4) {
			type = IR_I32;
		} else if (size == 8) {
			type = IR_I64;
		}
	}

	ASSERT(type != IR_VOID);
	return type;
}

// TODO: This depends on the underlying system. For example, a long can either
// be 4 bytes or 8 bytes.
static ir_type
ir_type_from(type *type)
{
	switch (type->kind) {
	case TYPE_VOID:
		return IR_VOID;
	case TYPE_CHAR:
	case TYPE_CHAR_UNSIGNED:
		return IR_I8;
	case TYPE_SHORT:
	case TYPE_SHORT_UNSIGNED:
		return IR_I16;
	case TYPE_INT:
	case TYPE_INT_UNSIGNED:
		return IR_I32;
	case TYPE_LONG:
	case TYPE_LONG_UNSIGNED:
		return IR_I32;
	case TYPE_LLONG:
	case TYPE_LLONG_UNSIGNED:
		return IR_I64;
	case TYPE_FLOAT:
		return IR_F32;
	case TYPE_DOUBLE:
		return IR_F64;
	case TYPE_POINTER:
	case TYPE_ARRAY:
		return IR_I64;
	case TYPE_FUNCTION:
		return IR_I64;
	default:
		ASSERT(!"Invalid type");
	}

	return IR_VOID;
}
