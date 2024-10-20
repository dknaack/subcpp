typedef enum {
	IR_NOP,

	// declarations
	IR_ALLOC,
	IR_BUILTIN,
	IR_CONST,
	IR_GLOBAL,
	IR_LABEL,
	IR_PARAM,
	IR_VAR,

	// data movement
	IR_COPY,
	IR_LOAD,
	IR_MOV,
	IR_STORE,

	// control-flow operations
	IR_CALL,
	IR_JIZ,
	IR_JMP,
	IR_JNZ,

	// data operations
	IR_ADD,
	IR_AND,
	IR_CAST,
	IR_CASTU,
	IR_DIV,
	IR_EQL,
	IR_GEQ,
	IR_GEQU,
	IR_GT,
	IR_GTU,
	IR_LEQ,
	IR_LEQU,
	IR_LT,
	IR_LTU,
	IR_MOD,
	IR_MUL,
	IR_NOT,
	IR_OR,
	IR_RET,
	IR_SEXT,
	IR_SHL,
	IR_SHR,
	IR_SUB,
	IR_TRUNC,
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
	BUILTIN_POPCOUNT,
	BUILTIN_VA_ARG,
	BUILTIN_VA_END,
	BUILTIN_VA_LIST,
	BUILTIN_VA_START,
} ir_builtin;

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
} ir_inst;

typedef struct ir_function ir_function;
struct ir_function {
	str name;
	i32 param_count;
	i32 inst_index;
	i32 inst_count;
	i32 stack_size;
};

typedef struct {
	ir_inst *insts;
	ir_function *functions;
	symbol_table symtab;

	// Maximum number of registers/labels in each function
	isize max_reg_count;
	isize max_label_count;
	isize function_count;

	// Total number of instructions
	isize inst_count;
} ir_program;

typedef struct {
	u32 *locals;
	arena *arena;
	ir_inst *func_insts;
	ir_program *program;
	semantic_info *info;

	isize func_inst_count;
	isize max_inst_count;
	isize label_count;
	isize reg_count;

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
	case IR_I8:   return 1;
	case IR_I16:  return 2;
	case IR_I32:  return 4;
	case IR_I64:  return 8;
	case IR_F32:  return 4;
	case IR_F64:  return 8;
	case IR_VOID: return 0;
	}

	return 0;
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
	case TYPE_BITFIELD:
		// TODO: Implement bit fields properly
		return IR_I64;
	default:
		ASSERT(!"Invalid type");
	}

	return IR_VOID;
}

static char *
get_ir_opcode_str(ir_opcode opcode)
{
	switch (opcode) {
	case IR_NOP:
		return "nop";
	case IR_ALLOC:
		return "alloc";
	case IR_BUILTIN:
		return "builtin";
	case IR_CONST:
		return "const";
	case IR_GLOBAL:
		return "global";
	case IR_LABEL:
		return "label";
	case IR_PARAM:
		return "param";
	case IR_VAR:
		return "var";
	case IR_COPY:
		return "copy";
	case IR_LOAD:
		return "load";
	case IR_MOV:
		return "mov";
	case IR_STORE:
		return "store";
	case IR_CALL:
		return "call";
	case IR_JIZ:
		return "jiz";
	case IR_JMP:
		return "jmp";
	case IR_JNZ:
		return "jnz";
	case IR_ADD:
		return "add";
	case IR_AND:
		return "and";
	case IR_CAST:
		return "cast";
	case IR_CASTU:
		return "castu";
	case IR_DIV:
		return "div";
	case IR_EQL:
		return "eql";
	case IR_GEQ:
		return "geq";
	case IR_GEQU:
		return "gequ";
	case IR_GT:
		return "gt";
	case IR_GTU:
		return "gtu";
	case IR_LEQ:
		return "leq";
	case IR_LEQU:
		return "lequ";
	case IR_LT:
		return "lt";
	case IR_LTU:
		return "ltu";
	case IR_MOD:
		return "mod";
	case IR_MUL:
		return "mul";
	case IR_NOT:
		return "not";
	case IR_OR:
		return "or";
	case IR_RET:
		return "ret";
	case IR_SEXT:
		return "sext";
	case IR_SHL:
		return "shl";
	case IR_SHR:
		return "shr";
	case IR_SUB:
		return "sub";
	case IR_TRUNC:
		return "trunc";
	case IR_XOR:
		return "xor";
	case IR_ZEXT:
		return "zext";
	default:
		ASSERT(!"Invalid opcode");
		return "(invalid)";
	}
}
