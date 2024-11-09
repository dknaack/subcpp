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

typedef enum {
	SECTION_READ  = 1 << 0,
	SECTION_WRITE = 1 << 1,
	SECTION_EXEC  = 1 << 2,
	SECTION_ZERO  = 1 << 3,
	SECTION_COUNT = 1 << 4,

	SECTION_TEXT   = SECTION_READ | SECTION_EXEC,
	SECTION_DATA   = SECTION_READ | SECTION_WRITE,
	SECTION_RODATA = SECTION_READ,
	SECTION_BSS    = SECTION_READ | SECTION_WRITE | SECTION_ZERO,
} section;

typedef struct {
	i32 value;
} symbol_id;

typedef struct {
	symbol_id next;
	linkage linkage;
	str name;
	void *data;
	isize size;
} symbol;

typedef struct {
	symbol *symbols;
	isize symbol_count;
	isize max_symbol_count;
	symbol_id section[SECTION_COUNT];
} symbol_table;

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
	symbol_id sym_id;
	i32 param_count;
	i32 inst_index;
	i32 inst_count;
};

typedef struct {
	ir_inst *insts;
	ir_function *funcs;
	symbol_table symtab;

	isize max_reg_count; // Maximum number of registers in each function
	isize max_label_count; // Maximum number of labels in each function
	isize inst_count; // Total number of instructions
	isize func_count; // Total number of functions
} ir_program;

typedef struct {
	arena *arena;
	u32 *node_addr;
	ir_inst *func_insts;
	ir_program *program;
	semantic_info *info;
	symbol_id *section_tail[SECTION_COUNT];

	isize func_inst_count;
	isize max_inst_count;
	isize label_count;
	isize reg_count;

	u32 stack_size;
	u32 continue_label;
	u32 break_label;
	u32 case_label;
} ir_context;

static symbol *
new_symbol(ir_context *ctx, section section)
{
	symbol_table *symtab = &ctx->program->symtab;
	ASSERT(symtab->symbol_count < symtab->max_symbol_count);

	symbol_id sym_id = {symtab->symbol_count++};
	*ctx->section_tail[section] = sym_id;

	symbol *sym = &symtab->symbols[sym_id.value];
	ctx->section_tail[section] = &sym->next;
	return sym;
}

static symbol_id
get_symbol_id(symbol_table *symtab, symbol *sym)
{
	symbol_id result;
	result.value = sym - symtab->symbols;
	return result;
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

static ir_opcode_info
get_opcode_info(ir_opcode opcode)
{
	ir_opcode_info info = {0};
	switch (opcode) {
	case IR_JMP:
		info.op0 = IR_OPERAND_LABEL;
		break;
	case IR_CAST:
	case IR_CASTU:
	case IR_RET:
	case IR_LOAD:
	case IR_COPY:
	case IR_TRUNC:
	case IR_SEXT:
	case IR_ZEXT:
	case IR_NOT:
		info.op0 = IR_OPERAND_REG_SRC;
		break;
	case IR_MOV:
	case IR_STORE:
		info.op0 = IR_OPERAND_REG_DST;
		info.op1 = IR_OPERAND_REG_SRC;
		break;
	case IR_ADD:
	case IR_AND:
	case IR_SUB:
	case IR_MUL:
	case IR_DIV:
	case IR_MOD:
	case IR_EQL:
	case IR_LT:
	case IR_GT:
	case IR_LEQ:
	case IR_GEQ:
	case IR_LTU:
	case IR_GTU:
	case IR_LEQU:
	case IR_GEQU:
	case IR_OR:
	case IR_SHL:
	case IR_SHR:
	case IR_XOR:
		info.op0 = IR_OPERAND_REG_SRC;
		info.op1 = IR_OPERAND_REG_SRC;
		break;
	case IR_JIZ:
	case IR_JNZ:
		info.op0 = IR_OPERAND_REG_SRC;
		info.op1 = IR_OPERAND_LABEL;
		break;
	case IR_ALLOC:
		info.op0 = IR_OPERAND_CONST;
		info.op1 = IR_OPERAND_CONST;
		break;
	case IR_CONST:
	case IR_BUILTIN:
		info.op0 = IR_OPERAND_CONST;
		break;
	case IR_CALL:
		info.op0 = IR_OPERAND_REG_SRC;
		info.op1 = IR_OPERAND_REG_SRC;
		break;
	case IR_PARAM:
		info.op0 = IR_OPERAND_CONST;
		info.op1 = IR_OPERAND_CONST;
		break;
	case IR_GLOBAL:
		info.op0 = IR_OPERAND_GLOBAL;
		break;
	case IR_LABEL:
		info.op0 = IR_OPERAND_LABEL;
		break;
	case IR_NOP:
	case IR_VAR:
		break;
	}

	return info;
}

static b32
is_register_operand(ir_operand_type operand)
{
	b32 result = operand == IR_OPERAND_REG_SRC || operand == IR_OPERAND_REG_DST;
	return result;
}
