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

	// data movement operators
	IR_COPY,
	IR_LOAD,
	IR_MOV,
	IR_STORE,

	// control-flow operators
	IR_CALL,
	IR_JIZ,
	IR_JMP,
	IR_JNZ,
	IR_RET,

	// integer operators
	IR_ADD,
	IR_DIV,
	IR_MOD,
	IR_MUL,
	IR_SUB,

	// comparison operators
	IR_EQ,
	IR_GT,
	IR_GE,
	IR_LT,
	IR_LE,
	IR_GTU,
	IR_GEU,
	IR_LTU,
	IR_LEU,

	// bitwise operators
	IR_AND,
	IR_NOT,
	IR_OR,
	IR_SHL,
	IR_SHR,
	IR_XOR,

	// conversion operators
	IR_CVT,
	IR_SEXT,
	IR_TRUNC,
	IR_ZEXT,

	// float operations
	IR_FVAR,
	IR_FMOV,
	IR_FSTORE,
	IR_FLOAD,
	IR_FCOPY,

	IR_FADD,
	IR_FSUB,
	IR_FMUL,
	IR_FDIV,

	IR_FEQ,
	IR_FGT,
	IR_FGE,
	IR_FLT,
	IR_FLE,
	IR_FRET,
	IR_FCVT,
} ir_opcode;

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

typedef enum {
	LINK_DEFAULT,
	LINK_EXTERN,
	LINK_STATIC,
} linkage;

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
	ir_opcode opcode;
	u8 size;
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
	case IR_EQ:
	case IR_LT:
	case IR_GT:
	case IR_LE:
	case IR_GE:
	case IR_LTU:
	case IR_GTU:
	case IR_LEU:
	case IR_GEU:
		return true;
	default:
		return false;
	}
}

static char *
get_ir_opcode_str(ir_opcode opcode)
{
	switch (opcode) {
	case IR_NOP:
		return "nop";

	// declarations
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

	// data movement operators
	case IR_COPY:
		return "copy";
	case IR_LOAD:
		return "load";
	case IR_MOV:
		return "mov";
	case IR_STORE:
		return "store";

	// control-flow operators
	case IR_CALL:
		return "call";
	case IR_JIZ:
		return "jiz";
	case IR_JMP:
		return "jmp";
	case IR_JNZ:
		return "jnz";
	case IR_RET:
		return "ret";

	// integer operators
	case IR_ADD:
		return "add";
	case IR_DIV:
		return "div";
	case IR_MOD:
		return "mod";
	case IR_MUL:
		return "mul";
	case IR_SUB:
		return "sub";

	// comparison operators
	case IR_EQ:
		return "eq";
	case IR_GT:
		return "gt";
	case IR_GE:
		return "ge";
	case IR_LT:
		return "lt";
	case IR_LE:
		return "le";
	case IR_GTU:
		return "gtu";
	case IR_GEU:
		return "geu";
	case IR_LTU:
		return "ltu";
	case IR_LEU:
		return "leu";

	// bitwise operators
	case IR_AND:
		return "and";
	case IR_NOT:
		return "not";
	case IR_OR:
		return "or";
	case IR_SHL:
		return "shl";
	case IR_SHR:
		return "shr";
	case IR_XOR:
		return "xor";

	// conversion operators
	case IR_CVT:
		return "cvt";
	case IR_SEXT:
		return "sext";
	case IR_TRUNC:
		return "trunc";
	case IR_ZEXT:
		return "zext";

	// float operations
	case IR_FVAR:
		return "fvar";
	case IR_FMOV:
		return "fmov";
	case IR_FSTORE:
		return "fstore";
	case IR_FLOAD:
		return "fload";
	case IR_FCOPY:
		return "fcopy";

	case IR_FADD:
		return "fadd";
	case IR_FSUB:
		return "fsub";
	case IR_FMUL:
		return "fmul";
	case IR_FDIV:
		return "fdiv";

	case IR_FEQ:
		return "feq";
	case IR_FGT:
		return "fgt";
	case IR_FGE:
		return "fge";
	case IR_FLT:
		return "flt";
	case IR_FLE:
		return "fle";
	case IR_FRET:
		return "fret";
	case IR_FCVT:
		return "fcvt";
	}

	return "(invalid)";
}

static ir_opcode_info
get_opcode_info(ir_opcode opcode)
{
	ir_opcode_info info = {0};
	switch (opcode) {
	case IR_JMP:
		info.op0 = IR_OPERAND_LABEL;
		break;
	case IR_RET:
	case IR_LOAD:
	case IR_COPY:
	case IR_TRUNC:
	case IR_SEXT:
	case IR_ZEXT:
	case IR_NOT:
	case IR_CVT:
	case IR_FCVT:
	case IR_FCOPY:
	case IR_FLOAD:
	case IR_FRET:
		info.op0 = IR_OPERAND_REG_SRC;
		break;
	case IR_MOV:
	case IR_FMOV:
	case IR_STORE:
	case IR_FSTORE:
		info.op0 = IR_OPERAND_REG_DST;
		info.op1 = IR_OPERAND_REG_SRC;
		break;
	case IR_ADD:
	case IR_AND:
	case IR_SUB:
	case IR_MUL:
	case IR_DIV:
	case IR_MOD:
	case IR_EQ:
	case IR_LT:
	case IR_GT:
	case IR_LE:
	case IR_GE:
	case IR_LTU:
	case IR_GTU:
	case IR_LEU:
	case IR_GEU:
	case IR_OR:
	case IR_SHL:
	case IR_SHR:
	case IR_XOR:
	case IR_FADD:
	case IR_FSUB:
	case IR_FMUL:
	case IR_FDIV:
	case IR_FEQ:
	case IR_FLT:
	case IR_FGT:
	case IR_FLE:
	case IR_FGE:
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
	case IR_FVAR:
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
