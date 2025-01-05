typedef enum {
	// No operation, used during optimization
	IR_NOP,

	// Allocates on the stack
	// - op0: size
	// - op1: offset
	IR_ALLOC,

	// Declares a builtin function
	// - op0: builtin function
	IR_BUILTIN,

	// Declares a constant
	// - op0: value
	IR_CONST,

	// Declares a global variable
	// - op0: symbol id
	IR_GLOBAL,

	// Declares a label
	// - op0: label id
	IR_LABEL,

	// Declares a parameter
	// - op0: parameter index
	// - op1: parameter size
	IR_PARAM,

	// Declares a variable register. Takes no operands. The size field is used
	// to store the size of the variable.
	IR_VAR,
	IR_FVAR,

	// Copies a register, mainly used during optimization and later removed
	// - op0: source register
	IR_COPY,
	IR_FCOPY,

	// Loads a value from memory
	// - op0: source address
	IR_LOAD,
	IR_FLOAD,

	// Moves a value between registers
	// - op0: destination register
	// - op1: source register
	IR_MOV,
	IR_FMOV,

	// Stores a value to memory
	// - op0: destination address
	// - op1: source register
	IR_STORE,
	IR_FSTORE,

	// Calls a function. Multiple parameters are reprsented as a linked list of
	// call instructions. The first call instruction has the called function as
	// its first operand and the following call instructions have the
	// parameters as their first operand.
	// - op0: called function or parameter
	// - op1: next call instruction
	IR_CALL,

	// Jumps to a label if the register is zero, otherwise continues.
	// - op0: register to test
	// - op1: label to jump to
	IR_JIZ,

	// Jumps to a label if the register is not zero, otherwise continues.
	// - op0: register to test
	// - op1: label to jump to
	IR_JNZ,

	// Jumps to a label
	// - op0: label to jump to
	IR_JMP,

	// Returns from a function, either an integer or a float (or void).
	// - op0: optional return value
	IR_RET,
	IR_FRET,

	// Converts to an integer or a float, respectively.
	// - op0: source register
	IR_CVT,
	IR_FCVT,

	// Sign extends or zero extends an integer using the size field.
	// - op0: source register
	IR_SEXT,

	// Truncates an integer to the size specified in the respective field.
	// - op0: source register
	IR_TRUNC,

	// Zero extends an integer to the size specified in the respective field.
	// - op0: source register
	IR_ZEXT,

	// The following instructions all have two operands and return a value.

	// integer operators
	IR_ADD,
	IR_SUB,
	IR_MUL,
	IR_DIV,
	IR_MOD,

	// comparison operators, where unsigned comparison is suffixed with 'U'
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

	// float operators
	IR_FADD,
	IR_FSUB,
	IR_FMUL,
	IR_FDIV,

	// float comparison operators
	IR_FEQ,
	IR_FGT,
	IR_FGE,
	IR_FLT,
	IR_FLE,
} ir_opcode;

// The IR instructions are graph-based and are stored in a flat array. The
// operands are stored as indices into the instruction array. The result of an
// instruction is stored in a register, which is also an index into the
// instruction array. Hence, these registers can only be assigned once.
typedef struct {
	ir_opcode opcode;
	u8 size;
	u32 op0;
	u32 op1;
} ir_inst;

typedef enum {
	BUILTIN_POPCOUNT,
	BUILTIN_VA_ARG,
	BUILTIN_VA_END,
	BUILTIN_VA_LIST,
	BUILTIN_VA_START,
} ir_builtin;

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

// Additional information about the types of operands for each opcode.
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
	ast_pool *ast;
	u32 *node_addr;
	ir_inst *func_insts;
	ir_program *program;
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
