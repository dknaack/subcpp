typedef enum {
	// No operation, used during optimization
	IR_NOP,

	// Phi node
	IR_PHI,

	// Allocates on the stack
	// - args[0]: size
	// - args[1]: offset
	IR_ALLOC,

	// Declares a builtin function
	// - args[0]: builtin function
	IR_BUILTIN,

	// Declares a constant
	// - args[0]: value
	IR_CONST,

	// Declares a global variable
	// - args[0]: global id
	IR_GLOBAL,

	// Declares a function
	// - args[0]: function id
	IR_FUNC,

	// Declares a label
	// - args[0]: label id
	IR_LABEL,

	// Declares a parameter
	// - args[0]: parameter index
	// - args[1]: parameter size
	IR_PARAM,

	// Copies a register, mainly used during optimization and later removed
	// - args[0]: source register
	IR_COPY,
	IR_FCOPY,

	// Loads a value from memory
	// - args[0]: source address
	IR_LOAD,
	IR_FLOAD,

	// Stores a value to memory
	// - args[0]: destination address
	// - args[1]: source register
	IR_STORE,
	IR_FSTORE,

	// Calls a function. Multiple parameters are reprsented as a linked list of
	// call instructions. The first call instruction has the called function as
	// its first operand and the following call instructions have the
	// parameters as their first operand.
	// - args[0]: called function or parameter
	// - args[1]: next call instruction
	IR_CALL,

	// Jumps to a label if the register is zero, otherwise continues.
	// - args[0]: register to test
	// - args[1]: label to jump to
	IR_JIZ,

	// Jumps to a label if the register is not zero, otherwise continues.
	// - args[0]: register to test
	// - args[1]: label to jump to
	IR_JNZ,

	// Jumps to a label
	// - args[0]: label to jump to
	IR_JMP,

	// Returns from a function, either an integer or a float (or void).
	// - args[0]: optional return value
	IR_RET,
	IR_FRET,

	// Converts to an integer or a float, respectively.
	// - args[0]: source register
	IR_F2I,
	IR_I2F,

	// Sign extends or zero extends an integer using the size field.
	// - args[0]: source register
	IR_SEXT,

	// Truncates an integer to the size specified in the respective field.
	// - args[0]: source register
	IR_TRUNC,

	// Zero extends an integer to the size specified in the respective field.
	// - args[0]: source register
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

typedef enum {
	INST_DEF  = 1 << 0,
	INST_USE0 = 1 << 1,
	INST_USE1 = 1 << 2,
	INST_CONT = 1 << 3,
} inst_flags;

// The IR instructions are graph-based and are stored in a flat array. The
// operands are stored as indices into the instruction array. The result of an
// instruction is stored in a register, which is also an index into the
// instruction array. Hence, these registers can only be assigned once.
typedef struct {
	i32 opcode;
	i8 size;
	i8 flags;
	i8 hint;
	i32 args[2];
} inst;

typedef enum {
	BUILTIN_MEMCPY,
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
} global_id;

typedef enum {
	LINK_DEFAULT,
	LINK_EXTERN,
	LINK_STATIC,
	LINK_COUNT
} linkage;

typedef struct {
	str name;
	void *data;
	isize size;
	linkage linkage;
	section section;
} global;

typedef struct {
	i32 begin;
	i32 end;
	i32 succ[2];
	i32 *pred;
	i32 pred_count;
} block;

typedef struct {
	str name;
	inst *insts;
	block *blocks;
	linkage linkage;
	i32 inst_count;
	i32 block_count;
} function;

typedef struct {
	function *funcs;
	global *globals;

	isize func_count;
	isize global_count;
} program;

typedef struct {
	inst *insts;
	isize inst_count;
	isize max_inst_count;
	isize label_count;
} inst_buffer;

typedef struct {
	arena *arena;
	ast_pool *ast;
	i32 *symbol_ids;
	program *program;
	inst_buffer buffer;

	i32 stack_size;
	i32 continue_label;
	i32 break_label;
	i32 case_label;
} ir_context;

static i32 emit(inst_buffer *buf, i32 opcode, i8 hint, i8 size, i8 flags, i32 arg0, i32 arg1)
{
	if (buf->inst_count + 1 >= buf->max_inst_count) {
		if (buf->max_inst_count == 0) {
			buf->max_inst_count = 1024 * 1024;
		} else {
			buf->max_inst_count *= 2;
		}

		buf->insts = realloc(buf->insts, buf->max_inst_count * sizeof(*buf->insts));
	}

	i32 inst_index = buf->inst_count++;
	inst *inst = &buf->insts[inst_index];
	inst->opcode = opcode;
	inst->hint = hint;
	inst->size = size;
	inst->flags = flags;
	inst->args[0] = arg0;
	inst->args[1] = arg1;
	return inst_index;
}

static i32
new_label(inst_buffer *buf)
{
	i32 result = buf->label_count++;
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

static b32
is_float_opcode(ir_opcode opcode)
{
	switch (opcode) {
	case IR_FADD:
	case IR_FCOPY:
	case IR_FDIV:
	case IR_FEQ:
	case IR_FGE:
	case IR_FGT:
	case IR_FLE:
	case IR_FLOAD:
	case IR_FLT:
	case IR_FMUL:
	case IR_FRET:
	case IR_FSTORE:
	case IR_FSUB:
	case IR_I2F:
		return true;
	default:
		return false;
	}
}
