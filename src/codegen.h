typedef enum {
	MACH_INVALID,
	MACH_INST,
	MACH_MREG,
	MACH_VREG,
	MACH_SPILL,
	MACH_LABEL,
	MACH_FLOAT,
	MACH_FUNC,
	MACH_GLOBAL,
	MACH_CONST,
} mach_operand_kind;

typedef enum {
	MACH_USE      = (1 << 0),
	MACH_DEF      = (1 << 1),
	MACH_IMPLICIT = (1 << 2),
	MACH_INDIRECT = (1 << 3),
	MACH_ISFLOAT  = (1 << 4),
} mach_operand_flags;

typedef struct {
	u8 kind;
	u8 size;
	u16 flags;
	u32 value;
} mach_operand;

typedef struct {
	i32 inst_count;
	i32 stack_size;
} mach_function;

typedef struct {
	mach_operand *code;
	mach_function *funcs;
	u32 *tmp_mregs;

	isize inst_count;
	isize max_inst_count;
	u32 tmp_mreg_count;
	u32 int_mreg_count;
	u32 mreg_count;
	u32 func_count;
	u32 max_vreg_count;
	u32 max_label_count;
} mach_program;

typedef struct {
	b32 *used;
	u32 spill_count;
} regalloc_info;

static b32
equals_operand(mach_operand a, mach_operand b)
{
	b32 result = (a.kind == b.kind && a.value == b.value);
	return result;
}

static mach_operand
make_operand(u32 kind, u32 value, u32 size)
{
	mach_operand operand = {0};
	operand.kind = kind;
	operand.value = value;
	operand.size = size;
	ASSERT(size <= 16);
	return operand;
}

static mach_operand
make_spill(u32 index)
{
	mach_operand operand = make_operand(MACH_SPILL, index, 8);
	return operand;
}

static mach_operand
make_float(u32 index)
{
	mach_operand operand = make_operand(MACH_FLOAT, index, 4);
	return operand;
}

static mach_operand
make_label(u32 value)
{
	mach_operand operand = make_operand(MACH_LABEL, value, 0);
	return operand;
}

static mach_operand
make_func(u32 index)
{
	mach_operand operand = make_operand(MACH_FUNC, index, 8);
	return operand;
}

static mach_operand
make_global(u32 index)
{
	mach_operand operand = make_operand(MACH_GLOBAL, index, 8);
	return operand;
}

static void
push_operand(mach_program *p, mach_operand arg)
{
	ASSERT(arg.kind != MACH_INVALID);
	ASSERT(arg.kind != MACH_VREG || arg.value < p->max_vreg_count);
	p->code[p->inst_count++] = arg;
}

static void
push_inst(mach_program *p, u32 opcode, u32 operand_count)
{
	mach_operand operand = make_operand(MACH_INST, opcode, 0);
	(void)operand_count;
	push_operand(p, operand);
}
