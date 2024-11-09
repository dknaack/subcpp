typedef enum {
	MOP_INVALID,
	MOP_INST,
	MOP_MREG,
	MOP_VREG,
	MOP_SPILL,
	MOP_LABEL,
	MOP_FLOAT,
	MOP_FUNC,
	MOP_GLOBAL,
	MOP_CONST,
} mach_operand_kind;

typedef enum {
	MOP_USE      = (1 << 0),
	MOP_DEF      = (1 << 1),
	MOP_IMPLICIT = (1 << 2),
	MOP_INDIRECT = (1 << 3),
	MOP_ISFLOAT  = (1 << 4),
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

	u32 size;
	u32 max_size;
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
	mach_operand operand = make_operand(MOP_SPILL, index, 8);
	return operand;
}

static mach_operand
make_float(u32 index)
{
	mach_operand operand = make_operand(MOP_FLOAT, index, 4);
	return operand;
}

static mach_operand
make_label(u32 value)
{
	mach_operand operand = make_operand(MOP_LABEL, value, 0);
	return operand;
}

static mach_operand
make_func(u32 index)
{
	mach_operand operand = make_operand(MOP_FUNC, index, 8);
	return operand;
}

static mach_operand
make_global(u32 index)
{
	mach_operand operand = make_operand(MOP_GLOBAL, index, 8);
	return operand;
}

static void
push_operand(mach_program *p, mach_operand arg)
{
	ASSERT(arg.kind != MOP_VREG || arg.value < p->max_vreg_count);
	memcpy((char *)p->code + p->size, &arg, sizeof(arg));
	p->size += sizeof(arg);
}

static void
push_inst(mach_program *p, u32 opcode, u32 operand_count)
{
	mach_operand operand = make_operand(MOP_INST, opcode, 0);
	(void)operand_count;
	push_operand(p, operand);
}
