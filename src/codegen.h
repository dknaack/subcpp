typedef struct {
	u32 opcode:24;
	u32 operand_count:8;
} mach_inst;

typedef enum {
	MOP_INVALID,
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
	uint8_t kind;
	uint8_t size;
	uint16_t flags;
	u32 value;
} mach_operand;

typedef struct {
	u32 inst_count;
	u32 vreg_count;
	u32 label_count;
	u32 stack_size;
	u32 *inst_offsets;
} mach_function;

typedef struct {
	u32 *tmp_mregs;
	u32 tmp_mreg_count;
	u32 int_mreg_count;
	u32 mreg_count;
} mach_register_info;

typedef struct {
	void *code;
	mach_function *funcs;
	mach_register_info mreg_info;

	u32 size;
	u32 max_size;
	u32 vreg_count;
	u32 inst_count;
	u32 func_count;
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

static mach_inst *
get_inst(void *code, u32 *offsets, u32 index)
{
	mach_inst *inst = (mach_inst *)((char *)code + offsets[index]);
	return inst;
}

static void
push_inst(mach_program *program, u32 opcode, u32 operand_count)
{
	mach_inst inst = {0};
	inst.opcode = opcode;
	inst.operand_count = operand_count;
	ASSERT(program->size + sizeof(inst) + operand_count
		* sizeof(mach_operand) <= program->max_size);
	memcpy((char *)program->code + program->size, &inst, sizeof(inst));
	program->size += sizeof(inst);
	program->inst_count++;
}

static void
push_operand(mach_program *program, mach_operand operand)
{
	mach_function *func = &program->funcs[program->func_count - 1];
	ASSERT(operand.kind != MOP_VREG || operand.value < func->vreg_count);
	memcpy((char *)program->code + program->size, &operand, sizeof(operand));
	program->size += sizeof(operand);
}
