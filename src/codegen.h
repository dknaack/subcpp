typedef struct {
	u32 opcode:24;
	u32 operand_count:8;
} machine_instr;

typedef enum {
	MOP_INVALID,
	MOP_MREG,
	MOP_VREG,
	MOP_SPILL,
	MOP_LABEL,
	MOP_FUNC,
	MOP_IMMEDIATE,
} machine_operand_kind;

typedef enum {
	MOP_USE        = (1 << 0),
	MOP_DEF        = (1 << 1),
} machine_operand_flags;

typedef struct {
	uint8_t kind;
	uint8_t size;
	uint16_t flags;
	u32 value;
} machine_operand;

typedef struct {
	u32 instr_index;
} machine_block;

typedef struct {
	str name;
	u32 block_index;
	u32 instr_index;
	u32 stack_size;
} machine_function;

typedef struct {
	void *code;
	u32 *instr_offsets;
	machine_block *blocks;
	machine_function *functions;
	u32 *temp_mregs;

	u32 size;
	u32 max_size;
	u32 mreg_count;
	u32 vreg_count;
	u32 instr_count;
	u32 block_count;
	u32 function_count;
	u32 temp_mreg_count;
} machine_program;

static b32
machine_operand_equals(machine_operand a, machine_operand b)
{
	b32 result = (a.kind == b.kind && a.value == b.value);
	return result;
}

static machine_operand
make_mreg(u32 mreg)
{
	machine_operand operand = {0};
	operand.kind = MOP_MREG;
	operand.value = mreg;
	return operand;
}

static machine_operand
make_vreg(u32 vreg)
{
	machine_operand operand = {0};
	operand.kind = MOP_VREG;
	operand.value = vreg;
	operand.size = 8;
	return operand;
}

static machine_operand
make_spill(u32 index)
{
	machine_operand operand = {0};
	operand.kind = MOP_SPILL;
	operand.value = index;
	operand.size = 8;
	return operand;
}

static machine_operand
make_immediate(u32 value)
{
	machine_operand operand = {0};
	operand.kind = MOP_IMMEDIATE;
	operand.value = value;
	return operand;
}

static machine_operand
make_label(u32 value)
{
	machine_operand operand = {0};
	operand.kind = MOP_LABEL;
	operand.value = value;
	return operand;
}

static machine_operand
make_func(u32 index)
{
	machine_operand operand = {0};
	operand.kind = MOP_FUNC;
	operand.value = index;
	return operand;
}

static u32
get_instr_size(machine_instr instr)
{
	u32 size = sizeof(instr);
	size += instr.operand_count * sizeof(machine_operand);
	return size;
}

static machine_instr *
get_instr(machine_program program, u32 instr_index)
{
	u32 offset = program.instr_offsets[instr_index];
	char *ptr = (char *)program.code;
	machine_instr *instr = (machine_instr *)(ptr + offset);
	return instr;
}
