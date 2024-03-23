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
	MOP_FLOAT,
	MOP_FUNC,
	MOP_GLOBAL,
	MOP_IMMEDIATE,
} machine_operand_kind;

typedef enum {
	MOP_USE      = (1 << 0),
	MOP_DEF      = (1 << 1),
	MOP_IMPLICIT = (1 << 2),
	MOP_INDIRECT = (1 << 3),
	MOP_ISFLOAT  = (1 << 4),
} machine_operand_flags;

typedef struct {
	uint8_t kind;
	uint8_t size;
	uint16_t flags;
	u32 value;
} machine_operand;

typedef struct {
	str name;
	u32 instr_count;
	u32 register_count;
	u32 stack_size;
	u32 *instr_offsets;
	i32 *floats;
	i32 float_count;
} machine_function;

typedef struct {
	u32 *volatile_registers;
	u32 volatile_register_count;
	u32 int_register_count;
	u32 register_count;
} machine_register_info;

typedef struct {
	void *code;
	symbol_table *symtab;
	machine_function *functions;
	machine_register_info register_info;

	u32 size;
	u32 max_size;
	u32 vreg_count;
	u32 instr_count;
	u32 function_count;
} machine_program;

static b32
machine_operand_equals(machine_operand a, machine_operand b)
{
	b32 result = (a.kind == b.kind && a.value == b.value);
	return result;
}

static machine_operand
make_operand(u32 kind, u32 value, u32 size)
{
	machine_operand operand = {0};
	operand.kind = kind;
	operand.value = value;
	operand.size = size;
	ASSERT(size <= 16);
	return operand;
}

static machine_operand
make_spill(u32 index)
{
	machine_operand operand = make_operand(MOP_SPILL, index, 8);
	return operand;
}

static machine_operand
make_immediate(u32 value, u32 size)
{
	machine_operand operand = make_operand(MOP_IMMEDIATE, value, size);
	return operand;
}

static machine_operand
make_float(u32 index)
{
	machine_operand operand = make_operand(MOP_FLOAT, index, 4);
	return operand;
}

static machine_operand
make_label(u32 value)
{
	machine_operand operand = make_operand(MOP_LABEL, value, 0);
	return operand;
}

static machine_operand
make_func(u32 index)
{
	machine_operand operand = make_operand(MOP_FUNC, index, 8);
	return operand;
}

static machine_operand
make_global(u32 index, u32 size)
{
	machine_operand operand = make_operand(MOP_GLOBAL, index, size);
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
get_instr(void *code, u32 *offsets, u32 index)
{
	machine_instr *instr = (machine_instr *)((char *)code + offsets[index]);
	return instr;
}
