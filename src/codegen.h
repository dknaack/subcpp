typedef struct {
	uint32_t opcode:24;
	uint32_t operand_count:8;
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
	MOP_FORCE_MREG = (1 << 2),
} machine_operand_flags;

typedef struct {
	uint8_t kind;
	uint8_t size;
	uint16_t flags;
	uint32_t value;
} machine_operand;

typedef struct {
	uint32_t instr_index;
} machine_block;

typedef struct {
	string name;
	uint32_t block_index;
	uint32_t instr_index;
} machine_function;

typedef struct {
	void *code;
	uint32_t *instr_offsets;
	machine_block *blocks;
	machine_function *functions;
	uint32_t *temp_mregs;

	uint32_t size;
	uint32_t max_size;
	uint32_t mreg_count;
	uint32_t vreg_count;
	uint32_t instr_count;
	uint32_t block_count;
	uint32_t function_count;
	uint32_t temp_mreg_count;
} machine_program;

static bool
machine_operand_equals(machine_operand a, machine_operand b)
{
	bool result = (a.kind == b.kind && a.value == b.value);
	return result;
}

static machine_operand
make_mreg(uint32_t mreg)
{
	machine_operand operand = {0};
	operand.kind = MOP_MREG;
	operand.value = mreg;
	return operand;
}

static machine_operand
make_vreg(uint32_t vreg)
{
	machine_operand operand = {0};
	operand.kind = MOP_VREG;
	operand.value = vreg;
	operand.size = 8;
	return operand;
}

static machine_operand
make_spill(uint32_t index)
{
	machine_operand operand = {0};
	operand.kind = MOP_SPILL;
	operand.value = index;
	operand.size = 8;
	return operand;
}

static machine_operand
make_immediate(uint32_t value)
{
	machine_operand operand = {0};
	operand.kind = MOP_IMMEDIATE;
	operand.value = value;
	return operand;
}

static machine_operand
make_label(uint32_t value)
{
	machine_operand operand = {0};
	operand.kind = MOP_LABEL;
	operand.value = value;
	return operand;
}

static machine_operand
make_func(uint32_t index)
{
	machine_operand operand = {0};
	operand.kind = MOP_FUNC;
	operand.value = index;
	return operand;
}

static void
push_instr(machine_program *program,
    uint32_t opcode, uint32_t operand_count)
{
	machine_instr instr = {0};
	instr.opcode = opcode;
	instr.operand_count = operand_count;
	ASSERT(program->size + sizeof(instr) + operand_count
	    * sizeof(machine_operand) <= program->max_size);
	memcpy((char *)program->code + program->size, &instr, sizeof(instr));
	program->size += sizeof(instr);
}

static void
push_operand(machine_program *program, machine_operand operand)
{
	memcpy((char *)program->code + program->size, &operand, sizeof(operand));
	program->size += sizeof(operand);
}

static uint32_t
get_instr_size(machine_instr instr)
{
	uint32_t size = sizeof(instr);
	size += instr.operand_count * sizeof(machine_operand);
	return size;
}

static machine_instr *
get_instr(machine_program program, uint32_t instr_index)
{
	uint32_t offset = program.instr_offsets[instr_index];
	char *ptr = (char *)program.code;
	machine_instr *instr = (machine_instr *)(ptr + offset);
	return instr;
}
