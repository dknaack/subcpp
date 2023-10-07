struct machine_instr {
	uint32_t opcode:24;
	uint32_t operand_count:8;
};

enum machine_operand_kind {
	MOP_INVALID,
	MOP_MREG,
	MOP_VREG,
	MOP_SPILL,
	MOP_LABEL,
	MOP_FUNC,
	MOP_IMMEDIATE,
};

enum machine_operand_flags {
	MOP_USE        = (1 << 0),
	MOP_DEF        = (1 << 1),
	MOP_FORCE_MREG = (1 << 2),
};

struct machine_operand {
	uint16_t kind;
	uint16_t flags;
	uint32_t value;
};

struct machine_block {
	uint32_t instr_index;
};

struct machine_function {
	struct string name;
	uint32_t block_index;
	uint32_t instr_index;
};

struct machine_program {
	void *code;
	uint32_t *instr_offsets;
	struct machine_block *blocks;
	struct machine_function *functions;

	uint32_t size;
	uint32_t max_size;
	uint32_t mreg_count;
	uint32_t vreg_count;
	uint32_t instr_count;
	uint32_t block_count;
	uint32_t function_count;
};

static bool
machine_operand_equals(struct machine_operand a, struct machine_operand b)
{
	bool result = (a.kind == b.kind && a.value == b.value);
	return result;
}

static struct machine_operand
make_mreg(uint32_t mreg)
{
	struct machine_operand operand = {0};
	operand.kind = MOP_MREG;
	operand.value = mreg;
	return operand;
}

static struct machine_operand
make_vreg(uint32_t vreg)
{
	struct machine_operand operand = {0};
	operand.kind = MOP_VREG;
	operand.value = vreg;
	return operand;
}

static struct machine_operand
make_spill(uint32_t index)
{
	struct machine_operand operand = {0};
	operand.kind = MOP_SPILL;
	operand.value = index;
	return operand;
}

static struct machine_operand
make_immediate(uint32_t value)
{
	struct machine_operand operand = {0};
	operand.kind = MOP_IMMEDIATE;
	operand.value = value;
	return operand;
}

static struct machine_operand
make_label(uint32_t value)
{
	struct machine_operand operand = {0};
	operand.kind = MOP_LABEL;
	operand.value = value;
	return operand;
}

static struct machine_operand
make_func(uint32_t index)
{
	struct machine_operand operand = {0};
	operand.kind = MOP_FUNC;
	operand.value = index;
	return operand;
}

static void
push_instr(struct machine_program *program,
    uint32_t opcode, uint32_t operand_count)
{
	struct machine_instr instr = {0};
	instr.opcode = opcode;
	instr.operand_count = operand_count;
	ASSERT(program->size + sizeof(instr) + operand_count
	    * sizeof(struct machine_operand) <= program->max_size);
	memcpy((char *)program->code + program->size, &instr, sizeof(instr));
	program->size += sizeof(instr);
}

static void
push_operand(struct machine_program *program, struct machine_operand operand)
{
	memcpy((char *)program->code + program->size, &operand, sizeof(operand));
	program->size += sizeof(operand);
}

static uint32_t
get_instr_size(struct machine_instr instr)
{
	uint32_t size = sizeof(instr);
	size += instr.operand_count * sizeof(struct machine_operand);
	return size;
}

static struct machine_instr *
get_instr(struct machine_program program, uint32_t instr_index)
{
	uint32_t offset = program.instr_offsets[instr_index];
	char *ptr = (char *)program.code;
	struct machine_instr *instr = (struct machine_instr *)(ptr + offset);
	return instr;
}
