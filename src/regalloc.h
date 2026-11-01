typedef enum {
	MACHINE_USE   = (1 << 0),
	MACHINE_DEF   = (1 << 1),
	// Indicates that this token is a call instruction, which clears any
	// registers which are not preserved across calls.
	// One important limitation with this design is that call instructions
	// cannot really have a return value, unless the backend implements special
	// parsing of the token stream.
	MACHINE_CALL  = (1 << 2),
	// The register allocator only allows to different register classes,
	// integers or floats. This flag indicates that the register is contained
	// in the latter class.
	MACHINE_FLOAT = (1 << 3),
	// We reuse the float flag to indicate when an instruction occurs,
	// since tokens with the float flag and no use or def flag don't really
	// make any sense, hence this additional alias.
	MACHINE_INST  = MACHINE_FLOAT,
} machine_token_flags;

typedef struct {
	// A token is considered a virtual register if it has a USE or DEF flag.
	// Otherwise, it is completely ingored by the register allocator.
	u8 flags;
	// Specifies the machine register to allocate to for virtual registers.
	// A virtual register is allowed to have multiple hints to different
	// machine registers. In this case, the allocator inserts move instructions.
	//
	// Other tokens can use this field however they want. For instruction
	// opcodes, the hint can store the opcode, while the value stores the
	// format, i.e. what the next tokens mean.
	u8 hint;
	i32 value;
} machine_token;

typedef struct {
	b32 is_stack;
	i32 value;
} machine_location;

typedef struct {
	i32 *int_registers;
	i32 *float_registers;
	i32 *volatile_registers;

	isize virtual_register_count;
	isize machine_register_count;
	isize int_register_count;
	isize float_register_count;
	isize volatile_register_count;
} machine_info;

typedef struct {
	i32 spill_count;
	b32 error;
} regalloc_result;

typedef struct {
	i32 reg;
	i32 start;
	i32 end;
} live_range;

typedef struct {
	b32 *bits;
	i32 size;
} bitset;

#define make_spill(value) make_machine_token(value, 8)
#define make_label(value) make_machine_token(value, 0)
#define make_global(value) make_machine_token(value, 8)
#define make_const(value, size) make_machine_token(value, size)

static b32
equals_token(machine_token a, machine_token b)
{
	b32 result = (a.flags == b.flags && a.value == b.value);
	return result;
}

static machine_token
make_machine_token(u32 value, u32 size)
{
	machine_token token = {0};
	token.value = value;
	token.hint = size;
	ASSERT(size <= 16);
	return token;
}

static machine_token
register_token(u32 value, b32 is_float)
{
	machine_token token = {0};
	token.flags = is_float ? MACHINE_FLOAT : 0;
	token.value = value;
	return token;
}

static machine_token
inst_token(u8 opcode, u32 format)
{
	machine_token token = {0};
	token.flags = MACHINE_INST;
	token.hint  = opcode;
	token.value = format;
	return token;
}

static b32
is_inst_token(machine_token token)
{
	b32 result = ((token.flags & (MACHINE_USE | MACHINE_DEF | MACHINE_INST)) == MACHINE_INST);
	return result;
}

static bitset
new_bitset(i32 size, arena *perm)
{
	bitset result = {0};
	result.size = size;
	result.bits = ALLOC(perm, (size + 31) / 32, b32);
	return result;
}

static void
set_bit(bitset set, i32 i, b32 value)
{
	set.bits[i / 32] &= ~(1u << (u32)(i % 32));
	set.bits[i / 32] |= (u32)(value != 0) << (i % 32);
}

static b32
get_bit(bitset set, i32 i)
{
	b32 result = (set.bits[i / 32] >> (i % 32)) & 1;
	return result;
}

static bitset
clone_bitset(bitset src, arena *perm)
{
	bitset dst = new_bitset(src.size, perm);

	isize word_count = (src.size + 31) / 32;
	for (isize i = 0; i < word_count; i++) {
		dst.bits[i] = src.bits[i];
	}

	return dst;
}

static void
swap_u32(u32 *a, isize i, isize j)
{
	u32 tmp = a[i];
	a[i] = a[j];
	a[j] = tmp;
}
