typedef enum {
	MACH_USE   = (1 << 0),
	MACH_DEF   = (1 << 1),
	// Indicates that this token is a call instruction, which clears any
	// registers which are not preserved across calls
	MACH_CALL  = (1 << 2),
	// The register allocator only allows to different register classes,
	// integers or floats. This flag indicates that the register is contained
	// in the latter class.
	MACH_FLOAT = (1 << 3),
	// We reuse the float flag to indicate when an instruction occurs,
	// since tokens with the float flag and no use or def flag don't really
	// make any sense, hence this flag.
	MACH_INST  = MACH_FLOAT,
} mach_token_flags;

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
	u32 value;
} mach_token;

typedef struct {
	i32 succ[2];
	i32 offset;
	i32 size;
} basic_block;

typedef struct {
	u32 *pool;
	u32 *tmp_mregs;

	i32 pool_size;
	u32 vreg_count;
	u32 mreg_count;
	u32 tmp_mreg_count;
} mach_info;

typedef struct mach_mov mach_mov;
struct mach_mov {
	mach_mov *next;
	i32 inserted_before;
	u32 dst, src;
};

typedef struct {
	mach_mov *movs;    // dst is reg, src is reg
	mach_mov *spills;  // dst is mem, src is reg
	mach_mov *reloads; // dst is reg, src is mem
	u32 spill_count;
	b32 error;
} regalloc_result;

typedef struct {
	u32 vreg;
	u32 start;
	u32 end;
} live_interval;

typedef struct {
	b32 *bits;
	i32 size;
} bitset;

#define make_spill(value) make_mach_token(value, 8)
#define make_label(value) make_mach_token(value, 0)
#define make_global(value) make_mach_token(value, 8)
#define make_const(value, size) make_mach_token(value, size)

static b32
equals_token(mach_token a, mach_token b)
{
	b32 result = (a.flags == b.flags && a.value == b.value);
	return result;
}

static mach_token
make_mach_token(u32 value, u32 size)
{
	mach_token token = {0};
	token.value = value;
	token.hint = size;
	ASSERT(size <= 16);
	return token;
}

static mach_token
register_token(u32 value, b32 is_float)
{
	mach_token token = {0};
	token.flags = is_float ? MACH_FLOAT : 0;
	token.value = value;
	return token;
}

static mach_token
inst_token(u8 opcode, u32 format)
{
	mach_token token = {0};
	token.flags = MACH_INST;
	token.hint  = opcode;
	token.value = format;
	return token;
}
static b32
is_inst_token(mach_token token)
{
	b32 result = ((token.flags & (MACH_USE | MACH_DEF | MACH_INST)) == MACH_INST);
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
	set.bits[i / 32] &= ~(1 << (i % 32));
	set.bits[i / 32] |= (value != 0) << (i % 32);
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
