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
	u32 *mreg_class;
	u32 *vreg_class;

	i32 pool_size;
	u32 vreg_count;
	u32 mreg_count;
	u32 tmp_mreg_count;
} mach_info;

typedef struct {
	b32 *used;
	u32 spill_count;
} regalloc_info;

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
