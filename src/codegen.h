typedef enum {
	MACH_USE      = (1 << 0),
	MACH_DEF      = (1 << 1),
	MACH_CALL     = (1 << 2),
	MACH_OPCODE   = (1 << 3),
} mach_token_flags;

typedef struct {
	mach_token_flags flags;
	u8 size;
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
	token.size = size;
	ASSERT(size <= 16);
	return token;
}

static mach_token
make_spill(u32 index)
{
	mach_token token = make_mach_token(index, 8);
	return token;
}

static mach_token
make_label(u32 value)
{
	mach_token token = make_mach_token(value, 0);
	return token;
}

static mach_token
make_global(u32 index)
{
	mach_token token = make_mach_token(index, 8);
	return token;
}

static mach_token
make_const(u32 value, u32 size)
{
	mach_token token = make_mach_token(value, size);
	return token;
}
