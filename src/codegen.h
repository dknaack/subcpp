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
