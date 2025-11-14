typedef struct {
	b32 is_stack;
	i32 value;
} mach_location;

typedef struct {
	i32 *int_mregs;
	i32 *float_mregs;
	i32 *tmp_mregs;

	isize vreg_count;
	isize mreg_count;
	isize int_mreg_count;
	isize float_mreg_count;
	isize tmp_mreg_count;
} mach_info;

typedef struct {
	i32 spill_count;
	b32 error;
} regalloc_result;

typedef struct {
	i32 vreg;
	i32 start;
	i32 end;
} live_range;

typedef struct {
	b32 *bits;
	i32 size;
} bitset;

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
