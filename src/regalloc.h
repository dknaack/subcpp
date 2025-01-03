typedef struct {
	u32 start;
	u32 end;
} live_interval;

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
	set.bits[i / 32] &= ~(1 << (i % 32));
	set.bits[i / 32] |= (value != 0) << (i % 32);
}

static b32
get_bit(bitset set, i32 i)
{
	b32 result = (set.bits[i / 32] >> (i % 32)) & 1;
	return result;
}

static void
swap_u32(u32 *a, isize i, isize j)
{
	u32 tmp = a[i];
	a[i] = a[j];
	a[j] = tmp;
}
