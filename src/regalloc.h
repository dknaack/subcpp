typedef struct {
	b32 is_spilled;
	i32 value;
} machine_location;

typedef struct {
	i32 *int_registers;
	i32 *float_registers;
	i32 *volatile_registers;

	isize int_register_count;
	isize float_register_count;
	isize volatile_register_count;
	isize machine_register_count;
} machine_info;

typedef struct {
	i32 virtual_register;
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
