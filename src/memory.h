#define ALLOC_NOZERO(arena, count, type) ((type *)alloc_nozero(arena, count, sizeof(type)))
#define ALLOC(arena, count, type) ((type *)alloc(arena, count, sizeof(type)))

typedef struct {
	char *data;
	usize size;
	usize pos;
} arena;

typedef struct {
	arena *arena;
	usize pos;
} arena_temp;

static arena *
new_arena(usize size)
{
	arena *a = (arena *)calloc(size + sizeof(arena), 1);
	a->data = (char *)(a + 1);
	a->size = size;
	return a;
}

static void *
alloc_nozero(arena *arena, usize count, usize size)
{
	arena->pos = (arena->pos + 7) & -8;
	ASSERT(arena->pos + size * count < arena->size);
	void *result = arena->data + arena->pos;
	arena->pos += size * count;
	return result;
}

static void *
alloc(arena *arena, usize count, usize size)
{
	char *byte = (char *)alloc_nozero(arena, count, size);
	for (usize i = 0; i < count * size; i++) {
		byte[i] = 0;
	}

	return byte;
}

static arena_temp
arena_temp_begin(arena *arena)
{
	arena_temp temp;
	temp.arena = arena;
	temp.pos = arena->pos;
	return temp;
}

static void
arena_temp_end(arena_temp temp)
{
	temp.arena->pos = temp.pos;
}
