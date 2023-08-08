#define ALLOC(arena, count, type) ((type *)alloc(arena, count, sizeof(type)))
#define ZALLOC(arena, count, type) ((type *)zalloc(arena, count, sizeof(type)))

struct arena {
	char *data;
	size_t size;
	size_t pos;
};

struct arena_temp {
	struct arena *arena;
	size_t pos;
};

static struct arena *
arena_create(size_t size)
{
	struct arena *arena = calloc(size + sizeof(*arena), 1);
	arena->data = (char *)(arena + 1);
	arena->size = size;
	return arena;
}

static void *
alloc(struct arena *arena, size_t count, size_t size)
{
	ASSERT(arena->pos + size * count < arena->size);
	void *result = arena->data + arena->pos;
	arena->pos += size * count;
	return result;
}

static void *
zalloc(struct arena *arena, size_t count, size_t size)
{
	char *byte = (char *)alloc(arena, count, size);
	for (size_t i = 0; i < count * size; i++) {
		byte[i] = 0;
	}

	return byte;
}

static struct arena_temp
arena_temp_begin(struct arena *arena)
{
	struct arena_temp temp;
	temp.arena = arena;
	temp.pos = arena->pos;
	return temp;
}

static void
arena_temp_end(struct arena_temp temp)
{
	temp.arena->pos = temp.pos;
}
