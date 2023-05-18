#define ALLOC(arena, count, type) ((type *)alloc(arena, count, sizeof(type)))

struct arena {
	char *data;
	size_t size;
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
	void *result = arena->data + size * count;
	arena->pos += size * count;
	return result;
}


