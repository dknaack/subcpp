typedef struct {
	u32 start;
	u32 end;
} live_interval;

typedef struct {
	b32 *bits;
	i32 size;
} bitset;
