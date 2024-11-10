typedef enum {
	MACH_INVALID,
	MACH_INST,
	MACH_MREG,
	MACH_VREG,
	MACH_SPILL,
	MACH_LABEL,
	MACH_FUNC,
	MACH_GLOBAL,
	MACH_CONST,
} mach_token_kind;

typedef enum {
	MACH_USE      = (1 << 0),
	MACH_DEF      = (1 << 1),
	MACH_IMPLICIT = (1 << 2),
	MACH_INDIRECT = (1 << 3),
	MACH_FLOAT    = (1 << 4),
} mach_token_flags;

typedef struct {
	u8 kind;
	u8 size;
	u16 flags;
	u32 value;
} mach_token;

typedef struct {
	symbol_id sym_id;
	i32 token_count;
	i32 stack_size;
} mach_function;

typedef struct {
	mach_token *tokens;
	mach_function *funcs;
	u32 *tmp_mregs;

	isize token_count;
	isize max_token_count;
	u32 tmp_mreg_count;
	u32 int_mreg_count;
	u32 mreg_count;
	u32 func_count;
	u32 max_vreg_count;
	u32 max_label_count;
} mach_program;

typedef struct {
	b32 *used;
	u32 spill_count;
} regalloc_info;

static b32
equals_token(mach_token a, mach_token b)
{
	b32 result = (a.kind == b.kind && a.value == b.value);
	return result;
}

static mach_token
make_mach_token(u32 kind, u32 value, u32 size)
{
	mach_token token = {0};
	token.kind = kind;
	token.value = value;
	token.size = size;
	ASSERT(size <= 16);
	return token;
}

static mach_token
make_spill(u32 index)
{
	mach_token token = make_mach_token(MACH_SPILL, index, 8);
	return token;
}

static mach_token
make_label(u32 value)
{
	mach_token token = make_mach_token(MACH_LABEL, value, 0);
	return token;
}

static mach_token
make_func(u32 index)
{
	mach_token token = make_mach_token(MACH_FUNC, index, 8);
	return token;
}

static mach_token
make_global(u32 index)
{
	mach_token token = make_mach_token(MACH_GLOBAL, index, 8);
	return token;
}

static void
push_token(mach_program *p, mach_token arg)
{
	ASSERT(arg.kind != MACH_INVALID);
	ASSERT(arg.kind != MACH_VREG || arg.value < p->max_vreg_count);
	p->tokens[p->token_count++] = arg;
}

static void
push_inst(mach_program *p, u32 opcode, u32 token_count)
{
	mach_token token = make_mach_token(MACH_INST, opcode, 0);
	(void)token_count;
	push_token(p, token);
}
