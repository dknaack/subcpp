struct type_id {
	uint32_t value;
};

enum type_kind {
	TYPE_UNKNOWN,
	TYPE_VOID,
	TYPE_INT,
	TYPE_CHAR,
	TYPE_FUNCTION
};

struct type_function {
	struct type *return_type;
	struct type *param_types;
};

struct type {
	enum type_kind kind;
	struct type *next;

	union {
		struct type_function function;
	} u;
};

struct symbol {
	struct symbol *next;
	struct string name;
	struct type *type;
};

struct symbol_table {
	struct symbol *symbols;
	struct symbol *free_symbols;
};

static struct string scope_marker = S("(scope)");
static struct type type_void = {TYPE_VOID};

static char *
type_get_name(enum type_kind type)
{
	switch (type) {
	case TYPE_VOID:     return "void";
	case TYPE_INT:      return "int";
	case TYPE_CHAR:     return "char";
	case TYPE_FUNCTION: return "function";
	case TYPE_UNKNOWN:  return "(unknown)";
	}

	return "(invalid)";
}

static struct type *
type_create(enum type_kind kind, struct arena *arena)
{
	struct type *type = ZALLOC(arena, 1, struct type);
	type->kind = kind;
	return type;
}
