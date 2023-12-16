typedef struct type type;
typedef struct {
	uint32_t value;
} type_id;

typedef enum {
	TYPE_UNKNOWN,
	TYPE_CHAR,
	TYPE_FUNCTION,
	TYPE_INT,
	TYPE_POINTER,
	TYPE_VOID,
} type_kind;

typedef struct {
	type *return_type;
	type *param_types;
} type_function;

typedef struct {
	type *target;
} type_pointer;

struct type {
	type_kind kind;
	type *next;

	union {
		type_function function;
		type_pointer pointer;
	} u;
};

typedef struct symbol symbol;
struct symbol {
	symbol *next;
	string name;
	type *type;
};

typedef struct {
	symbol *symbols;
	symbol *free_symbols;
} symbol_table;

static string scope_marker = S("(scope)");
static type type_void = {TYPE_VOID};

static char *
type_get_name(type_kind type)
{
	switch (type) {
	case TYPE_VOID:     return "void";
	case TYPE_INT:      return "int";
	case TYPE_CHAR:     return "char";
	case TYPE_FUNCTION: return "(function)";
	case TYPE_POINTER:  return "(pointer)";
	case TYPE_UNKNOWN:  return "(unknown)";
	}

	return "(invalid)";
}

static type *
type_create(type_kind kind, arena *arena)
{
	type *t = ZALLOC(arena, 1, type);
	t->kind = kind;
	return t;
}

static size_t
type_sizeof(type *type)
{
	switch (type->kind) {
	case TYPE_CHAR:
		return 1;
	case TYPE_INT:
		return 4;
	case TYPE_POINTER:
		return 8;
	case TYPE_VOID:
	case TYPE_FUNCTION:
		ASSERT(!"Type does not have a size");
		return 0;
	default:
		ASSERT(!"Invalid type");
	}

	return 0;
}
