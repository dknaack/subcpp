typedef struct type type;

typedef struct symbol symbol;
struct symbol {
	str name;
	type *type;
	symbol *child[4];
	symbol *next;
};

typedef struct symbol_table symbol_table;
struct symbol_table {
	symbol_table *parent;
	symbol *head;
	symbol *tail;
};

typedef enum {
	TYPE_UNKNOWN,
	TYPE_ARRAY,
	TYPE_CHAR,
	TYPE_FUNCTION,
	TYPE_INT,
	TYPE_POINTER,
	TYPE_STRUCT,
	TYPE_VOID,
} type_kind;

struct type {
	type_kind kind;
	type *next;
	type *children;
	symbol *members;
	i64 size;
};

static type type_void = {TYPE_VOID};
static type type_char = {TYPE_CHAR};
static type type_int = {TYPE_INT};

static char *
type_get_name(type_kind type)
{
	switch (type) {
	case TYPE_VOID:     return "void";
	case TYPE_INT:      return "int";
	case TYPE_CHAR:     return "char";
	case TYPE_FUNCTION: return "(function)";
	case TYPE_ARRAY:    return "(array)";
	case TYPE_POINTER:  return "(pointer)";
	case TYPE_STRUCT:   return "(struct)";
	case TYPE_UNKNOWN:  return "(unknown)";
	}

	return "(invalid)";
}

static type *
type_create(type_kind kind, arena *arena)
{
	type *t = ALLOC(arena, 1, type);
	t->kind = kind;
	return t;
}

static usize
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
	case TYPE_UNKNOWN:
		ASSERT(!"Type does not have a size");
		return 0;
	case TYPE_ARRAY:
		{
			usize target_size = type_sizeof(type->children);
			return type->size * target_size;
		} break;
	case TYPE_STRUCT:
		{
			usize size = 0;
			for (symbol *s = type->members; s; s = s->next) {
				size += type_sizeof(s->type);
			}
			return size;
		} break;
	}

	return 0;
}

static usize
type_offsetof(type *type, str member)
{
	usize offset = 0;

	if (type->kind == TYPE_STRUCT) {
		// TODO: member could be in an unnamed struct
		for (symbol *s = type->members; s; s = s->next) {
			if (str_equals(member, s->name)) {
				break;
			}

			offset += type_sizeof(s->type);
		}
	} else {
		// TODO: report error
		ASSERT(!"Type must be struct or union");
	}

	return offset;
}
