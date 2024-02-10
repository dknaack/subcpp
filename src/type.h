#define TYPE_NIL (&type_nil)

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
	b32 error;
};

typedef enum {
	TYPE_UNKNOWN,
	TYPE_VOID,

	// integer types
	TYPE_CHAR,
	TYPE_CHAR_UNSIGNED,
	TYPE_SHORT,
	TYPE_SHORT_UNSIGNED,
	TYPE_INT,
	TYPE_INT_UNSIGNED,
	TYPE_LONG,
	TYPE_LONG_UNSIGNED,
	TYPE_LLONG,
	TYPE_LLONG_UNSIGNED,

	// compound types
	TYPE_ARRAY,
	TYPE_FUNCTION,
	TYPE_POINTER,
	TYPE_STRUCT,

	TYPE_UNSIGNED = 1,
} type_kind;

struct type {
	type_kind kind;
	type *next;
	type *children;
	symbol *members;
	i64 size;
};

static type type_nil = {0, &type_nil, &type_nil};
static type type_void = {TYPE_VOID};
static type type_char  = {TYPE_CHAR};
static type type_short = {TYPE_SHORT};
static type type_int   = {TYPE_INT};
static type type_long  = {TYPE_LONG};
static type type_llong = {TYPE_LLONG};
static type type_char_unsigned  = {TYPE_CHAR_UNSIGNED};
static type type_short_unsigned = {TYPE_SHORT_UNSIGNED};
static type type_int_unsigned   = {TYPE_INT_UNSIGNED};
static type type_long_unsigned  = {TYPE_LONG_UNSIGNED};
static type type_llong_unsigned = {TYPE_LLONG_UNSIGNED};

static char *
type_get_name(type_kind type)
{
	switch (type) {
	case TYPE_VOID:           return "void";
	case TYPE_CHAR:           return "char";
	case TYPE_CHAR_UNSIGNED:  return "unsigned char";
	case TYPE_SHORT:          return "short";
	case TYPE_SHORT_UNSIGNED: return "unsigned short";
	case TYPE_INT:            return "int";
	case TYPE_INT_UNSIGNED:   return "unsigned int";
	case TYPE_LONG:           return "long";
	case TYPE_LONG_UNSIGNED:  return "unsigned long";
	case TYPE_LLONG:          return "long long";
	case TYPE_LLONG_UNSIGNED: return "unsigned long long";
	case TYPE_FUNCTION:       return "(function)";
	case TYPE_ARRAY:          return "(array)";
	case TYPE_POINTER:        return "(pointer)";
	case TYPE_STRUCT:         return "(struct)";
	case TYPE_UNKNOWN:        return "(unknown)";
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
	ASSERT(type != NULL);

	switch (type->kind) {
	case TYPE_CHAR:
	case TYPE_CHAR_UNSIGNED:
		return 1;
	case TYPE_SHORT:
	case TYPE_SHORT_UNSIGNED:
		return 2;
	case TYPE_INT:
	case TYPE_INT_UNSIGNED:
		return 4;
	case TYPE_POINTER:
	case TYPE_LONG:
	case TYPE_LONG_UNSIGNED:
	case TYPE_LLONG:
	case TYPE_LLONG_UNSIGNED:
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

	ASSERT(type != NULL);
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
