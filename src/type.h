#define TYPE_NIL (&type_nil)

typedef struct member member;
typedef struct type type;
typedef struct ast_node ast_node;

typedef enum {
	LINK_DEFAULT,
	LINK_EXTERN,
	LINK_STATIC,
} linkage;

typedef struct symbol_id {
	i32 value;
} symbol_id;

typedef struct case_symbol case_symbol;
struct case_symbol {
	case_symbol *next;
	ast_id case_id;
	u32 label;
};

typedef struct {
	case_symbol *first;
	case_symbol *last;
	ast_id default_case;
} switch_symbol;

typedef struct {
	str name;
	type *type;
	ast_id definition;
	linkage linkage;
	b8 is_global;
	b8 is_function;
} decl_symbol;

typedef struct {
	isize decl_count;
	isize switch_count;
	isize case_count;
	isize label_count;
	symbol_id *symbols;
	u32 *labels;
	decl_symbol *decls;
	case_symbol *cases;
	switch_symbol *switches;
} symbol_table;

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

	// float types
	TYPE_FLOAT,
	TYPE_DOUBLE,

	// compound types
	TYPE_ARRAY,
	TYPE_BITFIELD,
	TYPE_FUNCTION,
	TYPE_POINTER,
	TYPE_STRUCT,

	TYPE_UNSIGNED = 1,
} type_kind;

struct member {
	member *next;
	str name;
	type *type;
};

struct type {
	type_kind kind;
	type *next;
	type *children;
	member *members;
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
static type type_float = {TYPE_FLOAT};
static type type_double = {TYPE_DOUBLE};

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
	case TYPE_FLOAT:          return "float";
	case TYPE_DOUBLE:         return "double";
	case TYPE_FUNCTION:       return "(function)";
	case TYPE_ARRAY:          return "(array)";
	case TYPE_POINTER:        return "(pointer)";
	case TYPE_STRUCT:         return "(struct)";
	case TYPE_BITFIELD:       return "(bitfield)";
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

static usize type_alignof(type *type);

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
	case TYPE_FLOAT:
		return 4;
	case TYPE_DOUBLE:
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
	case TYPE_BITFIELD:
		ASSERT(!"TODO");
		return 0;
	case TYPE_ARRAY:
		{
			usize target_size = type_sizeof(type->children);
			return type->size * target_size;
		} break;
	case TYPE_STRUCT:
		{
			usize size = 0;
			for (member *s = type->members; s; s = s->next) {
				u32 align = type_alignof(s->type);
				size = (size + align - 1) & ~(align - 1);
				size += type_sizeof(s->type);
			}
			return size;
		} break;
	}

	return 0;
}

static usize
type_alignof(type *type)
{
	usize result = 0;

	// TODO: proper alignment for structs and arrays
	if (type->kind == TYPE_ARRAY) {
		result = 16;
	} else if (type->kind == TYPE_STRUCT) {
		for (member *s = type->members; s; s = s->next) {
			u32 align = type_alignof(s->type);
			result = MAX(result, align);
		}
	} else {
		result = type_sizeof(type);
	}

	return result;
}

static usize
type_offsetof(type *type, str member_name)
{
	usize offset = 0;

	ASSERT(type != NULL);
	if (type->kind == TYPE_STRUCT) {
		// TODO: member could be in an unnamed struct
		for (member *s = type->members; s; s = s->next) {
			u32 align = type_alignof(s->type);
			offset = (offset + align - 1) & ~(align - 1);
			if (str_equals(member_name, s->name)) {
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

static usize
type_offsetof_index(type *type, isize index)
{
	usize offset = 0;

	ASSERT(type != NULL);
	if (type->kind == TYPE_STRUCT) {
		// TODO: member could be in an unnamed struct
		for (member *s = type->members; s; s = s->next) {
			u32 align = type_alignof(s->type);
			offset = (offset + align - 1) & ~(align - 1);
			if (index-- <= 0) {
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

static member *
get_member(member *list, str key)
{
	for (member *m = list; m; m = m->next) {
		if (str_equals(m->name, key)) {
			return m;
		}
	}

	return NULL;
}

static decl_symbol *
get_decl_symbol(symbol_table symtab, symbol_id id)
{
	ASSERT(id.value < symtab.decl_count);
	decl_symbol *symbol = &symtab.decls[id.value];
	return symbol;
}

static switch_symbol *
get_switch_symbol(symbol_table symtab, symbol_id id)
{
	ASSERT(id.value < symtab.switch_count);
	switch_symbol *symbol = &symtab.switches[id.value];
	return symbol;
}

static case_symbol *
get_case_symbol(symbol_table symtab, symbol_id id)
{
	ASSERT(id.value < symtab.case_count);
	case_symbol *symbol = &symtab.cases[id.value];
	return symbol;
}
