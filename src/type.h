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

typedef enum {
	SYM_NONE,
	SYM_CASE,
	SYM_DECL,
	SYM_LABEL,
	SYM_SWITCH,
	SYM_STRING,
} symbol_kind;

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
	symbol_id *symbols;
	symbol_kind *kind;

	u32 *labels;
	str *strings;
	decl_symbol *decls;
	case_symbol *cases;
	switch_symbol *switches;

	isize decl_count;
	isize switch_count;
	isize case_count;
	isize label_count;
	isize string_count;
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
	TYPE_UNION,
	TYPE_OPAQUE,

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
	type *base_type;
	member *members;
	i64 size;
};

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
	case TYPE_UNION:          return "(union)";
	case TYPE_BITFIELD:       return "(bitfield)";
	case TYPE_UNKNOWN:        return "(unknown)";
	case TYPE_OPAQUE:         return "(opaque)";
	}

	ASSERT(!"Invalid type");
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

static b32 is_compound_type(type_kind kind)
{
	switch (kind) {
	case TYPE_STRUCT:
	case TYPE_UNION:
	case TYPE_OPAQUE:
		return true;
	default:
		return false;
	}
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
		// TODO: Implement bitfield size
		return 8;
	case TYPE_ARRAY:
		{
			usize target_size = type_sizeof(type->base_type);
			return type->size * target_size;
		} break;
	case TYPE_UNION:
		{
			isize size = 0;

			for (member *s = type->members; s; s = s->next) {
				isize member_size = type_sizeof(s->type);
				size = MAX(size, member_size);
			}

			return size;
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
	case TYPE_OPAQUE:
		{
			if (type->base_type) {
				return type_sizeof(type->base_type);
			}
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
	} else if (type->kind == TYPE_STRUCT || type->kind == TYPE_UNION) {
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
	if (type->kind == TYPE_UNION) {
		b32 found = false;

		for (member *s = type->members; !found && s; s = s->next) {
			if (equals(member_name, s->name)) {
				found = true;
			}
		}

		ASSERT(found);
	} else if (type->kind == TYPE_STRUCT) {
		// TODO: member could be in an unnamed struct
		for (member *s = type->members; s; s = s->next) {
			u32 align = type_alignof(s->type);
			offset = (offset + align - 1) & ~(align - 1);
			if (equals(member_name, s->name)) {
				break;
			}

			offset += type_sizeof(s->type);
		}
	} else if (type->kind == TYPE_OPAQUE && type->base_type) {
		offset = type_offsetof(type->base_type, member_name);
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
		if (equals(m->name, key)) {
			return m;
		}
	}

	return NULL;
}

static decl_symbol *
get_decl_symbol(symbol_table symtab, ast_id node_id)
{
	ASSERT(symtab.kind[node_id.value] == SYM_DECL);
	symbol_id sym_id = symtab.symbols[node_id.value];

	ASSERT(sym_id.value < symtab.decl_count);
	decl_symbol *symbol = &symtab.decls[sym_id.value];
	return symbol;
}

static switch_symbol *
get_switch_symbol(symbol_table symtab, ast_id node_id)
{
	ASSERT(symtab.kind[node_id.value] == SYM_SWITCH);
	symbol_id sym_id = symtab.symbols[node_id.value];

	ASSERT(sym_id.value < symtab.switch_count);
	switch_symbol *symbol = &symtab.switches[sym_id.value];
	return symbol;
}

static case_symbol *
get_case_symbol(symbol_table symtab, ast_id node_id)
{
	ASSERT(symtab.kind[node_id.value] == SYM_CASE);
	symbol_id sym_id = symtab.symbols[node_id.value];

	ASSERT(sym_id.value < symtab.case_count);
	case_symbol *symbol = &symtab.cases[sym_id.value];
	return symbol;
}

static str *
get_string_symbol(symbol_table symtab, ast_id node_id)
{
	ASSERT(symtab.kind[node_id.value] == SYM_STRING);
	symbol_id sym_id = symtab.symbols[node_id.value];

	ASSERT(sym_id.value < symtab.string_count);
	str *symbol = &symtab.strings[sym_id.value];
	return symbol;
}
