typedef struct member member;
typedef struct type type;
typedef struct ast_node ast_node;

typedef enum {
	LINK_DEFAULT,
	LINK_EXTERN,
	LINK_STATIC,
} linkage;

typedef struct info_id {
	i32 value;
} info_id;

typedef enum {
	INFO_NONE,
	INFO_CASE,
	INFO_DECL,
	INFO_LABEL,
	INFO_SWITCH,
	INFO_STRING,
} info_kind;

typedef struct case_info case_info;
struct case_info {
	case_info *next;
	ast_id case_id;
	u32 label;
};

typedef struct {
	case_info *first;
	case_info *last;
	ast_id default_case;
} switch_info;

typedef struct {
	str name;
	type_id type;
	ast_id definition;
	linkage linkage;
	b8 is_global;
	b8 is_function;
} decl_info;

typedef struct {
	linkage linkage;
	str name;
	void *data;
	isize size;
} symbol;

typedef struct {
	symbol *symbols;

	isize symbol_count;
	isize text_offset;
	isize data_offset;
	isize rodata_offset;
	isize bss_offset;
} symbol_table;

typedef struct {
	type_id *at;
	type *data;
	isize size;
	isize cap;
} type_pool;

typedef struct {
	info_id *of;
	info_kind *kind;

	u32 *labels;
	str *strings;
	decl_info *decls;
	case_info *cases;
	switch_info *switches;
	symbol_table symtab;
	type_pool types;

	isize decl_count;
	isize switch_count;
	isize case_count;
	isize label_count;
	isize string_count;
} semantic_info;

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
	type_id type;
};

struct type {
	type_kind kind;
	type_id base_type;
	member *members;
	i64 size;
};

typedef struct {
	ast_pool *ast;
	arena *arena;
	type_pool *types;
} semantic_context;

static type *
get_type_data(type_pool *p, type_id id)
{
	if (0 < id.value) {
		return p->data + id.value;
	}

	ASSERT(!"ID is out of bounds");
	return NULL;
}

static type_id
get_type(type_pool *p, ast_id id)
{
	type_id result = {0};
	if (0 < id.value) {
		result = p->at[id.value];
	}

	ASSERT(!"ID is out of bounds");
	return result;
}

static void
set_type(type_pool *p, ast_id id, type_id type)
{
	p->at[id.value] = type;
}

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

static isize type_alignof(type_id type_id, type_pool *pool);

static isize
type_sizeof(type_id type_id, type_pool *pool)
{
	ASSERT(type_id.value != 0);

	type *type = get_type_data(pool, type_id);
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
			isize target_size = type_sizeof(type->base_type, pool);
			return type->size * target_size;
		} break;
	case TYPE_UNION:
		{
			isize size = 0;

			for (member *s = type->members; s; s = s->next) {
				isize member_size = type_sizeof(s->type, pool);
				size = MAX(size, member_size);
			}

			return size;
		} break;
	case TYPE_STRUCT:
		{
			usize size = 0;
			for (member *s = type->members; s; s = s->next) {
				u32 align = type_alignof(s->type, pool);
				size = (size + align - 1) & ~(align - 1);
				size += type_sizeof(s->type, pool);
			}
			return size;
		} break;
	case TYPE_OPAQUE:
		{
			if (type->base_type.value != 0) {
				return type_sizeof(type->base_type, pool);
			}
		} break;
	}

	return 0;
}

static isize
type_alignof(type_id type_id, type_pool *pool)
{
	usize result = 0;
	type *type = get_type_data(pool, type_id);

	// TODO: proper alignment for structs and arrays
	if (type->kind == TYPE_ARRAY) {
		result = 16;
	} else if (type->kind == TYPE_STRUCT || type->kind == TYPE_UNION) {
		for (member *s = type->members; s; s = s->next) {
			u32 align = type_alignof(s->type, pool);
			result = MAX(result, align);
		}
	} else {
		result = type_sizeof(type_id, pool);
	}

	return result;
}

static isize
type_offsetof(type_id type_id, str member_name, type_pool *pool)
{
	isize offset = 0;
	type *type = get_type_data(pool, type_id);

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
			u32 align = type_alignof(s->type, pool);
			offset = (offset + align - 1) & ~(align - 1);
			if (equals(member_name, s->name)) {
				break;
			}

			offset += type_sizeof(s->type, pool);
		}
	} else if (type->kind == TYPE_OPAQUE && type->base_type.value != 0) {
		offset = type_offsetof(type->base_type, member_name, pool);
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

static decl_info *
get_decl_info(semantic_info info, ast_id node_id)
{
	ASSERT(info.kind[node_id.value] == INFO_DECL);
	info_id sym_id = info.of[node_id.value];

	ASSERT(sym_id.value < info.decl_count);
	decl_info *decl = &info.decls[sym_id.value];
	return decl;
}

static switch_info *
get_switch_info(semantic_info info, ast_id node_id)
{
	ASSERT(info.kind[node_id.value] == INFO_SWITCH);
	info_id sym_id = info.of[node_id.value];

	ASSERT(sym_id.value < info.switch_count);
	switch_info *_switch = &info.switches[sym_id.value];
	return _switch;
}

static case_info *
get_case_info(semantic_info info, ast_id node_id)
{
	ASSERT(info.kind[node_id.value] == INFO_CASE);
	info_id sym_id = info.of[node_id.value];

	ASSERT(sym_id.value < info.case_count);
	case_info *_case = &info.cases[sym_id.value];
	return _case;
}

static str *
get_string_info(semantic_info info, ast_id node_id)
{
	ASSERT(info.kind[node_id.value] == INFO_STRING);
	info_id sym_id = info.of[node_id.value];

	ASSERT(sym_id.value < info.string_count);
	str *s = &info.strings[sym_id.value];
	return s;
}
