typedef struct {
	i32 value;
} info_id;

typedef enum {
	INFO_NONE,
	INFO_COUNT
} info_kind;

typedef struct label_info label_info;
struct label_info {
	label_info *next;
	ast_id label_id;
	str name;
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
	TYPE_BUILTIN_VA_LIST,

	TYPE_UNSIGNED = 1,
} type_kind;

typedef struct member member;
struct member {
	member *next;
	str name;
	type_id type;
};

typedef struct {
	type_kind kind;
	type_id base_type;
	member *members;
	i64 size;
} type;

typedef struct {
	type_id *at;
	type *data;
	isize size;
	isize cap;
} type_pool;

typedef union {
	ast_id id;
	i64 i;
	f64 f;
} ast_info;

typedef struct {
	info_id *of;
	info_kind *kind;

	label_info *labels;
	type_pool types;

	isize label_count;

	ast_info *at;
} semantic_info;

typedef struct scope_entry scope_entry;
struct scope_entry {
	scope_entry *next;
	str key;
	ast_id value;
};

typedef struct scope scope;
struct scope {
	scope *parent;
	scope_entry *entries;
};

typedef struct {
	ast_pool *ast;
	arena *arena;
	type_pool *types;
	semantic_info *info;
	ast_id switch_id;
	label_info *labels;
	scope *idents;
	scope *tags;
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
get_type_id(type_pool *p, ast_id id)
{
	type_id result = {0};

	if (0 < id.value) {
		result = p->at[id.value];
	} else {
		ASSERT(!"ID is out of bounds");
	}

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
	case TYPE_BUILTIN_VA_LIST: return "va_list";
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
	case TYPE_BUILTIN_VA_LIST:
		// TODO
		return 8;
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
