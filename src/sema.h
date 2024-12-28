typedef struct label_info label_info;
struct label_info {
	label_info *next;
	ast_id label_id;
	str name;
};

typedef struct {
	ast_id node_id;
	i32 depth;
	i32 age;
} scope_entry;

typedef struct {
	scope_entry *at;
	i32 *ages;
	i32 depth, max_depth;
	i32 size, max_size;
} scope;

typedef struct {
	ast_pool *ast;
	arena *arena;
	ast_id switch_id;
	scope scope;
	label_info *labels;
} sema_context;

static b32
is_type(ast_node_kind kind)
{
	switch (kind) {
	case AST_TYPE_BASIC:
	case AST_TYPE_ARRAY:
	case AST_TYPE_BITFIELD:
	case AST_TYPE_ENUM:
	case AST_TYPE_FUNC:
	case AST_TYPE_IDENT:
	case AST_TYPE_POINTER:
	case AST_TYPE_STRUCT:
	case AST_TYPE_UNION:
		return true;
	default:
		return false;
	}
}

static b32
is_integer(token_kind kind)
{
	switch (kind) {
	case TOKEN_INT:
	case TOKEN_LONG:
	case TOKEN_SHORT:
	case TOKEN_UNSIGNED:
	case TOKEN_ENUM:
		return true;
	default:
		return false;
	}
}

static b32
is_pointer(ast_node_kind kind)
{
	b32 result = (kind == AST_TYPE_POINTER || kind == AST_TYPE_ARRAY);
	return result;
}

static b32 is_compound_type(ast_node_kind kind)
{
	b32 result = (kind == AST_TYPE_STRUCT || kind == AST_TYPE_UNION);
	return result;
}

static isize get_node_alignment(ast_pool *p, type_id type);

static type_id
get_type_id(ast_pool *pool, ast_id id)
{
	type_id result = pool->nodes[id.value].info.type;

	// Identifiers do not point to their type, which is why we first extract
	// the declaration. The declaration then points to the type of the
	// identifier.
	if (result.value != pool->nodes[result.value].info.type.value) {
		result = pool->nodes[result.value].info.type;
	}

	return result;
}

static ast_node
get_type(ast_pool *p, type_id id)
{
	ast_id tmp = {id.value};
	ast_node result = get_node(p, tmp);
	return result;
}

static void
set_type(ast_pool *pool, ast_id node_id, type_id type)
{
	pool->nodes[node_id.value].info.type = type;
}

static ast_id
get_member(ast_pool *p, type_id type, str member)
{
	ast_id result = get_type(p, type).children;
	while (result.value != 0) {
		ast_node child = get_node(p, result);
		if (equals(child.token.value, member)) {
			break;
		}

		result = child.next;
	}

	return result;
}

static isize
get_node_size(ast_pool *p, type_id node_id)
{
	ASSERT(node_id.value != 0);

	ast_node node = get_type(p, node_id);
	switch (node.kind) {
	case AST_TYPE_BASIC:
		switch (node.token.kind) {
		case TOKEN_INT:
			if (node.flags & AST_LLONG) {
				return 8;
			} else if (node.flags & AST_LONG) {
				return 8;
			} else if (node.flags & AST_SHORT) {
				return 2;
			} else {
				return 4;
			}
		case TOKEN_CHAR:
			return 1;
		case TOKEN_FLOAT:
			return 4;
		case TOKEN_DOUBLE:
			// TODO: sizeof long double
			return 8;
		default:
			ASSERT(!"Invalid type");
			return 0;
		}

		break;
	case AST_TYPE_POINTER:
	case AST_TYPE_FUNC:
		return 8;
	case AST_TYPE_ARRAY:
		{
			type_id subtype = get_type_id(p, node.children);
			isize length = get_type(p, subtype).info.i;
			isize subtype_size = get_node_size(p, subtype);
			ASSERT(length != 0 && subtype_size != 0);
			return length * subtype_size;
		} break;
	case AST_TYPE_STRUCT:
	case AST_TYPE_UNION:
		{
			isize result = 0;
			isize max_align = 0;

			ast_id member_id = node.children;
			while (member_id.value != 0) {
				type_id member_type = get_type_id(p, member_id);
				isize member_size = get_node_size(p, member_type);
				isize member_align = get_node_alignment(p, member_type);

				if (node.kind == AST_TYPE_UNION) {
					if (result < member_size) {
						result = member_size;
					}
				} else {
					isize padding = (result + member_align - 1) & ~(member_align - 1);
					result += padding;
					result += member_size;
				}

				if (member_align > max_align) {
					max_align = member_align;
				}

				member_id = get_node(p, member_id).next;
			}

			isize padding = (result + max_align - 1) & ~(max_align - 1);
			result += padding;
			return result;
		} break;
	default:
		ASSERT(!"Invalid type");
		return 0;
	}
}

static isize
get_node_alignment(ast_pool *p, type_id type)
{
	// TODO: Fix this function
	isize result = get_node_size(p, type);
	if (result > 16) {
		result = 16;
	}

	return result;
}

static isize
get_member_offset(ast_pool *p, type_id type, ast_id member)
{
	isize result = 0;

	ast_node type_node = get_type(p, type);
	if (is_compound_type(type_node.kind)) {
		ast_id child = type_node.children;
		while (child.value != 0) {
			type_id child_type = get_type_id(p, child);
			isize align = get_node_alignment(p, child_type);
			result = (result + align - 1) & ~(align - 1);
			if (child.value == member.value) {
				break;
			}

			result += get_node_size(p, child_type);
			child = get_node(p, child).next;
		}
	}

	return result;
}
