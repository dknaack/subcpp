typedef struct label_info label_info;
struct label_info {
	label_info *next;
	syntax_id label_id;
	str name;
};

typedef struct {
	syntax_id node_id;
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
	syntax_pool *ast;
	arena *arena;
	syntax_id switch_id;
	scope scope;
	label_info *labels;
} sema_context;

static b32
is_type(syntax_node_kind kind)
{
	switch (kind) {
	case SYNTAX_TYPE_BASIC:
	case SYNTAX_TYPE_ARRAY:
	case SYNTAX_TYPE_BITFIELD:
	case SYNTAX_TYPE_ENUM:
	case SYNTAX_TYPE_FUNC:
	case SYNTAX_TYPE_IDENT:
	case SYNTAX_TYPE_POINTER:
	case SYNTAX_TYPE_STRUCT:
	case SYNTAX_TYPE_UNION:
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
is_pointer(syntax_node_kind kind)
{
	b32 result = (kind == SYNTAX_TYPE_POINTER || kind == SYNTAX_TYPE_ARRAY);
	return result;
}

static b32 is_compound_type(syntax_node_kind kind)
{
	b32 result = (kind == SYNTAX_TYPE_STRUCT || kind == SYNTAX_TYPE_UNION);
	return result;
}

static isize get_node_alignment(syntax_pool *p, type_id type);

static b32
has_type(syntax_node *node)
{
	return node->kind != SYNTAX_EXPR_IDENT
		&& node->kind != SYNTAX_DECL
		&& node->kind != SYNTAX_STMT_SWITCH
		&& node->kind != SYNTAX_STMT_CASE;
}

static b32
has_ref(syntax_node *node)
{
	return node->kind == SYNTAX_EXPR_IDENT
		|| node->kind == SYNTAX_DECL;
}

static type_id
get_type_id(syntax_pool *pool, syntax_id id)
{
	syntax_node *node = &pool->nodes[id.value];
	type_id result = node->info.type;
	ASSERT(has_type(node));
	return result;
}

static syntax_id
find_decl(syntax_pool *pool, syntax_id id)
{
	syntax_id result = {0};

	syntax_node *node = &pool->nodes[id.value];
	if (has_ref(node)) {
		if (node->info.ref.value != id.value) {
			node->info.ref = find_decl(pool, node->info.ref);
		}

		result = node->info.ref;
	} else {
		ASSERT(!"Invalid node kind");
	}

	return result;
}

static type_id
find_type_id(syntax_pool *pool, syntax_id id)
{
	type_id result = {0};

	syntax_node *node = &pool->nodes[id.value];
	if (has_type(node)) {
		result = node->info.type;
	} else if (has_ref(node)) {
		id = find_decl(pool, id);
		node = &pool->nodes[id.value];
		result.value = node->children.value;
	}

	return result;
}

static syntax_node
get_type(syntax_pool *p, type_id id)
{
	syntax_node *node = &p->nodes[id.value];
	ASSERT(has_type(node));
	return *node;
}

static void
set_type(syntax_pool *p, syntax_id node_id, type_id type)
{
	syntax_node *node = &p->nodes[node_id.value];
	ASSERT(has_type(node));
	node->info.type = type;
}

static syntax_id
get_member(syntax_pool *p, type_id type, str member)
{
	syntax_id result = get_type(p, type).children;
	while (result.value != 0) {
		syntax_node child = get_node(p, result);
		if (equals(child.token.value, member)) {
			break;
		}

		result = child.next;
	}

	return result;
}

static isize
get_node_size(syntax_pool *p, type_id node_id)
{
	ASSERT(node_id.value != 0);

	syntax_node node = get_type(p, node_id);
	switch (node.kind) {
	case SYNTAX_TYPE_BASIC:
		switch (node.token.kind) {
		case TOKEN_INT:
			if (node.flags & SYNTAX_LLONG) {
				return 8;
			} else if (node.flags & SYNTAX_LONG) {
				return 8;
			} else if (node.flags & SYNTAX_SHORT) {
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
	case SYNTAX_TYPE_POINTER:
	case SYNTAX_TYPE_FUNC:
		return 8;
	case SYNTAX_TYPE_ARRAY:
		{
			type_id subtype = get_type_id(p, node.children);
			isize length = get_type(p, subtype).info.i;
			isize subtype_size = get_node_size(p, subtype);
			ASSERT(length != 0 && subtype_size != 0);
			return length * subtype_size;
		} break;
	case SYNTAX_TYPE_STRUCT:
	case SYNTAX_TYPE_UNION:
		{
			isize result = 0;
			isize max_align = 0;

			syntax_id member_id = node.children;
			while (member_id.value != 0) {
				type_id member_type = get_type_id(p, member_id);
				isize member_size = get_node_size(p, member_type);
				isize member_align = get_node_alignment(p, member_type);

				if (node.kind == SYNTAX_TYPE_UNION) {
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
get_node_alignment(syntax_pool *p, type_id type)
{
	// TODO: Fix this function
	isize result = get_node_size(p, type);
	if (result > 16) {
		result = 16;
	}

	return result;
}

static isize
get_member_offset(syntax_pool *p, type_id type, syntax_id member)
{
	isize result = 0;

	syntax_node type_node = get_type(p, type);
	if (is_compound_type(type_node.kind)) {
		syntax_id child = type_node.children;
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
