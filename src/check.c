typedef struct scope_entry scope_entry;
struct scope_entry {
	str key;
	ast_node *value;

	i64 depth;
	scope_entry *child[4];
};

typedef struct {
	scope_entry *root;
	isize depth;
	isize *count;
} scope;

static scope *
new_scope(scope *parent, arena *perm)
{
	scope *s = ALLOC(perm, 1, scope);
	if (parent) {
		memcpy(s, parent, sizeof(*s));
		s->depth++;
	}

	return s;
}

static ast_node **
scope_upsert(scope *s, str key, arena *perm)
{
	scope_entry **m = &s->root;
	isize depth = s->depth;

	for (u64 h = hash(key); *m; h <<= 2) {
		if (str_equals((*m)->key, key)) {
			return &(*m)->value;
		}

		if ((*m)->depth != depth) {
			scope_entry *tmp = ALLOC(perm, 1, scope_entry);
			*m = memcpy(tmp, *m, sizeof(**m));
			(*m)->depth = depth;
		}

		m = &(*m)->child[h >> 62];
	}

	if (!perm) {
		return NULL;
	}

	*m = ALLOC(perm, 1, scope_entry);
	(*m)->key = key;
	(*m)->depth = depth;
	return &(*m)->value;
}

static b32
type_equals(type *lhs, type *rhs)
{
	member *l, *r;
	if (lhs->kind == TYPE_VOID || rhs->kind == TYPE_VOID) {
		return false;
	}

	if (lhs->kind != rhs->kind) {
		return false;
	}

	switch (lhs->kind) {
	case TYPE_FUNCTION:
		l = lhs->members;
		r = rhs->members;
		while (l && r) {
			if (!type_equals(l->type, r->type)) {
				return false;
			}

			l = l->next;
			r = r->next;
		}

		b32 result = (!l && !r);
		return result;
	default:
		return true;
	}
}

static b32
is_integer(type_kind type)
{
	switch (type) {
	case TYPE_CHAR:
	case TYPE_CHAR_UNSIGNED:
	case TYPE_SHORT:
	case TYPE_SHORT_UNSIGNED:
	case TYPE_INT:
	case TYPE_INT_UNSIGNED:
	case TYPE_LONG:
	case TYPE_LONG_UNSIGNED:
	case TYPE_LLONG:
	case TYPE_LLONG_UNSIGNED:
		return true;
	default:
		return false;
	}
}

static b32
is_pointer(type *type)
{
	b32 result = type->kind == TYPE_POINTER || type->kind == TYPE_ARRAY;
	return result;
}

static void
merge_identifiers(ast_node *node, scope *s, arena *perm, b32 *error)
{
	scope *orig = NULL;
	arena temp = {0};

	if (*error || node->kind == AST_TYPE_STRUCT_DEF) {
		return;
	}

	// TODO: Ensure that this works for function declarators but also function
	// definitions. For function declarations the parameters should be only
	// accessible from the parameters themselves. In a definition, the
	// parameters should be accessible from within the function.
	if (node->kind == AST_STMT_COMPOUND) {
		orig = s;
		temp = *perm;
		perm = &temp;
		s = new_scope(orig, perm);
	}

	for (ast_node **child = &node->children; *child != AST_NIL; child = &(*child)->next) {
		if ((*child)->kind == AST_DECL) {
			ASSERT((*child)->index == 0);
			(*child)->index = (*s->count)++;
			*scope_upsert(s, (*child)->value.s, perm) = *child;
		} else if ((*child)->kind == AST_EXPR_IDENT) {
			ast_node **resolved = scope_upsert(s, (*child)->value.s, NULL);
			if (resolved) {
				*child = *resolved;
			} else {
				errorf(node->loc, "Variable was never declared");
				*error = true;
			}
		}

		merge_identifiers(*child, s, perm, error);
	}
}

static void
check_type(ast_node *node, arena *arena, b32 *error)
{
	if (node->type) {
		return;
	}

	switch (node->kind) {
	case AST_INVALID:
		*error = true;
		break;
	case AST_ROOT:
		{
			for (ast_node *child = node->children; child != AST_NIL; child = child->next) {
				check_type(child, arena, error);
			}
		} break;
	case AST_INIT:
		{
			ASSERT(node->type->kind == TYPE_STRUCT);
			member *member = node->type->members;

			// TODO: Check the type of each field in the initializer
			ast_node *child = node->children;
			while (member && child != AST_NIL) {
				if (child->kind == AST_INIT) {
					child->type = member->type;
				}

				check_type(child, arena, error);
				// TODO: Type conversion
				if (!type_equals(member->type, child->type)) {
					//errorf(node->loc, "Invalid type");
				}

				member = member->next;
				child = child->next;
			}

			if (!member && child != AST_NIL) {
				errorf(node->loc, "Too many fields in the initializer");
			}
		} break;
	case AST_EXPR_MEMBER:
		{
			ast_node *lhs = node->children;
			check_type(lhs, arena, error);
			if (lhs->type->kind != TYPE_STRUCT) {
				errorf(node->loc, "Left-hand side is not a struct");
				*error = true;
			}

			member *s = get_member(lhs->type->members, node->value.s);
			if (s) {
				node->type = s->type;
			} else {
				errorf(node->loc, "Member does not exist");
			}
		} break;
	case AST_EXPR_BINARY:
		{
			ast_node *lhs = node->children;
			ast_node *rhs = lhs->next;
			check_type(lhs, arena, error);
			check_type(rhs, arena, error);

			u32 operator = node->value.i;
			if (operator == TOKEN_LBRACKET) {

				// NOTE: ensure that one operand is a pointer and the other one
				// is an integral type.
				if (is_pointer(lhs->type)) {
					node->type = lhs->type->children;
				} else if (is_pointer(rhs->type)) {
					node->type = rhs->type->children;
				} else {
					node->type = TYPE_NIL;
					*error = true;
					errorf(node->loc, "Incompatible types: %s, %s",
						type_get_name(lhs->type->kind),
						type_get_name(rhs->type->kind));
				}
			} else {
				// Apply integer promotion
				b32 same_sign = (lhs->type->kind & TYPE_UNSIGNED) == (rhs->type->kind & TYPE_UNSIGNED);
				if (lhs->type->kind == rhs->type->kind) {
					// do nothing
				} else if (same_sign) {
					if (lhs->type->kind < rhs->type->kind) {
						lhs->type->kind = rhs->type->kind;
					}
				} else {
					type *unsigned_type = (lhs->type->kind & TYPE_UNSIGNED ? lhs->type : rhs->type);
					type *signed_type = (lhs->type->kind & TYPE_UNSIGNED ? rhs->type : lhs->type);
					if (unsigned_type->kind > signed_type->kind) {
						signed_type->kind = unsigned_type->kind;
					} else {
						signed_type->kind |= TYPE_UNSIGNED;
						unsigned_type->kind = signed_type->kind;
					}
				}

				node->type = lhs->type;
			}
		} break;
	case AST_EXPR_CALL:
		{
			ast_node *called = node->children;
			check_type(called, arena, error);
			if (called->type->kind != TYPE_FUNCTION) {
				*error = true;
				errorf(node->loc, "Not a function: %s", type_get_name(called->type->kind));
				break;
			}

			u32 param_index = 0;
			ast_node *param = called->next;
			type *return_type = called->type->children;
			member *param_sym = called->type->members;
			while (param != AST_NIL || param_sym != NULL) {
				check_type(param, arena, error);
				if (!type_equals(param_sym->type, param->type)) {
					errorf(node->loc, "Parameter %d has wrong type: Expected %s, but found %s",
						param_index + 1, type_get_name(param_sym->type->kind),
						type_get_name(param->type->kind));
					*error = true;
				}

				param_sym = param_sym->next;
				param = param->next;
				param_index++;
			}

			node->type = return_type;
		} break;
	case AST_EXPR_IDENT:
		{
			ASSERT(!"This node should have been eliminated by merge_identifiers");
		} break;
	case AST_EXPR_INT:
		{
			node->type = &type_int;
		} break;
	case AST_EXPR_POSTFIX:
	case AST_EXPR_UNARY:
		{
			ast_node *operand = node->children;
			check_type(operand, arena, error);
			switch (node->value.i) {
			case TOKEN_STAR:
				if (operand->type->kind == TYPE_POINTER) {
					node->type = operand->type->children;
				} else {
					node->type = TYPE_NIL;
					*error = true;
					errorf(node->loc, "Expected pointer type");
				}
				break;
			case TOKEN_AMP:
				node->type = type_create(TYPE_POINTER, arena);
				node->type->children = operand->type;
				break;
			case TOKEN_BANG:
			case TOKEN_PLUS:
			case TOKEN_MINUS:
				// TODO: ensure that type is integer
				node->type = operand->type;
				break;
			case TOKEN_PLUS_PLUS:
			case TOKEN_MINUS_MINUS:
				// TODO: ensure that type is integer or pointer
				node->type = operand->type;
				break;
			default:
				ASSERT(!"Invalid operator");
				break;
			}
		} break;
	case AST_STMT_BREAK:
		break;
	case AST_STMT_COMPOUND:
	case AST_DECL_LIST:
		{
			for (ast_node *child = node->children; child != AST_NIL; child = child->next) {
				check_type(child, arena, error);
			}
		} break;
	case AST_DECL:
	case AST_EXTERN_DEF:
		{
			for (ast_node *child = node->children; child != AST_NIL; child = child->next) {
				check_type(child, arena, error);
			}

			node->type = node->children->type;
		} break;
	case AST_STMT_CONTINUE:
	case AST_STMT_EMPTY:
		break;
	case AST_STMT_FOR:
	case AST_STMT_IF:
	case AST_STMT_DO_WHILE:
	case AST_STMT_WHILE:
		{
			for (ast_node *child = node->children; child != AST_NIL; child = child->next) {
				check_type(child, arena, error);
			}
		} break;
	case AST_STMT_PRINT:
		{
			check_type(node->children, arena, error);
		} break;
	case AST_STMT_RETURN:
		{
			check_type(node->children, arena, error);
		} break;
	case AST_TYPE_VOID:
		{
			node->type = &type_void;
		} break;
	case AST_TYPE_CHAR:
		{
			if (node->flags & AST_UNSIGNED) {
				node->type = &type_char_unsigned;
			} else {
				node->type = &type_char;
			}
		} break;
	case AST_TYPE_FLOAT:
		{
			node->type = &type_float;
		} break;
	case AST_TYPE_INT:
		{
			u32 int_flags = AST_LONG | AST_LLONG | AST_SHORT | AST_UNSIGNED;
			switch (node->flags & int_flags) {
			case AST_LLONG | AST_UNSIGNED:
				node->type = &type_llong_unsigned;
				break;
			case AST_LLONG:
				node->type = &type_llong;
				break;
			case AST_LONG | AST_UNSIGNED:
				node->type = &type_long_unsigned;
				break;
			case AST_LONG:
				node->type = &type_long;
				break;
			case AST_SHORT | AST_UNSIGNED:
				node->type = &type_short_unsigned;
				break;
			case AST_SHORT:
				node->type = &type_short;
				break;
			case AST_UNSIGNED:
				node->type = &type_int_unsigned;
				break;
			default:
				node->type = &type_int;
			}
		} break;
	case AST_TYPE_POINTER:
		{
			check_type(node->children, arena, error);

			node->type = type_create(TYPE_POINTER, arena);
			node->type->children = node->children->type;
		} break;
	case AST_TYPE_ARRAY:
		{
			for (ast_node *child = node->children; child != AST_NIL; child = child->next) {
				check_type(child, arena, error);
			}

			node->type = type_create(TYPE_ARRAY, arena);
			// TODO: Evaluate the size of the array
			node->type->size = 1;
			node->type->children = node->children->next->type;
		} break;
	case AST_TYPE_FUNC:
		{
			ast_node *param_list = node->children;
			ast_node *return_type = param_list->next;

			node->type = type_create(TYPE_FUNCTION, arena);
			member **m = &node->type->members;
			for (ast_node *param = param_list->children; param != AST_NIL; param = param->next) {
				check_type(param, arena, error);
				*m = ALLOC(arena, 1, member);
				(*m)->name = param->value.s;
				(*m)->type = param->type;
				m = &(*m)->next;
			}

			check_type(return_type, arena, error);
			node->type->children = return_type->type;
		} break;
	case AST_TYPE_STRUCT:
		{
			// TODO
		} break;
	case AST_TYPE_STRUCT_DEF:
		{
			// TODO: add the struct tag to the scope and ensure that the
			// struct is only defined once.
			node->type = type_create(TYPE_STRUCT, arena);

			// TODO: Collect the members of the struct
			for (ast_node *child = node->children; child != AST_NIL; child = child->next) {
				check_type(child, arena, error);
			}
		} break;
	}
}

static symbol_table
analyze(ast_node *root, arena *perm, b32 *error)
{
	symbol_table table = {0};
	scope *s = new_scope(NULL, perm);
	s->count = &table.count;

	merge_identifiers(root, s, perm, error);
	check_type(root, perm, error);
	return table;
}
