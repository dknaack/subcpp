static symbol *
upsert_symbol(symbol **p, string key, arena *perm)
{
	for (u64 h = hash(key); *p; h <<= 2) {
		if (string_equals(key, (*p)->name)) {
			return *p;
		}

		p = &(*p)->child[h >> 62];
	}

	if (!perm) {
		return NULL;
	}

	*p = ALLOC(perm, 1, symbol);
	(*p)->name = key;
	return *p;
}

static void
add_variable(symbol_table *table, string name, type *type, arena *arena)
{
	// TODO: report error when symbol is already in scope
	symbol *s = upsert_symbol(&table->head, name, arena);
	if (!s->type) {
		s->type = type;
		if (table->tail) {
			table->tail->next = s;
		}

		table->tail = s;
	}
}

static type *
get_variable(symbol_table *table, string name)
{
	while (table) {
		symbol *s = upsert_symbol(&table->head, name, NULL);
		if (s) {
			return s->type;
		}

		table = table->parent;
	}

	return &type_void;
}

static void
push_scope(symbol_table *table, arena *arena)
{
	symbol_table *parent = ALLOC(arena, 1, symbol_table);
	*parent = *table;
	table->parent = parent;
	table->head = table->tail = NULL;
}

static void
pop_scope(symbol_table *table)
{
	*table = *table->parent;
}

static b32
type_equals(type *lhs, type *rhs)
{
	if (lhs->kind == TYPE_VOID || rhs->kind == TYPE_VOID) {
		return false;
	}

	if (lhs->kind != rhs->kind) {
		return false;
	}

	switch (lhs->kind) {
	case TYPE_FUNCTION:
		if (!type_equals(lhs, rhs)) {
			return false;
		}

		lhs = lhs->u.function.param_types;
		rhs = rhs->u.function.param_types;
		while (lhs && rhs) {
			if (!type_equals(lhs, rhs)) {
				return false;
			}

			lhs = lhs->next;
			rhs = rhs->next;
		}

		b32 result = (lhs == NULL && rhs == NULL);
		return result;
	default:
		return true;
	}
}

static type *
check_node(ast_node *node, symbol_table *symbols, arena *arena)
{
	type *result = &type_void;
	if (!node) {
		return result;
	}

	switch (node->kind) {
	case AST_INVALID:
		ASSERT(!"Invalid node");
		break;
	case AST_ROOT:
		{
			result = &type_void;
			for (ast_node *child = node->u.children; child; child = child->next) {
				check_node(child, symbols, arena);
			}
		} break;
	case AST_EXPR_BINARY:
		{
			type *lhs = check_node(node->u.bin_expr.lhs, symbols, arena);
			type *rhs = check_node(node->u.bin_expr.rhs, symbols, arena);

			u32 operator = node->u.bin_expr.op;
			if (operator == TOKEN_ASSIGN) {
				// TODO: type conversion
			} else if (operator == TOKEN_LBRACKET) {
				// TODO: ensure that one operand is a pointer and the other one
				// is an integral type.
				ASSERT(lhs->kind != TYPE_POINTER || rhs->kind == TYPE_POINTER);
				ASSERT(lhs->kind == TYPE_POINTER || rhs->kind != TYPE_POINTER);
			} else {
				if (!type_equals(lhs, rhs)) {
					errorf(node->loc, "Incompatible types: %s, %s",
						type_get_name(lhs->kind), type_get_name(rhs->kind));
				}
			}

			result = lhs;
		} break;
	case AST_EXPR_CALL:
		{
			type *called = check_node(node->u.call_expr.called, symbols, arena);
			if (called->kind != TYPE_FUNCTION) {
				errorf(node->loc, "Not a function: %s", type_get_name(called->kind));
				break;
			}

			type *function_param_type = called->u.function.param_types;
			u32 param_index = 0;
			for (ast_node *param = node->u.call_expr.params; param; param = param->next) {
				type *expr_param_type = check_node(param, symbols, arena);
				if (!type_equals(function_param_type, expr_param_type)) {
					errorf(node->loc, "Parameter %d has wrong type: Expected %s, but found %s",
						param_index + 1,
						type_get_name(function_param_type->kind),
						type_get_name(expr_param_type->kind));
				}

				function_param_type = function_param_type->next;
				param_index++;
			}

			result = called->u.function.return_type;
		} break;
	case AST_EXPR_IDENT:
		{
			result = get_variable(symbols, node->u.ident);
		} break;
	case AST_EXPR_UNARY:
		{
			type *operand_type = check_node(node->u.unary_expr.operand, symbols, arena);
			switch (node->u.unary_expr.op) {
			case TOKEN_MUL:
				if (operand_type->kind == TYPE_POINTER) {
					result = operand_type->u.pointer.target;
				} else {
					errorf(node->loc, "Expected pointer type");
				}
				break;
			case TOKEN_AMPERSAND:
				result = type_create(TYPE_POINTER, arena);
				result->u.pointer.target = operand_type;
				break;
			default:
				ASSERT(!"Invalid operator");
				break;
			}
		} break;
	case AST_STMT_BREAK:
		result = &type_void;
		break;
	case AST_STMT_COMPOUND:
		{
			result = &type_void;
			push_scope(symbols, arena);
			for (ast_node *child = node->u.children; child; child = child->next) {
				check_node(child, symbols, arena);
			}

			pop_scope(symbols);
		} break;
	case AST_DECL:
		{
			result = &type_void;
			type *type_specifier = check_node(node->u.decl.type_specifier, symbols, arena);
			for (ast_node *child = node->u.decl.list; child; child = child->next) {
				ast_node *declarator = child->u.decl_list.declarator;
				type *decl_type = type_specifier;
				string name = {0};
				for (;;) {
					switch (declarator->kind) {
					case AST_DECL_IDENT:
						{
							name = declarator->u.ident;
							declarator->type = decl_type;
							goto end;
						} break;
					case AST_DECL_POINTER:
						{
							type *target = decl_type;
							decl_type = type_create(TYPE_POINTER, arena);
							decl_type->u.pointer.target = target;
							// TODO: add qualifiers to the pointer
							declarator = declarator->u.decl_pointer.declarator;
						} break;
					case AST_DECL_ARRAY:
						{
							type *target = decl_type;
							decl_type = type_create(TYPE_ARRAY, arena);
							decl_type->u.array.target = target;
							// TODO: Evaluate the array size of the declarator
							decl_type->u.array.size = 1;
							ast_node *size = declarator->u.decl_array.size;
							if (size->kind == AST_EXPR_INT) {
								decl_type->u.array.size = size->u.ival;
							}
							declarator = declarator->u.decl_array.declarator;
						} break;
					default:
						// TODO: report syntax error
						ASSERT(!"Invalid node in declarator");
						break;
					}
				}

end:
				ASSERT(name.at);
				add_variable(symbols, name, decl_type, arena);
			}
		} break;
	case AST_DECL_LIST:
	case AST_DECL_POINTER:
	case AST_DECL_ARRAY:
	case AST_DECL_IDENT:
		{
			// TODO: report syntax error when declarator is encountered here.
			ASSERT(!"Should be handled in decl");
		} break;
	case AST_STMT_DECL:
		{
			result = &type_void;
			for (ast_node *child = node->u.children; child; child = child->next) {
				check_node(child, symbols, arena);
			}
		} break;
	case AST_STMT_CONTINUE:
		{
			result = &type_void;
		} break;
	case AST_STMT_EMPTY:
		{
			result = &type_void;
		} break;
	case AST_STMT_FOR:
		{
			result = &type_void;
			check_node(node->u.for_stmt.init, symbols, arena);
			check_node(node->u.for_stmt.cond, symbols, arena);
			check_node(node->u.for_stmt.post, symbols, arena);
			check_node(node->u.for_stmt.body, symbols, arena);
		} break;
	case AST_STMT_IF:
		{
			result = &type_void;
			check_node(node->u.if_stmt.cond, symbols, arena);
			check_node(node->u.if_stmt.then, symbols, arena);
			check_node(node->u.if_stmt.otherwise, symbols, arena);
		} break;
	case AST_STMT_PRINT:
		{
			result = &type_void;
			check_node(node->u.children, symbols, arena);
		} break;
	case AST_STMT_WHILE:
		{
			result = &type_void;
			check_node(node->u.while_stmt.cond, symbols, arena);
			check_node(node->u.while_stmt.body, symbols, arena);
		} break;
	case AST_STMT_RETURN:
		{
			result = &type_void;
			check_node(node->u.children, symbols, arena);
		} break;
	case AST_FUNCTION:
		{
			result = type_create(TYPE_FUNCTION, arena);
			result->u.function.return_type = type_create(TYPE_INT, arena);
			push_scope(symbols, arena);
			type **param_type = &result->u.function.param_types;
			for (ast_node *param = node->u.function.params; param; param = param->next) {
				*param_type = check_node(param, symbols, arena);
				param_type = &(*param_type)->next;
			}

			add_variable(symbols, node->u.function.name, result, arena);

			for (ast_node *child = node->u.function.body; child; child = child->next) {
				check_node(child, symbols, arena);
			}

			pop_scope(symbols);
		} break;
	case AST_TYPE_VOID:
		{
			result = &type_void;
		} break;
	case AST_TYPE_CHAR:
		{
			result = &type_char;
		} break;
	case AST_TYPE_INT:
	case AST_EXPR_INT:
		{
			result = &type_int;
		} break;
	}

	return node->type = result;
}
