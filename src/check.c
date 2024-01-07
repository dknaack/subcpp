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

static void
check_type(ast_node *node, symbol_table *symbols, arena *arena)
{
	if (!node) {
		return;
	}

	switch (node->kind) {
	case AST_INVALID:
		ASSERT(!"Invalid node");
		break;
	case AST_ROOT:
		{
			for (ast_node *child = node->children; child != AST_NIL; child = child->next) {
				check_type(child, symbols, arena);
			}
		} break;
	case AST_EXPR_BINARY:
		{
			ast_node *lhs = node->u.bin_expr.lhs;
			ast_node *rhs = node->u.bin_expr.rhs;
			check_type(lhs, symbols, arena);

			u32 operator = node->u.bin_expr.op;
			if (operator == TOKEN_DOT) {
				if (lhs->type->kind != TYPE_STRUCT) {
					errorf(node->loc, "Left-hand side is not a struct");
				}

				if (node->u.bin_expr.rhs->kind != AST_EXPR_IDENT) {
					errorf(node->loc, "Right-hand side is not an identifier");
				}

				symbol *s = upsert_symbol(&lhs->type->u.members, rhs->u.ident, NULL);
				rhs->type = s->type;
			} else {
				check_type(rhs, symbols, arena);
			}

			if (operator == TOKEN_ASSIGN) {
				// TODO: type conversion
			} else if (operator == TOKEN_LBRACKET) {
				// TODO: ensure that one operand is a pointer and the other one
				// is an integral type.
				ASSERT(lhs->type->kind != TYPE_POINTER || rhs->type->kind == TYPE_POINTER);
				ASSERT(lhs->type->kind == TYPE_POINTER || rhs->type->kind != TYPE_POINTER);
			} else if (operator != TOKEN_DOT) {
				if (!type_equals(lhs->type, rhs->type)) {
					errorf(node->loc, "Incompatible types: %s, %s",
						type_get_name(lhs->type->kind),
						type_get_name(rhs->type->kind));
				}

				node->type = lhs->type;
			}
		} break;
	case AST_EXPR_CALL:
		{
			ast_node *called = node->u.call_expr.called;
			check_type(called, symbols, arena);
			if (called->type->kind != TYPE_FUNCTION) {
				errorf(node->loc, "Not a function: %s", type_get_name(called->type->kind));
				break;
			}

			u32 param_index = 0;
			ast_node *param = node->u.call_expr.params;
			type *param_type = called->type->u.function.param_types;
			while (param || param_type) {
				check_type(param, symbols, arena);
				if (!type_equals(param_type, param->type)) {
					errorf(node->loc, "Parameter %d has wrong type: Expected %s, but found %s",
						param_index + 1, type_get_name(param_type->kind),
						type_get_name(param->type->kind));
				}

				param_type = param_type->next;
				param_index++;
			}

			node->type = called->type->u.function.return_type;
		} break;
	case AST_EXPR_IDENT:
		{
			node->type = get_variable(symbols, node->u.ident);
		} break;
	case AST_EXPR_UNARY:
		{
			ast_node *operand = node->u.unary_expr.operand;
			check_type(operand, symbols, arena);
			switch (node->u.unary_expr.op) {
			case TOKEN_MUL:
				if (operand->type->kind == TYPE_POINTER) {
					node->type = operand->type->u.pointer.target;
				} else {
					errorf(node->loc, "Expected pointer type");
				}
				break;
			case TOKEN_AMPERSAND:
				node->type = type_create(TYPE_POINTER, arena);
				node->type->u.pointer.target = operand->type;
				break;
			default:
				ASSERT(!"Invalid operator");
				break;
			}
		} break;
	case AST_STMT_BREAK:
		break;
	case AST_STMT_COMPOUND:
		{
			arena_temp temp = arena_temp_begin(arena);
			push_scope(symbols, arena);
			for (ast_node *child = node->children; child != AST_NIL; child = child->next) {
				check_type(child, symbols, arena);
			}

			pop_scope(symbols);
			arena_temp_end(temp);
		} break;
	case AST_DECL:
		{
			ast_node *type_specifier = node->u.decl.type_specifier;
			check_type(type_specifier, symbols, arena);

			for (ast_node *child = node->u.decl.list; child != AST_NIL; child = child->next) {
				ast_node *declarator = child->u.decl_list.declarator;
				type *decl_type = type_specifier->type;
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
			for (ast_node *child = node->children; child != AST_NIL; child = child->next) {
				check_type(child, symbols, arena);
			}
		} break;
	case AST_STMT_CONTINUE:
	case AST_STMT_EMPTY:
		break;
	case AST_STMT_FOR:
		{
			for (ast_node *child = node->children; child != &ast_nil; child = child->next) {
				check_type(child, symbols, arena);
			}
		} break;
	case AST_STMT_IF:
		{
			check_type(node->u.if_stmt.cond, symbols, arena);
			check_type(node->u.if_stmt.then, symbols, arena);
			check_type(node->u.if_stmt.otherwise, symbols, arena);
		} break;
	case AST_STMT_PRINT:
		{
			check_type(node->children, symbols, arena);
		} break;
	case AST_STMT_WHILE:
		{
			check_type(node->u.while_stmt.cond, symbols, arena);
			check_type(node->u.while_stmt.body, symbols, arena);
		} break;
	case AST_STMT_RETURN:
		{
			check_type(node->children, symbols, arena);
		} break;
	case AST_FUNCTION:
		{
			node->type = type_create(TYPE_FUNCTION, arena);
			node->type->u.function.return_type = type_create(TYPE_INT, arena);
			push_scope(symbols, arena);
			type **param_type = &node->type->u.function.param_types;
			for (ast_node *param = node->u.function.params; param; param = param->next) {
				check_type(param, symbols, arena);
				*param_type = param->type;
				param_type = &(*param_type)->next;
			}

			add_variable(symbols, node->u.function.name, node->type, arena);

			for (ast_node *child = node->u.function.body; child != AST_NIL; child = child->next) {
				check_type(child, symbols, arena);
			}

			pop_scope(symbols);
		} break;
	case AST_TYPE_VOID:
		{
			node->type = &type_void;
		} break;
	case AST_TYPE_CHAR:
		{
			node->type = &type_char;
		} break;
	case AST_TYPE_INT:
	case AST_EXPR_INT:
		{
			node->type = &type_int;
		} break;
	case AST_TYPE_STRUCT:
		{
			// TODO
		} break;
	case AST_TYPE_STRUCT_DEF:
	case AST_TYPE_STRUCT_ANON:
		{
			node->type = type_create(TYPE_STRUCT, arena);

			push_scope(symbols, arena);
			for (ast_node *child = node->children; child != AST_NIL; child = child->next) {
				check_type(child, symbols, arena);
			}

			node->type->u.members = symbols->head;
			pop_scope(symbols);
		} break;
	}
}
