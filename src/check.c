static void
add_variable(symbol_table *table, string name, type *type, arena *arena)
{
	symbol *variable = table->free_symbols;
	if (variable) {
		table->free_symbols = variable->next;
	} else {
		variable = ALLOC(arena, 1, symbol);
	}

	ASSERT(name.at == scope_marker.at || type->kind != TYPE_VOID);
	variable->name = name;
	variable->type = type;
	variable->next = table->symbols;
	table->symbols = variable;
}

static type *
get_variable(symbol_table *table, string name)
{
	type *type = &type_void;

	for (symbol *symbol = table->symbols; symbol; symbol = symbol->next) {
		if (string_equals(symbol->name, name)) {
			type = symbol->type;
			break;
		}
	}

	return type;
}

static void
push_scope(symbol_table *table, arena *arena)
{
	add_variable(table, scope_marker, &type_void, arena);
}

static void
pop_scope(symbol_table *table)
{
	symbol *symbol;

	for (symbol = table->symbols; symbol; symbol = table->symbols) {
		table->symbols = symbol->next;
		symbol->next = table->free_symbols;
		table->free_symbols = symbol;

		if (symbol->name.at == scope_marker.at) {
			break;
		}
	}
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
	type *lhs, *rhs, *type, *operand_type, **param_type;
	ast_node *param;
	if (!node) {
		return &type_void;
	}

	ast_node *orig_node = node;
	type = &type_void;
	switch (node->kind) {
	case AST_INVALID:
		ASSERT(!"Invalid node");
		break;
	case AST_ROOT:
		type = &type_void;
		for (node = node->u.children; node; node = node->next) {
			check_node(node, symbols, arena);
		}
		break;
	case AST_EXPR_BINARY:
		lhs = check_node(node->u.bin_expr.lhs, symbols, arena);
		rhs = check_node(node->u.bin_expr.rhs, symbols, arena);
		if (!type_equals(lhs, rhs)) {
			errorf(node->loc, "Incompatible types: %s, %s",
				type_get_name(lhs->kind), type_get_name(rhs->kind));
		}

		type = lhs;
		break;
	case AST_EXPR_CALL:
		type = check_node(node->u.call_expr.called, symbols, arena);
		if (type->kind != TYPE_FUNCTION) {
			errorf(node->loc, "Not a function: %s", type_get_name(type->kind));
			break;
		}

		rhs = type->u.function.param_types;
		for (param = node->u.call_expr.params; param; param = param->next) {
			lhs = check_node(param, symbols, arena);
			if (!type_equals(lhs, rhs)) {
				errorf(node->loc, "Incompatible types: %s, %s",
					type_get_name(lhs->kind), type_get_name(rhs->kind));
			}

			rhs = rhs->next;
		}

		type = type->u.function.return_type;
		break;
	case AST_EXPR_IDENT:
		type = get_variable(symbols, node->u.ident);
		break;
	case AST_EXPR_UNARY:
		operand_type = check_node(node->u.unary_expr.operand, symbols, arena);
		switch (node->u.unary_expr.op) {
		case TOKEN_MUL:
			if (operand_type->kind == TYPE_POINTER) {
				type = operand_type->u.pointer.target;
			} else {
				errorf(node->loc, "Expected pointer type");
			}
			break;
		case TOKEN_AMPERSAND:
			type = type_create(TYPE_POINTER, arena);
			type->u.pointer.target = operand_type;
			break;
		default:
			ASSERT(!"Invalid operator");
			break;
		}
		break;
	case AST_STMT_BREAK:
		type = type_create(TYPE_VOID, arena);
		break;
	case AST_STMT_COMPOUND:
		type = type_create(TYPE_VOID, arena);
		push_scope(symbols, arena);
		for (node = node->u.children; node; node = node->next) {
			check_node(node, symbols, arena);
		}

		pop_scope(symbols);
		break;
	case AST_DECL:
		lhs = check_node(node->u.decl.type, symbols, arena);
		if (node->u.decl.expr) {
			rhs = check_node(node->u.decl.expr, symbols, arena);
			if (!type_equals(lhs, rhs)) {
				errorf(node->loc, "Incompatible types: %s, %s",
					type_get_name(lhs->kind), type_get_name(rhs->kind));
			}
		}

		add_variable(symbols, node->u.decl.name, lhs, arena);
		type = lhs;
		break;
	case AST_STMT_DECL:
		type = type_create(TYPE_VOID, arena);
		for (node = node->u.children; node; node = node->next) {
			check_node(node, symbols, arena);
		}

		break;
	case AST_STMT_CONTINUE:
		type = &type_void;
		break;
	case AST_STMT_EMPTY:
		type = &type_void;
		break;
	case AST_STMT_FOR:
		type = &type_void;
		check_node(node->u.for_stmt.init, symbols, arena);
		check_node(node->u.for_stmt.cond, symbols, arena);
		check_node(node->u.for_stmt.post, symbols, arena);
		check_node(node->u.for_stmt.body, symbols, arena);
		break;
	case AST_STMT_IF:
		type = &type_void;
		check_node(node->u.if_stmt.cond, symbols, arena);
		check_node(node->u.if_stmt.then, symbols, arena);
		check_node(node->u.if_stmt.otherwise, symbols, arena);
		break;
	case AST_STMT_PRINT:
		type = &type_void;
		check_node(node->u.children, symbols, arena);
		break;
	case AST_STMT_WHILE:
		type = &type_void;
		check_node(node->u.while_stmt.cond, symbols, arena);
		check_node(node->u.while_stmt.body, symbols, arena);
		break;
	case AST_STMT_RETURN:
		type = &type_void;
		check_node(node->u.children, symbols, arena);
		break;
	case AST_FUNCTION:
		type = type_create(TYPE_FUNCTION, arena);
		type->u.function.return_type = type_create(TYPE_INT, arena);
		param_type = &type->u.function.param_types;
		for (param = node->u.function.params; param; param = param->next) {
			*param_type = check_node(param, symbols, arena);
			param_type = &(*param_type)->next;
		}

		add_variable(symbols, node->u.function.name, type, arena);
		push_scope(symbols, arena);
		param_type = &type->u.function.param_types;
		for (param = node->u.function.params; param; param = param->next) {
			add_variable(symbols, param->u.decl.name, *param_type, arena);
			param_type = &(*param_type)->next;
		}

		for (node = node->u.function.body; node; node = node->next) {
			check_node(node, symbols, arena);
		}
		pop_scope(symbols);
		break;
	case AST_TYPE_POINTER:
		type = type_create(TYPE_POINTER, arena);
		type->u.pointer.target = check_node(node->u.pointer_type.target, symbols, arena);
		break;
	case AST_TYPE_VOID:
		type = type_create(TYPE_VOID, arena);
		break;
	case AST_TYPE_CHAR:
		type = type_create(TYPE_CHAR, arena);
		break;
	case AST_TYPE_INT:
	case AST_EXPR_INT:
		type = type_create(TYPE_INT, arena);
		break;
	}

	orig_node->type = type;
	return type;
}
