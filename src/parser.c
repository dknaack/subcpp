static bool
accept(struct tokenizer *tokenizer, enum token_kind expected_token)
{
	struct token token = peek_token(tokenizer);
	if (token.kind == expected_token) {
		get_token(tokenizer);
		return true;
	} else {
		return false;
	}
}

static void
expect(struct tokenizer *tokenizer, enum token_kind expected_token)
{
	struct token token = get_token(tokenizer);
	if (token.kind != expected_token) {
		/* TODO: report syntax error */
		ASSERT(!"Syntax error");
	}
}

/* NOTE: An operator with negative precedence is right associative. */
static int
get_binary_precedence(enum token_kind token)
{
	switch (token) {
	case TOKEN_ASSIGN:
		return -10;
	case TOKEN_ADD:
	case TOKEN_SUB:
		return 20;
	case TOKEN_MUL:
	case TOKEN_DIV:
	case TOKEN_MOD:
		return 30;
	default:
		return 0;
	}
}

static intmax_t
parse_int(struct string str)
{
	intmax_t ival = 0;

	while (str.length-- > 0) {
		ival *= 10;
		ival += (*str.at++ - '0');
	}

	return ival;
}

static struct expr *
parse_expr(struct tokenizer *tokenizer, int prev_precedence, struct arena *arena)
{
	int precedence, right_associative;
	struct expr *expr, *tmp;
	struct token token;

	token = peek_token(tokenizer);
	switch (token.kind) {
	case TOKEN_IDENTIFIER:
		get_token(tokenizer);
		expr = ALLOC(arena, 1, struct expr);
		expr->kind = EXPR_IDENTIFIER;
		expr->u.identifier = token.value;
		break;
	case TOKEN_LITERAL_INT:
		get_token(tokenizer);
		expr = ALLOC(arena, 1, struct expr);
		expr->kind = EXPR_INT;
		expr->u.ival = parse_int(token.value);
		break;
	case TOKEN_LPAREN:
		get_token(tokenizer);
		expr = parse_expr(tokenizer, 0, arena);
		expect(tokenizer, TOKEN_RPAREN);
		break;
	default:
		return NULL;
	}

	for (;;) {
		token = peek_token(tokenizer);
		if (token.kind == TOKEN_EOF
		    || token.kind == TOKEN_SEMICOLON
		    || token.kind == TOKEN_RPAREN) {
			break;
		}

		precedence = get_binary_precedence(token.kind);
		if (precedence == 0) {
			/* TODO: report syntax error */
			ASSERT(!"Invalid expression");
			return NULL;
		}

		right_associative = (precedence < 0);
		if (right_associative) {
			precedence = -precedence;
		}

		if (precedence <= prev_precedence) {
			break;
		}

		get_token(tokenizer);
		tmp = expr;
		/* Ensure that right associative exprs are parsed correctly. */
		precedence -= right_associative;

		expr = ALLOC(arena, 1, struct expr);
		expr->kind = EXPR_BINARY;
		expr->u.binary.op = token.kind;
		expr->u.binary.lhs = tmp;
		expr->u.binary.rhs = parse_expr(tokenizer, precedence, arena);
	}

	return expr;
}

static struct expr *
parse_assign_expr(struct tokenizer *tokenizer, struct arena *arena)
{
	struct expr *expr;
	int precedence;

	precedence = get_binary_precedence(TOKEN_ASSIGN);
	expr = parse_expr(tokenizer, -precedence - 5, arena);
	return expr;
}

static struct stmt *
parse_stmt(struct tokenizer *tokenizer, struct arena *arena)
{
	struct token token;
	struct stmt *stmt, **ptr;

	token = peek_token(tokenizer);
	switch (token.kind) {
	case TOKEN_IF:
		get_token(tokenizer);
		stmt = ZALLOC(arena, 1, struct stmt);
		stmt->kind = STMT_IF;
		expect(tokenizer, TOKEN_LPAREN);
		stmt->u._if.condition = parse_assign_expr(tokenizer, arena);
		expect(tokenizer, TOKEN_RPAREN);
		stmt->u._if.then = parse_stmt(tokenizer, arena);
		if (accept(tokenizer, TOKEN_ELSE)) {
			stmt->u._if.otherwise = parse_stmt(tokenizer, arena);
		}
		break;
	case TOKEN_WHILE:
		get_token(tokenizer);
		stmt = ZALLOC(arena, 1, struct stmt);
		stmt->kind = STMT_WHILE;
		expect(tokenizer, TOKEN_LPAREN);
		stmt->u._while.condition = parse_assign_expr(tokenizer, arena);
		expect(tokenizer, TOKEN_RPAREN);
		stmt->u._while.body = parse_stmt(tokenizer, arena);
		break;
	case TOKEN_RETURN:
		get_token(tokenizer);
		stmt = ZALLOC(arena, 1, struct stmt);
		stmt->kind = STMT_RETURN;
		stmt->u.expr = parse_assign_expr(tokenizer, arena);
		expect(tokenizer, TOKEN_SEMICOLON);
		break;
	case TOKEN_SEMICOLON:
		get_token(tokenizer);
		stmt = ZALLOC(arena, 1, struct stmt);
		stmt->kind = STMT_EMPTY;
		break;
	case TOKEN_LBRACE:
		get_token(tokenizer);
		stmt = ZALLOC(arena, 1, struct stmt);
		stmt->kind = STMT_COMPOUND;
		ptr = &stmt->u.compound;
		while (!accept(tokenizer, TOKEN_RBRACE)) {
			*ptr = parse_stmt(tokenizer, arena);
			ptr = &(*ptr)->next;
		}
		break;
	default:
		stmt = ZALLOC(arena, 1, struct stmt);
		stmt->kind = STMT_EXPR;
		stmt->u.expr = parse_assign_expr(tokenizer, arena);
		expect(tokenizer, TOKEN_SEMICOLON);
	}

	return stmt;
}
