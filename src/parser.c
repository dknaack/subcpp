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

static struct string
parse_identifier(struct tokenizer *tokenizer)
{
	struct token token = get_token(tokenizer);
	if (token.kind != TOKEN_IDENTIFIER) {
		/* TODO: report error */
		ASSERT(!"Syntax error");
	}

	return token.value;
}

static struct expr * parse_assign_expr(struct tokenizer *tokenizer, struct arena *arena);

static struct expr *
parse_expr(struct tokenizer *tokenizer, int prev_precedence, struct arena *arena)
{
	struct expr *expr;

	struct token token = peek_token(tokenizer);
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

		if (token.kind == TOKEN_LPAREN) {
			get_token(tokenizer);
			struct expr *parameter = NULL;
			if (!accept(tokenizer, TOKEN_RPAREN)) {
				parameter = parse_assign_expr(tokenizer, arena);
				expect(tokenizer, TOKEN_RPAREN);
			}
			struct expr *called = expr;
			expr = ALLOC(arena, 1, struct expr);
			expr->kind = EXPR_CALL;
			expr->u.call.called = called;
			expr->u.call.parameter = parameter;
		} else {
			int precedence = get_binary_precedence(token.kind);
			if (precedence == 0) {
				/* TODO: report syntax error */
				ASSERT(!"Invalid expression");
				return NULL;
			}

			bool is_right_associative = (precedence < 0);
			if (is_right_associative) {
				precedence = -precedence;
			}

			if (precedence <= prev_precedence) {
				break;
			}

			get_token(tokenizer);

			/* NOTE: Ensure that right associative expressions are
			 * parsed correctly. */
			precedence -= is_right_associative;
			struct expr *lhs = expr;
			struct expr *rhs = parse_expr(tokenizer, precedence, arena);

			expr = ALLOC(arena, 1, struct expr);
			expr->kind = EXPR_BINARY;
			expr->u.binary.op = token.kind;
			expr->u.binary.lhs = lhs;
			expr->u.binary.rhs = rhs;
		}
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

enum parse_decl_flags {
	PARSE_SINGLE_DECL = (1 << 0),
};

static struct decl *
parse_decl(struct tokenizer *tokenizer, uint32_t flags, struct arena *arena)
{
	struct decl *decl;
	struct decl **ptr = &decl;
	expect(tokenizer, TOKEN_INT);

	do {
		*ptr = ZALLOC(arena, 1, struct decl);
		(*ptr)->name = parse_identifier(tokenizer);
		if (accept(tokenizer, TOKEN_ASSIGN)) {
			(*ptr)->expr = parse_assign_expr(tokenizer, arena);
		}
		ptr = &(*ptr)->next;
	} while (!(flags & PARSE_SINGLE_DECL) && accept(tokenizer, TOKEN_COMMA));

	return decl;
}

static struct stmt *parse_stmt(struct tokenizer *tokenizer, struct arena *arena);

static struct stmt *
parse_compound_stmt(struct tokenizer *tokenizer, struct arena *arena)
{
	struct stmt *head, **ptr = &head;

	expect(tokenizer, TOKEN_LBRACE);
	while (!accept(tokenizer, TOKEN_RBRACE)) {
		*ptr = parse_stmt(tokenizer, arena);
		ptr = &(*ptr)->next;
	}

	return head;
}

static struct stmt *
parse_stmt(struct tokenizer *tokenizer, struct arena *arena)
{
	struct stmt *stmt;

	struct token token = peek_token(tokenizer);
	switch (token.kind) {
	case TOKEN_BREAK:
		get_token(tokenizer);
		stmt = ZALLOC(arena, 1, struct stmt);
		stmt->kind = STMT_BREAK;
		expect(tokenizer, TOKEN_SEMICOLON);
		break;
	case TOKEN_CONTINUE:
		get_token(tokenizer);
		stmt = ZALLOC(arena, 1, struct stmt);
		stmt->kind = STMT_CONTINUE;
		expect(tokenizer, TOKEN_SEMICOLON);
		break;
	case TOKEN_FOR:
		get_token(tokenizer);
		stmt = ZALLOC(arena, 1, struct stmt);
		expect(tokenizer, TOKEN_LPAREN);

		if (!accept(tokenizer, TOKEN_SEMICOLON)) {
			token = peek_token(tokenizer);
			if (token.kind == TOKEN_INT) {
				stmt->kind = STMT_FOR_DECL;
				stmt->u._for.init.decl = parse_decl(tokenizer, 0, arena);
			} else {
				stmt->kind = STMT_FOR_EXPR;
				stmt->u._for.init.expr = parse_assign_expr(tokenizer, arena);
			}

			expect(tokenizer, TOKEN_SEMICOLON);
		}

		if (!accept(tokenizer, TOKEN_SEMICOLON)) {
			stmt->u._for.condition = parse_assign_expr(tokenizer, arena);
			expect(tokenizer, TOKEN_SEMICOLON);
		}

		if (!accept(tokenizer, TOKEN_RPAREN)) {
			stmt->u._for.post = parse_assign_expr(tokenizer, arena);
			expect(tokenizer, TOKEN_RPAREN);
		}

		stmt->u._for.body = parse_stmt(tokenizer, arena);
		break;
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
		if (!accept(tokenizer, TOKEN_SEMICOLON)) {
			stmt->u.expr = parse_assign_expr(tokenizer, arena);
			expect(tokenizer, TOKEN_SEMICOLON);
		}
		break;
	case TOKEN_PRINT:
		get_token(tokenizer);
		stmt = ZALLOC(arena, 1, struct stmt);
		stmt->kind = STMT_PRINT;
		stmt->u.expr = parse_assign_expr(tokenizer, arena);
		expect(tokenizer, TOKEN_SEMICOLON);
		break;
	case TOKEN_SEMICOLON:
		get_token(tokenizer);
		stmt = ZALLOC(arena, 1, struct stmt);
		stmt->kind = STMT_EMPTY;
		break;
	case TOKEN_LBRACE:
		stmt = ZALLOC(arena, 1, struct stmt);
		stmt->kind = STMT_COMPOUND;
		stmt->u.compound = parse_compound_stmt(tokenizer, arena);
		break;
	case TOKEN_INT:
		stmt = ZALLOC(arena, 1, struct stmt);
		stmt->kind = STMT_DECL;
		stmt->u.decl = parse_decl(tokenizer, 0, arena);
		expect(tokenizer, TOKEN_SEMICOLON);
		break;
	default:
		stmt = ZALLOC(arena, 1, struct stmt);
		stmt->kind = STMT_EXPR;
		stmt->u.expr = parse_assign_expr(tokenizer, arena);
		expect(tokenizer, TOKEN_SEMICOLON);
	}

	return stmt;
}

static struct function *
parse_function(struct tokenizer *tokenizer, struct arena *arena)
{
	struct function *function = ZALLOC(arena, 1, struct function);

	expect(tokenizer, TOKEN_INT);
	struct token token = get_token(tokenizer);
	if (token.kind != TOKEN_IDENTIFIER) {
		ASSERT(!"Expected identifier");
	}

	function->name = token.value;
	expect(tokenizer, TOKEN_LPAREN);
	token = peek_token(tokenizer);
	if (token.kind == TOKEN_INT) {
		function->parameter = parse_decl(tokenizer, PARSE_SINGLE_DECL, arena);
	} else if (token.kind == TOKEN_VOID) {
		get_token(tokenizer);
	}
	expect(tokenizer, TOKEN_RPAREN);
	function->body = parse_compound_stmt(tokenizer, arena);

	return function;
}

static struct function *
parse(struct tokenizer *tokenizer, struct arena *arena)
{
	struct function *head, **ptr = &head;
	while (!accept(tokenizer, TOKEN_EOF)) {
		*ptr = parse_function(tokenizer, arena);
		ptr = &(*ptr)->next;
	}

	return head;
}
