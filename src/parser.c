static struct expr *
parse_primary_expr(struct tokenizer *tokenizer, struct arena *arena)
{
	struct token token;
	struct expr *expr = NULL;

	token = peek_token(tokenizer);
	if (token.kind == TOKEN_IDENTIFIER) {
		get_token(tokenizer);
		expr = ALLOC(arena, 1, struct expr);
		expr->kind = EXPR_IDENTIFIER;
		expr->u.identifier = token.value;
	}

	return expr;
}

// NOTE: An operator with negative precedence is right associative.
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

static struct expr *
parse_expr(struct tokenizer *tokenizer, int prev_precedence, struct arena *arena)
{
	int precedence, right_associative;
	struct expr *expr, *tmp;
	struct token token;

	if (!(expr = parse_primary_expr(tokenizer, arena))) {
		return NULL;
	}

	for (;;) {
		token = peek_token(tokenizer);
		if (token.kind == TOKEN_EOF) {
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
	struct expr *expr = parse_expr(tokenizer, 0, arena);
	return expr;
}
