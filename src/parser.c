#include <stdarg.h>

static void
vsyntax_error(struct tokenizer *tokenizer, char *fmt, va_list ap)
{
	uint32_t line = 1;
	uint32_t column = 0;

	if (tokenizer->error) {
		return;
	}

	for (uint32_t pos = 0; pos < tokenizer->pos; pos++) {
		if (tokenizer->source.at[pos] == '\n') {
			line++;
			column = 0;
		}

		column++;
	}

	fprintf(stderr, "%s:%d:%d: ", tokenizer->filename, line, column);
	vfprintf(stderr, fmt, ap);
	fputc('\n', stderr);

	tokenizer->error = true;
}

static void
syntax_error(struct tokenizer *tokenizer, char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	vsyntax_error(tokenizer, fmt, ap);
	va_end(ap);
}

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
		syntax_error(tokenizer, "Expected %s, but found %s",
		    get_token_name(expected_token), get_token_name(token.kind));
	}
}

/* NOTE: An operator with negative precedence is right associative. */
static int
get_binary_precedence(enum token_kind token)
{
	switch (token) {
	case TOKEN_COMMA:
		return -5;
	case TOKEN_ASSIGN:
		return -10;
	case TOKEN_ADD:
	case TOKEN_SUB:
		return 20;
	case TOKEN_MUL:
	case TOKEN_DIV:
	case TOKEN_MOD:
		return 30;
	case TOKEN_EQUALS:
	case TOKEN_LT:
	case TOKEN_GT:
	case TOKEN_LEQ:
	case TOKEN_GEQ:
		return 40;
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
parse_ident(struct tokenizer *tokenizer)
{
	static struct string empty = {"", 0};
	struct token token = get_token(tokenizer);
	if (token.kind != TOKEN_IDENT) {
		syntax_error(tokenizer, "Expected identifier, but found %s",
		    get_token_name(token.kind));
		return empty;
	}

	return token.value;
}

static struct ast_node * parse_assign_expr(struct tokenizer *tokenizer, struct arena *arena);

static struct ast_node *
parse_expr(struct tokenizer *tokenizer, int prev_precedence, struct arena *arena)
{
	struct ast_node *expr;

	struct token token = peek_token(tokenizer);
	switch (token.kind) {
	case TOKEN_IDENT:
		get_token(tokenizer);
		expr = ALLOC(arena, 1, struct ast_node);
		expr->kind = AST_IDENT;
		expr->u.ident = token.value;
		break;
	case TOKEN_LITERAL_INT:
		get_token(tokenizer);
		expr = ALLOC(arena, 1, struct ast_node);
		expr->kind = AST_INT;
		expr->u.ival = parse_int(token.value);
		break;
	case TOKEN_LPAREN:
		get_token(tokenizer);
		expr = parse_expr(tokenizer, 0, arena);
		expect(tokenizer, TOKEN_RPAREN);
		break;
	default:
		syntax_error(tokenizer, "Expected expression");
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
			struct ast_node *parameters = NULL;
			struct ast_node **ptr = &parameters;
			if (!accept(tokenizer, TOKEN_RPAREN)) {
				do {
					*ptr = parse_assign_expr(tokenizer, arena);
					ptr = &(*ptr)->next;
				} while (accept(tokenizer, TOKEN_COMMA));
				expect(tokenizer, TOKEN_RPAREN);
			}

			struct ast_node *called = expr;
			expr = ALLOC(arena, 1, struct ast_node);
			expr->kind = AST_CALL;
			expr->u.call_expr.called = called;
			expr->u.call_expr.parameters = parameters;
		} else {
			int precedence = get_binary_precedence(token.kind);
			if (precedence == 0) {
				syntax_error(tokenizer, "Expected operator");
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
			struct ast_node *lhs = expr;
			struct ast_node *rhs = parse_expr(tokenizer, precedence, arena);

			expr = ALLOC(arena, 1, struct ast_node);
			expr->kind = AST_BINARY;
			expr->u.bin_expr.op = token.kind;
			expr->u.bin_expr.lhs = lhs;
			expr->u.bin_expr.rhs = rhs;
		}
	}

	return expr;
}

static struct ast_node *
parse_assign_expr(struct tokenizer *tokenizer, struct arena *arena)
{
	struct ast_node *expr;
	int precedence;

	precedence = get_binary_precedence(TOKEN_ASSIGN);
	expr = parse_expr(tokenizer, -precedence - 5, arena);
	return expr;
}

enum parse_decl_flags {
	PARSE_SINGLE_DECL = (1 << 0),
};

static struct ast_node *
parse_decl(struct tokenizer *tokenizer, uint32_t flags, struct arena *arena)
{
	struct ast_node *decl;
	struct ast_node **ptr = &decl;
	expect(tokenizer, TOKEN_INT);

	do {
		*ptr = ZALLOC(arena, 1, struct ast_node);
		(*ptr)->kind = AST_DECL;
		(*ptr)->u.decl.name = parse_ident(tokenizer);
		if (accept(tokenizer, TOKEN_ASSIGN)) {
			(*ptr)->u.decl.expr = parse_assign_expr(tokenizer, arena);
		}
		ptr = &(*ptr)->next;
	} while (!tokenizer->error
	    && !(flags & PARSE_SINGLE_DECL)
	    && accept(tokenizer, TOKEN_COMMA));

	return decl;
}

static struct ast_node *parse_stmt(struct tokenizer *tokenizer, struct arena *arena);

static struct ast_node *
parse_compound_stmt(struct tokenizer *tokenizer, struct arena *arena)
{
	struct ast_node *head, **ptr = &head;

	expect(tokenizer, TOKEN_LBRACE);
	while (!tokenizer->error && !accept(tokenizer, TOKEN_RBRACE)) {
		*ptr = parse_stmt(tokenizer, arena);
		ptr = &(*ptr)->next;
	}

	return head;
}

static struct ast_node *
parse_stmt(struct tokenizer *tokenizer, struct arena *arena)
{
	struct ast_node *node = NULL;

	struct token token = peek_token(tokenizer);
	switch (token.kind) {
	case TOKEN_BREAK:
		get_token(tokenizer);
		node = ZALLOC(arena, 1, struct ast_node);
		node->kind = AST_BREAK;
		expect(tokenizer, TOKEN_SEMICOLON);
		break;
	case TOKEN_CONTINUE:
		get_token(tokenizer);
		node = ZALLOC(arena, 1, struct ast_node);
		node->kind = AST_CONTINUE;
		expect(tokenizer, TOKEN_SEMICOLON);
		break;
	case TOKEN_FOR:
		get_token(tokenizer);
		node = ZALLOC(arena, 1, struct ast_node);
		expect(tokenizer, TOKEN_LPAREN);

		if (!accept(tokenizer, TOKEN_SEMICOLON)) {
			token = peek_token(tokenizer);
			node->kind = AST_FOR;
			if (token.kind == TOKEN_INT) {
				node->u.for_stmt.init = parse_decl(tokenizer, 0, arena);
			} else {
				node->u.for_stmt.init = parse_assign_expr(tokenizer, arena);
			}

			expect(tokenizer, TOKEN_SEMICOLON);
		}

		if (!accept(tokenizer, TOKEN_SEMICOLON)) {
			node->u.for_stmt.cond = parse_assign_expr(tokenizer, arena);
			expect(tokenizer, TOKEN_SEMICOLON);
		}

		if (!accept(tokenizer, TOKEN_RPAREN)) {
			node->u.for_stmt.post = parse_assign_expr(tokenizer, arena);
			expect(tokenizer, TOKEN_RPAREN);
		}

		node->u.for_stmt.body = parse_stmt(tokenizer, arena);
		break;
	case TOKEN_IF:
		get_token(tokenizer);
		node = ZALLOC(arena, 1, struct ast_node);
		node->kind = AST_IF;
		expect(tokenizer, TOKEN_LPAREN);
		node->u.if_stmt.cond = parse_assign_expr(tokenizer, arena);
		expect(tokenizer, TOKEN_RPAREN);
		node->u.if_stmt.then = parse_stmt(tokenizer, arena);
		if (accept(tokenizer, TOKEN_ELSE)) {
			node->u.if_stmt.otherwise = parse_stmt(tokenizer, arena);
		}
		break;
	case TOKEN_WHILE:
		get_token(tokenizer);
		node = ZALLOC(arena, 1, struct ast_node);
		node->kind = AST_WHILE;
		expect(tokenizer, TOKEN_LPAREN);
		node->u.while_stmt.cond = parse_assign_expr(tokenizer, arena);
		expect(tokenizer, TOKEN_RPAREN);
		node->u.while_stmt.body = parse_stmt(tokenizer, arena);
		break;
	case TOKEN_RETURN:
		get_token(tokenizer);
		node = ZALLOC(arena, 1, struct ast_node);
		node->kind = AST_RETURN;
		if (!accept(tokenizer, TOKEN_SEMICOLON)) {
			node->u.children = parse_assign_expr(tokenizer, arena);
			expect(tokenizer, TOKEN_SEMICOLON);
		}
		break;
	case TOKEN_PRINT:
		get_token(tokenizer);
		node = ZALLOC(arena, 1, struct ast_node);
		node->kind = AST_PRINT;
		node->u.children = parse_assign_expr(tokenizer, arena);
		expect(tokenizer, TOKEN_SEMICOLON);
		break;
	case TOKEN_SEMICOLON:
		get_token(tokenizer);
		node = ZALLOC(arena, 1, struct ast_node);
		node->kind = AST_EMPTY;
		break;
	case TOKEN_LBRACE:
		node = ZALLOC(arena, 1, struct ast_node);
		node->kind = AST_COMPOUND;
		node->u.children = parse_compound_stmt(tokenizer, arena);
		break;
	case TOKEN_INT:
		node = ZALLOC(arena, 1, struct ast_node);
		node->kind = AST_DECL_STMT;
		node->u.children = parse_decl(tokenizer, 0, arena);
		expect(tokenizer, TOKEN_SEMICOLON);
		break;
	default:
		node = parse_assign_expr(tokenizer, arena);
		expect(tokenizer, TOKEN_SEMICOLON);
	}

	if (tokenizer->error) {
		tokenizer->error = false;
		while (!accept(tokenizer, TOKEN_SEMICOLON)
		    && !accept(tokenizer, TOKEN_RBRACE)) {
			get_token(tokenizer);
		}
	}

	return node;
}

static struct ast_node *
parse_function(struct tokenizer *tokenizer, struct arena *arena)
{
	struct ast_node *node = ZALLOC(arena, 1, struct ast_node);
	struct ast_node *parameters = NULL;
	struct ast_node **ptr = &parameters;
	struct string name;

	expect(tokenizer, TOKEN_INT);
	struct token token = get_token(tokenizer);
	if (token.kind != TOKEN_IDENT) {
		syntax_error(tokenizer, "Expected identifier");
	}

	name = token.value;
	expect(tokenizer, TOKEN_LPAREN);
	token = peek_token(tokenizer);
	if (token.kind == TOKEN_INT) {
		do {
			*ptr = parse_decl(tokenizer, PARSE_SINGLE_DECL, arena);
			ptr = &(*ptr)->next;
		} while (accept(tokenizer, TOKEN_COMMA));
	} else if (token.kind == TOKEN_VOID) {
		get_token(tokenizer);
	}
	expect(tokenizer, TOKEN_RPAREN);

	node->kind = AST_FUNCTION;
	node->u.function.body = parse_compound_stmt(tokenizer, arena);
	node->u.function.name = name;
	node->u.function.parameters = parameters;

	return node;
}

static struct ast_node *
parse(struct tokenizer *tokenizer, struct arena *arena)
{
	struct ast_node *root = ZALLOC(arena, 1, struct ast_node);
	root->kind = AST_ROOT;

	struct ast_node **ptr = &root->u.children;
	while (!tokenizer->error && !accept(tokenizer, TOKEN_EOF)) {
		*ptr = parse_function(tokenizer, arena);
		ptr = &(*ptr)->next;
	}

	return root;
}
