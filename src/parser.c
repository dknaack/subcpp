#include <stdarg.h>

static void
verrorf(location loc, char *fmt, va_list ap)
{
	fprintf(stderr, "%s:%d:%d: ", loc.file, loc.line-1, loc.column);
	vfprintf(stderr, fmt, ap);
	fputc('\n', stderr);
}

static void
errorf(location loc, char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	verrorf(loc, fmt, ap);
	va_end(ap);
}

static void
vsyntax_error(tokenizer *tokenizer, char *fmt, va_list ap)
{
	if (tokenizer->error) {
		return;
	}

	verrorf(tokenizer->loc, fmt, ap);
	tokenizer->error = true;
}

static void
syntax_error(tokenizer *tokenizer, char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	vsyntax_error(tokenizer, fmt, ap);
	va_end(ap);
}

static bool
accept(tokenizer *tokenizer, token_kind expected_token)
{
	token token = peek_token(tokenizer);
	if (token.kind == expected_token) {
		get_token(tokenizer);
		return true;
	} else {
		return false;
	}
}

static void
expect(tokenizer *tokenizer, token_kind expected_token)
{
	token token = get_token(tokenizer);
	if (token.kind != expected_token) {
		syntax_error(tokenizer, "Expected %s, but found %s",
		    get_token_name(expected_token), get_token_name(token.kind));
	}
}

static ast_node *
new_ast_node(ast_node_kind kind, location loc, arena *arena)
{
	ast_node *node = ZALLOC(arena, 1, ast_node);
	node->kind = kind;
	node->loc = loc;
	return node;
}

/* NOTE: An operator with negative precedence is right associative. */
static int
get_binary_precedence(token_kind token)
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
parse_int(string str)
{
	intmax_t ival = 0;

	while (str.length-- > 0) {
		ival *= 10;
		ival += (*str.at++ - '0');
	}

	return ival;
}

static string
parse_ident(tokenizer *tokenizer)
{
	static string empty = {"", 0};
	token token = get_token(tokenizer);
	if (token.kind != TOKEN_IDENT) {
		syntax_error(tokenizer, "Expected identifier, but found %s",
		    get_token_name(token.kind));
		return empty;
	}

	return token.value;
}

static ast_node *parse_assign_expr(tokenizer *tokenizer, arena *arena);

static ast_node *
parse_expr(tokenizer *tokenizer, int prev_precedence, arena *arena)
{
	ast_node *expr;

	token token = peek_token(tokenizer);
	switch (token.kind) {
	case TOKEN_IDENT:
		get_token(tokenizer);
		expr = new_ast_node(AST_EXPR_IDENT, tokenizer->loc, arena);
		expr->u.ident = token.value;
		break;
	case TOKEN_LITERAL_INT:
		get_token(tokenizer);
		expr = new_ast_node(AST_EXPR_INT, tokenizer->loc, arena);
		expr->u.ival = parse_int(token.value);
		break;
	case TOKEN_LPAREN:
		get_token(tokenizer);
		expr = parse_expr(tokenizer, 0, arena);
		expect(tokenizer, TOKEN_RPAREN);
		break;
	case TOKEN_MUL:
	case TOKEN_AMPERSAND:
		get_token(tokenizer);
		expr = new_ast_node(AST_EXPR_UNARY, tokenizer->loc, arena);
		expr->u.unary_expr.op = token.kind;
		expr->u.unary_expr.operand = parse_expr(tokenizer, 0, arena);
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
			ast_node *params = NULL;
			ast_node **ptr = &params;
			if (!accept(tokenizer, TOKEN_RPAREN)) {
				do {
					*ptr = parse_assign_expr(tokenizer, arena);
					ptr = &(*ptr)->next;
				} while (accept(tokenizer, TOKEN_COMMA));
				expect(tokenizer, TOKEN_RPAREN);
			}

			ast_node *called = expr;
			expr = new_ast_node(AST_EXPR_CALL, tokenizer->loc, arena);
			expr->u.call_expr.called = called;
			expr->u.call_expr.params = params;
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
			ast_node *lhs = expr;
			ast_node *rhs = parse_expr(tokenizer, precedence, arena);

			expr = new_ast_node(AST_EXPR_BINARY, tokenizer->loc, arena);
			expr->u.bin_expr.op = token.kind;
			expr->u.bin_expr.lhs = lhs;
			expr->u.bin_expr.rhs = rhs;
		}
	}

	return expr;
}

static ast_node *
parse_assign_expr(tokenizer *tokenizer, arena *arena)
{
	int precedence = get_binary_precedence(TOKEN_ASSIGN);
	ast_node *expr = parse_expr(tokenizer, -precedence - 5, arena);
	return expr;
}

typedef enum {
	PARSE_SINGLE_DECL = (1 << 0),
} parse_decl_flags;

static ast_node *
parse_decl(tokenizer *tokenizer, uint32_t flags, arena *arena)
{
	ast_node *decl;
	ast_node **ptr = &decl;
	token token = peek_token(tokenizer);
	ast_node *type = NULL;
	switch (token.kind) {
	case TOKEN_INT:
		type = new_ast_node(AST_TYPE_INT, tokenizer->loc, arena);
		get_token(tokenizer);
		break;
	case TOKEN_CHAR:
		type = new_ast_node(AST_TYPE_CHAR, tokenizer->loc, arena);
		get_token(tokenizer);
		break;
	default:
		return NULL;
	}

	if (accept(tokenizer, TOKEN_MUL)) {
		ast_node *target = type;
		type = new_ast_node(AST_TYPE_POINTER, tokenizer->loc, arena);
		type->u.pointer_type.target = target;
	}

	do {
		*ptr = new_ast_node(AST_DECL, tokenizer->loc, arena);
		(*ptr)->u.decl.name = parse_ident(tokenizer);
		(*ptr)->u.decl.type = type;
		if (accept(tokenizer, TOKEN_ASSIGN)) {
			(*ptr)->u.decl.expr = parse_assign_expr(tokenizer, arena);
		}
		ptr = &(*ptr)->next;
	} while (!tokenizer->error
	    && !(flags & PARSE_SINGLE_DECL)
	    && accept(tokenizer, TOKEN_COMMA));

	return decl;
}

static ast_node *parse_stmt(tokenizer *tokenizer, arena *arena);

static ast_node *
parse_compound_stmt(tokenizer *tokenizer, arena *arena)
{
	ast_node *head = NULL;
	ast_node **ptr = &head;

	expect(tokenizer, TOKEN_LBRACE);
	while (!tokenizer->error && !accept(tokenizer, TOKEN_RBRACE)) {
		*ptr = parse_stmt(tokenizer, arena);
		ptr = &(*ptr)->next;
	}

	return head;
}

static ast_node *
parse_stmt(tokenizer *tokenizer, arena *arena)
{
	ast_node *node = NULL;
	ast_node *decl = NULL;

	token token = peek_token(tokenizer);
	switch (token.kind) {
	case TOKEN_BREAK:
		get_token(tokenizer);
		node = new_ast_node(AST_STMT_BREAK, tokenizer->loc, arena);
		expect(tokenizer, TOKEN_SEMICOLON);
		break;
	case TOKEN_CONTINUE:
		get_token(tokenizer);
		node = new_ast_node(AST_STMT_CONTINUE, tokenizer->loc, arena);
		expect(tokenizer, TOKEN_SEMICOLON);
		break;
	case TOKEN_FOR:
		get_token(tokenizer);
		node = new_ast_node(AST_STMT_FOR, tokenizer->loc, arena);
		expect(tokenizer, TOKEN_LPAREN);

		if (!accept(tokenizer, TOKEN_SEMICOLON)) {
			token = peek_token(tokenizer);
			node->u.for_stmt.init = parse_decl(tokenizer, 0, arena);
			if (!node->u.for_stmt.init) {
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
		node = new_ast_node(AST_STMT_IF, tokenizer->loc, arena);
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
		node = new_ast_node(AST_STMT_WHILE, tokenizer->loc, arena);
		expect(tokenizer, TOKEN_LPAREN);
		node->u.while_stmt.cond = parse_assign_expr(tokenizer, arena);
		expect(tokenizer, TOKEN_RPAREN);
		node->u.while_stmt.body = parse_stmt(tokenizer, arena);
		break;
	case TOKEN_RETURN:
		get_token(tokenizer);
		node = new_ast_node(AST_STMT_RETURN, tokenizer->loc, arena);
		if (!accept(tokenizer, TOKEN_SEMICOLON)) {
			node->u.children = parse_assign_expr(tokenizer, arena);
			expect(tokenizer, TOKEN_SEMICOLON);
		}
		break;
	case TOKEN_PRINT:
		get_token(tokenizer);
		node = new_ast_node(AST_STMT_PRINT, tokenizer->loc, arena);
		node->u.children = parse_assign_expr(tokenizer, arena);
		expect(tokenizer, TOKEN_SEMICOLON);
		break;
	case TOKEN_SEMICOLON:
		get_token(tokenizer);
		node = new_ast_node(AST_STMT_EMPTY, tokenizer->loc, arena);
		break;
	case TOKEN_LBRACE:
		node = new_ast_node(AST_STMT_COMPOUND, tokenizer->loc, arena);
		node->u.children = parse_compound_stmt(tokenizer, arena);
		break;
	default:
		decl = parse_decl(tokenizer, 0, arena);
		if (decl) {
			node = new_ast_node(AST_STMT_DECL, tokenizer->loc, arena);
			node->u.children = decl;
		} else {
			node = parse_assign_expr(tokenizer, arena);
		}
		expect(tokenizer, TOKEN_SEMICOLON);
		break;
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

static ast_node *
parse_function(tokenizer *tokenizer, arena *arena)
{
	ast_node *node;
	ast_node *params = NULL;
	ast_node **ptr = &params;
	string name;
	location loc = tokenizer->loc;

	expect(tokenizer, TOKEN_INT);
	token token = get_token(tokenizer);
	if (token.kind != TOKEN_IDENT) {
		syntax_error(tokenizer, "Expected identifier");
	}

	name = token.value;
	expect(tokenizer, TOKEN_LPAREN);
	token = peek_token(tokenizer);
	if (token.kind == TOKEN_INT || token.kind == TOKEN_CHAR) {
		do {
			*ptr = parse_decl(tokenizer, PARSE_SINGLE_DECL, arena);
			if (*ptr) {
				ptr = &(*ptr)->next;
			}
		} while (accept(tokenizer, TOKEN_COMMA));
	} else if (token.kind == TOKEN_VOID) {
		get_token(tokenizer);
	}
	expect(tokenizer, TOKEN_RPAREN);

	node = new_ast_node(AST_FUNCTION, loc, arena);
	node->u.function.body = parse_compound_stmt(tokenizer, arena);
	node->u.function.name = name;
	node->u.function.params = params;

	return node;
}

static ast_node *
parse(tokenizer *tokenizer, arena *arena)
{
	ast_node *root = new_ast_node(AST_ROOT, tokenizer->loc, arena);
	ast_node **ptr = &root->u.children;
	while (!tokenizer->error && !accept(tokenizer, TOKEN_EOF)) {
		*ptr = parse_function(tokenizer, arena);
		ptr = &(*ptr)->next;
	}

	return root;
}
