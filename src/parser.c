#include <stdarg.h>

static void
verrorf(location loc, char *fmt, va_list ap)
{
	fprintf(stderr, "%s:%d:%d: ", loc.file, loc.line-1, loc.column);
	vfprintf(stderr, fmt, ap);
	fputc('\n', stderr);
	fflush(stderr);

	ASSERT(false);
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

static b32
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
	ast_node *node = ALLOC(arena, 1, ast_node);
	*node = ast_nil;
	node->kind = kind;
	node->loc = loc;
	return node;
}

#define MAX_PRECEDENCE 65

/* NOTE: An operator with negative precedence is right associative. */
static int
get_binary_precedence(token_kind token)
{
	switch (token) {
	case TOKEN_COMMA:
		return -5;
	case TOKEN_EQUAL:
		return -10;
	case TOKEN_PLUS:
	case TOKEN_MINUS:
		return 20;
	case TOKEN_STAR:
	case TOKEN_SLASH:
	case TOKEN_PERCENT:
		return 30;
	case TOKEN_EQUAL_EQUAL:
	case TOKEN_LT:
	case TOKEN_GT:
	case TOKEN_LEQ:
	case TOKEN_GEQ:
		return 40;
	case TOKEN_LBRACKET:
	case TOKEN_DOT:
		return 50;
		return 60;
	default:
		return 0;
	}
}

static intmax_t
parse_int(str str)
{
	intmax_t ival = 0;

	while (str.length-- > 0) {
		ival *= 10;
		ival += (*str.at++ - '0');
	}

	return ival;
}

static str
parse_ident(tokenizer *tokenizer)
{
	static str empty = {"", 0};
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
		expr->value.s = token.value;
		break;
	case TOKEN_LITERAL_INT:
		get_token(tokenizer);
		expr = new_ast_node(AST_EXPR_INT, tokenizer->loc, arena);
		expr->value.i = parse_int(token.value);
		break;
	case TOKEN_LPAREN:
		get_token(tokenizer);
		expr = parse_expr(tokenizer, 0, arena);
		if (expr == AST_NIL) {
			syntax_error(tokenizer, "Expected expression");
			return expr;
		}

		expect(tokenizer, TOKEN_RPAREN);
		break;
	case TOKEN_STAR:
	case TOKEN_AMPERSAND:
		get_token(tokenizer);
		expr = new_ast_node(AST_EXPR_UNARY, tokenizer->loc, arena);
		expr->value.i = token.kind;
		expr->children = parse_expr(tokenizer, MAX_PRECEDENCE, arena);
		break;
	default:
		syntax_error(tokenizer, "Expected expression");
		return AST_NIL;
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
			ast_node *params = AST_NIL;
			ast_node **ptr = &params;
			if (!accept(tokenizer, TOKEN_RPAREN)) {
				do {
					*ptr = parse_assign_expr(tokenizer, arena);
					if (*ptr != AST_NIL) {
						ptr = &(*ptr)->next;
					}
				} while (*ptr != AST_NIL && accept(tokenizer, TOKEN_COMMA));
				expect(tokenizer, TOKEN_RPAREN);
			}

			ast_node *called = expr;
			expr = new_ast_node(AST_EXPR_CALL, tokenizer->loc, arena);
			expr->children = called;
			called->next = params;
		} else {
			int precedence = get_binary_precedence(token.kind);
			if (precedence == 0) {
				return expr;
			}

			b32 is_right_associative = (precedence < 0);
			if (is_right_associative) {
				precedence = -precedence;
			}

			if (precedence <= prev_precedence) {
				break;
			}

			/* NOTE: Ensure that right associative expressions are
			 * parsed correctly. */
			precedence -= is_right_associative;

			if (token.kind == TOKEN_LBRACKET) {
				precedence = 0;
			}

			get_token(tokenizer);
			ast_node *lhs = expr;
			ast_node *rhs = parse_expr(tokenizer, precedence, arena);
			ASSERT(rhs);

			expr = new_ast_node(AST_EXPR_BINARY, tokenizer->loc, arena);
			expr->value.i = token.kind;
			expr->children = lhs;
			lhs->next = rhs;

			if (token.kind == TOKEN_LBRACKET) {
				expect(tokenizer, TOKEN_RBRACKET);
			}
		}
	}

	return expr;
}

static ast_node *
parse_assign_expr(tokenizer *tokenizer, arena *arena)
{
	int precedence = get_binary_precedence(TOKEN_EQUAL);
	ast_node *expr = parse_expr(tokenizer, -precedence - 5, arena);
	return expr;
}

typedef enum {
	PARSE_SINGLE_DECL = (1 << 0),

	PARSE_DECL_EXTERNAL = 0,
} parse_decl_flags;

static ast_node *parse_decl(tokenizer *tokenizer, u32 flags, arena *arena);

static ast_node *
parse_declarator(tokenizer *tokenizer, arena *arena)
{
	ast_node *result = AST_NIL;

	if (accept(tokenizer, TOKEN_LPAREN)) {
		result = parse_declarator(tokenizer, arena);
		expect(tokenizer, TOKEN_RPAREN);
	} else if (accept(tokenizer, TOKEN_STAR)) {
		result = new_ast_node(AST_DECL_POINTER, tokenizer->loc, arena);
		result->children = parse_declarator(tokenizer, arena);
	} else {
		result = new_ast_node(AST_DECL_IDENT, tokenizer->loc, arena);
		result->value.s = parse_ident(tokenizer);
	}

	if (accept(tokenizer, TOKEN_LBRACKET)) {
		ast_node *declarator = result;
		result = new_ast_node(AST_DECL_ARRAY, tokenizer->loc, arena);
		result->children = declarator;
		declarator->next = parse_assign_expr(tokenizer, arena);
		expect(tokenizer, TOKEN_RBRACKET);
	} else if (accept(tokenizer, TOKEN_LPAREN)) {
		ast_node *declarator = result;
		result = new_ast_node(AST_DECL_FUNC, tokenizer->loc, arena);
		result->children = declarator;

		// TODO: correctly parse functions without any arguments
		if (tokenizer->lookahead[0].kind == TOKEN_RPAREN) {
			get_token(tokenizer);
		} else if (tokenizer->lookahead[0].kind == TOKEN_VOID
			&& tokenizer->lookahead[1].kind == TOKEN_RPAREN)
		{
			get_token(tokenizer);
			get_token(tokenizer);
		} else {
			ast_node **ptr = &declarator->next;
			while (*ptr != AST_NIL && !accept(tokenizer, TOKEN_RPAREN)) {
				*ptr = parse_decl(tokenizer, PARSE_SINGLE_DECL, arena);
				if (*ptr != AST_NIL) {
					ptr = &(*ptr)->next;
				}
			}
		}
	}

	return result;
}

static ast_node *
parse_decl(tokenizer *tokenizer, u32 flags, arena *arena)
{
	ast_node *type_specifier = AST_NIL;
	token token = peek_token(tokenizer);
	switch (token.kind) {
	case TOKEN_INT:
		type_specifier = new_ast_node(AST_TYPE_INT, tokenizer->loc, arena);
		get_token(tokenizer);
		break;
	case TOKEN_CHAR:
		type_specifier = new_ast_node(AST_TYPE_CHAR, tokenizer->loc, arena);
		get_token(tokenizer);
		break;
	case TOKEN_VOID:
		type_specifier = new_ast_node(AST_TYPE_VOID, tokenizer->loc, arena);
		get_token(tokenizer);
		break;
	case TOKEN_STRUCT:
		get_token(tokenizer);
		accept(tokenizer, TOKEN_IDENT);

		type_specifier = new_ast_node(AST_TYPE_STRUCT, tokenizer->loc, arena);
		token = peek_token(tokenizer);
		if (token.kind == TOKEN_IDENT) {
			type_specifier->value.s = token.value;
		}

		if (accept(tokenizer, TOKEN_LBRACE)) {
			type_specifier->kind = AST_TYPE_STRUCT_DEF;

			ast_node **ptr = & type_specifier->children;
			// TODO: set correct flags for parsing struct members, i.e.
			// declarations are allowed to have bitfields.
			while (!accept(tokenizer, TOKEN_RBRACE)) {
				*ptr = parse_decl(tokenizer, 0, arena);
				ptr = &(*ptr)->next;

				expect(tokenizer, TOKEN_SEMICOLON);
			}

		}
		break;
	default:
		return AST_NIL;
	}

	ast_node *decl = new_ast_node(AST_DECL, tokenizer->loc, arena);
	decl->children = type_specifier;
	ast_node **ptr = &decl->children->next;
	do {
		ast_node *declarator = parse_declarator(tokenizer, arena);

		if (accept(tokenizer, TOKEN_EQUAL)) {
			if (accept(tokenizer, TOKEN_LBRACE)) {
				parse_assign_expr(tokenizer, arena);
				expect(tokenizer, TOKEN_RBRACE);
			} else {
				declarator->next = parse_assign_expr(tokenizer, arena);
			}

			*ptr = new_ast_node(AST_DECL_INIT, tokenizer->loc, arena);
			(*ptr)->children = declarator;
		} else {
			*ptr = declarator;
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
	ast_node *result = new_ast_node(AST_STMT_COMPOUND, tokenizer->loc, arena);
	ast_node **ptr = &result->children;

	expect(tokenizer, TOKEN_LBRACE);
	while (!tokenizer->error && !accept(tokenizer, TOKEN_RBRACE)) {
		*ptr = parse_stmt(tokenizer, arena);
		ptr = &(*ptr)->next;
	}

	return result;
}

static ast_node *
parse_stmt(tokenizer *tokenizer, arena *arena)
{
	ast_node *node = AST_NIL;

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
		{
			ast_node *init, *post, *cond, *body;
			init = post = cond = body = AST_NIL;

			get_token(tokenizer);
			expect(tokenizer, TOKEN_LPAREN);

			if (!accept(tokenizer, TOKEN_SEMICOLON)) {
				token = peek_token(tokenizer);
				init = parse_decl(tokenizer, 0, arena);
				if (init == AST_NIL) {
					init = parse_assign_expr(tokenizer, arena);
				}

				expect(tokenizer, TOKEN_SEMICOLON);
			}

			if (!accept(tokenizer, TOKEN_SEMICOLON)) {
				cond = parse_assign_expr(tokenizer, arena);
				expect(tokenizer, TOKEN_SEMICOLON);
			}

			if (!accept(tokenizer, TOKEN_RPAREN)) {
				post = parse_assign_expr(tokenizer, arena);
				expect(tokenizer, TOKEN_RPAREN);
			}

			body = parse_stmt(tokenizer, arena);

			node = new_ast_node(AST_STMT_FOR, tokenizer->loc, arena);
			node->children = init;
			init->next = cond;
			cond->next = post;
			post->next = body;
		} break;
	case TOKEN_IF:
		get_token(tokenizer);
		expect(tokenizer, TOKEN_LPAREN);
		ast_node *cond = parse_assign_expr(tokenizer, arena);
		expect(tokenizer, TOKEN_RPAREN);
		ast_node *then = parse_stmt(tokenizer, arena);
		if (accept(tokenizer, TOKEN_ELSE)) {
			ast_node *otherwise = parse_stmt(tokenizer, arena);
			then->next = otherwise;
		}

		node = new_ast_node(AST_STMT_IF, tokenizer->loc, arena);
		node->children = cond;
		cond->next = then;
		break;
	case TOKEN_WHILE:
		{
			get_token(tokenizer);
			expect(tokenizer, TOKEN_LPAREN);
			ast_node *cond = parse_assign_expr(tokenizer, arena);
			expect(tokenizer, TOKEN_RPAREN);
			ast_node *body = parse_stmt(tokenizer, arena);

			node = new_ast_node(AST_STMT_WHILE, tokenizer->loc, arena);
			node->children = cond;
			cond->next = body;
		} break;
	case TOKEN_RETURN:
		get_token(tokenizer);
		node = new_ast_node(AST_STMT_RETURN, tokenizer->loc, arena);
		if (!accept(tokenizer, TOKEN_SEMICOLON)) {
			node->children = parse_assign_expr(tokenizer, arena);
			expect(tokenizer, TOKEN_SEMICOLON);
		}
		break;
	case TOKEN_PRINT:
		get_token(tokenizer);
		node = new_ast_node(AST_STMT_PRINT, tokenizer->loc, arena);
		node->children = parse_assign_expr(tokenizer, arena);
		expect(tokenizer, TOKEN_SEMICOLON);
		break;
	case TOKEN_SEMICOLON:
		get_token(tokenizer);
		node = new_ast_node(AST_STMT_EMPTY, tokenizer->loc, arena);
		break;
	case TOKEN_LBRACE:
		node = new_ast_node(AST_STMT_COMPOUND, tokenizer->loc, arena);
		node->children = parse_compound_stmt(tokenizer, arena);
		break;
	default:
		node = parse_decl(tokenizer, 0, arena);
		if (node == AST_NIL) {
			node = parse_assign_expr(tokenizer, arena);
		}
		expect(tokenizer, TOKEN_SEMICOLON);
		break;
	}

#if 0
	if (tokenizer->error) {
		tokenizer->error = false;
		while (!accept(tokenizer, TOKEN_SEMICOLON)
		    && !accept(tokenizer, TOKEN_RBRACE)) {
			get_token(tokenizer);
		}
	}
#endif

	return node;
}

static b32
is_function_decl(ast_node *declarator)
{
	b32 only_one_declarator = (declarator->next == AST_NIL);
	b32 no_initializer = (declarator->kind != AST_DECL_INIT);
	if (only_one_declarator && no_initializer) {
		while (declarator != AST_NIL) {
			if (declarator->kind == AST_DECL_FUNC
				&& declarator->children->kind == AST_DECL_IDENT)
			{
				return true;
			}

			declarator = declarator->children;
		}
	}

	return false;
}

static ast_node *
parse_external_decl(tokenizer *tokenizer, arena *arena)
{
	ast_node *decl = parse_decl(tokenizer, PARSE_DECL_EXTERNAL, arena);

	ast_node *declarator = decl->children->next;
	if (is_function_decl(declarator)) {
		token token = peek_token(tokenizer);
		if (token.kind == TOKEN_LBRACE) {
			ast_node *body = parse_compound_stmt(tokenizer, arena);
			ast_node *node = new_ast_node(AST_FUNCTION, tokenizer->loc, arena);
			node->children = decl;
			decl->next = body;
			decl = node;
		} else {
			expect(tokenizer, TOKEN_SEMICOLON);
		}
	} else {
		expect(tokenizer, TOKEN_SEMICOLON);
	}

	return decl;
}

static ast_node *
parse(tokenizer *tokenizer, arena *arena)
{
	ast_node *root = new_ast_node(AST_ROOT, tokenizer->loc, arena);
	ast_node **ptr = &root->children;
	do {
		*ptr = parse_external_decl(tokenizer, arena);
		if (*ptr != AST_NIL) {
			ptr = &(*ptr)->next;
		}
	} while (!tokenizer->error && *ptr != AST_NIL && !accept(tokenizer, TOKEN_EOF));

	if (tokenizer->error) {
		root->kind = AST_INVALID;
	}

	return root;
}
