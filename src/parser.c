#include <stdarg.h>

static void
verrorf(location loc, char *fmt, va_list ap)
{
	fprintf(stderr, "%s:%d:%d: ", loc.file, loc.line-1, loc.column);
	vfprintf(stderr, fmt, ap);
	fputc('\n', stderr);
	fflush(stderr);

	ASSERT(!"Syntax error");
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
	if (!accept(tokenizer, expected_token)) {
		token found_token = tokenizer->lookahead[0];
		syntax_error(tokenizer, "Expected %s, but found %s",
		    get_token_name(expected_token), get_token_name(found_token.kind));
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

typedef enum {
	PREC_NONE,
	PREC_COMMA,
	PREC_ASSIGN,
	PREC_TERNARY,
	PREC_LOR,
	PREC_LAND,
	PREC_BOR,
	PREC_XOR,
	PREC_BAND,
	PREC_EQUAL,
	PREC_COMPARE,
	PREC_SHIFT,
	PREC_TERM,
	PREC_FACTOR,
	PREC_PRIMARY,
} precedence;

/* NOTE: An operator with negative precedence is right associative. */
static precedence
get_precedence(token_kind token)
{
	switch (token) {
	case TOKEN_COMMA:
		return PREC_COMMA;
	case TOKEN_EQUAL:
	case TOKEN_PLUS_EQUAL:
	case TOKEN_MINUS_EQUAL:
	case TOKEN_STAR_EQUAL:
	case TOKEN_SLASH_EQUAL:
	case TOKEN_PERCENT_EQUAL:
	case TOKEN_AMP_EQUAL:
	case TOKEN_BAR_EQUAL:
		return PREC_ASSIGN;
	case TOKEN_BAR_BAR:
		return PREC_LOR;
	case TOKEN_AMP_AMP:
		return PREC_LAND;
	case TOKEN_BAR:
		return PREC_BOR;
	case TOKEN_CARET:
		return PREC_XOR;
	case TOKEN_AMP:
		return PREC_BAND;
	case TOKEN_LSHIFT:
	case TOKEN_RSHIFT:
		return PREC_SHIFT;
	case TOKEN_PLUS:
	case TOKEN_MINUS:
		return PREC_TERM;
	case TOKEN_STAR:
	case TOKEN_SLASH:
	case TOKEN_PERCENT:
		return PREC_FACTOR;
	case TOKEN_EQUAL_EQUAL:
		return PREC_EQUAL;
	case TOKEN_LESS:
	case TOKEN_GREATER:
	case TOKEN_LESS_EQUAL:
	case TOKEN_GREATER_EQUAL:
		return PREC_COMPARE;
	case TOKEN_LBRACKET:
	case TOKEN_DOT:
	case TOKEN_PLUS_PLUS:
	case TOKEN_MINUS_MINUS:
		return PREC_PRIMARY;
	default:
		return PREC_NONE;
	}
}

static b32
is_right_associative(token_kind token)
{
	switch (token) {
	case TOKEN_EQUAL:
		return true;
	default:
		return false;
	}
}

static b32
is_postfix_operator(token_kind token)
{
	switch (token) {
	case TOKEN_PLUS_PLUS:
	case TOKEN_MINUS_MINUS:
		return true;
	default:
		return false;
	}
}

static ast_node *parse_assign_expr(tokenizer *tokenizer, arena *arena);

static ast_node *
parse_expr(tokenizer *tokenizer, precedence prev_prec, arena *arena)
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
		{
			get_token(tokenizer);
			expr = new_ast_node(AST_EXPR_INT, tokenizer->loc, arena);
			expr->value.i = 0;

			isize i = 0;
			for (i = 0; i < token.value.length && is_digit(token.value.at[i]); i++) {
				expr->value.i *= 10;
				expr->value.i += (token.value.at[i] - '0');
			}

			for (; !tokenizer->error && i < token.value.length; i++) {
				switch (token.value.at[i]) {
				case 'l':
					if (expr->flags & AST_LLONG) {
						syntax_error(tokenizer, "Invalid suffix '%.*s'",
							(int)token.value.length, token.value.at);
					} else if (expr->flags & AST_LONG) {
						expr->flags |= AST_LLONG;
					} else {
						expr->flags |= AST_LONG;
					}

					break;
				case 'u':
					if (expr->flags & AST_UNSIGNED) {
						syntax_error(tokenizer, "Invalid suffix '%.*s'",
							(int)token.value.length, token.value.at);
					}

					expr->flags |= AST_UNSIGNED;
					break;
				default:
					syntax_error(tokenizer, "Invalid suffix '%.*s'",
						(int)token.value.length, token.value.at);
				}
			}
		} break;
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
	case TOKEN_AMP:
	case TOKEN_PLUS:
	case TOKEN_BANG:
	case TOKEN_MINUS:
	case TOKEN_PLUS_PLUS:
	case TOKEN_MINUS_MINUS:
		get_token(tokenizer);
		expr = new_ast_node(AST_EXPR_UNARY, tokenizer->loc, arena);
		expr->value.i = token.kind;
		expr->child[0] = parse_expr(tokenizer, PREC_PRIMARY, arena);
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

		token_kind operator = token.kind;
		if (operator == TOKEN_LPAREN) {
			get_token(tokenizer);
			ast_node *params = AST_NIL;
			ast_node **ptr = &params;
			if (!accept(tokenizer, TOKEN_RPAREN)) {
				do {
					*ptr = new_ast_node(AST_EXPR_LIST, tokenizer->loc, arena);
					(*ptr)->child[0] = parse_assign_expr(tokenizer, arena);
					ptr = &(*ptr)->child[1];
				} while (!tokenizer->error && accept(tokenizer, TOKEN_COMMA));
				expect(tokenizer, TOKEN_RPAREN);
			}

			ast_node *called = expr;
			expr = new_ast_node(AST_EXPR_CALL, tokenizer->loc, arena);
			expr->child[0] = called;
			expr->child[1] = params;
		} else if (token.kind == TOKEN_DOT) {
			get_token(tokenizer);
			token = peek_token(tokenizer);
			expect(tokenizer, TOKEN_IDENT);

			ast_node *accessed = expr;
			expr = new_ast_node(AST_EXPR_MEMBER, tokenizer->loc, arena);
			expr->child[0] = accessed;
			expr->value.s = token.value;
		} else {
			precedence prec = get_precedence(operator);
			if (prec == PREC_NONE) {
				return expr;
			}

			if (prec < prev_prec) {
				break;
			}

			get_token(tokenizer);
			ast_node *lhs = expr;
			if (is_postfix_operator(token.kind)) {
				ast_node *operand = expr;
				expr = new_ast_node(AST_EXPR_POSTFIX, tokenizer->loc, arena);
				expr->value.op = operator;
				expr->child[0] = operand;
			} else {
				if (token.kind == TOKEN_LBRACKET) {
					prec = PREC_NONE;
				}

				prec -= is_right_associative(operator);
				ast_node *rhs = parse_expr(tokenizer, prec, arena);
				ASSERT(rhs != AST_NIL);

				expr = new_ast_node(AST_EXPR_BINARY, tokenizer->loc, arena);
				expr->value.i = token.kind;
				expr->child[0] = lhs;
				expr->child[1] = rhs;

				if (token.kind == TOKEN_LBRACKET) {
					expect(tokenizer, TOKEN_RBRACKET);
				}
			}
		}
	}

	return expr;
}

static ast_node *
parse_assign_expr(tokenizer *tokenizer, arena *arena)
{
	ast_node *expr = parse_expr(tokenizer, PREC_ASSIGN, arena);
	return expr;
}

static ast_node *
parse_initializer(tokenizer *t, arena *perm)
{
	ast_node *list = AST_NIL;
	ast_node **ptr = &list;
	expect(t, TOKEN_LBRACE);

	do {
		*ptr = new_ast_node(AST_INIT_LIST, t->loc, perm);
		if (t->lookahead[0].kind == TOKEN_LBRACE) {
			(*ptr)->child[0] = parse_initializer(t, perm);
		} else {
			(*ptr)->child[0] = parse_assign_expr(t, perm);
		}

		ptr = &(*ptr)->child[1];
		if (!accept(t, TOKEN_COMMA)) {
			break;
		}
	} while (!t->error && t->lookahead[0].kind != TOKEN_RBRACE);

	expect(t, TOKEN_RBRACE);
	return list;
}

typedef enum {
	PARSE_SINGLE_DECL    = 1 << 0,
	PARSE_BITFIELD       = 1 << 1,
	PARSE_NO_INITIALIZER = 1 << 2,

	PARSE_PARAM = PARSE_SINGLE_DECL | PARSE_NO_INITIALIZER,
	PARSE_STRUCT_MEMBER = PARSE_BITFIELD | PARSE_NO_INITIALIZER,
	PARSE_EXTERNAL_DECL = 0,
} parse_decl_flags;

static ast_node *parse_decl(tokenizer *tokenizer, u32 flags, arena *arena);

static ast_node_flags
get_qualifier(token_kind token)
{
	switch (token) {
	case TOKEN_AUTO:
		return AST_AUTO;
	case TOKEN_CONST:
		return AST_CONST;
	case TOKEN_EXTERN:
		return AST_EXTERN;
	case TOKEN_LONG:
		return AST_LONG;
	case TOKEN_REGISTER:
		return AST_REGISTER;
	case TOKEN_RESTRICT:
		return AST_RESTRICT;
	case TOKEN_SHORT:
		return AST_SHORT;
	case TOKEN_SIGNED:
		return AST_SIGNED;
	case TOKEN_STATIC:
		return AST_STATIC;
	case TOKEN_THREAD_LOCAL:
		return AST_THREAD_LOCAL;
	case TOKEN_TYPEDEF:
		return AST_TYPEDEF;
	case TOKEN_UNSIGNED:
		return AST_UNSIGNED;
	default:
		return 0;
	}
}

/*
 * NOTE: The pointer argument points to the children member of the declaration
 * node. Thus, this function fills in the children field. It then returns the
 * part that still needs to be filled in. For example, parsing int *x would
 * look like this:
 *
 *     <ptr>
 *     x -> <ptr>
 *     x -> * -> <ptr>
 *     x -> * -> int
 */
static ast_node **
parse_declarator(tokenizer *tokenizer, ast_node **ptr, arena *arena)
{
	ast_node *pointer_decl = AST_NIL;
	// This is where the base type goes. Filled in by parse_declaration.
	ast_node **base_type = NULL;
	token token = {0};

	while (accept(tokenizer, TOKEN_STAR)) {
		ast_node *tmp = pointer_decl;
		pointer_decl = new_ast_node(AST_TYPE_POINTER, tokenizer->loc, arena);
		if (!base_type) {
			base_type = &pointer_decl->child[0];
		} else {
			pointer_decl->child[0] = tmp;
		}

		token_kind qualifier_token = tokenizer->lookahead[0].kind;
		switch (qualifier_token) {
		case TOKEN_CONST:
		case TOKEN_RESTRICT:
		case TOKEN_VOLATILE:
			get_token(tokenizer);
			pointer_decl->flags |= get_qualifier(token.kind);
			break;
		default:
			break;
		}
	}

	if (accept(tokenizer, TOKEN_LPAREN)) {
		ptr = parse_declarator(tokenizer, ptr, arena);
		expect(tokenizer, TOKEN_RPAREN);
	} else {
		token = tokenizer->lookahead[0];
		if (token.kind != TOKEN_IDENT) {
			syntax_error(tokenizer, "Expected identifier, but found %s",
				get_token_name(token.kind));
			token.value.length = 0;
		} else {
			get_token(tokenizer);
		}

		*ptr = new_ast_node(AST_DECL, tokenizer->loc, arena);
		(*ptr)->value.s = token.value;
		ptr = &(*ptr)->child[0];
	}

	while (!tokenizer->error) {
		if (accept(tokenizer, TOKEN_LBRACKET)) {
			*ptr = new_ast_node(AST_TYPE_ARRAY, tokenizer->loc, arena);
			(*ptr)->child[0] = parse_assign_expr(tokenizer, arena);
			ptr = &(*ptr)->child[1];
			expect(tokenizer, TOKEN_RBRACKET);
		} else if (accept(tokenizer, TOKEN_LPAREN)) {
			*ptr = new_ast_node(AST_TYPE_FUNC, tokenizer->loc, arena);

			ast_node *params = AST_NIL;
			if (tokenizer->lookahead[0].kind == TOKEN_RPAREN) {
				get_token(tokenizer);
			} else if (tokenizer->lookahead[0].kind == TOKEN_VOID
				&& tokenizer->lookahead[1].kind == TOKEN_RPAREN)
			{
				get_token(tokenizer);
				get_token(tokenizer);
			} else {
				ast_node **ptr = &params;
				params = new_ast_node(AST_DECL_LIST, tokenizer->loc, arena);
				do {
					*ptr = parse_decl(tokenizer, PARSE_PARAM, arena);
					if (*ptr != AST_NIL) {
						// Only single declaration allowed
						ASSERT((*ptr)->child[1] == AST_NIL);
						ptr = &(*ptr)->child[1];
					} else {
						tokenizer->error = true;
					}
				} while (!tokenizer->error && accept(tokenizer, TOKEN_COMMA));
				expect(tokenizer, TOKEN_RPAREN);
			}

			(*ptr)->child[0] = params;
			ptr = &(*ptr)->child[1];
		} else {
			break;
		}
	}

	if (pointer_decl != AST_NIL) {
		*ptr = pointer_decl;
		ptr = base_type;
	}

	return ptr;
}

static ast_node *
parse_decl(tokenizer *tokenizer, u32 flags, arena *arena)
{
	ast_node *type_specifier = AST_NIL;
	u32 qualifiers = 0;

	b32 found_qualifier = true;
	while (found_qualifier) {
		token token = peek_token(tokenizer);
		switch (token.kind) {
		case TOKEN_FLOAT:
			type_specifier = new_ast_node(AST_TYPE_FLOAT, tokenizer->loc, arena);
			get_token(tokenizer);
			break;
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

				ast_node **ptr = &type_specifier->child[0];
				// TODO: set correct flags for parsing struct members, i.e.
				// declarations are allowed to have bitfields.
				while (!tokenizer->error && !accept(tokenizer, TOKEN_RBRACE)) {
					*ptr = parse_decl(tokenizer, PARSE_STRUCT_MEMBER, arena);
					if (*ptr != AST_NIL) {
						// TODO: Walk to the end of the declaration
						ASSERT((*ptr)->child[1] == AST_NIL);
						ptr = &(*ptr)->child[1];
					} else {
						tokenizer->error = true;
					}

					expect(tokenizer, TOKEN_SEMICOLON);
				}

			}
			break;
		default:
			{
				u32 qualifier = get_qualifier(token.kind);
				if (qualifiers & AST_LONG && (qualifier == AST_LONG)) {
					qualifier = AST_LLONG;
				}

				if ((qualifiers & AST_UNSIGNED && qualifier == AST_SIGNED)
					|| (qualifiers & AST_UNSIGNED && qualifier == AST_SIGNED))
				{
					syntax_error(tokenizer, "Type cannot be both signed and unsigned");
				} else if ((qualifiers & AST_SHORT && qualifier == AST_LONG)
					|| (qualifiers & AST_LONG && qualifier == AST_SHORT))
				{
					syntax_error(tokenizer, "Integer cannot be both long and short");
				} else if (qualifiers & qualifier) {
					syntax_error(tokenizer, "Declaration already has %s qualifier.",
						get_token_name(token.kind));
				}

				found_qualifier = (qualifier != 0);
				if (found_qualifier) {
					qualifiers |= qualifier;
					get_token(tokenizer);
				}
			} break;
		}
	}

	if (qualifiers & (AST_LLONG | AST_LONG | AST_SHORT | AST_SHORT | AST_SIGNED | AST_UNSIGNED)) {
		type_specifier = new_ast_node(AST_TYPE_INT, tokenizer->loc, arena);
	}

	if (type_specifier == AST_NIL) {
		if (flags != 0) {
			syntax_error(tokenizer, "Expected type after qualifiers");
		}

		return AST_NIL;
	}

	type_specifier->flags = qualifiers;
	ast_node *list = NULL;
	ast_node **ptr = &list;
	do {
		*ptr = new_ast_node(AST_DECL_LIST, tokenizer->loc, arena);
		ast_node *decl = NULL;
		ast_node **base_ptr = parse_declarator(tokenizer, &decl, arena);
		ASSERT(base_ptr);
		if (base_ptr) {
			*base_ptr = type_specifier;
		}

		if ((flags & PARSE_BITFIELD) && accept(tokenizer, TOKEN_COLON)) {
			ast_node *type = decl->child[0];
			ast_node *bitfield = new_ast_node(AST_TYPE_BITFIELD, tokenizer->loc, arena);
			bitfield->child[0] = parse_assign_expr(tokenizer, arena);
			bitfield->child[1] = type;
			decl->child[0] = bitfield;
		}

		if (!(flags & PARSE_NO_INITIALIZER) && accept(tokenizer, TOKEN_EQUAL)) {
			if (tokenizer->lookahead[0].kind == TOKEN_LBRACE) {
				decl->child[1] = parse_initializer(tokenizer, arena);
			} else {
				decl->child[1] = parse_assign_expr(tokenizer, arena);
			}
		}

		(*ptr)->child[0] = decl;
		ptr = &(*ptr)->child[1];
	} while (!tokenizer->error
	    && !(flags & PARSE_SINGLE_DECL)
	    && accept(tokenizer, TOKEN_COMMA));

	return list;
}

static ast_node *parse_stmt(tokenizer *tokenizer, arena *arena);

static ast_node *
parse_compound_stmt(tokenizer *tokenizer, arena *arena)
{
	ast_node *result = AST_NIL;
	ast_node **ptr = &result;

	expect(tokenizer, TOKEN_LBRACE);
	while (!tokenizer->error && !accept(tokenizer, TOKEN_RBRACE)) {
		*ptr = new_ast_node(AST_STMT_LIST, tokenizer->loc, arena);
		(*ptr)->child[0] = parse_stmt(tokenizer, arena);
		ptr = &(*ptr)->child[1];
	}

	if (result == AST_NIL) {
		result = new_ast_node(AST_STMT_EMPTY, tokenizer->loc, arena);
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
	case TOKEN_CASE:
		{
			get_token(tokenizer);
			node = new_ast_node(AST_STMT_CASE, tokenizer->loc, arena);
			node->child[0] = parse_assign_expr(tokenizer, arena);
			expect(tokenizer, TOKEN_COLON);
			node->child[1] = parse_stmt(tokenizer, arena);
		} break;
	case TOKEN_CONTINUE:
		get_token(tokenizer);
		node = new_ast_node(AST_STMT_CONTINUE, tokenizer->loc, arena);
		expect(tokenizer, TOKEN_SEMICOLON);
		break;
	case TOKEN_DEFAULT:
		{
			get_token(tokenizer);
			expect(tokenizer, TOKEN_COLON);
			node = new_ast_node(AST_STMT_DEFAULT, tokenizer->loc, arena);
			node->child[0] = parse_stmt(tokenizer, arena);
		} break;
	case TOKEN_DO:
		{
			get_token(tokenizer);
			ast_node *body = parse_stmt(tokenizer, arena);
			expect(tokenizer, TOKEN_WHILE);
			ast_node *cond = parse_assign_expr(tokenizer, arena);
			expect(tokenizer, TOKEN_SEMICOLON);

			node = new_ast_node(AST_STMT_DO_WHILE, tokenizer->loc, arena);
			node->child[0] = cond;
			node->child[1] = body;
		} break;
	case TOKEN_FOR:
		{
			get_token(tokenizer);
			expect(tokenizer, TOKEN_LPAREN);

			ast_node *init = new_ast_node(AST_STMT_FOR_INIT, tokenizer->loc, arena);
			if (!accept(tokenizer, TOKEN_SEMICOLON)) {
				token = peek_token(tokenizer);
				init->child[0] = parse_decl(tokenizer, 0, arena);
				if (init == AST_NIL) {
					init->child[0] = parse_assign_expr(tokenizer, arena);
				}

				expect(tokenizer, TOKEN_SEMICOLON);
			} else {
				init->child[0] = new_ast_node(AST_STMT_EMPTY, tokenizer->loc, arena);
			}

			ast_node *cond = new_ast_node(AST_STMT_FOR_COND, tokenizer->loc, arena);
			init->child[1] = cond;
			if (!accept(tokenizer, TOKEN_SEMICOLON)) {
				cond->child[0] = parse_assign_expr(tokenizer, arena);
				expect(tokenizer, TOKEN_SEMICOLON);
			} else {
				cond->child[0] = new_ast_node(AST_EXPR_INT, tokenizer->loc, arena);
				cond->value.i = 1;
			}

			ast_node *post = new_ast_node(AST_STMT_FOR_POST, tokenizer->loc, arena);
			cond->child[1] = post;
			if (!accept(tokenizer, TOKEN_RPAREN)) {
				post->child[0] = parse_assign_expr(tokenizer, arena);
				expect(tokenizer, TOKEN_RPAREN);
			} else {
				post->child[0] = new_ast_node(AST_STMT_EMPTY, tokenizer->loc, arena);
			}

			post->child[1] = parse_stmt(tokenizer, arena);
			node = init;
		} break;
	case TOKEN_GOTO:
		{
			get_token(tokenizer);
			token = peek_token(tokenizer);
			expect(tokenizer, TOKEN_IDENT);
			node = new_ast_node(AST_STMT_GOTO, tokenizer->loc, arena);
			node->value.s = token.value;
			expect(tokenizer, TOKEN_SEMICOLON);
		} break;
	case TOKEN_IF:
		get_token(tokenizer);
		expect(tokenizer, TOKEN_LPAREN);
		ast_node *cond = parse_assign_expr(tokenizer, arena);
		expect(tokenizer, TOKEN_RPAREN);
		ast_node *if_else = new_ast_node(AST_STMT_IF_ELSE, tokenizer->loc, arena);
		if_else->child[0] = parse_stmt(tokenizer, arena);
		if (accept(tokenizer, TOKEN_ELSE)) {
			if_else->child[1] = parse_stmt(tokenizer, arena);
		}

		node = new_ast_node(AST_STMT_IF_COND, tokenizer->loc, arena);
		node->child[0] = cond;
		node->child[1] = if_else;
		break;
	case TOKEN_WHILE:
		{
			get_token(tokenizer);
			expect(tokenizer, TOKEN_LPAREN);
			ast_node *cond = parse_assign_expr(tokenizer, arena);
			expect(tokenizer, TOKEN_RPAREN);
			ast_node *body = parse_stmt(tokenizer, arena);

			node = new_ast_node(AST_STMT_WHILE, tokenizer->loc, arena);
			node->child[0] = cond;
			node->child[1] = body;
		} break;
	case TOKEN_RETURN:
		get_token(tokenizer);
		node = new_ast_node(AST_STMT_RETURN, tokenizer->loc, arena);
		if (!accept(tokenizer, TOKEN_SEMICOLON)) {
			node->child[0] = parse_assign_expr(tokenizer, arena);
			expect(tokenizer, TOKEN_SEMICOLON);
		}
		break;
	case TOKEN_PRINT:
		get_token(tokenizer);
		node = new_ast_node(AST_STMT_PRINT, tokenizer->loc, arena);
		node->child[0] = parse_assign_expr(tokenizer, arena);
		expect(tokenizer, TOKEN_SEMICOLON);
		break;
	case TOKEN_SWITCH:
		{
			get_token(tokenizer);
			node = new_ast_node(AST_STMT_SWITCH, tokenizer->loc, arena);
			expect(tokenizer, TOKEN_LPAREN);
			node->child[0] = parse_assign_expr(tokenizer, arena);
			expect(tokenizer, TOKEN_RPAREN);
			node->child[1] = parse_stmt(tokenizer, arena);
		} break;
	case TOKEN_SEMICOLON:
		get_token(tokenizer);
		node = new_ast_node(AST_STMT_EMPTY, tokenizer->loc, arena);
		break;
	case TOKEN_LBRACE:
		node = parse_compound_stmt(tokenizer, arena);
		break;
	default:
		if (tokenizer->lookahead[0].kind == TOKEN_IDENT
			&& tokenizer->lookahead[1].kind == TOKEN_COLON)
		{
			node = new_ast_node(AST_STMT_LABEL, tokenizer->loc, arena);
			node->value.s = token.value;
			get_token(tokenizer);
			get_token(tokenizer);
			node->child[0] = parse_stmt(tokenizer, arena);
		} else {
			node = parse_decl(tokenizer, 0, arena);
			if (node == AST_NIL) {
				node = parse_assign_expr(tokenizer, arena);
			}
			expect(tokenizer, TOKEN_SEMICOLON);
		}
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

static ast_node *
parse_external_decl(tokenizer *tokenizer, arena *arena)
{
	ast_node *list = parse_decl(tokenizer, PARSE_EXTERNAL_DECL, arena);
	if (list == AST_NIL) {
		return list;
	}

	ast_node *decl = list->child[0];
	decl->kind = AST_EXTERN_DEF;

	ast_node *type = decl->child[0];
	if (list->child[1] == AST_NIL && type->kind == AST_TYPE_FUNC) {
		token token = peek_token(tokenizer);
		if (token.kind == TOKEN_LBRACE) {
			ast_node *body = parse_compound_stmt(tokenizer, arena);
			decl->child[1] = body;
		} else {
			expect(tokenizer, TOKEN_SEMICOLON);
		}
	} else {
		expect(tokenizer, TOKEN_SEMICOLON);
	}

	return list;
}

static ast_node *
parse(tokenizer *tokenizer, arena *arena)
{
	ast_node *root = AST_NIL;
	ast_node **ptr = &root;
	do {
		*ptr = parse_external_decl(tokenizer, arena);
		while (*ptr != AST_NIL) {
			ASSERT((*ptr)->kind == AST_DECL_LIST);
			ptr = &(*ptr)->child[1];
		}
	} while (!tokenizer->error && !accept(tokenizer, TOKEN_EOF));

	if (tokenizer->error) {
		root->kind = AST_INVALID;
	}

	return root;
}
