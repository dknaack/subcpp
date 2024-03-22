#include <stdarg.h>

static ast_node
ast_make_node(ast_node_kind kind, location loc)
{
	ast_node node = {0};
	node.kind = kind;
	node.loc = loc;
	return node;
}

static ast_id
ast_get_id(ast_pool *pool, ast_node *node)
{
	ast_id id;
	id.value = node - pool->nodes;
	return id;
}

static ast_id
ast_push(ast_pool *p, ast_node node)
{
	if (p->size + 1 >= p->cap) {
		if (!p->cap) {
			p->cap = 1024;
			p->size = 1;
		}

		p->cap *= 2;
		p->nodes = realloc(p->nodes, p->cap * sizeof(*p->nodes));
	}

	ast_id id = {0};
	if (!p->nodes) {
		return id;
	}

	id.value = p->size++;
	memcpy(&p->nodes[id.value], &node, sizeof(node));
	return id;
}

static ast_node *
ast_get(ast_pool *p, ast_id id)
{
	if (0 < id.value && id.value < p->size) {
		return p->nodes + id.value;
	}

	ASSERT(!"ID is out of bounds");
	return NULL;
}

static void
ast_append(ast_pool *p, ast_list *l, ast_node node)
{
	ast_id node_id = ast_push(p, node);
	if (l->last.value != 0) {
		ast_node *last = ast_get(p, l->last);
		l->last = last->child[1] = node_id;
	} else {
		l->last = l->first = node_id;
	}
}

static void
ast_prepend(ast_pool *p, ast_list *l, ast_node node)
{
	node.child[1] = l->first;
	l->first = ast_push(p, node);
	if (l->last.value == 0) {
		l->last = l->first;
	}
}

static void
ast_concat(ast_pool *p, ast_list *a, ast_list b)
{
	if (b.first.value != 0) {
		if (a->last.value != 0) {
			ast_node *last = ast_get(p, a->last);
			last->child[1] = b.first;
			a->last = b.last;
		} else {
			*a = b;
		}
	}
}

static void
ast_shrink(ast_pool *p)
{
	ast_node *nodes = realloc(p->nodes, p->size * sizeof(*nodes));
	if (nodes) {
		p->nodes = nodes;
		p->cap = p->size;
	}
}

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
vsyntax_error(lexer *lexer, char *fmt, va_list ap)
{
	if (lexer->error) {
		return;
	}

	verrorf(lexer->loc, fmt, ap);
	lexer->error = true;
}

static void
syntax_error(lexer *lexer, char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	vsyntax_error(lexer, fmt, ap);
	va_end(ap);
}

static b32
accept(lexer *lexer, token_kind expected_token)
{
	token token = lexer->peek[0];
	if (token.kind == expected_token) {
		get_token(lexer);
		return true;
	} else {
		return false;
	}
}

static void
expect(lexer *lexer, token_kind expected_token)
{
	if (!accept(lexer, expected_token)) {
		token found_token = lexer->peek[0];
		syntax_error(lexer, "Expected %s, but found %s",
			get_token_name(expected_token), get_token_name(found_token.kind));
	}
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

typedef enum {
	PARSE_SINGLE_DECL    = 1 << 0,
	PARSE_BITFIELD       = 1 << 1,
	PARSE_NO_INITIALIZER = 1 << 2,
	PARSE_NO_IDENT       = 1 << 3,

	PARSE_CAST = PARSE_NO_IDENT | PARSE_SINGLE_DECL | PARSE_NO_INITIALIZER,
	PARSE_PARAM = PARSE_SINGLE_DECL | PARSE_NO_INITIALIZER,
	PARSE_STRUCT_MEMBER = PARSE_BITFIELD | PARSE_NO_INITIALIZER,
	PARSE_EXTERNAL_DECL = 0,
} parse_decl_flags;

static ast_id parse_assign_expr(lexer *lexer, ast_pool *pool);
static ast_list parse_decl(lexer *lexer, u32 flags, ast_pool *pool);

static ast_id
parse_expr(lexer *lexer, precedence prev_prec, ast_pool *pool)
{
	ast_id expr;

	token token = lexer->peek[0];
	switch (token.kind) {
	case TOKEN_IDENT:
		{
			get_token(lexer);
			ast_node ident = ast_make_node(AST_EXPR_IDENT, lexer->loc);
			ident.value.s = token.value;
			expr = ast_push(pool, ident);
		} break;
	case TOKEN_LITERAL_STRING:
		{
			get_token(lexer);
			ast_node literal = ast_make_node(AST_EXPR_STRING, lexer->loc);
			literal.value.s = token.value;
			expr = ast_push(pool, literal);
		} break;
	case TOKEN_LITERAL_FLOAT:
		{
			get_token(lexer);
			ast_node literal = ast_make_node(AST_EXPR_FLOAT, lexer->loc);
			// TODO: Parse the value
			literal.value.f = 1.23456f;
			expr = ast_push(pool, literal);
		} break;
	case TOKEN_LITERAL_INT:
		{
			get_token(lexer);
			ast_node literal = ast_make_node(AST_EXPR_INT, lexer->loc);
			literal.value.i = 0;

			isize i = 0;
			for (i = 0; i < token.value.length && is_digit(token.value.at[i]); i++) {
				literal.value.i *= 10;
				literal.value.i += (token.value.at[i] - '0');
			}

			for (; !lexer->error && i < token.value.length; i++) {
				switch (token.value.at[i]) {
				case 'l':
					if (literal.flags & AST_LLONG) {
						syntax_error(lexer, "Invalid suffix '%.*s'",
							(int)token.value.length, token.value.at);
					} else if (literal.flags & AST_LONG) {
						literal.flags |= AST_LLONG;
					} else {
						literal.flags |= AST_LONG;
					}

					break;
				case 'u':
					if (literal.flags & AST_UNSIGNED) {
						syntax_error(lexer, "Invalid suffix '%.*s'",
							(int)token.value.length, token.value.at);
					}

					literal.flags |= AST_UNSIGNED;
					break;
				default:
					syntax_error(lexer, "Invalid suffix '%.*s'",
						(int)token.value.length, token.value.at);
				}
			}

			expr = ast_push(pool, literal);
		} break;
	case TOKEN_LPAREN:
		{
			get_token(lexer);

			ast_list decl = parse_decl(lexer, PARSE_CAST, pool);
			if (decl.first.value != 0) {
				expect(lexer, TOKEN_RPAREN);
				if (lexer->peek[0].kind == TOKEN_LBRACE) {
					ASSERT(!"TODO: Parse initializer");
				} else {
					ast_node *decl_node = ast_get(pool, decl.first);
					ast_id type = decl_node->child[0];

					ast_node node = ast_make_node(AST_EXPR_CAST, lexer->loc);
					node.child[0] = type;
					node.child[1] = parse_expr(lexer, PREC_PRIMARY, pool);
					expr = ast_push(pool, node);
				}
			} else {
				expr = parse_expr(lexer, 0, pool);
				if (expr.value == 0) {
					syntax_error(lexer, "Expected expression");
					return expr;
				}

				expect(lexer, TOKEN_RPAREN);
			}
		} break;
	case TOKEN_STAR:
	case TOKEN_AMP:
	case TOKEN_PLUS:
	case TOKEN_BANG:
	case TOKEN_TILDE:
	case TOKEN_MINUS:
	case TOKEN_PLUS_PLUS:
	case TOKEN_MINUS_MINUS:
		{
			get_token(lexer);
			ast_node node = ast_make_node(AST_EXPR_UNARY, lexer->loc);
			node.value.i = token.kind;
			node.child[0] = parse_expr(lexer, PREC_PRIMARY, pool);
			expr = ast_push(pool, node);
		} break;
	default:
		syntax_error(lexer, "Expected expression");
		return ast_id_nil;
	}

	for (;;) {
		token = lexer->peek[0];
		if (token.kind == TOKEN_EOF
			|| token.kind == TOKEN_SEMICOLON
			|| token.kind == TOKEN_RPAREN)
		{
			break;
		}

		token_kind operator = token.kind;
		if (operator == TOKEN_LPAREN) {
			get_token(lexer);
			ast_list params = {0};
			if (!accept(lexer, TOKEN_RPAREN)) {
				do {
					ast_node node = ast_make_node(AST_EXPR_LIST, lexer->loc);
					node.child[0] = parse_assign_expr(lexer, pool);
					ast_append(pool, &params, node);
				} while (!lexer->error && accept(lexer, TOKEN_COMMA));
				expect(lexer, TOKEN_RPAREN);
			}

			ast_id called = expr;
			ast_node node = ast_make_node(AST_EXPR_CALL, lexer->loc);
			node.child[0] = called;
			node.child[1] = params.first;
			expr = ast_push(pool, node);
		} else if (token.kind == TOKEN_DOT || token.kind == TOKEN_ARROW) {
			ast_node_kind kind = AST_EXPR_MEMBER;
			if (token.kind == TOKEN_ARROW) {
				kind = AST_EXPR_MEMBER_PTR;
			}

			get_token(lexer);
			token = lexer->peek[0];
			expect(lexer, TOKEN_IDENT);

			ast_node node = ast_make_node(kind, lexer->loc);
			node.child[0] = expr;
			node.value.s = token.value;
			expr = ast_push(pool, node);
		} else {
			precedence prec = get_precedence(operator);
			if (prec == PREC_NONE) {
				return expr;
			}

			if (prec < prev_prec) {
				break;
			}

			get_token(lexer);
			ast_id lhs = expr;
			if (is_postfix_operator(token.kind)) {
				ast_node node = ast_make_node(AST_EXPR_POSTFIX, lexer->loc);
				node.value.op = operator;
				node.child[0] = expr;
				expr = ast_push(pool, node);
			} else {
				if (token.kind == TOKEN_LBRACKET) {
					prec = PREC_NONE;
				}

				prec -= is_right_associative(operator);
				ast_id rhs = parse_expr(lexer, prec, pool);
				ASSERT(rhs.value != 0);

				ast_node node = ast_make_node(AST_EXPR_BINARY, lexer->loc);
				node.value.i = token.kind;
				node.child[0] = lhs;
				node.child[1] = rhs;
				expr = ast_push(pool, node);

				if (token.kind == TOKEN_LBRACKET) {
					expect(lexer, TOKEN_RBRACKET);
				}
			}
		}
	}

	return expr;
}

static ast_id
parse_assign_expr(lexer *lexer, ast_pool *pool)
{
	ast_id expr = parse_expr(lexer, PREC_ASSIGN, pool);
	return expr;
}

static ast_id
parse_initializer(lexer *t, ast_pool *pool)
{
	ast_list list = {0};
	expect(t, TOKEN_LBRACE);

	do {
		ast_node node = ast_make_node(AST_INIT_LIST, t->loc);
		if (t->peek[0].kind == TOKEN_LBRACE) {
			node.child[0] = parse_initializer(t, pool);
		} else {
			node.child[0] = parse_assign_expr(t, pool);
		}

		ast_append(pool, &list, node);
		if (!accept(t, TOKEN_COMMA)) {
			break;
		}
	} while (!t->error && t->peek[0].kind != TOKEN_RBRACE);

	expect(t, TOKEN_RBRACE);
	return list.first;
}

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

static ast_list
parse_declarator(lexer *lexer, u32 flags, ast_pool *pool)
{
	ast_list pointer_declarator = {0};
	while (accept(lexer, TOKEN_STAR)) {
		ast_node node = ast_make_node(AST_TYPE_POINTER, lexer->loc);
		token_kind qualifier_token = lexer->peek[0].kind;
		switch (qualifier_token) {
		case TOKEN_CONST:
		case TOKEN_RESTRICT:
		case TOKEN_VOLATILE:
			get_token(lexer);
			node.flags |= get_qualifier(qualifier_token);
			break;
		default:
			break;
		}

		ast_prepend(pool, &pointer_declarator, node);
	}

	ast_list declarator = {0};
	if (accept(lexer, TOKEN_LPAREN)) {
		declarator = parse_declarator(lexer, flags, pool);
		expect(lexer, TOKEN_RPAREN);
	} else if (!(flags & PARSE_NO_IDENT)) {
		token token = lexer->peek[0];
		if (token.kind != TOKEN_IDENT) {
			syntax_error(lexer, "Expected identifier, but found %s",
				get_token_name(token.kind));
			token.value.length = 0;
		} else {
			get_token(lexer);
		}

		ast_node node = ast_make_node(AST_DECL, lexer->loc);
		node.value.s = token.value;
		ast_append(pool, &declarator, node);
	}

	while (!lexer->error) {
		if (accept(lexer, TOKEN_LBRACKET)) {
			ast_node node = ast_make_node(AST_TYPE_ARRAY, lexer->loc);
			node.child[0] = parse_assign_expr(lexer, pool);
			ast_append(pool, &declarator, node);
			expect(lexer, TOKEN_RBRACKET);
		} else if (accept(lexer, TOKEN_LPAREN)) {
			ast_node node = ast_make_node(AST_TYPE_FUNC, lexer->loc);
			if (lexer->peek[0].kind == TOKEN_RPAREN) {
				get_token(lexer);
			} else if (lexer->peek[0].kind == TOKEN_VOID
				&& lexer->peek[1].kind == TOKEN_RPAREN)
			{
				get_token(lexer);
				get_token(lexer);
			} else {
				ast_list params = {0};
				do {
					ast_list param = {0};
					if (accept(lexer, TOKEN_ELLIPSIS)) {
						node.flags |= AST_VARIADIC;
						break;
					}

					param = parse_decl(lexer, PARSE_PARAM, pool);
					ast_concat(pool, &params, param);
					if (param.first.value == 0) {
						lexer->error = true;
					}
				} while (!lexer->error && accept(lexer, TOKEN_COMMA));
				expect(lexer, TOKEN_RPAREN);
				node.child[0] = params.first;
			}

			ast_append(pool, &declarator, node);
		} else {
			break;
		}
	}

	ast_concat(pool, &declarator, pointer_declarator);
	return declarator;
}

static ast_list
parse_decl(lexer *lexer, u32 flags, ast_pool *pool)
{
	ast_node type_specifier = {0};
	u32 qualifiers = 0;

	b32 found_qualifier = true;
	while (found_qualifier) {
		token token = lexer->peek[0];
		switch (token.kind) {
		case TOKEN_FLOAT:
			type_specifier = ast_make_node(AST_TYPE_FLOAT, lexer->loc);
			get_token(lexer);
			break;
		case TOKEN_INT:
			type_specifier = ast_make_node(AST_TYPE_INT, lexer->loc);
			get_token(lexer);
			break;
		case TOKEN_CHAR:
			type_specifier = ast_make_node(AST_TYPE_CHAR, lexer->loc);
			get_token(lexer);
			break;
		case TOKEN_VOID:
			type_specifier = ast_make_node(AST_TYPE_VOID, lexer->loc);
			get_token(lexer);
			break;
		case TOKEN_ENUM:
			{
				type_specifier = ast_make_node(AST_TYPE_ENUM, lexer->loc);
				get_token(lexer);
				accept(lexer, TOKEN_IDENT);
				expect(lexer, TOKEN_LBRACE);

				ast_list enumerators = {0};
				while (!lexer->error && lexer->peek[0].kind != TOKEN_RBRACE) {
					ast_node node = ast_make_node(AST_ENUMERATOR, lexer->loc);
					node.value.s = lexer->peek[0].value;
					expect(lexer, TOKEN_IDENT);

					if (accept(lexer, TOKEN_EQUAL)) {
						node.child[0] = parse_assign_expr(lexer, pool);
					}

					if (!accept(lexer, TOKEN_COMMA)) {
						break;
					}

					ast_append(pool, &enumerators, node);
				}

				type_specifier.child[0] = enumerators.first;
				expect(lexer, TOKEN_RBRACE);
			} break;
		case TOKEN_STRUCT:
			get_token(lexer);
			type_specifier = ast_make_node(AST_TYPE_STRUCT, lexer->loc);
			token = lexer->peek[0];
			if (token.kind == TOKEN_IDENT) {
				type_specifier.value.s = token.value;
				get_token(lexer);
			}

			if (accept(lexer, TOKEN_LBRACE)) {
				ast_list members = {0};
				while (!lexer->error && !accept(lexer, TOKEN_RBRACE)) {
					ast_list decl = parse_decl(lexer, PARSE_STRUCT_MEMBER, pool);
					if (decl.first.value != 0) {
						ast_concat(pool, &members, decl);
					} else {
						lexer->error = true;
					}

					expect(lexer, TOKEN_SEMICOLON);
				}

				type_specifier.child[0] = members.first;
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
					syntax_error(lexer, "Type cannot be both signed and unsigned");
				} else if ((qualifiers & AST_SHORT && qualifier == AST_LONG)
					|| (qualifiers & AST_LONG && qualifier == AST_SHORT))
				{
					syntax_error(lexer, "Integer cannot be both long and short");
				} else if (qualifiers & qualifier) {
					syntax_error(lexer, "Declaration already has %s qualifier.",
						get_token_name(token.kind));
				}

				found_qualifier = (qualifier != 0);
				if (found_qualifier) {
					qualifiers |= qualifier;
					get_token(lexer);
				}
			} break;
		}
	}

	if (qualifiers & (AST_LLONG | AST_LONG | AST_SHORT | AST_SHORT | AST_SIGNED | AST_UNSIGNED)) {
		type_specifier = ast_make_node(AST_TYPE_INT, lexer->loc);
	}

	ast_list list = {0};
	if (type_specifier.kind == AST_INVALID) {
		if (qualifiers != 0) {
			syntax_error(lexer, "Expected type after qualifiers");
		}

		return list;
	}

	type_specifier.flags = qualifiers;
	if (lexer->peek[0].kind == TOKEN_SEMICOLON) {
		ast_node decl = ast_make_node(AST_DECL, lexer->loc);
		decl.child[0] = ast_push(pool, type_specifier);

		ast_node list_node = ast_make_node(AST_DECL_LIST, lexer->loc);
		list_node.child[0] = ast_push(pool, decl);
		ast_append(pool, &list, list_node);
		return list;
	}

	do {
		ast_node node = ast_make_node(AST_DECL_LIST, lexer->loc);
		ast_list decl = parse_declarator(lexer, flags, pool);
		ASSERT((flags & PARSE_NO_IDENT) || decl.first.value != 0);
		ASSERT((flags & PARSE_NO_IDENT) || decl.last.value != 0);
		ast_append(pool, &decl, type_specifier);

		ast_node *decl_node = ast_get(pool, decl.first);
		decl_node->flags |= type_specifier.flags;
		decl_node->child[0] = decl_node->child[1];
		decl_node->child[1] = ast_id_nil;

		if ((flags & PARSE_BITFIELD) && accept(lexer, TOKEN_COLON)) {
			ast_id type = ast_get(pool, decl.first)->child[0];
			ast_node bitfield = ast_make_node(AST_TYPE_BITFIELD, lexer->loc);
			bitfield.child[0] = parse_assign_expr(lexer, pool);
			bitfield.child[1] = type;

			ast_id bitfield_id = ast_push(pool, bitfield);
			ast_node *decl_node = ast_get(pool, decl.first);
			decl_node->child[0] = bitfield_id;
		}

		if (!(flags & PARSE_NO_INITIALIZER) && accept(lexer, TOKEN_EQUAL)) {
			ast_id initializer = {0};
			if (lexer->peek[0].kind == TOKEN_LBRACE) {
				initializer = parse_initializer(lexer, pool);
			} else {
				initializer = parse_assign_expr(lexer, pool);
			}

			ast_node *decl_node = ast_get(pool, decl.first);
			decl_node->child[1] = initializer;
		}

		node.child[0] = decl.first;
		ast_append(pool, &list, node);
	} while (!lexer->error
		&& !(flags & PARSE_SINGLE_DECL)
		&& accept(lexer, TOKEN_COMMA));

	return list;
}

static ast_id
parse_stmt(lexer *lexer, ast_pool *pool)
{
	ast_id result;

	token token = lexer->peek[0];
	switch (token.kind) {
	case TOKEN_BREAK:
		{
			get_token(lexer);
			expect(lexer, TOKEN_SEMICOLON);

			ast_node node = ast_make_node(AST_STMT_BREAK, lexer->loc);
			result = ast_push(pool, node);
		} break;
	case TOKEN_CASE:
		{
			get_token(lexer);
			ast_node node = ast_make_node(AST_STMT_CASE, lexer->loc);
			node.child[0] = parse_assign_expr(lexer, pool);
			expect(lexer, TOKEN_COLON);
			node.child[1] = parse_stmt(lexer, pool);
			result = ast_push(pool, node);
		} break;
	case TOKEN_CONTINUE:
		{
			get_token(lexer);
			ast_node node = ast_make_node(AST_STMT_CONTINUE, lexer->loc);
			expect(lexer, TOKEN_SEMICOLON);
			result = ast_push(pool, node);
		} break;
	case TOKEN_DEFAULT:
		{
			get_token(lexer);
			expect(lexer, TOKEN_COLON);
			ast_node node = ast_make_node(AST_STMT_DEFAULT, lexer->loc);
			node.child[0] = parse_stmt(lexer, pool);
			result = ast_push(pool, node);
		} break;
	case TOKEN_DO:
		{
			get_token(lexer);
			ast_id body = parse_stmt(lexer, pool);
			expect(lexer, TOKEN_WHILE);
			ast_id cond = parse_assign_expr(lexer, pool);
			expect(lexer, TOKEN_SEMICOLON);

			ast_node node = ast_make_node(AST_STMT_DO_WHILE, lexer->loc);
			node.child[0] = cond;
			node.child[1] = body;
			result = ast_push(pool, node);
		} break;
	case TOKEN_FOR:
		{
			get_token(lexer);
			expect(lexer, TOKEN_LPAREN);

			ast_node init = ast_make_node(AST_STMT_FOR1, lexer->loc);
			if (!accept(lexer, TOKEN_SEMICOLON)) {
				token = lexer->peek[0];
				init.child[0] = parse_decl(lexer, 0, pool).first;
				if (init.child[0].value == 0) {
					init.child[0] = parse_assign_expr(lexer, pool);
				}

				expect(lexer, TOKEN_SEMICOLON);
			} else {
				ast_node empty = ast_make_node(AST_STMT_EMPTY, lexer->loc);
				init.child[0] = ast_push(pool, empty);
			}

			ast_node cond = ast_make_node(AST_STMT_FOR2, lexer->loc);
			if (!accept(lexer, TOKEN_SEMICOLON)) {
				cond.child[0] = parse_assign_expr(lexer, pool);
				expect(lexer, TOKEN_SEMICOLON);
			} else {
				ast_node one = ast_make_node(AST_EXPR_INT, lexer->loc);
				one.value.i = 1;
				cond.child[0] = ast_push(pool, one);
			}

			ast_node post = ast_make_node(AST_STMT_FOR3, lexer->loc);
			if (!accept(lexer, TOKEN_RPAREN)) {
				post.child[0] = parse_assign_expr(lexer, pool);
				expect(lexer, TOKEN_RPAREN);
			} else {
				ast_node empty = ast_make_node(AST_STMT_EMPTY, lexer->loc);
				post.child[0] = ast_push(pool, empty);
			}

			post.child[1] = parse_stmt(lexer, pool);
			cond.child[1] = ast_push(pool, post);
			init.child[1] = ast_push(pool, cond);
			result = ast_push(pool, init);
		} break;
	case TOKEN_GOTO:
		{
			get_token(lexer);
			token = lexer->peek[0];
			expect(lexer, TOKEN_IDENT);
			ast_node node = ast_make_node(AST_STMT_GOTO, lexer->loc);
			node.value.s = token.value;
			result = ast_push(pool, node);
			expect(lexer, TOKEN_SEMICOLON);
		} break;
	case TOKEN_IF:
		{
			get_token(lexer);
			expect(lexer, TOKEN_LPAREN);
			ast_id cond = parse_assign_expr(lexer, pool);
			expect(lexer, TOKEN_RPAREN);
			ast_node if_else = ast_make_node(AST_STMT_IF2, lexer->loc);
			if_else.child[0] = parse_stmt(lexer, pool);
			if (accept(lexer, TOKEN_ELSE)) {
				if_else.child[1] = parse_stmt(lexer, pool);
			} else {
				ast_node empty = ast_make_node(AST_STMT_EMPTY, lexer->loc);
				if_else.child[1] = ast_push(pool, empty);
			}

			ast_node node = ast_make_node(AST_STMT_IF1, lexer->loc);
			node.child[0] = cond;
			node.child[1] = ast_push(pool, if_else);
			result = ast_push(pool, node);
		} break;
	case TOKEN_WHILE:
		{
			get_token(lexer);
			expect(lexer, TOKEN_LPAREN);
			ast_id cond = parse_assign_expr(lexer, pool);
			expect(lexer, TOKEN_RPAREN);
			ast_id body = parse_stmt(lexer, pool);

			ast_node node = ast_make_node(AST_STMT_WHILE, lexer->loc);
			node.child[0] = cond;
			node.child[1] = body;
			result = ast_push(pool, node);
		} break;
	case TOKEN_RETURN:
		{
			get_token(lexer);
			ast_node node = ast_make_node(AST_STMT_RETURN, lexer->loc);
			if (!accept(lexer, TOKEN_SEMICOLON)) {
				node.child[0] = parse_assign_expr(lexer, pool);
				expect(lexer, TOKEN_SEMICOLON);
			}
			result = ast_push(pool, node);
		} break;
	case TOKEN_SWITCH:
		{
			get_token(lexer);
			ast_node node = ast_make_node(AST_STMT_SWITCH, lexer->loc);
			expect(lexer, TOKEN_LPAREN);
			node.child[0] = parse_assign_expr(lexer, pool);
			expect(lexer, TOKEN_RPAREN);
			node.child[1] = parse_stmt(lexer, pool);
			result = ast_push(pool, node);
		} break;
	case TOKEN_SEMICOLON:
		{
			get_token(lexer);
			ast_node node = ast_make_node(AST_STMT_EMPTY, lexer->loc);
			result = ast_push(pool, node);
		} break;
	case TOKEN_LBRACE:
		{
			ast_list list = {0};

			expect(lexer, TOKEN_LBRACE);
			while (!lexer->error && !accept(lexer, TOKEN_RBRACE)) {
				ast_node node = ast_make_node(AST_STMT_LIST, lexer->loc);
				node.child[0] = parse_stmt(lexer, pool);
				ast_append(pool, &list, node);
			}

			if (list.first.value == 0) {
				ast_node node = ast_make_node(AST_STMT_EMPTY, lexer->loc);
				list.first = ast_push(pool, node);
			}

			result = list.first;
		} break;
	default:
		if (lexer->peek[0].kind == TOKEN_IDENT
			&& lexer->peek[1].kind == TOKEN_COLON)
		{
			ast_node node = ast_make_node(AST_STMT_LABEL, lexer->loc);
			node.value.s = token.value;
			get_token(lexer);
			get_token(lexer);
			node.child[0] = parse_stmt(lexer, pool);
			result = ast_push(pool, node);
		} else {
			result = parse_decl(lexer, 0, pool).first;
			if (result.value == 0) {
				result = parse_assign_expr(lexer, pool);
			}
			expect(lexer, TOKEN_SEMICOLON);
		}
	}

#if 0
	if (lexer->error) {
		lexer->error = false;
		while (!accept(lexer, TOKEN_SEMICOLON)
			&& !accept(lexer, TOKEN_RBRACE))
		{
			get_token(lexer);
		}
	}
#endif

	return result;
}

static ast_list
parse_external_decl(lexer *lexer, ast_pool *pool)
{
	ast_list list = parse_decl(lexer, PARSE_EXTERNAL_DECL, pool);
	if (list.first.value == 0) {
		syntax_error(lexer, "Expected declaration");
		return list;
	}

	ast_node *decl_list = ast_get(pool, list.first);
	ast_id decl_id = decl_list->child[0];
	ast_node *decl = ast_get(pool, decl_id);
	ast_node *type = ast_get(pool, decl->child[0]);
	decl->kind = AST_EXTERN_DEF;

	if (decl_list->child[1].value == 0 && type->kind == AST_TYPE_FUNC) {
		token token = lexer->peek[0];
		if (token.kind == TOKEN_LBRACE) {
			ast_id body = parse_stmt(lexer, pool);
			ast_node *decl = ast_get(pool, decl_id);
			decl->child[1] = body;
		} else {
			expect(lexer, TOKEN_SEMICOLON);
		}
	} else {
		expect(lexer, TOKEN_SEMICOLON);
	}

	return list;
}

static b32
parse(lexer *lexer, ast_pool *pool)
{
	ast_list list = {0};

	do {
		ast_list decls = parse_external_decl(lexer, pool);
		ast_concat(pool, &list, decls);
	} while (!lexer->error && !accept(lexer, TOKEN_EOF));

	pool->root = list.first;
	ast_shrink(pool);

	if (pool->root.value == 0) {
		lexer->error = true;
	}

	return lexer->error;
}
