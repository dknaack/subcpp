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
ast_push(ast_pool *pool, ast_node node)
{
	ast_id id = {0};

	if (!pool->cap) {
		pool->cap = 1024;
		pool->size = 1;
		pool->nodes = calloc(pool->cap, sizeof(*pool->nodes));
	} else if (pool->size + 1 >= pool->cap) {
		pool->cap *= 2;
		pool->nodes = realloc(pool->nodes, pool->cap * sizeof(*pool->nodes));
	}

	if (!pool->nodes) {
		return id;
	}

	id.value = pool->size++;
	memcpy(&pool->nodes[id.value], &node, sizeof(node));
	return id;
}

static ast_node *
ast_get(ast_pool *pool, ast_id id)
{
	ast_node *node = NULL;

	if (0 < id.value && id.value < pool->size) {
		node = pool->nodes + id.value;
	} else {
		ASSERT(!"ID is out of bounds");
	}

	return node;
}

static void
ast_append(ast_pool *pool, ast_list *list, ast_node node)
{
	ast_id node_id = ast_push(pool, node);

	if (list->last.value != 0) {
		ast_node *last_node = ast_get(pool, list->last);
		last_node->child[1] = node_id;
	} else {
		list->first = node_id;
	}

	list->last = node_id;
}

static void
ast_prepend(ast_pool *pool, ast_list *list, ast_node node)
{
	node.child[1] = list->first;
	list->first = ast_push(pool, node);
	if (list->last.value == 0) {
		list->last = list->first;
	}
}

static void
ast_concat(ast_pool *pool, ast_list *a, ast_list b)
{
	if (b.first.value != 0) {
		if (a->last.value != 0) {
			ast_node *last =  ast_get(pool, a->last);
			last->child[1] = b.first;
			a->last = b.last;
		} else {
			*a = b;
		}
	}
}

static void
ast_shrink(ast_pool *pool)
{
	ast_node *nodes = realloc(pool->nodes, pool->size * sizeof(*pool->nodes));
	if (nodes) {
		pool->nodes = nodes;
		pool->cap = pool->size;
	}
}

static void
verrorf(location loc, char *fmt, va_list ap)
{
	fprintf(stderr, "%s:%d:%d: ", loc.file, loc.line-1, loc.column);
	vfprintf(stderr, fmt, ap);
	fputc('\n', stderr);
	fflush(stderr);

	//ASSERT(!"Syntax error");
}

static void
errorf(location loc, char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	verrorf(loc, fmt, ap);
	va_end(ap);

	ASSERT(false);
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
		token found_token = tokenizer->peek[0];
		syntax_error(tokenizer, "Expected %s, but found %s",
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

static ast_id parse_assign_expr(tokenizer *tokenizer, ast_pool *pool);

static ast_id
parse_expr(tokenizer *tokenizer, precedence prev_prec, ast_pool *pool)
{
	ast_id expr;

	token token = peek_token(tokenizer);
	switch (token.kind) {
	case TOKEN_IDENT:
		{
			get_token(tokenizer);
			ast_node ident = ast_make_node(AST_EXPR_IDENT, tokenizer->loc);
			ident.value.s = token.value;
			expr = ast_push(pool, ident);
		} break;
	case TOKEN_LITERAL_FLOAT:
		{
			get_token(tokenizer);
			ast_node literal = ast_make_node(AST_EXPR_FLOAT, tokenizer->loc);
			// TODO: Parse the value
			literal.value.f = 1.23456f;
			expr = ast_push(pool, literal);
		} break;
	case TOKEN_LITERAL_INT:
		{
			get_token(tokenizer);
			ast_node literal = ast_make_node(AST_EXPR_INT, tokenizer->loc);
			literal.value.i = 0;

			isize i = 0;
			for (i = 0; i < token.value.length && is_digit(token.value.at[i]); i++) {
				literal.value.i *= 10;
				literal.value.i += (token.value.at[i] - '0');
			}

			for (; !tokenizer->error && i < token.value.length; i++) {
				switch (token.value.at[i]) {
				case 'l':
					if (literal.flags & AST_LLONG) {
						syntax_error(tokenizer, "Invalid suffix '%.*s'",
							(int)token.value.length, token.value.at);
					} else if (literal.flags & AST_LONG) {
						literal.flags |= AST_LLONG;
					} else {
						literal.flags |= AST_LONG;
					}

					break;
				case 'u':
					if (literal.flags & AST_UNSIGNED) {
						syntax_error(tokenizer, "Invalid suffix '%.*s'",
							(int)token.value.length, token.value.at);
					}

					literal.flags |= AST_UNSIGNED;
					break;
				default:
					syntax_error(tokenizer, "Invalid suffix '%.*s'",
						(int)token.value.length, token.value.at);
				}
			}

			expr = ast_push(pool, literal);
		} break;
	case TOKEN_LPAREN:
		get_token(tokenizer);
		expr = parse_expr(tokenizer, 0, pool);
		if (expr.value == 0) {
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
		{
			get_token(tokenizer);
			ast_node node = ast_make_node(AST_EXPR_UNARY, tokenizer->loc);
			node.value.i = token.kind;
			node.child[0] = parse_expr(tokenizer, PREC_PRIMARY, pool);
			expr = ast_push(pool, node);
		} break;
	default:
		syntax_error(tokenizer, "Expected expression");
		return ast_id_nil;
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
			ast_list params = {0};
			if (!accept(tokenizer, TOKEN_RPAREN)) {
				do {
					ast_node node = ast_make_node(AST_EXPR_LIST, tokenizer->loc);
					node.child[0] = parse_assign_expr(tokenizer, pool);
					ast_append(pool, &params, node);
				} while (!tokenizer->error && accept(tokenizer, TOKEN_COMMA));
				expect(tokenizer, TOKEN_RPAREN);
			}

			ast_id called = expr;
			ast_node node = ast_make_node(AST_EXPR_CALL, tokenizer->loc);
			node.child[0] = called;
			node.child[1] = params.first;
			expr = ast_push(pool, node);
		} else if (token.kind == TOKEN_DOT) {
			get_token(tokenizer);
			token = peek_token(tokenizer);
			expect(tokenizer, TOKEN_IDENT);

			ast_node node = ast_make_node(AST_EXPR_MEMBER, tokenizer->loc);
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

			get_token(tokenizer);
			ast_id lhs = expr;
			if (is_postfix_operator(token.kind)) {
				ast_node node = ast_make_node(AST_EXPR_POSTFIX, tokenizer->loc);
				node.value.op = operator;
				node.child[0] = expr;
				expr = ast_push(pool, node);
			} else {
				if (token.kind == TOKEN_LBRACKET) {
					prec = PREC_NONE;
				}

				prec -= is_right_associative(operator);
				ast_id rhs = parse_expr(tokenizer, prec, pool);
				ASSERT(rhs.value != 0);

				ast_node node = ast_make_node(AST_EXPR_BINARY, tokenizer->loc);
				node.value.i = token.kind;
				node.child[0] = lhs;
				node.child[1] = rhs;
				expr = ast_push(pool, node);

				if (token.kind == TOKEN_LBRACKET) {
					expect(tokenizer, TOKEN_RBRACKET);
				}
			}
		}
	}

	return expr;
}

static ast_id
parse_assign_expr(tokenizer *tokenizer, ast_pool *pool)
{
	ast_id expr = parse_expr(tokenizer, PREC_ASSIGN, pool);
	return expr;
}

static ast_id
parse_initializer(tokenizer *t, ast_pool *pool)
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

typedef enum {
	PARSE_SINGLE_DECL    = 1 << 0,
	PARSE_BITFIELD       = 1 << 1,
	PARSE_NO_INITIALIZER = 1 << 2,

	PARSE_PARAM = PARSE_SINGLE_DECL | PARSE_NO_INITIALIZER,
	PARSE_STRUCT_MEMBER = PARSE_BITFIELD | PARSE_NO_INITIALIZER,
	PARSE_EXTERNAL_DECL = 0,
} parse_decl_flags;

static ast_list parse_decl(tokenizer *tokenizer, u32 flags, ast_pool *pool);

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
parse_declarator(tokenizer *tokenizer, ast_pool *pool)
{
	ast_list declarator = {0};
	ast_list pointer_declarator = {0};

	while (accept(tokenizer, TOKEN_STAR)) {
		ast_node node = ast_make_node(AST_TYPE_POINTER, tokenizer->loc);
		token_kind qualifier_token = tokenizer->peek[0].kind;
		switch (qualifier_token) {
		case TOKEN_CONST:
		case TOKEN_RESTRICT:
		case TOKEN_VOLATILE:
			get_token(tokenizer);
			node.flags |= get_qualifier(qualifier_token);
			break;
		default:
			break;
		}

		ast_prepend(pool, &pointer_declarator, node);
	}

	token token = {0};
	if (accept(tokenizer, TOKEN_LPAREN)) {
		declarator = parse_declarator(tokenizer, pool);
		expect(tokenizer, TOKEN_RPAREN);
	} else {
		token = tokenizer->peek[0];
		if (token.kind != TOKEN_IDENT) {
			syntax_error(tokenizer, "Expected identifier, but found %s",
				get_token_name(token.kind));
			token.value.length = 0;
		} else {
			get_token(tokenizer);
		}

		ast_node node = ast_make_node(AST_DECL, tokenizer->loc);
		node.value.s = token.value;
		ast_append(pool, &declarator, node);
	}

	while (!tokenizer->error) {
		if (accept(tokenizer, TOKEN_LBRACKET)) {
			ast_node node = ast_make_node(AST_TYPE_ARRAY, tokenizer->loc);
			node.child[0] = parse_assign_expr(tokenizer, pool);
			ast_append(pool, &declarator, node);
			expect(tokenizer, TOKEN_RBRACKET);
		} else if (accept(tokenizer, TOKEN_LPAREN)) {
			ast_node node = ast_make_node(AST_TYPE_FUNC, tokenizer->loc);

			if (tokenizer->peek[0].kind == TOKEN_RPAREN) {
				get_token(tokenizer);
			} else if (tokenizer->peek[0].kind == TOKEN_VOID
				&& tokenizer->peek[1].kind == TOKEN_RPAREN)
			{
				get_token(tokenizer);
				get_token(tokenizer);
			} else {
				ast_list params = {0};
				do {
					ast_list param = parse_decl(tokenizer, PARSE_PARAM, pool);
					ast_concat(pool, &params, param);
					if (param.first.value == 0) {
						tokenizer->error = true;
					}
				} while (!tokenizer->error && accept(tokenizer, TOKEN_COMMA));
				expect(tokenizer, TOKEN_RPAREN);
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
parse_decl(tokenizer *tokenizer, u32 flags, ast_pool *pool)
{
	ast_node type_specifier = {0};
	u32 qualifiers = 0;

	b32 found_qualifier = true;
	while (found_qualifier) {
		token token = peek_token(tokenizer);
		switch (token.kind) {
		case TOKEN_FLOAT:
			type_specifier = ast_make_node(AST_TYPE_FLOAT, tokenizer->loc);
			get_token(tokenizer);
			break;
		case TOKEN_INT:
			type_specifier = ast_make_node(AST_TYPE_INT, tokenizer->loc);
			get_token(tokenizer);
			break;
		case TOKEN_CHAR:
			type_specifier = ast_make_node(AST_TYPE_CHAR, tokenizer->loc);
			get_token(tokenizer);
			break;
		case TOKEN_VOID:
			type_specifier = ast_make_node(AST_TYPE_VOID, tokenizer->loc);
			get_token(tokenizer);
			break;
		case TOKEN_ENUM:
			{
				type_specifier = ast_make_node(AST_TYPE_ENUM, tokenizer->loc);
				get_token(tokenizer);
				accept(tokenizer, TOKEN_IDENT);
				expect(tokenizer, TOKEN_LBRACE);

				while (!tokenizer->error && tokenizer->peek[0].kind != TOKEN_RBRACE) {
					expect(tokenizer, TOKEN_IDENT);
					if (accept(tokenizer, TOKEN_EQUAL)) {
						parse_assign_expr(tokenizer, pool);
					}

					if (!accept(tokenizer, TOKEN_COMMA)) {
						break;
					}
				}

				expect(tokenizer, TOKEN_RBRACE);
			} break;
		case TOKEN_STRUCT:
			get_token(tokenizer);
			accept(tokenizer, TOKEN_IDENT);

			type_specifier = ast_make_node(AST_TYPE_STRUCT, tokenizer->loc);
			token = peek_token(tokenizer);
			if (token.kind == TOKEN_IDENT) {
				type_specifier.value.s = token.value;
			}

			if (accept(tokenizer, TOKEN_LBRACE)) {
				ast_list members = {0};
				while (!tokenizer->error && !accept(tokenizer, TOKEN_RBRACE)) {
					ast_list decl = parse_decl(tokenizer, PARSE_STRUCT_MEMBER, pool);
					if (decl.first.value != 0) {
						ast_concat(pool, &members, decl);
					} else {
						tokenizer->error = true;
					}

					expect(tokenizer, TOKEN_SEMICOLON);
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
		type_specifier = ast_make_node(AST_TYPE_INT, tokenizer->loc);
	}

	ast_list list = {0};
	if (type_specifier.kind == AST_INVALID) {
		if (flags != 0) {
			syntax_error(tokenizer, "Expected type after qualifiers");
		}

		return list;
	}

	type_specifier.flags = qualifiers;
	if (tokenizer->peek[0].kind == TOKEN_SEMICOLON) {
		ast_node decl = ast_make_node(AST_DECL, tokenizer->loc);
		decl.child[0] = ast_push(pool, type_specifier);

		ast_node list_node = ast_make_node(AST_DECL_LIST, tokenizer->loc);
		list_node.child[0] = ast_push(pool, decl);
		ast_append(pool, &list, list_node);
		return list;
	}

	do {
		ast_node node = ast_make_node(AST_DECL_LIST, tokenizer->loc);
		ast_list decl = parse_declarator(tokenizer, pool);
		ASSERT(decl.first.value != 0);
		ASSERT(decl.last.value != 0);
		ast_append(pool, &decl, type_specifier);

		ast_node *decl_node = ast_get(pool, decl.first);
		decl_node->flags |= type_specifier.flags;
		decl_node->child[0] = decl_node->child[1];
		decl_node->child[1] = ast_id_nil;

		if ((flags & PARSE_BITFIELD) && accept(tokenizer, TOKEN_COLON)) {
			ast_id type = ast_get(pool, decl.first)->child[0];
			ast_node bitfield = ast_make_node(AST_TYPE_BITFIELD, tokenizer->loc);
			bitfield.child[0] = parse_assign_expr(tokenizer, pool);
			bitfield.child[1] = type;

			ast_id bitfield_id = ast_push(pool, bitfield);
			ast_node *decl_node = ast_get(pool, decl.first);
			decl_node->child[0] = bitfield_id;
		}

		if (!(flags & PARSE_NO_INITIALIZER) && accept(tokenizer, TOKEN_EQUAL)) {
			ast_id initializer = {0};
			if (tokenizer->peek[0].kind == TOKEN_LBRACE) {
				initializer = parse_initializer(tokenizer, pool);
			} else {
				initializer = parse_assign_expr(tokenizer, pool);
			}

			ast_node *decl_node = ast_get(pool, decl.first);
			decl_node->child[1] = initializer;
		}

		node.child[0] = decl.first;
		ast_append(pool, &list, node);
	} while (!tokenizer->error
	    && !(flags & PARSE_SINGLE_DECL)
	    && accept(tokenizer, TOKEN_COMMA));

	return list;
}

static ast_id parse_stmt(tokenizer *tokenizer, ast_pool *pool);

static ast_id
parse_compound_stmt(tokenizer *tokenizer, ast_pool *pool)
{
	ast_list list = {0};

	expect(tokenizer, TOKEN_LBRACE);
	while (!tokenizer->error && !accept(tokenizer, TOKEN_RBRACE)) {
		ast_node node = ast_make_node(AST_STMT_LIST, tokenizer->loc);
		node.child[0] = parse_stmt(tokenizer, pool);
		ast_append(pool, &list, node);
	}

	if (list.first.value == 0) {
		ast_node node = ast_make_node(AST_STMT_EMPTY, tokenizer->loc);
		list.first = ast_push(pool, node);
	}

	return list.first;
}

static ast_id
parse_stmt(tokenizer *tokenizer, ast_pool *pool)
{
	ast_id result;

	token token = peek_token(tokenizer);
	switch (token.kind) {
	case TOKEN_BREAK:
		{
			get_token(tokenizer);
			expect(tokenizer, TOKEN_SEMICOLON);

			ast_node node = ast_make_node(AST_STMT_BREAK, tokenizer->loc);
			result = ast_push(pool, node);
		} break;
	case TOKEN_CASE:
		{
			get_token(tokenizer);
			ast_node node = ast_make_node(AST_STMT_CASE, tokenizer->loc);
			node.child[0] = parse_assign_expr(tokenizer, pool);
			expect(tokenizer, TOKEN_COLON);
			node.child[1] = parse_stmt(tokenizer, pool);
			result = ast_push(pool, node);
		} break;
	case TOKEN_CONTINUE:
		{
			get_token(tokenizer);
			ast_node node = ast_make_node(AST_STMT_CONTINUE, tokenizer->loc);
			expect(tokenizer, TOKEN_SEMICOLON);
			result = ast_push(pool, node);
		} break;
	case TOKEN_DEFAULT:
		{
			get_token(tokenizer);
			expect(tokenizer, TOKEN_COLON);
			ast_node node = ast_make_node(AST_STMT_DEFAULT, tokenizer->loc);
			node.child[0] = parse_stmt(tokenizer, pool);
			result = ast_push(pool, node);
		} break;
	case TOKEN_DO:
		{
			get_token(tokenizer);
			ast_id body = parse_stmt(tokenizer, pool);
			expect(tokenizer, TOKEN_WHILE);
			ast_id cond = parse_assign_expr(tokenizer, pool);
			expect(tokenizer, TOKEN_SEMICOLON);

			ast_node node = ast_make_node(AST_STMT_DO_WHILE, tokenizer->loc);
			node.child[0] = cond;
			node.child[1] = body;
			result = ast_push(pool, node);
		} break;
	case TOKEN_FOR:
		{
			get_token(tokenizer);
			expect(tokenizer, TOKEN_LPAREN);

			ast_node init = ast_make_node(AST_STMT_FOR1, tokenizer->loc);
			if (!accept(tokenizer, TOKEN_SEMICOLON)) {
				token = peek_token(tokenizer);
				init.child[0] = parse_decl(tokenizer, 0, pool).first;
				if (init.child[0].value == 0) {
					init.child[0] = parse_assign_expr(tokenizer, pool);
				}

				expect(tokenizer, TOKEN_SEMICOLON);
			} else {
				ast_node empty = ast_make_node(AST_STMT_EMPTY, tokenizer->loc);
				init.child[0] = ast_push(pool, empty);
			}

			ast_node cond = ast_make_node(AST_STMT_FOR2, tokenizer->loc);
			if (!accept(tokenizer, TOKEN_SEMICOLON)) {
				cond.child[0] = parse_assign_expr(tokenizer, pool);
				expect(tokenizer, TOKEN_SEMICOLON);
			} else {
				ast_node one = ast_make_node(AST_EXPR_INT, tokenizer->loc);
				one.value.i = 1;
				cond.child[0] = ast_push(pool, one);
			}

			ast_node post = ast_make_node(AST_STMT_FOR3, tokenizer->loc);
			if (!accept(tokenizer, TOKEN_RPAREN)) {
				post.child[0] = parse_assign_expr(tokenizer, pool);
				expect(tokenizer, TOKEN_RPAREN);
			} else {
				ast_node empty = ast_make_node(AST_STMT_EMPTY, tokenizer->loc);
				post.child[0] = ast_push(pool, empty);
			}

			post.child[1] = parse_stmt(tokenizer, pool);
			cond.child[1] = ast_push(pool, post);
			init.child[1] = ast_push(pool, cond);
			result = ast_push(pool, init);
		} break;
	case TOKEN_GOTO:
		{
			get_token(tokenizer);
			token = peek_token(tokenizer);
			expect(tokenizer, TOKEN_IDENT);
			ast_node node = ast_make_node(AST_STMT_GOTO, tokenizer->loc);
			node.value.s = token.value;
			result = ast_push(pool, node);
			expect(tokenizer, TOKEN_SEMICOLON);
		} break;
	case TOKEN_IF:
		{
			get_token(tokenizer);
			expect(tokenizer, TOKEN_LPAREN);
			ast_id cond = parse_assign_expr(tokenizer, pool);
			expect(tokenizer, TOKEN_RPAREN);
			ast_node if_else = ast_make_node(AST_STMT_IF2, tokenizer->loc);
			if_else.child[0] = parse_stmt(tokenizer, pool);
			if (accept(tokenizer, TOKEN_ELSE)) {
				if_else.child[1] = parse_stmt(tokenizer, pool);
			} else {
				ast_node empty = ast_make_node(AST_STMT_EMPTY, tokenizer->loc);
				if_else.child[1] = ast_push(pool, empty);
			}

			ast_node node = ast_make_node(AST_STMT_IF1, tokenizer->loc);
			node.child[0] = cond;
			node.child[1] = ast_push(pool, if_else);
			result = ast_push(pool, node);
		} break;
	case TOKEN_WHILE:
		{
			get_token(tokenizer);
			expect(tokenizer, TOKEN_LPAREN);
			ast_id cond = parse_assign_expr(tokenizer, pool);
			expect(tokenizer, TOKEN_RPAREN);
			ast_id body = parse_stmt(tokenizer, pool);

			ast_node node = ast_make_node(AST_STMT_WHILE, tokenizer->loc);
			node.child[0] = cond;
			node.child[1] = body;
			result = ast_push(pool, node);
		} break;
	case TOKEN_RETURN:
		{
			get_token(tokenizer);
			ast_node node = ast_make_node(AST_STMT_RETURN, tokenizer->loc);
			if (!accept(tokenizer, TOKEN_SEMICOLON)) {
				node.child[0] = parse_assign_expr(tokenizer, pool);
				expect(tokenizer, TOKEN_SEMICOLON);
			}
			result = ast_push(pool, node);
		} break;
	case TOKEN_PRINT:
		{
			get_token(tokenizer);
			ast_node node = ast_make_node(AST_STMT_PRINT, tokenizer->loc);
			node.child[0] = parse_assign_expr(tokenizer, pool);
			expect(tokenizer, TOKEN_SEMICOLON);
			result = ast_push(pool, node);
		} break;
	case TOKEN_SWITCH:
		{
			get_token(tokenizer);
			ast_node node = ast_make_node(AST_STMT_SWITCH, tokenizer->loc);
			expect(tokenizer, TOKEN_LPAREN);
			node.child[0] = parse_assign_expr(tokenizer, pool);
			expect(tokenizer, TOKEN_RPAREN);
			node.child[1] = parse_stmt(tokenizer, pool);
			result = ast_push(pool, node);
		} break;
	case TOKEN_SEMICOLON:
		{
			get_token(tokenizer);
			ast_node node = ast_make_node(AST_STMT_EMPTY, tokenizer->loc);
			result = ast_push(pool, node);
		} break;
	case TOKEN_LBRACE:
		result = parse_compound_stmt(tokenizer, pool);
		break;
	default:
		if (tokenizer->peek[0].kind == TOKEN_IDENT
			&& tokenizer->peek[1].kind == TOKEN_COLON)
		{
			ast_node node = ast_make_node(AST_STMT_LABEL, tokenizer->loc);
			node.value.s = token.value;
			get_token(tokenizer);
			get_token(tokenizer);
			node.child[0] = parse_stmt(tokenizer, pool);
			result = ast_push(pool, node);
		} else {
			result = parse_decl(tokenizer, 0, pool).first;
			if (result.value == 0) {
				result = parse_assign_expr(tokenizer, pool);
			}
			expect(tokenizer, TOKEN_SEMICOLON);
		}
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

	return result;
}

static ast_list
parse_external_decl(tokenizer *tokenizer, ast_pool *pool)
{
	ast_list list = parse_decl(tokenizer, PARSE_EXTERNAL_DECL, pool);
	if (list.first.value == 0) {
		return list;
	}

	ast_node *decl_list = ast_get(pool, list.first);
	ast_id decl_id = decl_list->child[0];
	ast_node *decl = ast_get(pool, decl_id);
	ast_node *type = ast_get(pool, decl->child[0]);
	decl->kind = AST_EXTERN_DEF;

	if (decl_list->child[1].value == 0 && type->kind == AST_TYPE_FUNC) {
		token token = peek_token(tokenizer);
		if (token.kind == TOKEN_LBRACE) {
			ast_id body = parse_compound_stmt(tokenizer, pool);
			ast_node *decl = ast_get(pool, decl_id);
			decl->child[1] = body;
		} else {
			expect(tokenizer, TOKEN_SEMICOLON);
		}
	} else {
		expect(tokenizer, TOKEN_SEMICOLON);
	}

	return list;
}

static b32
parse(tokenizer *tokenizer, ast_pool *pool)
{
	ast_list list = {0};

	do {
		ast_list decls = parse_external_decl(tokenizer, pool);
		ast_concat(pool, &list, decls);
	} while (!tokenizer->error && !accept(tokenizer, TOKEN_EOF));

	pool->root = list.first;
	ast_shrink(pool);
	return tokenizer->error;
}
