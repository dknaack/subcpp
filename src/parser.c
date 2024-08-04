typedef struct scope_entry scope_entry;
struct scope_entry {
	str key;
	ast_id node_id;
	b8 is_type;

	i32 depth;
	scope_entry *next;
};

typedef struct scope scope;
struct scope {
	scope *parent;
	scope_entry *idents;
	scope_entry *tags;
};

static scope
new_scope(scope *parent)
{
	scope s = {0};
	s.parent = parent;
	return s;
}

static scope_entry *
scope_upsert_ident(scope *s, str key, arena *perm)
{
	if (!s) {
		return NULL;
	}

	scope_entry *e = NULL;
	for (e = s->idents; e; e = e->next) {
		if (equals(key, e->key)) {
			return e;
		}
	}

	if (!perm) {
		return scope_upsert_ident(s->parent, key, NULL);
	}

	e = ALLOC(perm, 1, scope_entry);
	e->next = s->idents;
	e->key = key;
	s->idents = e;
	return e;
}

static scope_entry *
scope_upsert_tag(scope *s, str key, arena *perm)
{
	if (!s) {
		return NULL;
	}

	scope_entry *e = NULL;
	for (e = s->tags; e; e = e->next) {
		if (equals(key, e->key)) {
			return e;
		}
	}

	if (!perm) {
		return scope_upsert_tag(s->parent, key, NULL);
	}

	e = ALLOC(perm, 1, scope_entry);
	e->next = s->idents;
	e->key = key;
	s->tags = e;
	return e;
}

static ast_node
ast_make_node(ast_node_kind kind, location loc)
{
	ast_node node = {0};
	node.kind = kind;
	node.loc = loc;
	return node;
}

static ast_id
ast_push(ast_pool *p, ast_node node)
{
	if (p->size + 1 >= p->cap) {
		if (!p->cap) {
			p->cap = 1024;
			p->size = 1;
		}

		p->nodes = realloc(p->nodes, 2 * p->cap * sizeof(*p->nodes));
		memset(p->nodes + p->cap, 0, p->cap * sizeof(*p->nodes));
		p->cap *= 2;
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
ast_append_id(ast_pool *p, ast_list *l, ast_id node_id)
{
	if (l->last.value != 0) {
		ast_node *last = ast_get(p, l->last);
		l->last = last->child[1] = node_id;
	} else {
		l->last = l->first = node_id;
	}
}

static void
ast_append(ast_pool *p, ast_list *l, ast_node node)
{
	ast_id node_id = ast_push(p, node);
	ast_append_id(p, l, node_id);
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
vsyntax_error(cpp_state *lexer, char *fmt, va_list ap)
{
	if (lexer->error) {
		return;
	}

	vwarnf(get_location(lexer), fmt, ap);
	lexer->error = true;
	ASSERT(!"syntax error");
}

static void
syntax_error(cpp_state *lexer, char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	vsyntax_error(lexer, fmt, ap);
	va_end(ap);
}

static b32
accept(cpp_state *lexer, token_kind expected_token)
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
expect(cpp_state *lexer, token_kind expected_token)
{
	if (!accept(lexer, expected_token)) {
		token found_token = lexer->peek[0];
		syntax_error(lexer, "Expected %s, but found %s",
			get_token_name(expected_token), get_token_name(found_token.kind));
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
	PARSE_OPT_IDENT      = 1 << 4,

	PARSE_CAST = PARSE_NO_IDENT | PARSE_SINGLE_DECL | PARSE_NO_INITIALIZER,
	PARSE_PARAM = PARSE_SINGLE_DECL | PARSE_NO_INITIALIZER | PARSE_OPT_IDENT,
	PARSE_STRUCT_MEMBER = PARSE_BITFIELD | PARSE_NO_INITIALIZER,
	PARSE_EXTERNAL_DECL = 0,
} parse_decl_flags;

static ast_list parse_decl(cpp_state *lexer, u32 flags, scope *s, ast_pool *pool, arena *arena);
static ast_id parse_initializer(cpp_state *lexer, scope *s, ast_pool *pool, arena *arena);

static ast_id
parse_expr(cpp_state *lexer, precedence prev_prec, scope *s, ast_pool *pool, arena *arena)
{
	ast_id expr = {0};

	token token = lexer->peek[0];
	switch (token.kind) {
	case TOKEN_IDENT:
		{
			get_token(lexer);
			scope_entry *e = scope_upsert_ident(s, token.value, NULL);
			if (e) {
				ASSERT(e->node_id.value != 0);

				ast_node node = ast_make_node(AST_EXPR_IDENT, get_location(lexer));
				node.value.ref = e->node_id;
				expr = ast_push(pool, node);
			} else {
				syntax_error(lexer, "unknown ident");
				return expr;
			}
		} break;
	case TOKEN_LITERAL_STRING:
		{
			get_token(lexer);
			ast_node literal = ast_make_node(AST_EXPR_STRING, get_location(lexer));
			literal.value.s = token.value;
			expr = ast_push(pool, literal);
			// TODO: implement string concatenation
			while (accept(lexer, TOKEN_LITERAL_STRING)) {}
		} break;
	case TOKEN_LITERAL_CHAR:
		{
			get_token(lexer);
			ast_node literal = ast_make_node(AST_EXPR_CHAR, get_location(lexer));
			// TODO: Parse the value
			literal.value.i = 0;
			expr = ast_push(pool, literal);
		} break;
	case TOKEN_LITERAL_FLOAT:
		{
			get_token(lexer);
			ast_node literal = ast_make_node(AST_EXPR_FLOAT, get_location(lexer));
			// TODO: Parse the value
			literal.value.f = 1.23456f;
			expr = ast_push(pool, literal);
		} break;
	case TOKEN_LITERAL_INT:
		{
			get_token(lexer);
			ast_node literal = ast_make_node(AST_EXPR_INT, get_location(lexer));
			literal.value.i = 0;

			isize i = 0;
			if (token.value.at[1] != 'x') {
				for (i = 0; i < token.value.length && is_digit(token.value.at[i]); i++) {
					literal.value.i *= 10;
					literal.value.i += (token.value.at[i] - '0');
				}
			} else {
				for (i = 2; i < token.value.length && is_hex_digit(token.value.at[i]); i++) {
					literal.value.i *= 16;
					char c = token.value.at[i];
					if (is_digit(c)) {
						literal.value.i += (c - '0');
					} else if ('A' <= c && c <= 'F') {
						literal.value.i += (c - 'A' + 10);
					} else if ('a' <= c && c <= 'f') {
						literal.value.i += (c - 'A' + 10);
					}
				}
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

			ast_list decl = parse_decl(lexer, PARSE_CAST, s, pool, arena);
			// TODO: Add node type for compound initializers
			ast_id initializer = {0};
			if (decl.first.value != 0) {
				ast_node *decl_node = ast_get(pool, decl.first);
				ast_id type = decl_node->child[0];

				expect(lexer, TOKEN_RPAREN);
				if (lexer->peek[0].kind == TOKEN_LBRACE) {
					initializer = parse_initializer(lexer, s, pool, arena);

					ast_node node = ast_make_node(AST_EXPR_COMPOUND, get_location(lexer));
					node.child[0] = type;
					node.child[1] = initializer;
					expr = ast_push(pool, node);
				} else {
					ast_node node = ast_make_node(AST_EXPR_CAST, get_location(lexer));
					node.child[0] = type;
					node.child[1] = parse_expr(lexer, PREC_PRIMARY, s, pool, arena);
					expr = ast_push(pool, node);
				}
			} else {
				expr = parse_expr(lexer, 0, s, pool, arena);
				if (expr.value == 0) {
					syntax_error(lexer, "Expected expression");
					return expr;
				}

				expect(lexer, TOKEN_RPAREN);
			}

			ASSERT(expr.value != 0);
		} break;
	case TOKEN_SIZEOF:
		{
			get_token(lexer);

			if (accept(lexer, TOKEN_LPAREN)) {
				ast_list decl = parse_decl(lexer, PARSE_CAST, s, pool, arena);
				// TODO: Add node type for compound initializers
				ast_id initializer = {0};
				if (decl.first.value != 0) {
					expect(lexer, TOKEN_RPAREN);
					if (lexer->peek[0].kind == TOKEN_LBRACE) {
						initializer = parse_initializer(lexer, s, pool, arena);
						(void)initializer;
					} else {
						ast_node *decl_node = ast_get(pool, decl.first);
						ast_id type = decl_node->child[0];

						ast_node node = ast_make_node(AST_EXPR_SIZEOF, get_location(lexer));
						node.child[0] = type;
						expr = ast_push(pool, node);
					}
				} else {
					expr = parse_expr(lexer, 0, s, pool, arena);
					if (expr.value == 0) {
						syntax_error(lexer, "Expected expression");
						return expr;
					}

					expect(lexer, TOKEN_RPAREN);
				}
			} else {
				parse_expr(lexer, PREC_PRIMARY, s, pool, arena);
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
			ast_node node = ast_make_node(AST_EXPR_UNARY, get_location(lexer));
			node.value.i = token.kind;
			node.child[0] = parse_expr(lexer, PREC_PRIMARY, s, pool, arena);
			expr = ast_push(pool, node);
		} break;
	default:
		syntax_error(lexer, "Expected expression");
		return ast_id_nil;
	}

	ASSERT(expr.value != 0);

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
			ast_id called = expr;
			ast_node *called_node = ast_get(pool, called);
			if (called_node->kind == AST_EXPR_IDENT
				&& equals(called_node->value.s, S("__builtin_va_arg")))
			{
				ast_node expr = ast_make_node(AST_EXPR_LIST, get_location(lexer));
				expr.child[0] = parse_expr(lexer, PREC_ASSIGN, s, pool, arena);
				ast_append(pool, &params, expr);
				expect(lexer, TOKEN_COMMA);

				ast_node type = ast_make_node(AST_EXPR_LIST, get_location(lexer));
				type.child[0] = parse_decl(lexer, PARSE_CAST, s, pool, arena).first;
				ast_append(pool, &params, type);
			} else if (!accept(lexer, TOKEN_RPAREN)) {
				do {
					ast_node node = ast_make_node(AST_EXPR_LIST, get_location(lexer));
					node.child[0] = parse_expr(lexer, PREC_ASSIGN, s, pool, arena);
					ast_append(pool, &params, node);
				} while (!lexer->error && accept(lexer, TOKEN_COMMA));
				expect(lexer, TOKEN_RPAREN);
			}

			ast_node node = ast_make_node(AST_EXPR_CALL, get_location(lexer));
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

			ast_node node = ast_make_node(kind, get_location(lexer));
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
				ast_node node = ast_make_node(AST_EXPR_POSTFIX, get_location(lexer));
				node.value.op = operator;
				node.child[0] = expr;
				expr = ast_push(pool, node);
			} else if (operator == TOKEN_QMARK) {
				ast_node node2 = ast_make_node(AST_EXPR_TERNARY2, get_location(lexer));
				node2.child[0] = parse_expr(lexer, PREC_COMMA, s, pool, arena);
				expect(lexer, TOKEN_COLON);
				node2.child[1] = parse_expr(lexer, PREC_COMMA, s, pool, arena);

				ast_node node1 = ast_make_node(AST_EXPR_TERNARY1, get_location(lexer));
				node1.child[0] = expr;
				node1.child[1] = ast_push(pool, node2);
				expr = ast_push(pool, node1);
			} else {
				if (operator == TOKEN_LBRACKET) {
					prec = PREC_NONE;
				}

				prec -= is_right_associative(operator);
				ast_id rhs = parse_expr(lexer, prec, s, pool, arena);
				ASSERT(rhs.value != 0);

				ast_node node = ast_make_node(AST_EXPR_BINARY, get_location(lexer));
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
parse_initializer(cpp_state *lexer, scope *s, ast_pool *pool, arena *arena)
{
	ast_list list = {0};
	expect(lexer, TOKEN_LBRACE);

	do {
		ast_node node = ast_make_node(AST_INIT_LIST, get_location(lexer));
		if (lexer->peek[0].kind == TOKEN_LBRACE) {
			node.child[0] = parse_initializer(lexer, s, pool, arena);
		} else {
			node.child[0] = parse_expr(lexer, PREC_ASSIGN, s, pool, arena);
		}

		ast_append(pool, &list, node);
		if (!accept(lexer, TOKEN_COMMA)) {
			break;
		}
	} while (!lexer->error && lexer->peek[0].kind != TOKEN_RBRACE);

	expect(lexer, TOKEN_RBRACE);
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
	case TOKEN_VOLATILE:
		return AST_VOLATILE;
	default:
		return 0;
	}
}

static ast_list
parse_declarator(cpp_state *lexer, u32 flags, scope *s, ast_pool *pool, arena *arena)
{
	ast_list pointer_declarator = {0};
	while (accept(lexer, TOKEN_STAR)) {
		ast_node node = ast_make_node(AST_TYPE_POINTER, get_location(lexer));
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
		declarator = parse_declarator(lexer, flags, s, pool, arena);
		expect(lexer, TOKEN_RPAREN);
	} else if (!(flags & PARSE_NO_IDENT)) {
		token token = lexer->peek[0];
		if (token.kind == TOKEN_IDENT) {
			get_token(lexer);
		} else {
			token.value.length = 0;
			if (!((token.kind == TOKEN_COLON && (flags & PARSE_BITFIELD)) || (flags & PARSE_OPT_IDENT))) {
				syntax_error(lexer, "Expected identifier, but found %s",
					get_token_name(token.kind));
			}
		}

		ast_node node = ast_make_node(AST_DECL, get_location(lexer));
		node.value.s = token.value;
		ast_append(pool, &declarator, node);
	}

	while (!lexer->error) {
		if (accept(lexer, TOKEN_LBRACKET)) {
			ast_node node = ast_make_node(AST_TYPE_ARRAY, get_location(lexer));
			if (!accept(lexer, TOKEN_RBRACKET)) {
				node.child[0] = parse_expr(lexer, PREC_ASSIGN, s, pool, arena);
				expect(lexer, TOKEN_RBRACKET);
			}

			ast_append(pool, &declarator, node);
		} else if (accept(lexer, TOKEN_LPAREN)) {
			ast_node node = ast_make_node(AST_TYPE_FUNC, get_location(lexer));
			if (lexer->peek[0].kind == TOKEN_RPAREN) {
				get_token(lexer);
			} else if (lexer->peek[0].kind == TOKEN_VOID
				&& lexer->peek[1].kind == TOKEN_RPAREN)
			{
				get_token(lexer);
				get_token(lexer);
			} else {
				scope tmp = new_scope(s);
				ast_list params = {0};
				do {
					ast_list param = {0};
					if (accept(lexer, TOKEN_ELLIPSIS)) {
						node.flags |= AST_VARIADIC;
						break;
					}

					param = parse_decl(lexer, PARSE_PARAM, &tmp, pool, arena);
					ast_concat(pool, &params, param);
					if (param.first.value == 0) {
						syntax_error(lexer, "I don't even know what this was supposed to catch :(");
						lexer->error = true;
					}
				} while (!lexer->error && accept(lexer, TOKEN_COMMA));
				expect(lexer, TOKEN_RPAREN);
				node.child[0] = params.first;

				// TODO: For a function definition, the scope that is generated
				// here should be reused in the definition. The difficulty is
				// that only the inner most function declaration should
				// preserve its scope and not any outer functions. An example
				// would be the signal function.
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
parse_decl(cpp_state *lexer, u32 flags, scope *s, ast_pool *pool, arena *arena)
{
	ast_id type_id = {0};
	u32 qualifiers = 0;

	b32 found_qualifier = true;
	while (found_qualifier) {
		token token = lexer->peek[0];
		ast_node node = {0};
		switch (token.kind) {
		case TOKEN_FLOAT:
			node = ast_make_node(AST_TYPE_FLOAT, get_location(lexer));
			type_id = ast_push(pool, node);
			get_token(lexer);
			break;
		case TOKEN_DOUBLE:
			// TODO: Define double type in AST
			node = ast_make_node(AST_TYPE_FLOAT, get_location(lexer));
			type_id = ast_push(pool, node);
			get_token(lexer);
			break;
		case TOKEN_BOOL:
			// TODO: Define bool type in AST
			node = ast_make_node(AST_TYPE_INT, get_location(lexer));
			type_id = ast_push(pool, node);
			get_token(lexer);
			break;
		case TOKEN_INT:
			node = ast_make_node(AST_TYPE_INT, get_location(lexer));
			type_id = ast_push(pool, node);
			get_token(lexer);
			break;
		case TOKEN_CHAR:
			node = ast_make_node(AST_TYPE_CHAR, get_location(lexer));
			type_id = ast_push(pool, node);
			get_token(lexer);
			break;
		case TOKEN_VOID:
			node = ast_make_node(AST_TYPE_VOID, get_location(lexer));
			type_id = ast_push(pool, node);
			get_token(lexer);
			break;
		case TOKEN_IDENT:
			{
				scope_entry *e = scope_upsert_ident(s, token.value, NULL);
				if (type_id.value == 0 && e && e->is_type) {
					node = ast_make_node(AST_TYPE_IDENT, get_location(lexer));
					node.value.ref = e->node_id;
					type_id = ast_push(pool, node);
					ASSERT(e->node_id.value != 0);
					get_token(lexer);
				} else {
					found_qualifier = false;
				}
			} break;
		case TOKEN_ENUM:
			{
				node = ast_make_node(AST_TYPE_ENUM, get_location(lexer));
				get_token(lexer);
				accept(lexer, TOKEN_IDENT);
				expect(lexer, TOKEN_LBRACE);

				// TODO: Enumerators declared in a function parameter should be
				// visible from within the function.
				ast_list enumerators = {0};
				while (!lexer->error && lexer->peek[0].kind != TOKEN_RBRACE) {
					ast_node node = ast_make_node(AST_ENUMERATOR, get_location(lexer));
					node.value.s = lexer->peek[0].value;
					expect(lexer, TOKEN_IDENT);

					if (accept(lexer, TOKEN_EQUAL)) {
						node.child[0] = parse_expr(lexer, PREC_ASSIGN, s, pool, arena);
					}

					ast_append(pool, &enumerators, node);
					scope_entry *e = scope_upsert_ident(s, node.value.s, arena);
					e->node_id = enumerators.last;

					if (!accept(lexer, TOKEN_COMMA)) {
						break;
					}
				}

				node.child[0] = enumerators.first;
				type_id = ast_push(pool, node);
				expect(lexer, TOKEN_RBRACE);
			} break;
		case TOKEN_STRUCT:
		case TOKEN_UNION:
			{
				node = ast_make_node(AST_TYPE_STRUCT, get_location(lexer));
				if (token.kind == TOKEN_UNION) {
					node.kind = AST_TYPE_UNION;
				}

				get_token(lexer);
				token = lexer->peek[0];
				scope_entry *e = NULL;
				if (token.kind == TOKEN_IDENT) {
					get_token(lexer);
					e = scope_upsert_tag(s, token.value, arena);
					if (e->node_id.value == 0) {
						node.value.ref.value = 0;
						e->node_id = ast_push(pool, node);
						node.flags |= AST_OPAQUE;
					}

					node.value.ref = e->node_id;
				}

				type_id = ast_push(pool, node);
				if (accept(lexer, TOKEN_LBRACE)) {
					scope tmp = new_scope(s);
					ast_list members = {0};
					while (!lexer->error && !accept(lexer, TOKEN_RBRACE)) {
						ast_list decl = parse_decl(lexer, PARSE_STRUCT_MEMBER, &tmp, pool, arena);
						if (decl.first.value != 0) {
							ast_concat(pool, &members, decl);
						} else {
							syntax_error(lexer, "WTF");
							lexer->error = true;
						}

						expect(lexer, TOKEN_SEMICOLON);
					}

					ast_node *def_node = ast_get(pool, type_id);
					if (e != NULL) {
						def_node = ast_get(pool, e->node_id);
					}

					def_node->child[0] = members.first;
				}

			} break;
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

	u32 int_mask = (AST_LLONG | AST_LONG | AST_SHORT | AST_SHORT | AST_SIGNED | AST_UNSIGNED);
	if (type_id.value == 0 && (qualifiers & int_mask) != 0) {
		ast_node node = ast_make_node(AST_TYPE_INT, get_location(lexer));
		type_id = ast_push(pool, node);
	}

	ast_list list = {0};
	if (type_id.value == 0) {
		if (qualifiers != 0) {
			syntax_error(lexer, "Expected type after qualifiers");
		}

		return list;
	}

	if (lexer->peek[0].kind == TOKEN_SEMICOLON) {
		ast_node decl = ast_make_node(AST_DECL, get_location(lexer));
		decl.child[0] = type_id;

		ast_node list_node = ast_make_node(AST_DECL_LIST, get_location(lexer));
		list_node.child[0] = ast_push(pool, decl);
		ast_append(pool, &list, list_node);
		return list;
	}

	do {
		ast_node node = ast_make_node(AST_DECL_LIST, get_location(lexer));
		ast_list decl = parse_declarator(lexer, flags, s, pool, arena);
		ASSERT((flags & PARSE_NO_IDENT) || decl.first.value != 0);
		ASSERT((flags & PARSE_NO_IDENT) || decl.last.value != 0);
		ast_append_id(pool, &decl, type_id);

		if (!(flags & PARSE_NO_IDENT)) {
			ast_node *decl_node = ast_get(pool, decl.first);
			ASSERT(decl_node->kind == AST_DECL);
			decl_node->flags |= qualifiers;
			decl_node->child[0] = decl_node->child[1];
			decl_node->child[1] = ast_id_nil;

			scope_entry *e = scope_upsert_ident(s, decl_node->value.s, arena);
			e->is_type = ((qualifiers & AST_TYPEDEF) != 0);
			e->node_id = decl.first;
		}

		if ((flags & PARSE_BITFIELD) && accept(lexer, TOKEN_COLON)) {
			ast_id type = ast_get(pool, decl.first)->child[0];
			ast_node bitfield = ast_make_node(AST_TYPE_BITFIELD, get_location(lexer));
			bitfield.child[0] = parse_expr(lexer, PREC_ASSIGN, s, pool, arena);
			bitfield.child[1] = type;

			ast_id bitfield_id = ast_push(pool, bitfield);
			ast_node *decl_node = ast_get(pool, decl.first);
			decl_node->child[0] = bitfield_id;
		}

		if (!(flags & PARSE_NO_INITIALIZER) && accept(lexer, TOKEN_EQUAL)) {
			ast_id initializer = {0};
			if (lexer->peek[0].kind == TOKEN_LBRACE) {
				initializer = parse_initializer(lexer, s, pool, arena);
			} else {
				initializer = parse_expr(lexer, PREC_ASSIGN, s, pool, arena);
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
parse_stmt(cpp_state *lexer, scope *s, ast_pool *pool, arena *arena)
{
	ast_id result = {0};

	token token = lexer->peek[0];
	switch (token.kind) {
	case TOKEN_BREAK:
		{
			get_token(lexer);
			expect(lexer, TOKEN_SEMICOLON);

			ast_node node = ast_make_node(AST_STMT_BREAK, get_location(lexer));
			result = ast_push(pool, node);
		} break;
	case TOKEN_CASE:
		{
			get_token(lexer);
			ast_node node = ast_make_node(AST_STMT_CASE, get_location(lexer));
			node.child[0] = parse_expr(lexer, PREC_ASSIGN, s, pool, arena);
			expect(lexer, TOKEN_COLON);
			node.child[1] = parse_stmt(lexer, s, pool, arena);
			result = ast_push(pool, node);
		} break;
	case TOKEN_CONTINUE:
		{
			get_token(lexer);
			ast_node node = ast_make_node(AST_STMT_CONTINUE, get_location(lexer));
			expect(lexer, TOKEN_SEMICOLON);
			result = ast_push(pool, node);
		} break;
	case TOKEN_DEFAULT:
		{
			get_token(lexer);
			expect(lexer, TOKEN_COLON);
			ast_node node = ast_make_node(AST_STMT_DEFAULT, get_location(lexer));
			node.child[0] = parse_stmt(lexer, s, pool, arena);
			result = ast_push(pool, node);
		} break;
	case TOKEN_DO:
		{
			get_token(lexer);
			ast_id body = parse_stmt(lexer, s, pool, arena);
			expect(lexer, TOKEN_WHILE);
			ast_id cond = parse_expr(lexer, PREC_ASSIGN, s, pool, arena);
			expect(lexer, TOKEN_SEMICOLON);

			ast_node node = ast_make_node(AST_STMT_DO_WHILE, get_location(lexer));
			node.child[0] = cond;
			node.child[1] = body;
			result = ast_push(pool, node);
		} break;
	case TOKEN_FOR:
		{
			scope tmp = new_scope(s);
			s = &tmp;

			get_token(lexer);
			expect(lexer, TOKEN_LPAREN);

			ast_node init = ast_make_node(AST_STMT_FOR1, get_location(lexer));
			if (!accept(lexer, TOKEN_SEMICOLON)) {
				token = lexer->peek[0];
				init.child[0] = parse_decl(lexer, 0, s, pool, arena).first;
				if (init.child[0].value == 0) {
					init.child[0] = parse_expr(lexer, PREC_ASSIGN, s, pool, arena);
				}

				expect(lexer, TOKEN_SEMICOLON);
			} else {
				ast_node empty = ast_make_node(AST_STMT_EMPTY, get_location(lexer));
				init.child[0] = ast_push(pool, empty);
			}

			ast_node cond = ast_make_node(AST_STMT_FOR2, get_location(lexer));
			if (!accept(lexer, TOKEN_SEMICOLON)) {
				cond.child[0] = parse_expr(lexer, PREC_ASSIGN, s, pool, arena);
				expect(lexer, TOKEN_SEMICOLON);
			} else {
				ast_node one = ast_make_node(AST_EXPR_INT, get_location(lexer));
				one.value.i = 1;
				cond.child[0] = ast_push(pool, one);
			}

			ast_node post = ast_make_node(AST_STMT_FOR3, get_location(lexer));
			if (!accept(lexer, TOKEN_RPAREN)) {
				post.child[0] = parse_expr(lexer, PREC_ASSIGN, s, pool, arena);
				expect(lexer, TOKEN_RPAREN);
			} else {
				ast_node empty = ast_make_node(AST_STMT_EMPTY, get_location(lexer));
				post.child[0] = ast_push(pool, empty);
			}

			post.child[1] = parse_stmt(lexer, s, pool, arena);
			cond.child[1] = ast_push(pool, post);
			init.child[1] = ast_push(pool, cond);
			result = ast_push(pool, init);
		} break;
	case TOKEN_GOTO:
		{
			get_token(lexer);
			token = lexer->peek[0];
			expect(lexer, TOKEN_IDENT);
			ast_node node = ast_make_node(AST_STMT_GOTO, get_location(lexer));
			node.value.s = token.value;
			result = ast_push(pool, node);
			expect(lexer, TOKEN_SEMICOLON);
		} break;
	case TOKEN_IF:
		{
			get_token(lexer);
			expect(lexer, TOKEN_LPAREN);
			ast_id cond = parse_expr(lexer, PREC_ASSIGN, s, pool, arena);
			expect(lexer, TOKEN_RPAREN);

			ast_id if_branch = parse_stmt(lexer, s, pool, arena);
			ast_id else_branch = {0};
			if (accept(lexer, TOKEN_ELSE)) {
				else_branch = parse_stmt(lexer, s, pool, arena);
			} else {
				ast_node empty = ast_make_node(AST_STMT_EMPTY, get_location(lexer));
				else_branch = ast_push(pool, empty);
			}

			ast_node branches = ast_make_node(AST_STMT_IF2, get_location(lexer));
			branches.child[0] = if_branch;
			branches.child[1] = else_branch;

			ast_node node = ast_make_node(AST_STMT_IF1, get_location(lexer));
			node.child[0] = cond;
			node.child[1] = ast_push(pool, branches);
			result = ast_push(pool, node);
		} break;
	case TOKEN_WHILE:
		{
			get_token(lexer);
			expect(lexer, TOKEN_LPAREN);
			ast_id cond = parse_expr(lexer, PREC_ASSIGN, s, pool, arena);
			expect(lexer, TOKEN_RPAREN);
			ast_id body = parse_stmt(lexer, s, pool, arena);

			ast_node node = ast_make_node(AST_STMT_WHILE, get_location(lexer));
			node.child[0] = cond;
			node.child[1] = body;
			result = ast_push(pool, node);
		} break;
	case TOKEN_RETURN:
		{
			get_token(lexer);
			ast_node node = ast_make_node(AST_STMT_RETURN, get_location(lexer));
			if (!accept(lexer, TOKEN_SEMICOLON)) {
				node.child[0] = parse_expr(lexer, PREC_ASSIGN, s, pool, arena);
				expect(lexer, TOKEN_SEMICOLON);
			}
			result = ast_push(pool, node);
		} break;
	case TOKEN_SWITCH:
		{
			get_token(lexer);
			ast_node node = ast_make_node(AST_STMT_SWITCH, get_location(lexer));
			expect(lexer, TOKEN_LPAREN);
			node.child[0] = parse_expr(lexer, PREC_ASSIGN, s, pool, arena);
			expect(lexer, TOKEN_RPAREN);
			node.child[1] = parse_stmt(lexer, s, pool, arena);
			result = ast_push(pool, node);
		} break;
	case TOKEN_SEMICOLON:
		{
			get_token(lexer);
			ast_node node = ast_make_node(AST_STMT_EMPTY, get_location(lexer));
			result = ast_push(pool, node);
		} break;
	case TOKEN_LBRACE:
		{
			scope tmp = new_scope(s);
			s = &tmp;

			ast_list list = {0};
			expect(lexer, TOKEN_LBRACE);
			while (!lexer->error && !accept(lexer, TOKEN_RBRACE)) {
				ast_node node = ast_make_node(AST_STMT_LIST, get_location(lexer));
				node.child[0] = parse_stmt(lexer, s, pool, arena);
				ast_append(pool, &list, node);
			}

			if (list.first.value == 0) {
				ast_node node = ast_make_node(AST_STMT_EMPTY, get_location(lexer));
				list.first = ast_push(pool, node);
			}

			result = list.first;
		} break;
	default:
		if (lexer->peek[0].kind == TOKEN_IDENT
			&& lexer->peek[1].kind == TOKEN_COLON)
		{
			ast_node node = ast_make_node(AST_STMT_LABEL, get_location(lexer));
			node.value.s = token.value;
			get_token(lexer);
			get_token(lexer);
			node.child[0] = parse_stmt(lexer, s, pool, arena);
			result = ast_push(pool, node);
		} else {
			result = parse_decl(lexer, 0, s, pool, arena).first;
			if (result.value == 0) {
				result = parse_expr(lexer, PREC_ASSIGN, s, pool, arena);
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

static void make_builtin(str name, b32 is_type, ast_pool *pool, scope *s, arena *arena)
{
	static type type_builtin = {0};

	location loc = {0};
	ast_node node = ast_make_node(AST_DECL, loc);
	node.value.s = name;
	node.type = &type_builtin;

	scope_entry *e = scope_upsert_ident(s, name, arena);
	e->is_type = is_type;
	e->node_id = ast_push(pool, node);
	ASSERT(e->node_id.value != 0);
}

static ast_pool
parse(cpp_state *lexer, arena *arena)
{
	ast_pool pool = {0};
	ast_list list = {0};
	scope s = new_scope(NULL);

	// Insert __builtin_va_list into scope
	make_builtin(S("__builtin_va_list"),  true,  &pool, &s, arena);
	make_builtin(S("__builtin_va_start"), false, &pool, &s, arena);
	make_builtin(S("__builtin_va_end"),   false, &pool, &s, arena);
	make_builtin(S("__builtin_va_arg"),   false, &pool, &s, arena);
	make_builtin(S("asm"), false, &pool, &s, arena);

	do {
		ast_list decls = parse_decl(lexer, PARSE_EXTERNAL_DECL, &s, &pool, arena);
		if (decls.first.value == 0) {
			syntax_error(lexer, "Expected declaration");
			break;
		}

		ast_node *decl_list = ast_get(&pool, decls.first);
		ast_id decl_id = decl_list->child[0];
		ast_node *decl = ast_get(&pool, decl_id);
		ast_node *type = ast_get(&pool, decl->child[0]);
		decl->kind = AST_EXTERN_DEF;

		if (decl_list->child[1].value == 0 && type->kind == AST_TYPE_FUNC) {
			token token = lexer->peek[0];
			if (token.kind == TOKEN_LBRACE) {
				// Introduce parameters in the new scope
				scope tmp = new_scope(&s);
				for (ast_id param = type->child[0]; param.value != 0;) {
					ast_node *list_node = ast_get(&pool, param);
					str param_name = ast_get(&pool, list_node->child[0])->value.s;
					param = list_node->child[1];

					scope_entry *e = scope_upsert_ident(&tmp, param_name, arena);
					e->node_id = list_node->child[0];
					ASSERT(e->node_id.value != 0);

					ast_node *node = ast_get(&pool, e->node_id);
					ASSERT(node->kind == AST_DECL);
				}

				ast_id body = parse_stmt(lexer, &tmp, &pool, arena);
				ast_node *decl = ast_get(&pool, decl_id);
				decl->child[1] = body;
			} else {
				expect(lexer, TOKEN_SEMICOLON);
			}
		} else {
			expect(lexer, TOKEN_SEMICOLON);
		}

		ast_concat(&pool, &list, decls);
	} while (!lexer->error && !accept(lexer, TOKEN_EOF));

	pool.root = list.first;
	ast_shrink(&pool);

	if (pool.root.value == 0) {
		ASSERT(!"syntax error");
		lexer->error = true;
	}

	return pool;
}
