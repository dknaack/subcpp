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
upsert_ident(scope *s, str key, arena *perm)
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
		return upsert_ident(s->parent, key, NULL);
	}

	e = ALLOC(perm, 1, scope_entry);
	e->next = s->idents;
	e->key = key;
	s->idents = e;
	return e;
}

static scope_entry *
upsert_tag(scope *s, str key, arena *perm)
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
		return upsert_tag(s->parent, key, NULL);
	}

	e = ALLOC(perm, 1, scope_entry);
	e->next = s->tags;
	e->key = key;
	s->tags = e;
	return e;
}

static ast_id
new_node_(ast_pool *p, ast_node_kind kind, token token, ast_id child0, ast_id child1)
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
	ast_node *node = &p->nodes[id.value];
	node->kind = kind;
	node->token = token;
	node->child[0] = child0;
	node->child[1] = child1;
	return id;
}

static ast_id
new_node0(ast_pool *p, ast_node_kind kind, token token)
{
	ast_id id = new_node_(p, kind, token, ast_id_nil, ast_id_nil);
	return id;
}

static ast_id
new_node1(ast_pool *p, ast_node_kind kind, token token, ast_id child0)
{
	ASSERT(child0.value != 0);
	ast_id id = new_node_(p, kind, token, child0, ast_id_nil);
	return id;
}

static ast_id
new_node2(ast_pool *p, ast_node_kind kind, token token, ast_id child0, ast_id child1)
{
	ASSERT(child0.value != 0);
	ASSERT(child1.value != 0);
	ast_id id = new_node_(p, kind, token, child0, child1);
	return id;
}

static void
append_node_id(ast_pool *p, ast_list *l, ast_id node_id)
{
	if (l->last.value != 0) {
		ast_node *last = get_node(p, l->last);
		l->last = last->child[1] = node_id;
	} else {
		l->last = l->first = node_id;
	}
}

static void
append_list_node(ast_pool *p, ast_list *l, ast_id node_id)
{
	token empty_token = {0};
	ast_id id = new_node1(p, AST_LIST, empty_token, node_id);
	append_node_id(p, l, id);
}

static void
concat_nodes(ast_pool *p, ast_list *a, ast_list b)
{
	if (b.first.value != 0) {
		if (a->last.value != 0) {
			ast_node *last = get_node(p, a->last);
			last->child[1] = b.first;
			a->last = b.last;
		} else {
			*a = b;
		}
	}
}

static void
vsyntax_error(parse_context *ctx, char *fmt, va_list ap)
{
	if (ctx->error) {
		return;
	}

	vwarnf(get_location(ctx), fmt, ap);
	ctx->error = true;
	ASSERT(!"syntax error");
}

static void
syntax_error(parse_context *ctx, char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	vsyntax_error(ctx, fmt, ap);
	va_end(ap);
}

static b32
accept(parse_context *ctx, token_kind expected_token)
{
	token token = ctx->peek[0];
	if (token.kind == expected_token) {
		get_token(ctx);
		return true;
	} else {
		return false;
	}
}

static void
expect(parse_context *ctx, token_kind expected_token)
{
	if (!accept(ctx, expected_token)) {
		token found_token = ctx->peek[0];
		syntax_error(ctx, "Expected %s, but found %s",
			get_token_name(expected_token), get_token_name(found_token.kind));
	}
}

static b32
is_left_associative(token_kind token)
{
	switch (token) {
	case TOKEN_EQUAL:
	case TOKEN_PLUS_EQUAL:
	case TOKEN_MINUS_EQUAL:
	case TOKEN_STAR_EQUAL:
	case TOKEN_SLASH_EQUAL:
	case TOKEN_PERCENT_EQUAL:
	case TOKEN_AMP_EQUAL:
	case TOKEN_BAR_EQUAL:
	case TOKEN_CARET_EQUAL:
		return false;
	default:
		return true;
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

static ast_list parse_decl(parse_context *ctx, u32 flags, scope *s, ast_pool *pool, arena *arena);
static ast_id parse_initializer(parse_context *ctx, scope *s, ast_pool *pool, arena *arena);

static ast_id
parse_expr(parse_context *ctx, precedence prev_prec, scope *s, ast_pool *pool, arena *arena)
{
	ast_id expr = {0};

	token token = ctx->peek[0];
	switch (token.kind) {
	case TOKEN_IDENT:
		{
			get_token(ctx);
			scope_entry *e = upsert_ident(s, token.value, NULL);
			if (e) {
				ASSERT(e->node_id.value != 0);
				expr = new_node1(pool, AST_EXPR_IDENT, token, e->node_id);
			} else {
				syntax_error(ctx, "unknown ident: %.*s",
					(int)token.value.length, token.value.at);
				return expr;
			}
		} break;
	case TOKEN_LITERAL_STRING:
	case TOKEN_LITERAL_CHAR:
	case TOKEN_LITERAL_FLOAT:
	case TOKEN_LITERAL_INT:
		{
			get_token(ctx);
			expr = new_node0(pool, AST_EXPR_LITERAL, token);
		} break;
	case TOKEN_LPAREN:
		{
			get_token(ctx);

			ast_list decl = parse_decl(ctx, PARSE_CAST, s, pool, arena);
			if (decl.first.value != 0) {
				ast_node *decl_node = get_node(pool, decl.first);
				ast_id type = decl_node->child[0];

				expect(ctx, TOKEN_RPAREN);
				if (ctx->peek[0].kind == TOKEN_LBRACE) {
					ast_id initializer = parse_initializer(ctx, s, pool, arena);
					expr = new_node2(pool, AST_EXPR_COMPOUND, token, type, initializer);
				} else {
					ast_id subexpr = parse_expr(ctx, PREC_PRIMARY, s, pool, arena);
					expr = new_node2(pool, AST_EXPR_CAST, token, type, subexpr);
				}
			} else {
				expr = parse_expr(ctx, 0, s, pool, arena);
				if (expr.value == 0) {
					syntax_error(ctx, "Expected expression");
					return expr;
				}

				expect(ctx, TOKEN_RPAREN);
			}

			ASSERT(expr.value != 0);
		} break;
	case TOKEN_SIZEOF:
		{
			get_token(ctx);

			if (accept(ctx, TOKEN_LPAREN)) {
				ast_list decl = parse_decl(ctx, PARSE_CAST, s, pool, arena);
				// TODO: Add node type for compound initializers
				ast_id initializer = {0};
				if (decl.first.value != 0) {
					expect(ctx, TOKEN_RPAREN);
					if (ctx->peek[0].kind == TOKEN_LBRACE) {
						initializer = parse_initializer(ctx, s, pool, arena);
						(void)initializer;
					} else {
						ast_node *decl_node = get_node(pool, decl.first);
						ast_id type = decl_node->child[0];

						expr = new_node1(pool, AST_EXPR_SIZEOF, token, type);
					}
				} else {
					ast_id subexpr = parse_expr(ctx, 0, s, pool, arena);
					if (subexpr.value == 0) {
						syntax_error(ctx, "Expected expression");
						return expr;
					}

					expect(ctx, TOKEN_RPAREN);

					expr = new_node1(pool, AST_EXPR_SIZEOF, token, subexpr);
				}
			} else {
				ast_id subexpr = parse_expr(ctx, PREC_PRIMARY, s, pool, arena);
				expr = new_node1(pool, AST_EXPR_SIZEOF, token, subexpr);
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
			get_token(ctx);
			ast_id operand = parse_expr(ctx, PREC_PRIMARY, s, pool, arena);
			expr = new_node1(pool, AST_EXPR_UNARY, token, operand);
		} break;
	case TOKEN_BUILTIN_VA_ARG:
		{
			get_token(ctx);
			expect(ctx, TOKEN_LPAREN);
			ast_id expr = parse_expr(ctx, PREC_ASSIGN, s, pool, arena);
			expect(ctx, TOKEN_COMMA);
			ast_id type = parse_decl(ctx, PARSE_CAST, s, pool, arena).first;
			expect(ctx, TOKEN_RPAREN);

			ast_list params = {0};
			ast_id called = new_node0(pool, AST_BUILTIN, token);
			append_list_node(pool, &params, expr);
			append_list_node(pool, &params, type);
			expr = new_node2(pool, AST_EXPR_CALL, token, called, params.first);
		} break;
	case TOKEN_BUILTIN_VA_START:
	case TOKEN_BUILTIN_VA_END:
		{
			expr = new_node0(pool, AST_BUILTIN, token);
		} break;
	default:
		syntax_error(ctx, "Expected expression");
		return ast_id_nil;
	}

	ASSERT(expr.value != 0);

	for (;;) {
		token = ctx->peek[0];
		if (token.kind == TOKEN_EOF
			|| token.kind == TOKEN_SEMICOLON
			|| token.kind == TOKEN_RPAREN)
		{
			break;
		}

		token_kind operator = token.kind;
		if (operator == TOKEN_LPAREN) {
			get_token(ctx);
			ast_list params = {0};
			ast_id called = expr;
			ast_node *called_node = get_node(pool, called);
			if (called_node->kind == AST_EXPR_IDENT
				&& equals(called_node->token.value, S("__builtin_va_arg")))
			{
			} else if (accept(ctx, TOKEN_RPAREN)) {
				expr = new_node1(pool, AST_EXPR_CALL, token, called);
			} else {
				do {
					ast_id expr = parse_expr(ctx, PREC_ASSIGN, s, pool, arena);
					append_list_node(pool, &params, expr);
				} while (!ctx->error && accept(ctx, TOKEN_COMMA));
				expect(ctx, TOKEN_RPAREN);
				expr = new_node2(pool, AST_EXPR_CALL, token, called, params.first);
			}

		} else if (token.kind == TOKEN_DOT || token.kind == TOKEN_ARROW) {
			ast_node_kind kind = AST_EXPR_MEMBER;
			if (token.kind == TOKEN_ARROW) {
				kind = AST_EXPR_MEMBER_PTR;
			}

			get_token(ctx);
			token = ctx->peek[0];
			expect(ctx, TOKEN_IDENT);

			expr = new_node1(pool, kind, token, expr);
		} else {
			precedence prec = get_precedence(operator);
			if (prec == PREC_NONE) {
				return expr;
			}

			if (prec < prev_prec) {
				break;
			}

			get_token(ctx);
			if (is_postfix_operator(token.kind)) {
				ast_id operand = expr;
				expr = new_node1(pool, AST_EXPR_POSTFIX, token, operand);
			} else if (operator == TOKEN_QMARK) {
				ast_id cond = expr;
				ast_id lhs = parse_expr(ctx, PREC_ASSIGN, s, pool, arena);
				expect(ctx, TOKEN_COLON);
				ast_id rhs = parse_expr(ctx, PREC_ASSIGN, s, pool, arena);

				ast_id values = new_node2(pool, AST_EXPR_TERNARY2, token, lhs, rhs);
				expr = new_node2(pool, AST_EXPR_TERNARY1, token, cond, values);
			} else {
				if (operator == TOKEN_LBRACKET) {
					prec = PREC_NONE;
				}

				precedence new_prec = prec + is_left_associative(operator);
				ast_id lhs = expr;
				ast_id rhs = parse_expr(ctx, new_prec, s, pool, arena);
				ASSERT(rhs.value != 0);

				expr = new_node2(pool, AST_EXPR_BINARY, token, lhs, rhs);
				if (token.kind == TOKEN_LBRACKET) {
					expect(ctx, TOKEN_RBRACKET);
				}
			}
		}
	}

	return expr;
}

static ast_id
parse_initializer(parse_context *ctx, scope *s, ast_pool *pool, arena *arena)
{
	ast_list list = {0};
	token first_token = ctx->peek[0];
	expect(ctx, TOKEN_LBRACE);

	do {
		if (ctx->peek[0].kind == TOKEN_LBRACE) {
			ast_id initializer = parse_initializer(ctx, s, pool, arena);
			append_list_node(pool, &list, initializer);
		} else {
			ast_id expr = parse_expr(ctx, PREC_ASSIGN, s, pool, arena);
			append_list_node(pool, &list, expr);
		}

		if (!accept(ctx, TOKEN_COMMA)) {
			break;
		}
	} while (!ctx->error && ctx->peek[0].kind != TOKEN_RBRACE);

	expect(ctx, TOKEN_RBRACE);
	ast_id init = new_node2(pool, AST_INIT, first_token, list.first, list.last);
	return init;
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
parse_declarator(parse_context *ctx, u32 flags, scope *s, ast_pool *pool, arena *arena)
{
	ast_list pointer_declarator = {0};
	while (ctx->peek[0].kind == TOKEN_STAR) {
		token token = get_token(ctx);
		ast_id node = new_node0(pool, AST_TYPE_POINTER, token);
		token_kind qualifier_token = ctx->peek[0].kind;
		switch (qualifier_token) {
		case TOKEN_CONST:
		case TOKEN_RESTRICT:
		case TOKEN_VOLATILE:
			get_token(ctx);
			get_node(pool, node)->flags |= get_qualifier(qualifier_token);
			break;
		default:
			break;
		}

		// prepend node to pointer_declarator list
		get_node(pool, node)->child[1] = pointer_declarator.first;
		pointer_declarator.first = node;
		if (pointer_declarator.last.value == 0) {
			pointer_declarator.last = pointer_declarator.first;
		}
	}

	ast_list declarator = {0};
	if (accept(ctx, TOKEN_LPAREN)) {
		declarator = parse_declarator(ctx, flags, s, pool, arena);
		expect(ctx, TOKEN_RPAREN);
	} else if (!(flags & PARSE_NO_IDENT)) {
		token token = ctx->peek[0];
		if (token.kind == TOKEN_IDENT) {
			get_token(ctx);
		} else {
			token.value.length = 0;
			if (!((token.kind == TOKEN_COLON && (flags & PARSE_BITFIELD)) || (flags & PARSE_OPT_IDENT))) {
				syntax_error(ctx, "Expected identifier, but found %s",
					get_token_name(token.kind));
			}
		}

		ast_id node = new_node0(pool, AST_DECL, token);
		append_node_id(pool, &declarator, node);
	}

	while (!ctx->error) {
		if (ctx->peek[0].kind == TOKEN_LBRACKET) {
			ast_id node = {0};
			token token = get_token(ctx);
			if (!accept(ctx, TOKEN_RBRACKET)) {
				ast_id size_expr = parse_expr(ctx, PREC_ASSIGN, s, pool, arena);
				expect(ctx, TOKEN_RBRACKET);
				node = new_node1(pool, AST_TYPE_ARRAY, token, size_expr);
			} else {
				node = new_node0(pool, AST_TYPE_ARRAY, token);
			}

			append_node_id(pool, &declarator, node);
		} else if (ctx->peek[0].kind == TOKEN_LPAREN) {
			token token = get_token(ctx);
			ast_id node = {0};
			if (ctx->peek[0].kind == TOKEN_RPAREN) {
				node = new_node0(pool, AST_TYPE_FUNC, token);
				get_token(ctx);
			} else if (ctx->peek[0].kind == TOKEN_VOID
				&& ctx->peek[1].kind == TOKEN_RPAREN)
			{
				node = new_node0(pool, AST_TYPE_FUNC, token);
				get_token(ctx);
				get_token(ctx);
			} else {
				scope tmp = new_scope(s);
				ast_list params = {0};
				ast_node_flags flags = 0;
				do {
					ast_list param = {0};
					if (accept(ctx, TOKEN_ELLIPSIS)) {
						flags |= AST_VARIADIC;
						break;
					}

					param = parse_decl(ctx, PARSE_PARAM, &tmp, pool, arena);
					concat_nodes(pool, &params, param);
					if (param.first.value == 0) {
						syntax_error(ctx, "I don't even know what this was supposed to catch :(");
						ctx->error = true;
					}
				} while (!ctx->error && accept(ctx, TOKEN_COMMA));
				expect(ctx, TOKEN_RPAREN);

				node = new_node1(pool, AST_TYPE_FUNC, token, params.first);
				get_node(pool, node)->flags = flags;

				// TODO: For a function definition, the scope that is generated
				// here should be reused in the definition. The difficulty is
				// that only the inner most function declaration should
				// preserve its scope and not any outer functions. An example
				// would be the signal function.
			}

			append_node_id(pool, &declarator, node);
		} else {
			break;
		}
	}

	concat_nodes(pool, &declarator, pointer_declarator);
	return declarator;
}

static ast_list
parse_decl(parse_context *ctx, u32 flags, scope *s, ast_pool *pool, arena *arena)
{
	ast_id type_id = {0};
	u32 qualifiers = 0;
	token qualifier_token = {0};

	b32 found_qualifier = true;
	while (found_qualifier) {
		token token = ctx->peek[0];
		switch (token.kind) {
		case TOKEN_FLOAT:
		case TOKEN_DOUBLE:
		case TOKEN_BOOL:
		case TOKEN_INT:
		case TOKEN_CHAR:
		case TOKEN_VOID:
			type_id = new_node0(pool, AST_TYPE_BASIC, token);
			get_token(ctx);
			break;
		case TOKEN_IDENT:
			{
				scope_entry *e = upsert_ident(s, token.value, NULL);
				if (type_id.value == 0 && e && e->is_type) {
					type_id = new_node1(pool, AST_TYPE_IDENT, token, e->node_id);
					ASSERT(e->node_id.value != 0);
					get_token(ctx);
				} else {
					found_qualifier = false;
				}
			} break;
		case TOKEN_ENUM:
			{
				get_token(ctx);
				accept(ctx, TOKEN_IDENT);
				expect(ctx, TOKEN_LBRACE);

				// TODO: Enumerators declared in a function parameter should be
				// visible from within the function.
				ast_list enumerators = {0};
				token = ctx->peek[0];
				while (!ctx->error && token.kind != TOKEN_RBRACE) {
					expect(ctx, TOKEN_IDENT);

					ast_id node;
					if (accept(ctx, TOKEN_EQUAL)) {
						ast_id expr = parse_expr(ctx, PREC_ASSIGN, s, pool, arena);
						node = new_node1(pool, AST_ENUMERATOR, token, expr);
					} else {
						node = new_node0(pool, AST_ENUMERATOR, token);
					}

					append_node_id(pool, &enumerators, node);
					scope_entry *e = upsert_ident(s, token.value, arena);
					e->node_id = node;

					if (!accept(ctx, TOKEN_COMMA)) {
						break;
					}

					token = ctx->peek[0];
				}

				type_id = new_node1(pool, AST_TYPE_ENUM, token, enumerators.first);
				expect(ctx, TOKEN_RBRACE);
			} break;
		case TOKEN_STRUCT:
		case TOKEN_UNION:
			{
				ast_node_kind node_kind = AST_TYPE_STRUCT;
				if (token.kind == TOKEN_UNION) {
					node_kind = AST_TYPE_UNION;
				}

				get_token(ctx);
				token = ctx->peek[0];
				scope_entry *e = NULL;
				if (token.kind == TOKEN_IDENT) {
					get_token(ctx);
					e = upsert_tag(s, token.value, arena);
					if (e->node_id.value == 0) {
						e->node_id = new_node0(pool, node_kind, token);
						type_id = new_node1(pool, node_kind, token, e->node_id);
						get_node(pool, type_id)->flags |= AST_OPAQUE;
					}

					ASSERT(get_node(pool, e->node_id)->kind != AST_EXTERN_DEF);
				}

				if (accept(ctx, TOKEN_LBRACE)) {
					scope tmp = new_scope(s);
					ast_list members = {0};
					while (!ctx->error && !accept(ctx, TOKEN_RBRACE)) {
						ast_list decl = parse_decl(ctx, PARSE_STRUCT_MEMBER, &tmp, pool, arena);
						if (decl.first.value != 0) {
							concat_nodes(pool, &members, decl);
						} else {
							syntax_error(ctx, "WTF");
							ctx->error = true;
						}

						expect(ctx, TOKEN_SEMICOLON);
					}

					ast_node *def_node = NULL;
					if (e != NULL) {
						def_node = get_node_of_kind(pool, e->node_id, node_kind);
						type_id = e->node_id;
					} else {
						type_id = new_node0(pool, node_kind, token);
						def_node = get_node(pool, type_id);
					}

					def_node->child[0] = members.first;
				} else {
					type_id = new_node0(pool, node_kind, token);
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
					syntax_error(ctx, "Type cannot be both signed and unsigned");
				} else if ((qualifiers & AST_SHORT && qualifier == AST_LONG)
					|| (qualifiers & AST_LONG && qualifier == AST_SHORT))
				{
					syntax_error(ctx, "Integer cannot be both long and short");
				} else if (qualifiers & qualifier) {
					syntax_error(ctx, "Declaration already has %s qualifier.",
						get_token_name(token.kind));
				}

				found_qualifier = (qualifier != 0);
				if (found_qualifier) {
					qualifiers |= qualifier;
					token = get_token(ctx);
					if (qualifier_token.kind == TOKEN_INVALID) {
						qualifier_token = token;
					}
				}
			} break;
		}
	}

	u32 int_mask = (AST_LLONG | AST_LONG | AST_SHORT | AST_SHORT | AST_SIGNED | AST_UNSIGNED);
	if (type_id.value == 0 && (qualifiers & int_mask) != 0) {
		qualifier_token.kind = TOKEN_INT;
		type_id = new_node0(pool, AST_TYPE_BASIC, qualifier_token);
	}

	ast_list list = {0};
	if (type_id.value == 0) {
		if (qualifiers != 0) {
			syntax_error(ctx, "Expected type after qualifiers");
		}

		return list;
	}

	if (ctx->peek[0].kind == TOKEN_SEMICOLON) {
		ast_id decl = new_node1(pool, AST_DECL, ctx->peek[0], type_id);
		append_list_node(pool, &list, decl);
		return list;
	}

	do {
		ast_list decl = parse_declarator(ctx, flags, s, pool, arena);
		ASSERT((flags & PARSE_NO_IDENT) || decl.first.value != 0);
		ASSERT((flags & PARSE_NO_IDENT) || decl.last.value != 0);
		append_node_id(pool, &decl, type_id);

		if (!(flags & PARSE_NO_IDENT)) {
			ast_node *decl_node = get_node_of_kind(pool, decl.first, AST_DECL);
			decl_node->flags |= qualifiers;
			decl_node->child[0] = decl_node->child[1];
			decl_node->child[1] = ast_id_nil;

			scope_entry *e = upsert_ident(s, decl_node->token.value, arena);
			e->is_type = ((qualifiers & AST_TYPEDEF) != 0);
			e->node_id = decl.first;
		}

		if ((flags & PARSE_BITFIELD) && accept(ctx, TOKEN_COLON)) {
			ast_id type = get_node(pool, decl.first)->child[0];
			ast_id size_expr = parse_expr(ctx, PREC_ASSIGN, s, pool, arena);
			ast_id bitfield_id = new_node2(pool, AST_TYPE_BITFIELD, ctx->peek[0], size_expr, type);

			ast_node *decl_node = get_node(pool, decl.first);
			decl_node->child[0] = bitfield_id;
		}

		if (!(flags & PARSE_NO_INITIALIZER) && accept(ctx, TOKEN_EQUAL)) {
			ast_id initializer = {0};
			if (ctx->peek[0].kind == TOKEN_LBRACE) {
				initializer = parse_initializer(ctx, s, pool, arena);
			} else {
				initializer = parse_expr(ctx, PREC_ASSIGN, s, pool, arena);
			}

			ast_node *decl_node = get_node(pool, decl.first);
			decl_node->child[1] = initializer;
		}

		append_list_node(pool, &list, decl.first);
	} while (!ctx->error
		&& !(flags & PARSE_SINGLE_DECL)
		&& accept(ctx, TOKEN_COMMA));

	return list;
}

static ast_id
parse_stmt(parse_context *ctx, scope *s, ast_pool *pool, arena *arena)
{
	ast_id result = {0};

	token token = ctx->peek[0];
	switch (token.kind) {
	case TOKEN_ASM:
		{
			get_token(ctx);
			result = new_node0(pool, AST_STMT_ASM, ctx->peek[0]);
			expect(ctx, TOKEN_LITERAL_STRING);
		} break;
	case TOKEN_BREAK:
		{
			get_token(ctx);
			expect(ctx, TOKEN_SEMICOLON);
			result = new_node0(pool, AST_STMT_BREAK, token);
		} break;
	case TOKEN_CASE:
		{
			get_token(ctx);
			ast_id expr = parse_expr(ctx, PREC_ASSIGN, s, pool, arena);
			expect(ctx, TOKEN_COLON);
			ast_id stmt = parse_stmt(ctx, s, pool, arena);
			result = new_node2(pool, AST_STMT_CASE, token, expr, stmt);
		} break;
	case TOKEN_CONTINUE:
		{
			get_token(ctx);
			expect(ctx, TOKEN_SEMICOLON);
			result = new_node0(pool, AST_STMT_CONTINUE, token);
		} break;
	case TOKEN_DEFAULT:
		{
			get_token(ctx);
			expect(ctx, TOKEN_COLON);
			ast_id stmt = parse_stmt(ctx, s, pool, arena);
			result = new_node1(pool, AST_STMT_DEFAULT, token, stmt);
		} break;
	case TOKEN_DO:
		{
			get_token(ctx);
			ast_id body = parse_stmt(ctx, s, pool, arena);
			expect(ctx, TOKEN_WHILE);
			ast_id cond = parse_expr(ctx, PREC_ASSIGN, s, pool, arena);
			expect(ctx, TOKEN_SEMICOLON);
			result = new_node2(pool, AST_STMT_DO_WHILE, token, cond, body);
		} break;
	case TOKEN_FOR:
		{
			scope tmp = new_scope(s);
			s = &tmp;

			get_token(ctx);
			expect(ctx, TOKEN_LPAREN);

			ast_id init = {0};
			if (!accept(ctx, TOKEN_SEMICOLON)) {
				init = parse_decl(ctx, 0, s, pool, arena).first;
				if (init.value == 0) {
					init = parse_expr(ctx, PREC_ASSIGN, s, pool, arena);
				}

				expect(ctx, TOKEN_SEMICOLON);
			} else {
				init = new_node0(pool, AST_STMT_EMPTY, token);
			}

			ast_id cond = {0};
			if (!accept(ctx, TOKEN_SEMICOLON)) {
				cond = parse_expr(ctx, PREC_ASSIGN, s, pool, arena);
				expect(ctx, TOKEN_SEMICOLON);
			}

			ast_id post = {0};
			if (!accept(ctx, TOKEN_RPAREN)) {
				post = parse_expr(ctx, PREC_ASSIGN, s, pool, arena);
				expect(ctx, TOKEN_RPAREN);
			} else {
				post = new_node0(pool, AST_STMT_EMPTY, token);
			}

			ast_id stmt = parse_stmt(ctx, s, pool, arena);
			ast_id node3 = new_node_(pool, AST_STMT_FOR3, token, post, stmt);
			ast_id node2 = new_node_(pool, AST_STMT_FOR2, token, cond, node3);
			ast_id node1 = new_node_(pool, AST_STMT_FOR1, token, init, node2);
			result = node1;
		} break;
	case TOKEN_GOTO:
		{
			get_token(ctx);
			token = ctx->peek[0];
			expect(ctx, TOKEN_IDENT);
			expect(ctx, TOKEN_SEMICOLON);
			result = new_node0(pool, AST_STMT_GOTO, token);
		} break;
	case TOKEN_IF:
		{
			get_token(ctx);
			expect(ctx, TOKEN_LPAREN);
			ast_id cond = parse_expr(ctx, PREC_ASSIGN, s, pool, arena);
			expect(ctx, TOKEN_RPAREN);

			ast_id if_branch = parse_stmt(ctx, s, pool, arena);
			ast_id else_branch = {0};
			if (accept(ctx, TOKEN_ELSE)) {
				else_branch = parse_stmt(ctx, s, pool, arena);
			} else {
				else_branch = new_node0(pool, AST_STMT_EMPTY, token);
			}

			ast_id branches = new_node2(pool, AST_STMT_IF2, token, if_branch, else_branch);
			result = new_node2(pool, AST_STMT_IF1, token, cond, branches);
		} break;
	case TOKEN_WHILE:
		{
			get_token(ctx);
			expect(ctx, TOKEN_LPAREN);
			ast_id cond = parse_expr(ctx, PREC_ASSIGN, s, pool, arena);
			expect(ctx, TOKEN_RPAREN);
			ast_id body = parse_stmt(ctx, s, pool, arena);

			result = new_node2(pool, AST_STMT_WHILE, token, cond, body);
		} break;
	case TOKEN_RETURN:
		{
			get_token(ctx);
			if (!accept(ctx, TOKEN_SEMICOLON)) {
				ast_id expr = parse_expr(ctx, PREC_ASSIGN, s, pool, arena);
				expect(ctx, TOKEN_SEMICOLON);
				result = new_node1(pool, AST_STMT_RETURN, token, expr);
			} else {
				result = new_node0(pool, AST_STMT_RETURN, token);
			}
		} break;
	case TOKEN_SWITCH:
		{
			get_token(ctx);
			expect(ctx, TOKEN_LPAREN);
			ast_id expr = parse_expr(ctx, PREC_ASSIGN, s, pool, arena);
			expect(ctx, TOKEN_RPAREN);
			ast_id body = parse_stmt(ctx, s, pool, arena);
			result = new_node2(pool, AST_STMT_SWITCH, token, expr, body);
		} break;
	case TOKEN_SEMICOLON:
		{
			get_token(ctx);
			result = new_node0(pool, AST_STMT_EMPTY, token);
		} break;
	case TOKEN_LBRACE:
		{
			scope tmp = new_scope(s);
			s = &tmp;

			ast_list list = {0};
			expect(ctx, TOKEN_LBRACE);
			while (!ctx->error && !accept(ctx, TOKEN_RBRACE)) {
				ast_id stmt = parse_stmt(ctx, s, pool, arena);
				append_list_node(pool, &list, stmt);
			}

			if (list.first.value == 0) {
				list.first = new_node0(pool, AST_STMT_EMPTY, token);
			}

			result = list.first;
		} break;
	default:
		if (ctx->peek[0].kind == TOKEN_IDENT
			&& ctx->peek[1].kind == TOKEN_COLON)
		{
			get_token(ctx);
			get_token(ctx);
			ast_id stmt = parse_stmt(ctx, s, pool, arena);
			result = new_node1(pool, AST_STMT_LABEL, token, stmt);
		} else {
			result = parse_decl(ctx, 0, s, pool, arena).first;
			if (result.value == 0) {
				result = parse_expr(ctx, PREC_ASSIGN, s, pool, arena);
			}
			expect(ctx, TOKEN_SEMICOLON);
		}
	}

#if 0
	if (ctx->error) {
		ctx->error = false;
		while (!accept(ctx, TOKEN_SEMICOLON)
			&& !accept(ctx, TOKEN_RBRACE))
		{
			get_token(ctx);
		}
	}
#endif

	return result;
}

static void make_builtin(str name, b32 is_type, ast_pool *pool, scope *s, arena *arena)
{
	token token = {0};
	token.value = name;

	scope_entry *e = upsert_ident(s, name, arena);
	e->is_type = is_type;
	e->node_id = new_node0(pool, AST_DECL, token);
	ASSERT(e->node_id.value != 0);
}

static ast_pool
parse(parse_context *ctx, arena *arena)
{
	ast_pool pool = {0};
	ast_list list = {0};
	scope s = new_scope(NULL);

	// Insert __builtin_va_list into scope
	make_builtin(S("__builtin_va_list"),  true,  &pool, &s, arena);
	make_builtin(S("__builtin_va_start"), false, &pool, &s, arena);
	make_builtin(S("__builtin_va_end"),   false, &pool, &s, arena);
	make_builtin(S("__builtin_va_arg"),   false, &pool, &s, arena);

	do {
		ast_list decls = parse_decl(ctx, PARSE_EXTERNAL_DECL, &s, &pool, arena);
		if (decls.first.value == 0) {
			syntax_error(ctx, "Expected declaration");
			break;
		}

		ast_node *decl_list = get_node(&pool, decls.first);
		ast_id decl_id = decl_list->child[0];
		ast_node *decl = get_node(&pool, decl_id);
		ast_node *type = get_node(&pool, decl->child[0]);
		decl->kind = AST_EXTERN_DEF;

		if (decl_list->child[1].value == 0 && type->kind == AST_TYPE_FUNC) {
			token token = ctx->peek[0];
			if (token.kind == TOKEN_LBRACE) {
				// Introduce parameters in the new scope
				scope tmp = new_scope(&s);
				for (ast_id param = type->child[0]; param.value != 0;) {
					ast_node *list_node = get_node(&pool, param);
					str param_name = get_node(&pool, list_node->child[0])->token.value;
					param = list_node->child[1];

					scope_entry *e = upsert_ident(&tmp, param_name, arena);
					e->node_id = list_node->child[0];
					ASSERT(e->node_id.value != 0);

					ast_node *node = get_node(&pool, e->node_id);
					ASSERT(node->kind == AST_DECL);
				}

				ast_id body = parse_stmt(ctx, &tmp, &pool, arena);
				ast_node *decl = get_node(&pool, decl_id);
				decl->child[1] = body;
			} else {
				expect(ctx, TOKEN_SEMICOLON);
			}
		} else {
			expect(ctx, TOKEN_SEMICOLON);
		}

		concat_nodes(&pool, &list, decls);
	} while (!ctx->error && !accept(ctx, TOKEN_EOF));

	pool.root = list.first;

	// shrink the pool to size
	ast_node *nodes = realloc(pool.nodes, pool.size * sizeof(*nodes));
	if (nodes) {
		pool.nodes = nodes;
		pool.types = calloc(pool.size, sizeof(*nodes));
		pool.cap = pool.size;
	}

	if (pool.root.value == 0) {
		ASSERT(!"syntax error");
		ctx->error = true;
	}

	return pool;
}
