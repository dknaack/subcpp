static environment
new_environment(environment *parent)
{
	environment env = {0};
	env.typedefs.parent = &parent->typedefs;
	env.idents.parent = &parent->idents;
	env.tags.parent = &parent->tags;
	return env;
}

static scope_entry *
upsert_scope(scope *s, str key, arena *perm)
{
	if (!s) {
		return NULL;
	}

	scope_entry **e = NULL;
	for (e = &s->entries; *e != NULL; e = &(*e)->next) {
		if (equals(key, (*e)->key)) {
			return *e;
		}
	}

	if (!perm) {
		return upsert_scope(s->parent, key, NULL);
	}

	*e = ALLOC(perm, 1, scope_entry);
	(*e)->key = key;
	return *e;
}

static ast_id
new_node(ast_pool *p, ast_node_kind kind, token token, ast_id children)
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
	node->children = children;
	node->next.value = 0;
	return id;
}

static void
append_node(ast_pool *p, ast_list *list, ast_id node)
{
	ASSERT(node.value != 0);
	if (list->last.value != 0) {
		ast_node *tail = get_node(p, list->last);
		tail->next = list->last = node;
	} else {
		list->first = list->last = node;
	}
}

static void
append_list(ast_pool *p, ast_list *list1, ast_list list2)
{
	if (list1->last.value == 0) {
		*list1 = list2;
	} else {
		ast_node *tail = get_node(p, list1->last);
		tail->next = list2.first;
		if (list2.last.value != 0) {
			list1->last = list2.last;
		}
	}
}

// NOTE: Works the same as append_node, but links using children instead of
// next. However, the new child is inserted before the old list of children
// from the last node.
static void
insert_child(ast_pool *p, ast_list *list, ast_id child_id)
{
	if (list->first.value == 0) {
		list->first = list->last = child_id;
	} else {
		ast_node *parent = get_node(p, list->last);
		ast_node *child = get_node(p, child_id);
		child->next = parent->children;
		parent->children = child_id;
		list->last = child_id;
		ASSERT(child_id.value != 0);
	}

}

// NOTE: Works the same as append_node, but links using children instead of
// next. However, the new child is inserted before the old list of children
// from the last node.
static void
insert_root(ast_pool *p, ast_list *list, ast_id parent)
{
	if (list->first.value == 0) {
		list->first = list->last = parent;
	} else {
		ast_node *child_node = get_node(p, list->first);
		ast_node *parent_node = get_node(p, parent);
		child_node->next = parent_node->children;
		parent_node->children = list->first;
		list->first = parent;
		ASSERT(parent.value != 0);
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
	PARSE_OPT            = 1 << 5,

	PARSE_CAST = PARSE_NO_IDENT | PARSE_SINGLE_DECL | PARSE_NO_INITIALIZER,
	PARSE_PARAM = PARSE_SINGLE_DECL | PARSE_NO_INITIALIZER | PARSE_OPT_IDENT,
	PARSE_STRUCT_MEMBER = PARSE_BITFIELD | PARSE_NO_INITIALIZER,
	PARSE_EXTERNAL_DECL = 0,
	PARSE_STMT = PARSE_OPT,
} parse_decl_flags;

static ast_list parse_decl(parse_context *ctx, u32 flags, environment *env);
static ast_id parse_initializer(parse_context *ctx, environment *env);

static ast_id
parse_expr(parse_context *ctx, precedence prev_prec, environment *env)
{
	ast_pool *pool = ctx->pool;
	ast_id expr = {0};

	token token = ctx->peek[0];
	switch (token.kind) {
	case TOKEN_IDENT:
		{
			get_token(ctx);
			scope_entry *e = upsert_scope(&env->idents, token.value, NULL);
			if (e) {
				ASSERT(e->node_id.value != 0);
				expr = new_node(pool, AST_EXPR_IDENT, token, e->node_id);
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
			expr = new_node(pool, AST_EXPR_LITERAL, token, ast_id_nil);
		} break;
	case TOKEN_LPAREN:
		{
			get_token(ctx);

			ast_list decl = parse_decl(ctx, PARSE_CAST, env);
			if (decl.first.value != 0) {
				ast_list children = {0};
				ast_id type = decl.first;
				append_node(pool, &children, type);

				expect(ctx, TOKEN_RPAREN);
				if (ctx->peek[0].kind == TOKEN_LBRACE) {
					ast_id initializer = parse_initializer(ctx, env);
					append_node(pool, &children, initializer);
					expr = new_node(pool, AST_EXPR_COMPOUND, token, children.first);
				} else {
					ast_id subexpr = parse_expr(ctx, PREC_PRIMARY, env);
					append_node(pool, &children, subexpr);
					expr = new_node(pool, AST_EXPR_CAST, token, children.first);
				}
			} else {
				expr = parse_expr(ctx, 0, env);
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
				ast_list decl = parse_decl(ctx, PARSE_CAST, env);
				// TODO: Add node type for compound initializers
				ast_id initializer = {0};
				if (decl.first.value != 0) {
					expect(ctx, TOKEN_RPAREN);
					if (ctx->peek[0].kind == TOKEN_LBRACE) {
						initializer = parse_initializer(ctx, env);
						(void)initializer;
					} else {
						ast_node *decl_node = get_node(pool, decl.first);
						ast_id type = decl_node->children;

						expr = new_node(pool, AST_EXPR_SIZEOF, token, type);
					}
				} else {
					ast_id subexpr = parse_expr(ctx, 0, env);
					if (subexpr.value == 0) {
						syntax_error(ctx, "Expected expression");
						return expr;
					}

					expect(ctx, TOKEN_RPAREN);

					expr = new_node(pool, AST_EXPR_SIZEOF, token, subexpr);
				}
			} else {
				ast_id subexpr = parse_expr(ctx, PREC_PRIMARY, env);
				expr = new_node(pool, AST_EXPR_SIZEOF, token, subexpr);
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
			ast_id operand = parse_expr(ctx, PREC_PRIMARY, env);
			expr = new_node(pool, AST_EXPR_UNARY, token, operand);
		} break;
	case TOKEN_BUILTIN_VA_ARG:
		{
			get_token(ctx);
			expect(ctx, TOKEN_LPAREN);
			ast_id subexpr = parse_expr(ctx, PREC_ASSIGN, env);
			expect(ctx, TOKEN_COMMA);
			ast_id type = parse_decl(ctx, PARSE_CAST, env).first;
			expect(ctx, TOKEN_RPAREN);

			ast_id called = new_node(pool, AST_BUILTIN, token, ast_id_nil);

			ast_list children = {0};
			append_node(pool, &children, called);
			append_node(pool, &children, subexpr);
			append_node(pool, &children, type);
			expr = new_node(pool, AST_EXPR_CALL, token, children.first);
		} break;
	case TOKEN_BUILTIN_VA_START:
	case TOKEN_BUILTIN_VA_END:
		{
			get_token(ctx);
			expr = new_node(pool, AST_BUILTIN, token, ast_id_nil);
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

			ast_list children = {0};
			append_node(pool, &children, expr);
			if (!accept(ctx, TOKEN_RPAREN)) {
				ast_id params = {0};
				ast_id *p = &params;
				do {
					*p = parse_expr(ctx, PREC_ASSIGN, env);
					ASSERT(p->value != 0);
					ast_node *expr_node = get_node(pool, *p);
					p = &expr_node->next;
				} while (!ctx->error && accept(ctx, TOKEN_COMMA));
				expect(ctx, TOKEN_RPAREN);
				append_node(pool, &children, params);
			}

			expr = new_node(pool, AST_EXPR_CALL, token, children.first);
		} else if (token.kind == TOKEN_DOT || token.kind == TOKEN_ARROW) {
			ast_node_kind kind = AST_EXPR_MEMBER;
			if (token.kind == TOKEN_ARROW) {
				kind = AST_EXPR_MEMBER_PTR;
			}

			get_token(ctx);
			token = ctx->peek[0];
			expect(ctx, TOKEN_IDENT);

			expr = new_node(pool, kind, token, expr);
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
				expr = new_node(pool, AST_EXPR_POSTFIX, token, operand);
			} else if (operator == TOKEN_QMARK) {
				ast_id cond = expr;
				ast_id lhs = parse_expr(ctx, PREC_ASSIGN, env);
				expect(ctx, TOKEN_COLON);
				ast_id rhs = parse_expr(ctx, PREC_ASSIGN, env);

				ast_list children = {0};
				append_node(pool, &children, cond);
				append_node(pool, &children, lhs);
				append_node(pool, &children, rhs);
				expr = new_node(pool, AST_EXPR_TERNARY, token, children.first);
			} else {
				if (operator == TOKEN_LBRACKET) {
					prec = PREC_NONE;
				}

				precedence new_prec = prec + is_left_associative(operator);
				ast_id lhs = expr;
				ast_id rhs = parse_expr(ctx, new_prec, env);
				if (token.kind == TOKEN_LBRACKET) {
					expect(ctx, TOKEN_RBRACKET);
				}

				ast_list children = {0};
				append_node(pool, &children, lhs);
				append_node(pool, &children, rhs);
				expr = new_node(pool, AST_EXPR_BINARY, token, children.first);
			}
		}
	}

	return expr;
}

static ast_id
parse_initializer(parse_context *ctx, environment *env)
{
	ast_pool *pool = ctx->pool;
	ast_id children = {0};
	ast_id *p = &children;

	token first_token = ctx->peek[0];
	expect(ctx, TOKEN_LBRACE);
	do {
		if (ctx->peek[0].kind == TOKEN_LBRACE) {
			*p = parse_initializer(ctx, env);
		} else {
			*p = parse_expr(ctx, PREC_ASSIGN, env);
		}

		if (!accept(ctx, TOKEN_COMMA)) {
			break;
		}

		ast_node *node = get_node(pool, *p);
		p = &node->next;
	} while (!ctx->error && ctx->peek[0].kind != TOKEN_RBRACE);

	expect(ctx, TOKEN_RBRACE);
	ast_id init = new_node(pool, AST_INIT, first_token, children);
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
parse_declarator(parse_context *ctx, u32 flags, environment *env)
{
	ast_pool *pool = ctx->pool;
	ast_list pointer_declarator = {0};
	while (ctx->peek[0].kind == TOKEN_STAR) {
		token token = get_token(ctx);
		while (ctx->peek[0].kind == TOKEN_CONST || ctx->peek[0].kind == TOKEN_VOLATILE) {
			get_token(ctx);

			// TODO: Set the flags to the corresponding tokens and only parse
			// one token per qualifier. Right now, we can pass the same
			// qualifier multiple times.
		}

		ast_id node = new_node(pool, AST_TYPE_POINTER, token, ast_id_nil);

		insert_root(pool, &pointer_declarator, node);
	}

	ast_list result = {0};
	if (ctx->peek[0].kind == TOKEN_IDENT) {
		if (flags & PARSE_NO_IDENT) {
			syntax_error(ctx, "Unexpected identifier");
		}

		token token = get_token(ctx);
		result.first = result.last = new_node(pool, AST_DECL, token, ast_id_nil);
	} else if (ctx->peek[0].kind == TOKEN_LPAREN) {
		get_token(ctx);
		result = parse_declarator(ctx, flags, env);
		expect(ctx, TOKEN_RPAREN);
	} else if (!(flags & (PARSE_NO_IDENT | PARSE_OPT_IDENT))) {
		syntax_error(ctx, "Expected '(' or identifier");
	}

	ast_list declarator = {0};
	for (;;) {
		token token = ctx->peek[0];
		if (accept(ctx, TOKEN_LBRACKET)) {
			ast_id size = {0};
			if (!accept(ctx, TOKEN_RBRACKET)) {
				size = parse_expr(ctx, PREC_ASSIGN, env);
				expect(ctx, TOKEN_RBRACKET);
			}

			ast_id node = new_node(pool, AST_TYPE_ARRAY, token, size);
			insert_child(pool, &declarator, node);
		} else if (accept(ctx, TOKEN_LPAREN)) {
			ast_list params = {0};
			if (ctx->peek[0].kind == TOKEN_VOID && ctx->peek[1].kind == TOKEN_RPAREN) {
				get_token(ctx);
				get_token(ctx);
			} else if (!accept(ctx, TOKEN_RPAREN)) {
				environment tmp = new_environment(env);
				do {
					if (accept(ctx, TOKEN_ELLIPSIS)) {
						// TODO: Mark function as variadic
						break;
					}

					ast_id param = parse_decl(ctx, PARSE_PARAM, &tmp).first;
					append_node(pool, &params, param);
					if (!accept(ctx, TOKEN_COMMA)) {
						break;
					}
				} while (!ctx->error);
				expect(ctx, TOKEN_RPAREN);
			}

			ast_id node = new_node(pool, AST_TYPE_FUNC, token, params.first);
			insert_child(pool, &declarator, node);
		} else {
			break;
		}
	}

	// NOTE: Append the declarator tree to the result
	if (declarator.last.value != 0) {
		insert_child(pool, &result, declarator.first);
		result.last = declarator.last;
	}

	if (pointer_declarator.last.value != 0) {
		insert_child(pool, &result, pointer_declarator.first);
		result.last = pointer_declarator.last;
	}

	return result;
}

static ast_list
parse_decl(parse_context *ctx, u32 flags, environment *env)
{
	ast_pool *pool = ctx->pool;
	arena *arena = ctx->arena;
	ast_list list = {0};
	ast_id base_type = {0};
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
		case TOKEN_BUILTIN_VA_LIST:
			base_type = new_node(pool, AST_TYPE_BASIC, token, ast_id_nil);
			get_token(ctx);
			break;
		case TOKEN_IDENT:
			{
				scope_entry *e = upsert_scope(&env->typedefs, token.value, NULL);
				if (base_type.value == 0 && e != NULL) {
					base_type = new_node(pool, AST_TYPE_IDENT, token, e->node_id);
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
					ast_id expr = {0};
					if (accept(ctx, TOKEN_EQUAL)) {
						expr = parse_expr(ctx, PREC_ASSIGN, env);
					}

					node = new_node(pool, AST_ENUMERATOR, token, expr);
					append_node(pool, &enumerators, node);
					scope_entry *e = upsert_scope(&env->idents, token.value, arena);
					e->node_id = node;

					if (!accept(ctx, TOKEN_COMMA)) {
						break;
					}

					token = ctx->peek[0];
				}

				base_type = new_node(pool, AST_TYPE_ENUM, token, enumerators.first);
				expect(ctx, TOKEN_RBRACE);
			} break;
		case TOKEN_STRUCT:
		case TOKEN_UNION:
			{
				token = get_token(ctx);
				ASSERT(!"TODO");
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
	if (base_type.value == 0 && (qualifiers & int_mask) != 0) {
		qualifier_token.kind = TOKEN_INT;
		base_type = new_node(pool, AST_TYPE_BASIC, qualifier_token, ast_id_nil);
	}

	if (base_type.value == 0) {
		if (qualifiers != 0) {
			syntax_error(ctx, "Expected type after qualifiers");
		}

		return list;
	}

	if (ctx->peek[0].kind == TOKEN_SEMICOLON) {
		ast_id decl = new_node(pool, AST_DECL, ctx->peek[0], base_type);
		append_node(pool, &list, decl);
		return list;
	}

	do {
		ast_list declarator = parse_declarator(ctx, flags, env);
		insert_child(pool, &declarator, base_type);
		ASSERT(declarator.first.value != 0);

		ast_id decl = declarator.first;
		ast_node *decl_node = get_node(pool, decl);
		ast_id type = decl_node->children;
		if (!(flags & PARSE_OPT) && decl.value == 0) {
			syntax_error(ctx, "Expected declaration");
		}

		if (decl_node->kind == AST_DECL) {
			decl_node->flags |= qualifiers;
			ASSERT(decl_node->children.value != decl.value);

			str name = decl_node->token.value;
			scope *s = (qualifiers & AST_TYPEDEF) ? &env->typedefs : &env->idents;
			scope_entry *e = upsert_scope(s, name, arena);
			e->node_id = decl;
		} else {
			type = declarator.first;
		}

		if ((flags & PARSE_BITFIELD) && accept(ctx, TOKEN_COLON)) {
			ast_id size = parse_expr(ctx, PREC_ASSIGN, env);

			ast_list children = {0};
			append_node(pool, &children, type);
			append_node(pool, &children, size);
			ast_id bitfield = new_node(pool, AST_TYPE_BITFIELD, ctx->peek[0], children.first);

			ast_node *decl_node = get_node(pool, decl);
			decl_node->children = bitfield;
			ASSERT(bitfield.value != 0);
		}

		// Parse declaration initializer
		if (!(flags & PARSE_NO_INITIALIZER) && accept(ctx, TOKEN_EQUAL)) {
			ast_id initializer = {0};
			if (ctx->peek[0].kind == TOKEN_LBRACE) {
				initializer = parse_initializer(ctx, env);
			} else {
				initializer = parse_expr(ctx, PREC_ASSIGN, env);
			}

			ast_list children = {0};
			append_node(pool, &children, type);
			append_node(pool, &children, initializer);

			ast_node *decl_node = get_node(pool, decl);
			decl_node->children = children.first;
			ASSERT(children.first.value != 0);
		}

		append_node(pool, &list, decl);
	} while (!ctx->error
		&& !(flags & PARSE_SINGLE_DECL)
		&& accept(ctx, TOKEN_COMMA));

	return list;
}

static ast_id
parse_stmt(parse_context *ctx, environment *env)
{
	ast_pool *pool = ctx->pool;
	ast_id result = {0};

	token token = ctx->peek[0];
	switch (token.kind) {
	case TOKEN_ASM:
		{
			get_token(ctx);
			result = new_node(pool, AST_STMT_ASM, token, ast_id_nil);
			expect(ctx, TOKEN_LPAREN);
			expect(ctx, TOKEN_LITERAL_STRING);
			expect(ctx, TOKEN_RPAREN);
		} break;
	case TOKEN_BREAK:
		{
			get_token(ctx);
			expect(ctx, TOKEN_SEMICOLON);
			result = new_node(pool, AST_STMT_BREAK, token, ast_id_nil);
		} break;
	case TOKEN_CASE:
		{
			get_token(ctx);
			ast_id expr = parse_expr(ctx, PREC_ASSIGN, env);
			expect(ctx, TOKEN_COLON);
			ast_id stmt = parse_stmt(ctx, env);

			ast_list children = {0};
			append_node(pool, &children, expr);
			append_node(pool, &children, stmt);
			result = new_node(pool, AST_STMT_CASE, token, expr);
		} break;
	case TOKEN_CONTINUE:
		{
			get_token(ctx);
			expect(ctx, TOKEN_SEMICOLON);
			result = new_node(pool, AST_STMT_CONTINUE, token, ast_id_nil);
		} break;
	case TOKEN_DEFAULT:
		{
			get_token(ctx);
			expect(ctx, TOKEN_COLON);
			ast_id stmt = parse_stmt(ctx, env);
			result = new_node(pool, AST_STMT_DEFAULT, token, stmt);
		} break;
	case TOKEN_DO:
		{
			get_token(ctx);
			ast_id body = parse_stmt(ctx, env);
			expect(ctx, TOKEN_WHILE);
			ast_id cond = parse_expr(ctx, PREC_ASSIGN, env);
			expect(ctx, TOKEN_SEMICOLON);

			ast_list children = {0};
			append_node(pool, &children, body);
			append_node(pool, &children, cond);
			result = new_node(pool, AST_STMT_DO_WHILE, token, children.first);
		} break;
	case TOKEN_FOR:
		{
			environment tmp = new_environment(env);
			env = &tmp;

			get_token(ctx);
			expect(ctx, TOKEN_LPAREN);

			ast_id init = {0};
			if (!accept(ctx, TOKEN_SEMICOLON)) {
				init = parse_decl(ctx, PARSE_STMT, env).first;
				if (init.value == 0) {
					init = parse_expr(ctx, PREC_ASSIGN, env);
				}

				expect(ctx, TOKEN_SEMICOLON);
			} else {
				init = new_node(pool, AST_NONE, token, ast_id_nil);
			}

			ast_id cond = {0};
			if (!accept(ctx, TOKEN_SEMICOLON)) {
				cond = parse_expr(ctx, PREC_ASSIGN, env);
				expect(ctx, TOKEN_SEMICOLON);
			} else {
				cond = new_node(pool, AST_NONE, token, ast_id_nil);
			}

			ast_id post = {0};
			if (!accept(ctx, TOKEN_RPAREN)) {
				post = parse_expr(ctx, PREC_ASSIGN, env);
				expect(ctx, TOKEN_RPAREN);
			} else {
				post = new_node(pool, AST_NONE, token, ast_id_nil);
			}

			ast_id stmt = parse_stmt(ctx, env);

			ast_list children = {0};
			append_node(pool, &children, init);
			append_node(pool, &children, cond);
			append_node(pool, &children, post);
			append_node(pool, &children, stmt);
			result = new_node(pool, AST_STMT_FOR, token, children.first);
		} break;
	case TOKEN_GOTO:
		{
			get_token(ctx);
			token = ctx->peek[0];
			expect(ctx, TOKEN_IDENT);
			expect(ctx, TOKEN_SEMICOLON);
			result = new_node(pool, AST_STMT_GOTO, token, ast_id_nil);
		} break;
	case TOKEN_IF:
		{
			get_token(ctx);
			expect(ctx, TOKEN_LPAREN);
			ast_id cond = parse_expr(ctx, PREC_ASSIGN, env);
			expect(ctx, TOKEN_RPAREN);

			ast_id if_branch = parse_stmt(ctx, env);

			ast_list children = {0};
			append_node(pool, &children, cond);
			append_node(pool, &children, if_branch);

			if (accept(ctx, TOKEN_ELSE)) {
				ast_id else_branch = parse_stmt(ctx, env);
				append_node(pool, &children, else_branch);
			}

			result = new_node(pool, AST_STMT_IF, token, children.first);
		} break;
	case TOKEN_WHILE:
		{
			get_token(ctx);
			expect(ctx, TOKEN_LPAREN);
			ast_id cond = parse_expr(ctx, PREC_ASSIGN, env);
			expect(ctx, TOKEN_RPAREN);
			ast_id body = parse_stmt(ctx, env);

			ast_list children = {0};
			append_node(pool, &children, cond);
			append_node(pool, &children, body);
			result = new_node(pool, AST_STMT_WHILE, token, children.first);
		} break;
	case TOKEN_RETURN:
		{
			get_token(ctx);
			ast_id expr = {0};
			if (!accept(ctx, TOKEN_SEMICOLON)) {
				expr = parse_expr(ctx, PREC_ASSIGN, env);
				expect(ctx, TOKEN_SEMICOLON);
			}

			result = new_node(pool, AST_STMT_RETURN, token, expr);
		} break;
	case TOKEN_SWITCH:
		{
			get_token(ctx);
			expect(ctx, TOKEN_LPAREN);
			ast_id expr = parse_expr(ctx, PREC_ASSIGN, env);
			expect(ctx, TOKEN_RPAREN);
			ast_id body = parse_stmt(ctx, env);

			ast_list children = {0};
			append_node(pool, &children, expr);
			append_node(pool, &children, body);
			result = new_node(pool, AST_STMT_SWITCH, token, expr);
		} break;
	case TOKEN_SEMICOLON:
		{
			get_token(ctx);
			result = new_node(pool, AST_NONE, token, ast_id_nil);
		} break;
	case TOKEN_LBRACE:
		{
			environment tmp = new_environment(env);
			env = &tmp;

			ast_list children = {0};
			expect(ctx, TOKEN_LBRACE);
			while (!ctx->error && !accept(ctx, TOKEN_RBRACE)) {
				ast_id stmt = parse_stmt(ctx, env);
				append_node(pool, &children, stmt);
			}

			result = new_node(pool, AST_STMT_COMPOUND, token, children.first);
		} break;
	default:
		if (ctx->peek[0].kind == TOKEN_IDENT
			&& ctx->peek[1].kind == TOKEN_COLON)
		{
			get_token(ctx);
			get_token(ctx);
			ast_id stmt = parse_stmt(ctx, env);
			result = new_node(pool, AST_STMT_LABEL, token, stmt);
		} else {
			// TODO: Handle multiple declarations in one statement
			ast_list decl = parse_decl(ctx, PARSE_STMT, env);
			if (decl.first.value != 0) {
				result = new_node(pool, AST_STMT_COMPOUND, ctx->peek[0], decl.first);
			} else {
				result = parse_expr(ctx, PREC_NONE, env);
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

static ast_pool
parse(char *filename, arena *perm)
{
	ast_pool pool = {0};
	environment env = {0};

	str contents = read_file(filename, perm);
	parse_context ctx = tokenize(filename, contents, perm);
	ctx.pool = &pool;

	// Insert builtins into scope
	struct { str name; b32 is_type; } builtins[] = {
		{ S("__builtin_va_list"),  true  },
		{ S("__builtin_va_start"), false },
		{ S("__builtin_va_end"),   false },
		{ S("__builtin_va_arg"),   false },
	};

	for (isize i = 0; i < LENGTH(builtins); i++) {
		token token = {0};
		token.value = builtins[i].name;

		scope *s = builtins[i].is_type ? &env.typedefs : &env.idents;
		scope_entry *e = upsert_scope(s, builtins[i].name, perm);
		e->node_id = new_node(&pool, AST_DECL, token, ast_id_nil);
		ASSERT(e->node_id.value != 0);
	}

	ast_list list = {0};
	do {
		ast_list decls = parse_decl(&ctx, PARSE_EXTERNAL_DECL, &env);
		if (decls.first.value == 0) {
			syntax_error(&ctx, "Expected declaration");
			break;
		}

		ast_node *decl = get_node(&pool, decls.first);
		ASSERT(decl->kind == AST_DECL);
		ASSERT(decls.first.value != decl->children.value);
		decl->kind = AST_EXTERN_DEF;

		ast_id type_id = decl->children;
		ast_node *type = get_node(&pool, decl->children);
		if (decl->next.value == 0 && type->kind == AST_TYPE_FUNC) {
			token token = ctx.peek[0];
			if (token.kind == TOKEN_LBRACE) {
				// Introduce parameters in the new scope
				environment tmp = new_environment(&env);
				ast_node *return_node = get_node(&pool, type->children);
				for (ast_id param = return_node->next; param.value != 0;) {
					ast_node *param_node = get_node(&pool, param);
					str param_name = param_node->token.value;

					scope_entry *e = upsert_scope(&tmp.idents, param_name, perm);
					e->node_id = param;
					ASSERT(e->node_id.value != 0);

					ast_node *node = get_node(&pool, e->node_id);
					ASSERT(node->kind == AST_DECL);
					param = param_node->next;
				}

				ast_id body = parse_stmt(&ctx, &tmp);
				ast_node *type = get_node(&pool, type_id);
				type->next = body;
			} else {
				expect(&ctx, TOKEN_SEMICOLON);
			}
		} else {
			expect(&ctx, TOKEN_SEMICOLON);
		}

		append_list(&pool, &list, decls);
	} while (!ctx.error && !accept(&ctx, TOKEN_EOF));

	pool.root = list.first;

	// shrink the pool to size
	ast_node *nodes = realloc(pool.nodes, pool.size * sizeof(*nodes));
	if (nodes) {
		pool.nodes = nodes;
		pool.cap = pool.size;
	}

	if (pool.root.value == 0) {
		ASSERT(!"syntax error");
		ctx.error = true;
	}

	return pool;
}
