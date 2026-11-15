static b32
add_ident(parse_scope *s, str name, b32 is_type, arena *perm)
{
	for (parse_scope_entry *e = s->entries; e; e = e->next) {
		if (equals(e->name, name)) {
			return e->is_type == is_type;
		}
	}

	parse_scope_entry *e = ALLOC(perm, 1, parse_scope_entry);
	e->name = name;
	e->is_type = is_type;
	e->next = s->entries;
	s->entries = e;
	return true;
}

static b32
is_typename(parse_scope *s, str name)
{
	if (!s) {
		return false;
	}

	for (parse_scope_entry *e = s->entries; e; e = e->next) {
		if (equals(e->name, name)) {
			return e->is_type;
		}
	}

	return is_typename(s->parent, name);
}

static syntax_node_id
new_node_with_flags(syntax_tree *p, syntax_node_kind kind, syntax_node_flags flags, token token, syntax_node_id children)
{
	if (p->size + 1 >= p->max_size) {
		if (!p->max_size) {
			p->max_size = 1024;
			p->size = 1;
		}

		p->nodes = realloc(p->nodes, 2 * p->max_size * sizeof(*p->nodes));
		memset(p->nodes + p->max_size, 0, p->max_size * sizeof(*p->nodes));
		p->max_size *= 2;
	}

	syntax_node_id id = {0};
	if (!p->nodes) {
		return id;
	}

	id.value = p->size++;
	syntax_node *node = &p->nodes[id.value];
	node->kind = kind;
	node->flags = flags;
	node->token = token;
	node->children = children;
	node->next.value = 0;
	return id;
}

static syntax_node_id
new_node(syntax_tree *p, syntax_node_kind kind, token token, syntax_node_id children)
{
	syntax_node_id result = new_node_with_flags(p, kind, 0, token, children);
	return result;
}

static void
append_node(syntax_tree *p, syntax_node_list *list, syntax_node_id node)
{
	ASSERT(node.value != 0);
	if (list->last.value != 0) {
		syntax_node *tail = &p->nodes[list->last.value];
		tail->next = list->last = node;
	} else {
		list->first = list->last = node;
	}
}

static void
append_list(syntax_tree *p, syntax_node_list *list1, syntax_node_list list2)
{
	if (list1->last.value == 0) {
		*list1 = list2;
	} else {
		syntax_node *tail = &p->nodes[list1->last.value];
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
insert_child(syntax_tree *p, syntax_node_list *list, syntax_node_id child_id)
{
	if (list->first.value == 0) {
		list->first = list->last = child_id;
	} else {
		syntax_node *parent = &p->nodes[list->last.value];
		syntax_node *child = &p->nodes[child_id.value];
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
insert_root(syntax_tree *p, syntax_node_list *list, syntax_node_id parent)
{
	if (list->first.value == 0) {
		list->first = list->last = parent;
	} else {
		syntax_node *child_node = &p->nodes[list->first.value];
		syntax_node *parent_node = &p->nodes[parent.value];
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

typedef enum {
	PARSE_SINGLE_DECL    = 1 << 0,
	PARSE_BITFIELD       = 1 << 1,
	PARSE_NO_INITIALIZER = 1 << 2,
	PARSE_NO_IDENT       = 1 << 3,
	PARSE_OPT_IDENT      = 1 << 4,
	PARSE_OPT            = 1 << 5,
	PARSE_EXTERN_DEF     = 1 << 6, // return SYNTAX_EXTERN_DEF instead of SYNTAX_DECL

	PARSE_CAST = PARSE_NO_IDENT | PARSE_SINGLE_DECL | PARSE_NO_INITIALIZER,
	PARSE_PARAM = PARSE_SINGLE_DECL | PARSE_NO_INITIALIZER | PARSE_OPT_IDENT,
	PARSE_MEMBER = PARSE_BITFIELD | PARSE_NO_INITIALIZER,
	PARSE_EXTERNAL_DECL = PARSE_EXTERN_DEF,
	PARSE_STMT = PARSE_OPT,
} parse_decl_flags;

static syntax_node_list parse_decl(parse_context *ctx, u32 flags, parse_scope *s);
static syntax_node_id parse_initializer(parse_context *ctx, parse_scope *s);

static syntax_node_id
parse_expr(parse_context *ctx, precedence prev_prec, parse_scope *s)
{
	syntax_tree *tree = ctx->tree;
	syntax_node_id expr = {0};

	token token = ctx->peek[0];
	switch (token.kind) {
	case TOKEN_IDENT:
		{
			get_token(ctx);
			if (is_typename(s, token.value)) {
				return expr;
			}

			expr = new_node(tree, SYNTAX_EXPR_IDENT, token, syntax_nil);
		} break;
	case TOKEN_LITERAL_STRING:
		{
			while (accept(ctx, TOKEN_LITERAL_STRING)) {}
			expr = new_node(tree, SYNTAX_EXPR_LITERAL, token, syntax_nil);
		} break;
	case TOKEN_LITERAL_CHAR:
	case TOKEN_LITERAL_FLOAT:
	case TOKEN_LITERAL_INT:
		{
			get_token(ctx);
			expr = new_node(tree, SYNTAX_EXPR_LITERAL, token, syntax_nil);
		} break;
	case TOKEN_LPAREN:
		{
			get_token(ctx);

			syntax_node_list decl = parse_decl(ctx, PARSE_CAST, s);
			if (decl.first.value != 0) {
				syntax_node_list children = {0};
				syntax_node_id type = decl.first;
				append_node(tree, &children, type);

				expect(ctx, TOKEN_RPAREN);
				if (ctx->peek[0].kind == TOKEN_LBRACE) {
					syntax_node_id initializer = parse_initializer(ctx, s);
					append_node(tree, &children, initializer);
					expr = new_node(tree, SYNTAX_EXPR_COMPOUND, token, children.first);
				} else {
					syntax_node_id subexpr = parse_expr(ctx, PREC_PRIMARY, s);
					append_node(tree, &children, subexpr);
					expr = new_node(tree, SYNTAX_EXPR_CAST, token, children.first);
				}
			} else {
				expr = parse_expr(ctx, 0, s);
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
				syntax_node_list decl = parse_decl(ctx, PARSE_CAST, s);
				// TODO: Add node type for compound initializers
				syntax_node_id initializer = {0};
				if (decl.first.value != 0) {
					expect(ctx, TOKEN_RPAREN);
					if (ctx->peek[0].kind == TOKEN_LBRACE) {
						initializer = parse_initializer(ctx, s);
						(void)initializer;
					} else {
						syntax_node node = get_node(tree, decl.first);
						syntax_node_id type = node.children;

						expr = new_node(tree, SYNTAX_EXPR_SIZEOF, token, type);
					}
				} else {
					syntax_node_id subexpr = parse_expr(ctx, 0, s);
					if (subexpr.value == 0) {
						syntax_error(ctx, "Expected expression");
						return expr;
					}

					expect(ctx, TOKEN_RPAREN);

					expr = new_node(tree, SYNTAX_EXPR_SIZEOF, token, subexpr);
				}
			} else {
				syntax_node_id subexpr = parse_expr(ctx, PREC_PRIMARY, s);
				expr = new_node(tree, SYNTAX_EXPR_SIZEOF, token, subexpr);
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
			syntax_node_id operand = parse_expr(ctx, PREC_PRIMARY, s);
			expr = new_node(tree, SYNTAX_EXPR_UNARY, token, operand);
		} break;
	case TOKEN_BUILTIN_VA_ARG:
		{
			get_token(ctx);
			expect(ctx, TOKEN_LPAREN);
			syntax_node_id subexpr = parse_expr(ctx, PREC_ASSIGN, s);
			expect(ctx, TOKEN_COMMA);
			syntax_node_id type = parse_decl(ctx, PARSE_CAST, s).first;
			expect(ctx, TOKEN_RPAREN);

			syntax_node_id called = new_node(tree, SYNTAX_BUILTIN, token, syntax_nil);

			syntax_node_list children = {0};
			append_node(tree, &children, called);
			append_node(tree, &children, subexpr);
			append_node(tree, &children, type);
			expr = new_node(tree, SYNTAX_EXPR_CALL, token, children.first);
		} break;
	case TOKEN_BUILTIN_VA_START:
	case TOKEN_BUILTIN_VA_END:
		{
			get_token(ctx);
			expr = new_node(tree, SYNTAX_BUILTIN, token, syntax_nil);
		} break;
	default:
		syntax_error(ctx, "Expected expression");
		return syntax_nil;
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

			syntax_node_list children = {0};
			append_node(tree, &children, expr);
			if (!accept(ctx, TOKEN_RPAREN)) {
				syntax_node_list param_list = {0};
				do {
					syntax_node_id param = parse_expr(ctx, PREC_ASSIGN, s);
					append_node(tree, &param_list, param);
				} while (!ctx->error && accept(ctx, TOKEN_COMMA));
				expect(ctx, TOKEN_RPAREN);
				append_node(tree, &children, param_list.first);
			}

			expr = new_node(tree, SYNTAX_EXPR_CALL, token, children.first);
		} else if (token.kind == TOKEN_DOT || token.kind == TOKEN_ARROW) {
			syntax_node_kind kind = SYNTAX_EXPR_MEMBER;
			if (token.kind == TOKEN_ARROW) {
				kind = SYNTAX_EXPR_MEMBER_PTR;
			}

			get_token(ctx);
			token = ctx->peek[0];
			expect(ctx, TOKEN_IDENT);

			expr = new_node(tree, kind, token, expr);
		} else {
			precedence prec = get_precedence(operator);
			if (prec == PREC_NONE) {
				return expr;
			}

			if (prec < prev_prec) {
				break;
			}

			get_token(ctx);

			b32 is_postfix_operator = token.kind == TOKEN_PLUS_PLUS
				|| token.kind == TOKEN_MINUS_MINUS;
			if (is_postfix_operator) {
				syntax_node_id operand = expr;
				expr = new_node(tree, SYNTAX_EXPR_POSTFIX, token, operand);
			} else if (operator == TOKEN_QMARK) {
				syntax_node_id cond = expr;
				syntax_node_id lhs = parse_expr(ctx, PREC_ASSIGN, s);
				expect(ctx, TOKEN_COLON);
				syntax_node_id rhs = parse_expr(ctx, PREC_ASSIGN, s);

				syntax_node_list children = {0};
				append_node(tree, &children, cond);
				append_node(tree, &children, lhs);
				append_node(tree, &children, rhs);
				expr = new_node(tree, SYNTAX_EXPR_TERNARY, token, children.first);
			} else {
				if (operator == TOKEN_LBRACKET) {
					prec = PREC_NONE;
				}

				b32 is_left_associative = false;
				switch (operator) {
				case TOKEN_EQUAL:
				case TOKEN_PLUS_EQUAL:
				case TOKEN_MINUS_EQUAL:
				case TOKEN_STAR_EQUAL:
				case TOKEN_SLASH_EQUAL:
				case TOKEN_PERCENT_EQUAL:
				case TOKEN_AMP_EQUAL:
				case TOKEN_BAR_EQUAL:
				case TOKEN_CARET_EQUAL:
					is_left_associative = false;
					break;
				default:
					is_left_associative = true;
					break;
				}

				precedence new_prec = prec + is_left_associative;
				syntax_node_id lhs = expr;
				syntax_node_id rhs = parse_expr(ctx, new_prec, s);
				if (token.kind == TOKEN_LBRACKET) {
					expect(ctx, TOKEN_RBRACKET);
				}

				syntax_node_list children = {0};
				append_node(tree, &children, lhs);
				append_node(tree, &children, rhs);
				expr = new_node(tree, SYNTAX_EXPR_BINARY, token, children.first);
			}
		}
	}

	return expr;
}

static syntax_node_id
parse_initializer(parse_context *ctx, parse_scope *s)
{
	syntax_tree *tree = ctx->tree;
	syntax_node_list children = {0};

	token first_token = ctx->peek[0];
	expect(ctx, TOKEN_LBRACE);
	do {
		syntax_node_id node = {0};
		if (ctx->peek[0].kind == TOKEN_LBRACE) {
			node = parse_initializer(ctx, s);
		} else {
			node = parse_expr(ctx, PREC_ASSIGN, s);
		}

		append_node(tree, &children, node);
		if (!accept(ctx, TOKEN_COMMA)) {
			break;
		}
	} while (!ctx->error && ctx->peek[0].kind != TOKEN_RBRACE);

	expect(ctx, TOKEN_RBRACE);
	syntax_node_id init = new_node(tree, SYNTAX_INIT, first_token, children.first);
	return init;
}

static syntax_node_list
parse_declarator(parse_context *ctx, u32 flags, u32 qualifiers, parse_scope *s)
{
	syntax_tree *tree = ctx->tree;
	syntax_node_list pointer_declarator = {0};
	while (ctx->peek[0].kind == TOKEN_STAR) {
		token token = get_token(ctx);
		while (ctx->peek[0].kind == TOKEN_CONST || ctx->peek[0].kind == TOKEN_VOLATILE) {
			get_token(ctx);

			// TODO: Set the flags to the corresponding tokens and only parse
			// one token per qualifier. Right now, we can pass the same
			// qualifier multiple times.
		}

		syntax_node_id node = new_node(tree, SYNTAX_TYPE_POINTER, token, syntax_nil);
		insert_root(tree, &pointer_declarator, node);
	}

	syntax_node_list result = {0};
	if (ctx->peek[0].kind == TOKEN_IDENT) {
		if (flags & PARSE_NO_IDENT) {
			syntax_error(ctx, "Unexpected identifier");
		}

		token token = get_token(ctx);
		result.first = result.last = new_node_with_flags(tree, SYNTAX_DECL, qualifiers, token, syntax_nil);
	} else if (ctx->peek[0].kind == TOKEN_LPAREN) {
		get_token(ctx);
		result = parse_declarator(ctx, flags, qualifiers, s);
		expect(ctx, TOKEN_RPAREN);
	} else if (!(flags & (PARSE_NO_IDENT | PARSE_OPT_IDENT))) {
		syntax_error(ctx, "Expected '(' or identifier");
	}

	syntax_node_list declarator = {0};
	for (;;) {
		token token = ctx->peek[0];
		if (accept(ctx, TOKEN_LBRACKET)) {
			syntax_node_id size = {0};
			if (!accept(ctx, TOKEN_RBRACKET)) {
				size = parse_expr(ctx, PREC_ASSIGN, s);
				expect(ctx, TOKEN_RBRACKET);
			}

			syntax_node_id node = new_node(tree, SYNTAX_TYPE_ARRAY, token, size);
			insert_child(tree, &declarator, node);
		} else if (accept(ctx, TOKEN_LPAREN)) {
			syntax_node_list params = {0};
			if (ctx->peek[0].kind == TOKEN_VOID && ctx->peek[1].kind == TOKEN_RPAREN) {
				get_token(ctx);
				get_token(ctx);
			} else if (!accept(ctx, TOKEN_RPAREN)) {
				parse_scope param_scope = {s};
				do {
					if (accept(ctx, TOKEN_ELLIPSIS)) {
						// TODO: Mark function as variadic
						break;
					}

					syntax_node_id param = parse_decl(ctx, PARSE_PARAM, &param_scope).first;
					append_node(tree, &params, param);
					if (!accept(ctx, TOKEN_COMMA)) {
						break;
					}
				} while (!ctx->error);
				expect(ctx, TOKEN_RPAREN);
			}

			syntax_node_id node = new_node(tree, SYNTAX_TYPE_FUNC, token, params.first);
			insert_child(tree, &declarator, node);
		} else {
			break;
		}
	}

	// NOTE: Append the declarator tree to the result
	if (declarator.last.value != 0) {
		insert_child(tree, &result, declarator.first);
		result.last = declarator.last;
	}

	if (pointer_declarator.last.value != 0) {
		insert_child(tree, &result, pointer_declarator.first);
		result.last = pointer_declarator.last;
	}

	return result;
}

static syntax_node_list
parse_decl(parse_context *ctx, u32 flags, parse_scope *s)
{
	syntax_tree *tree = ctx->tree;
	arena *arena = ctx->arena;
	syntax_node_list list = {0};
	syntax_node_id base_type = {0};
	u32 qualifiers = 0;
	token qualifier_token = {0};

	b32 found_qualifier = true;
	while (found_qualifier) {
		token ident, token;

		token = ctx->peek[0];
		switch (token.kind) {
		case TOKEN_FLOAT:
		case TOKEN_DOUBLE:
		case TOKEN_BOOL:
		case TOKEN_INT:
		case TOKEN_CHAR:
		case TOKEN_VOID:
		case TOKEN_BUILTIN_VA_LIST:
			base_type = new_node(tree, SYNTAX_TYPE_BASIC, token, syntax_nil);
			get_token(ctx);
			break;
		case TOKEN_IDENT:
			{
				if (base_type.value == 0 && is_typename(s, token.value)) {
					base_type = new_node(tree, SYNTAX_TYPE_IDENT, token, syntax_nil);
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
				syntax_node_list enumerators = {0};
				token = ctx->peek[0];
				while (!ctx->error && token.kind != TOKEN_RBRACE) {
					expect(ctx, TOKEN_IDENT);

					syntax_node_id node;
					syntax_node_id expr = {0};
					if (accept(ctx, TOKEN_EQUAL)) {
						expr = parse_expr(ctx, PREC_ASSIGN, s);
					}

					node = new_node(tree, SYNTAX_ENUMERATOR, token, expr);
					append_node(tree, &enumerators, node);
					add_ident(s, token.value, false, arena);
					if (!accept(ctx, TOKEN_COMMA)) {
						break;
					}

					token = ctx->peek[0];
				}

				base_type = new_node(tree, SYNTAX_TYPE_ENUM, token, enumerators.first);
				expect(ctx, TOKEN_RBRACE);
			} break;
		case TOKEN_STRUCT:
		case TOKEN_UNION:
			{
				token = get_token(ctx);

				// parse optional tag
				ident = ctx->peek[0];
				b32 has_tag = accept(ctx, TOKEN_IDENT);
				if (!has_tag) {
					// for anonymous structs we store the original token,
					// so errors will report the location of the struct/union.
					ident = token;
				}

				// parse members
				syntax_node_list member_list = {0};
				if (accept(ctx, TOKEN_LBRACE)) {
					parse_scope member_scope = {s};
					while (!ctx->error && ctx->peek[0].kind != TOKEN_RBRACE) {
						syntax_node_list decl = parse_decl(ctx, PARSE_MEMBER, &member_scope);
						expect(ctx, TOKEN_SEMICOLON);
						append_list(tree, &member_list, decl);
					}

					expect(ctx, TOKEN_RBRACE);
				} else if (!has_tag) {
					// no tag and no definition, so next call will always fail.
					expect(ctx, TOKEN_LBRACE);
				}

				syntax_node_kind kind = token.kind == TOKEN_STRUCT ? SYNTAX_TYPE_STRUCT : SYNTAX_TYPE_UNION;
				base_type = new_node(tree, kind, ident, member_list.first);
			} break;
		default:
			{
				u32 qualifier = 0;
				switch (token.kind) {
				case TOKEN_AUTO:
					qualifier = SYNTAX_AUTO;
					break;
				case TOKEN_CONST:
					qualifier = SYNTAX_CONST;
					break;
				case TOKEN_EXTERN:
					qualifier = SYNTAX_EXTERN;
					break;
				case TOKEN_LONG:
					qualifier = SYNTAX_LONG;
					break;
				case TOKEN_REGISTER:
					qualifier = SYNTAX_REGISTER;
					break;
				case TOKEN_RESTRICT:
					qualifier = SYNTAX_RESTRICT;
					break;
				case TOKEN_SHORT:
					qualifier = SYNTAX_SHORT;
					break;
				case TOKEN_SIGNED:
					qualifier = SYNTAX_SIGNED;
					break;
				case TOKEN_STATIC:
					qualifier = SYNTAX_STATIC;
					break;
				case TOKEN_THREAD_LOCAL:
					qualifier = SYNTAX_THREAD_LOCAL;
					break;
				case TOKEN_TYPEDEF:
					qualifier = SYNTAX_TYPEDEF;
					break;
				case TOKEN_UNSIGNED:
					qualifier = SYNTAX_UNSIGNED;
					break;
				case TOKEN_VOLATILE:
					qualifier = SYNTAX_VOLATILE;
					break;
				default:
					qualifier = 0;
					break;
				}

				if (qualifiers & SYNTAX_LONG && (qualifier == SYNTAX_LONG)) {
					qualifier = SYNTAX_LLONG;
				}

				if ((qualifiers & SYNTAX_UNSIGNED && qualifier == SYNTAX_SIGNED)
					|| (qualifiers & SYNTAX_UNSIGNED && qualifier == SYNTAX_SIGNED))
				{
					syntax_error(ctx, "Type cannot be both signed and unsigned");
				} else if ((qualifiers & SYNTAX_SHORT && qualifier == SYNTAX_LONG)
					|| (qualifiers & SYNTAX_LONG && qualifier == SYNTAX_SHORT))
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

	// Top-level declarations are extern by default unless static is present
	if ((flags & PARSE_EXTERN_DEF) && !(qualifiers & SYNTAX_STATIC)) {
		qualifiers |= SYNTAX_EXTERN;
	}

	// Set the base type to int if it was not explicitly provided
	u32 int_mask = (SYNTAX_LLONG | SYNTAX_LONG | SYNTAX_SHORT | SYNTAX_SHORT | SYNTAX_SIGNED | SYNTAX_UNSIGNED);
	if (base_type.value == 0 && (qualifiers & int_mask) != 0) {
		qualifier_token.kind = TOKEN_INT;
		base_type = new_node(tree, SYNTAX_TYPE_BASIC, qualifier_token, syntax_nil);
	}

	if (base_type.value == 0) {
		if (qualifiers != 0) {
			syntax_error(ctx, "Expected type after qualifiers");
		}

		return list;
	}

	if (ctx->peek[0].kind == TOKEN_SEMICOLON) {
		syntax_node_id decl = new_node(tree, SYNTAX_DECL, ctx->peek[0], base_type);
		append_node(tree, &list, decl);
		return list;
	}

	do {
		syntax_node_list declarator = parse_declarator(ctx, flags, qualifiers, s);
		insert_child(tree, &declarator, base_type);
		ASSERT(declarator.first.value != 0);

		syntax_node_id decl = declarator.first;
		syntax_node decl_node = get_node(tree, decl);
		syntax_node_id type = decl_node.children;
		if (!(flags & PARSE_OPT) && decl.value == 0) {
			syntax_error(ctx, "Expected declaration");
		}

		b32 has_ident = (decl_node.kind == SYNTAX_DECL);
		if (has_ident) {
			ASSERT(decl_node.children.value != decl.value);
			b32 is_type = (qualifiers & SYNTAX_TYPEDEF) != 0;
			add_ident(s, decl_node.token.value, is_type, arena);
		} else {
			type = declarator.first;
		}

		if ((flags & PARSE_BITFIELD) && accept(ctx, TOKEN_COLON)) {
			syntax_node_id size = parse_expr(ctx, PREC_ASSIGN, s);

			syntax_node_list children = {0};
			append_node(tree, &children, type);
			append_node(tree, &children, size);
			syntax_node_id bitfield = new_node(tree, SYNTAX_TYPE_BITFIELD, ctx->peek[0], children.first);

			syntax_node *decl_node = &tree->nodes[decl.value];
			decl_node->children = bitfield;
			ASSERT(bitfield.value != 0);
		}

		// Parse declaration initializer
		if (!(flags & PARSE_NO_INITIALIZER) && accept(ctx, TOKEN_EQUAL)) {
			syntax_node_id initializer = {0};
			if (ctx->peek[0].kind == TOKEN_LBRACE) {
				initializer = parse_initializer(ctx, s);
			} else {
				initializer = parse_expr(ctx, PREC_ASSIGN, s);
			}

			syntax_node_list children = {0};
			append_node(tree, &children, type);
			append_node(tree, &children, initializer);

			syntax_node *decl_node = &tree->nodes[decl.value];
			decl_node->children = children.first;
			ASSERT(children.first.value != 0);
		}

		append_node(tree, &list, decl);
	} while (!ctx->error
		&& !(flags & PARSE_SINGLE_DECL)
		&& accept(ctx, TOKEN_COMMA));

	return list;
}

static syntax_node_id
parse_stmt(parse_context *ctx, parse_scope *s)
{
	syntax_tree *tree = ctx->tree;
	syntax_node_id result = {0};

	token token = ctx->peek[0];
	switch (token.kind) {
	case TOKEN_ASM:
		{
			get_token(ctx);
			result = new_node(tree, SYNTAX_STMT_ASM, token, syntax_nil);
			expect(ctx, TOKEN_LPAREN);
			expect(ctx, TOKEN_LITERAL_STRING);
			expect(ctx, TOKEN_RPAREN);
		} break;
	case TOKEN_BREAK:
		{
			get_token(ctx);
			expect(ctx, TOKEN_SEMICOLON);
			result = new_node(tree, SYNTAX_STMT_BREAK, token, syntax_nil);
		} break;
	case TOKEN_CASE:
		{
			get_token(ctx);
			syntax_node_id expr = parse_expr(ctx, PREC_ASSIGN, s);
			expect(ctx, TOKEN_COLON);
			syntax_node_id stmt = parse_stmt(ctx, s);

			syntax_node_list children = {0};
			append_node(tree, &children, expr);
			append_node(tree, &children, stmt);
			result = new_node(tree, SYNTAX_STMT_CASE, token, expr);
		} break;
	case TOKEN_CONTINUE:
		{
			get_token(ctx);
			expect(ctx, TOKEN_SEMICOLON);
			result = new_node(tree, SYNTAX_STMT_CONTINUE, token, syntax_nil);
		} break;
	case TOKEN_DEFAULT:
		{
			get_token(ctx);
			expect(ctx, TOKEN_COLON);
			syntax_node_id stmt = parse_stmt(ctx, s);
			result = new_node(tree, SYNTAX_STMT_DEFAULT, token, stmt);
		} break;
	case TOKEN_DO:
		{
			get_token(ctx);
			syntax_node_id body = parse_stmt(ctx, s);
			expect(ctx, TOKEN_WHILE);
			syntax_node_id cond = parse_expr(ctx, PREC_ASSIGN, s);
			expect(ctx, TOKEN_SEMICOLON);

			syntax_node_list children = {0};
			append_node(tree, &children, body);
			append_node(tree, &children, cond);
			result = new_node(tree, SYNTAX_STMT_DO_WHILE, token, children.first);
		} break;
	case TOKEN_FOR:
		{
			parse_scope tmp = {s};
			s = &tmp;

			get_token(ctx);
			expect(ctx, TOKEN_LPAREN);

			syntax_node_id init = {0};
			if (!accept(ctx, TOKEN_SEMICOLON)) {
				init = parse_decl(ctx, PARSE_STMT, s).first;
				if (init.value == 0) {
					init = parse_expr(ctx, PREC_ASSIGN, s);
				}

				expect(ctx, TOKEN_SEMICOLON);
			} else {
				init = new_node(tree, SYNTAX_NONE, token, syntax_nil);
			}

			syntax_node_id cond = {0};
			if (!accept(ctx, TOKEN_SEMICOLON)) {
				cond = parse_expr(ctx, PREC_ASSIGN, s);
				expect(ctx, TOKEN_SEMICOLON);
			} else {
				cond = new_node(tree, SYNTAX_NONE, token, syntax_nil);
			}

			syntax_node_id post = {0};
			if (!accept(ctx, TOKEN_RPAREN)) {
				post = parse_expr(ctx, PREC_ASSIGN, s);
				expect(ctx, TOKEN_RPAREN);
			} else {
				post = new_node(tree, SYNTAX_NONE, token, syntax_nil);
			}

			syntax_node_id stmt = parse_stmt(ctx, s);

			syntax_node_list children = {0};
			append_node(tree, &children, init);
			append_node(tree, &children, cond);
			append_node(tree, &children, post);
			append_node(tree, &children, stmt);
			result = new_node(tree, SYNTAX_STMT_FOR, token, children.first);
		} break;
	case TOKEN_GOTO:
		{
			get_token(ctx);
			token = ctx->peek[0];
			expect(ctx, TOKEN_IDENT);
			expect(ctx, TOKEN_SEMICOLON);
			result = new_node(tree, SYNTAX_STMT_GOTO, token, syntax_nil);
		} break;
	case TOKEN_IF:
		{
			get_token(ctx);
			expect(ctx, TOKEN_LPAREN);
			syntax_node_id cond = parse_expr(ctx, PREC_ASSIGN, s);
			expect(ctx, TOKEN_RPAREN);

			syntax_node_id if_branch = parse_stmt(ctx, s);

			syntax_node_list children = {0};
			append_node(tree, &children, cond);
			append_node(tree, &children, if_branch);

			if (accept(ctx, TOKEN_ELSE)) {
				syntax_node_id else_branch = parse_stmt(ctx, s);
				append_node(tree, &children, else_branch);
			}

			result = new_node(tree, SYNTAX_STMT_IF, token, children.first);
		} break;
	case TOKEN_WHILE:
		{
			get_token(ctx);
			expect(ctx, TOKEN_LPAREN);
			syntax_node_id cond = parse_expr(ctx, PREC_ASSIGN, s);
			expect(ctx, TOKEN_RPAREN);
			syntax_node_id body = parse_stmt(ctx, s);

			syntax_node_list children = {0};
			append_node(tree, &children, cond);
			append_node(tree, &children, body);
			result = new_node(tree, SYNTAX_STMT_WHILE, token, children.first);
		} break;
	case TOKEN_RETURN:
		{
			get_token(ctx);
			syntax_node_id expr = {0};
			if (!accept(ctx, TOKEN_SEMICOLON)) {
				expr = parse_expr(ctx, PREC_ASSIGN, s);
				expect(ctx, TOKEN_SEMICOLON);
			}

			result = new_node(tree, SYNTAX_STMT_RETURN, token, expr);
		} break;
	case TOKEN_SWITCH:
		{
			get_token(ctx);
			expect(ctx, TOKEN_LPAREN);
			syntax_node_id expr = parse_expr(ctx, PREC_ASSIGN, s);
			expect(ctx, TOKEN_RPAREN);
			syntax_node_id body = parse_stmt(ctx, s);

			syntax_node_list children = {0};
			append_node(tree, &children, expr);
			append_node(tree, &children, body);
			result = new_node(tree, SYNTAX_STMT_SWITCH, token, expr);
		} break;
	case TOKEN_SEMICOLON:
		{
			get_token(ctx);
			result = new_node(tree, SYNTAX_NONE, token, syntax_nil);
		} break;
	case TOKEN_LBRACE:
		{
			parse_scope tmp = {s};
			s = &tmp;

			syntax_node_list children = {0};
			expect(ctx, TOKEN_LBRACE);
			while (!ctx->error && !accept(ctx, TOKEN_RBRACE)) {
				syntax_node_id stmt = parse_stmt(ctx, s);
				append_node(tree, &children, stmt);
			}

			result = new_node(tree, SYNTAX_STMT_COMPOUND, token, children.first);
		} break;
	default:
		if (ctx->peek[0].kind == TOKEN_IDENT
			&& ctx->peek[1].kind == TOKEN_COLON)
		{
			get_token(ctx);
			get_token(ctx);
			syntax_node_id stmt = parse_stmt(ctx, s);
			result = new_node(tree, SYNTAX_STMT_LABEL, token, stmt);
		} else {
			// TODO: Handle multiple declarations in one statement
			syntax_node_list decl = parse_decl(ctx, PARSE_STMT, s);
			if (decl.first.value != 0) {
				result = new_node(tree, SYNTAX_STMT_DECL, ctx->peek[0], decl.first);
			} else {
				result = parse_expr(ctx, PREC_NONE, s);
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

static syntax_tree
parse(char *filename, arena *perm)
{
	syntax_tree tree = {0};
	parse_scope s = {0};

	str contents = read_file(filename, perm);
	parse_context ctx = tokenize(filename, contents, perm);
	ctx.tree = &tree;

	// Insert builtins into scope
	add_ident(&s, S("__builtin_va_list"),  true,  perm);
	add_ident(&s, S("__builtin_va_start"), false, perm);
	add_ident(&s, S("__builtin_va_end"),   false, perm);
	add_ident(&s, S("__builtin_va_arg"),   false, perm);

	syntax_node_list list = {0};
	do {
		syntax_node_list decl_list = parse_decl(&ctx, PARSE_EXTERNAL_DECL, &s);
		if (decl_list.first.value == 0) {
			syntax_error(&ctx, "Expected declaration");
			break;
		}

		syntax_node decl = get_node(&tree, decl_list.first);
		ASSERT(decl.flags & (SYNTAX_EXTERN | SYNTAX_STATIC));
		ASSERT(decl_list.first.value != decl.children.value);

		syntax_node_id type_id = decl.children;
		syntax_node type = get_node(&tree, type_id);

		b32 is_single_decl = (decl.next.value == 0);
		b32 is_func = (type.kind == SYNTAX_TYPE_FUNC);
		if (is_single_decl && is_func) {
			token token = ctx.peek[0];
			if (token.kind == TOKEN_LBRACE) {
				// Introduce parameters in the new scope
				parse_scope func_scope = {&s};
				syntax_node return_node = get_node(&tree, type.children);
				for (syntax_node_id param = return_node.next; param.value != 0;) {
					syntax_node param_node = get_node(&tree, param);
					str param_name = param_node.token.value;
					add_ident(&func_scope, param_name, false, perm);
					param = param_node.next;
				}

				syntax_node_id body = parse_stmt(&ctx, &func_scope);
				syntax_node *type = &tree.nodes[type_id.value];
				type->next = body;
			} else {
				expect(&ctx, TOKEN_SEMICOLON);
			}
		} else {
			expect(&ctx, TOKEN_SEMICOLON);
		}

		append_list(&tree, &list, decl_list);
	} while (!ctx.error && !accept(&ctx, TOKEN_EOF));

	tree.root = list.first;

	// shrink the tree to size
	syntax_node *nodes = realloc(tree.nodes, tree.size * sizeof(*nodes));
	if (nodes) {
		tree.nodes = nodes;
		tree.max_size = tree.size;
	}

	if (tree.root.value == 0) {
		ASSERT(!"syntax error");
		ctx.error = true;
	}

	return tree;
}
