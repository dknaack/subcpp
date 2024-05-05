#include <stdarg.h>

static void
vwarnf(location loc, char *fmt, va_list ap)
{
	fprintf(stderr, "%s[%ld]: ", loc.file, loc.offset);
	vfprintf(stderr, fmt, ap);
	fputc('\n', stderr);
	fflush(stderr);
}

static void
warnf(location loc, char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	vwarnf(loc, fmt, ap);
	va_end(ap);
}

static void
verrorf(location loc, char *fmt, va_list ap)
{
	vwarnf(loc, fmt, ap);
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
fatalf(location loc, char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	verrorf(loc, fmt, ap);
	va_end(ap);

	exit(EXIT_FAILURE);
}

static void
advance(lexer *lexer, i32 amount)
{
	isize remaining = lexer->file.contents.length - lexer->file.offset;
	lexer->file.offset += MIN(amount, remaining);

	for (isize i = 0; i < LENGTH(lexer->at); i++) {
		if (lexer->file.offset + i < lexer->file.contents.length) {
			lexer->at[i] = lexer->file.contents.at[lexer->file.offset + i];
		} else {
			lexer->at[i] = '\0';
		}
	}
}

static b32
is_alpha(char c)
{
	b32 result = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z');
	return result;
}

static b32
is_digit(char c)
{
	b32 result = ('0' <= c && c <= '9');
	return result;
}

static b32
is_ident(char c)
{
	b32 result = (is_alpha(c) || is_digit(c) || c == '_');
	return result;
}

static b32
is_whitespace(char c)
{
	b32 result = (c == ' ' || c == '\n' || c == '\t' || c == '\r' || c == '\f' || c == '\v');
	return result;
}

// NOTE: Cannot upcast to cpp_state in this function, because of peek_token!!
static token
cpp_get_token(lexer *l)
{
	token token = {0};

	if (l->tokens) {
		token = l->tokens->token;
		l->tokens = l->tokens->next;
		return token;
	}

	isize start = l->file.offset;
	char c = l->at[0];
	switch (c) {
	case '\0':
		token.kind = TOKEN_EOF;
		break;
	case '(':
		token.kind = TOKEN_LPAREN;
		break;
	case ')':
		token.kind = TOKEN_RPAREN;
		break;
	case '[':
		token.kind = TOKEN_LBRACKET;
		break;
	case ']':
		token.kind = TOKEN_RBRACKET;
		break;
	case '{':
		token.kind = TOKEN_LBRACE;
		break;
	case '}':
		token.kind = TOKEN_RBRACE;
		break;
	case '?':
		token.kind = TOKEN_QMARK;
		break;
	case ',':
		token.kind = TOKEN_COMMA;
		break;
	case ':':
		token.kind = TOKEN_COLON;
		break;
	case ';':
		token.kind = TOKEN_SEMICOLON;
		break;
	case '~':
		token.kind = TOKEN_TILDE;
		break;
	case '^':
		token.kind = TOKEN_CARET;
		break;
	case '!':
		token.kind = TOKEN_BANG;
		break;
	case '\n':
		token.kind = TOKEN_NEWLINE;
		break;
	case '\\':
		if (l->at[1] == '\n') {
			advance(l, 2);
			token.kind = TOKEN_WHITESPACE;
		} else {
			advance(l, 1);
			token.kind = TOKEN_BACKSLASH;
		}

		break;
	case '.':
		if (l->at[1] == '.' && l->at[2] == '.') {
			token.kind = TOKEN_ELLIPSIS;
			advance(l, 3);
		} else {
			token.kind = TOKEN_DOT;
			advance(l, 1);
		}

		break;
	case '&':
		if (l->at[1] == '&') {
			token.kind = TOKEN_AMP_AMP;
			advance(l, 2);
		} else if (l->at[1] == '=') {
			token.kind = TOKEN_AMP_EQUAL;
			advance(l, 2);
		} else {
			token.kind = TOKEN_AMP;
			advance(l, 1);
		}

		break;
	case '|':
		if (l->at[1] == '|') {
			token.kind = TOKEN_BAR_BAR;
			advance(l, 2);
		} else if (l->at[1] == '=') {
			token.kind = TOKEN_BAR_EQUAL;
			advance(l, 2);
		} else {
			token.kind = TOKEN_BAR;
			advance(l, 1);
		}

		break;
	case '+':
		if (l->at[1] == '+') {
			token.kind = TOKEN_PLUS_PLUS;
			advance(l, 2);
		} else if (l->at[1] == '=') {
			token.kind = TOKEN_PLUS_EQUAL;
			advance(l, 2);
		} else {
			token.kind = TOKEN_PLUS;
			advance(l, 1);
		}

		break;
	case '-':
		if (l->at[1] == '-') {
			token.kind = TOKEN_MINUS_MINUS;
			advance(l, 2);
		} else if (l->at[1] == '>') {
			token.kind = TOKEN_ARROW;
			advance(l, 2);
		} else if (l->at[1] == '=') {
			token.kind = TOKEN_MINUS_EQUAL;
			advance(l, 2);
		} else {
			token.kind = TOKEN_MINUS;
			advance(l, 1);
		}

		break;
	case '*':
		if (l->at[1] == '=') {
			token.kind = TOKEN_STAR_EQUAL;
			advance(l, 2);
		} else {
			token.kind = TOKEN_STAR;
			advance(l, 1);
		}

		break;
	case '/':
		if (l->at[1] == '=') {
			token.kind = TOKEN_SLASH_EQUAL;
			advance(l, 2);
		} else if (l->at[1] == '/') {
			token.kind = TOKEN_COMMENT;
			do {
				advance(l, 1);
			} while (l->at[0] != '\n' && l->at[0] != '\0');
		} else if (l->at[1] == '*') {
			advance(l, 2);
			token.kind = TOKEN_COMMENT;
			while (!(l->at[0] == '*' && l->at[1] == '/') && l->at[0] != '\0') {
				advance(l, 1);
			}

			advance(l, 2);
		} else {
			token.kind = TOKEN_SLASH;
			advance(l, 1);
		}

		break;
	case '%':
		if (l->at[1] == '=') {
			token.kind = TOKEN_PERCENT_EQUAL;
			advance(l, 2);
		} else {
			token.kind = TOKEN_PERCENT;
			advance(l, 1);
		}

		break;
	case '=':
		if (l->at[1] == '=') {
			token.kind = TOKEN_EQUAL_EQUAL;
			advance(l, 2);
		} else {
			token.kind = TOKEN_EQUAL;
			advance(l, 1);
		}

		break;
	case '<':
		if (l->at[1] == '=') {
			token.kind = TOKEN_LESS_EQUAL;
			advance(l, 2);
		} else if (l->at[1] == '<') {
			token.kind = TOKEN_LSHIFT;
			advance(l, 2);
		} else {
			token.kind = TOKEN_LESS;
			advance(l, 1);
		}

		break;
	case '>':
		if (l->at[1] == '=') {
			token.kind = TOKEN_GREATER_EQUAL;
			advance(l, 2);
		} else if (l->at[1] == '>') {
			token.kind = TOKEN_RSHIFT;
			advance(l, 2);
		} else {
			token.kind = TOKEN_GREATER;
			advance(l, 1);
		}

		break;
	case '#':
		if (l->at[1] == '#') {
			token.kind = TOKEN_HASH_HASH;
			advance(l, 2);
		} else {
			token.kind = TOKEN_HASH;
			advance(l, 1);
		}

		break;
	case '\r':
		if (l->at[1] == '\n') {
			token.kind = TOKEN_NEWLINE;
			advance(l, 2);
		} else {
			token.kind = TOKEN_NEWLINE;
			advance(l, 1);
		}

		break;
	case ' ': case '\t': case '\v': case '\f':
		token.kind = TOKEN_WHITESPACE;
		while (is_whitespace(l->at[0])) {
			advance(l, 1);
		}

		break;
	case '\'':
	case '"':
		do {
			if (l->at[1] == '\\') {
				advance(l, 2);
			} else {
				advance(l, 1);
			}
		} while (l->at[0] != c && l->at[0] != '\n' && l->at[0] != '\0');

		if (l->at[1] == c) {
			token.kind = c == '"' ? TOKEN_LITERAL_STRING : TOKEN_LITERAL_CHAR;
			advance(l, 1);
		}

		break;
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
		token.kind = TOKEN_LITERAL_INT;
		do {
			advance(l, 1);
		} while (is_digit(l->at[0]));

		if (l->at[0] == '.') {
			advance(l, 1);
			token.kind = TOKEN_LITERAL_FLOAT;
			while (is_digit(l->at[0])) {
				advance(l, 1);
			}
		}

		while (is_ident(l->at[0])) {
			advance(l, 1);
		}

		break;
	default:
		if (is_alpha(c) || c == '_') {
			token.kind = TOKEN_IDENT;
			while (is_ident(l->at[0])) {
				advance(l, 1);
			}
		}
	}

	isize end = l->file.offset;
	token.value = substr(l->file.contents, start, end);
	ASSERT(token.kind != TOKEN_EOF || token.value.length == 0);
	return token;
}

static token
cpp_peek_token(lexer *lex)
{
	lexer tmp = *lex;
	token token = cpp_get_token(&tmp);
	return token;
}

static b32
cpp_accept(lexer *lexer, token_kind expected_token)
{
	token token = cpp_peek_token(lexer);
	if (token.kind == expected_token) {
		cpp_get_token(lexer);
		return true;
	} else {
		return false;
	}
}

static void
cpp_expect(lexer *lexer, token_kind expected_token)
{
	token token = cpp_peek_token(lexer);
	if (!cpp_accept(lexer, expected_token)) {
		errorf(get_location(lexer), "Expected %s, but found %s",
			get_token_name(expected_token), get_token_name(token.kind));
	}
}

static void
skip_whitespace(lexer *lexer)
{
	while (cpp_accept(lexer, TOKEN_WHITESPACE)) {}
}

static void
skip_line(lexer *lexer)
{
	token token = cpp_peek_token(lexer);
	while (token.kind != TOKEN_NEWLINE && token.kind != TOKEN_EOF) {
		cpp_get_token(lexer);
		token = cpp_peek_token(lexer);
	}
}

static str
dirname(str path)
{
	str dir = path;
	while (dir.length > 0 && dir.at[dir.length - 1] != '/') {
		dir.length--;
	}

	return dir;
}

static str
concat_paths(str a, str b, arena *perm)
{
	str result;
	result.at = ALLOC(perm, a.length + b.length + 2, char);
	result.length = a.length + b.length + 1;

	copy(substr(result, 0, a.length), a);
	result.at[a.length] = '/';
	copy(substr(result, a.length + 1, -1), b);
	return result;
}

static file *
push_file(cpp_state *cpp)
{
	lexer *lexer = &cpp->lexer;
	file *f = ALLOC(cpp->arena, 1, file);
	*f = lexer->file;
	lexer->file.if_depth = 0;
	lexer->file.prev = f;
	return &lexer->file;
}

static str tombstone = S("(deleted)");

static macro *
upsert_macro(macro **p, str name, arena *perm)
{
	while (*p) {
		if (equals((*p)->name, name)) {
			return *p;
		}

		p = &(*p)->next;
	}

	if (perm) {
		*p = ALLOC(perm, 1, macro);
		return *p;
	}

	return NULL;
}

static token_list *
expand(token_list *list, str_list *expanded_macros, macro *macros, arena *arena)
{
	token_list **curr = &list;
	while (*curr) {
		// The next token comes after the expanded macro. In the case of a
		// function-like macro this is the token after the right parenthesis.
		token_list *next = (*curr)->next;
		while (next && next->token.kind == TOKEN_WHITESPACE) {
			next = next->next;
		}

		macro *m = NULL;
		if ((*curr)->token.kind == TOKEN_IDENT && !(*curr)->was_expanded) {
			m = upsert_macro(&macros, (*curr)->token.value, NULL);

			// Ensure that function-like macros are followed by a parenthesis.
			b32 next_is_lparen = (next && next->token.kind == TOKEN_LPAREN);
			if (m && m->param_count >= 0 && !next_is_lparen) {
				m = NULL;
			}
		}

		if (m == NULL) {
			*curr = ALLOC(arena, 1, token_list);
			**curr = *tmp;
			curr = &(*curr)->next;
			continue;
		}

		token_list **params = NULL;
		if (m->param_count > 0) {
			params = ALLOC(arena, m->param_count, token_list *);

			// Skip the first parenthesis
			ASSERT(next && next->token.kind == TOKEN_LPAREN);
			next = next->next;

			// Parse the actual parameters, counting balanced parentheses
			token_list **curr_param = params;
			*curr_param = next;
			i32 param_index = 1;
			i32 paren_depth = 1;
			while (next && paren_depth > 0) {
				if (next->token.kind == TOKEN_COMMA) {
					if (param_index < m->param_count) {
						*curr_param = NULL;
						curr_param = &params[param_index++];
						*curr_param = next->next;
					} else {
						// TODO: Report error
					}
				} else if (next->token.kind == TOKEN_EOF) {
					paren_depth = -1;
				} else {
					paren_depth += (next->token.kind == TOKEN_LPAREN);
					paren_depth -= (next->token.kind == TOKEN_RPAREN);
					if (paren_depth == 0) {
						*curr_param = NULL;
					} else {
						curr_param = &(*curr_param)->next;
					}
				}

				next = next->next;
			}
		} else if (m->param_count == 0) {
			ASSERT(next && next->token.kind == TOKEN_LPAREN);
			next = next->next;
			ASSERT(next && next->token.kind == TOKEN_RPAREN);
			next = next->next;
		}

		// Do parameter substitution
		token_list *new_list = NULL;
		token_list *prev = NULL;
		token_list **out = &new_list;
		for (token_list *input = m->tokens; input; input = input->next) {
			b32 stringize = false;
			b32 concat = false;
			if (input->token.kind == TOKEN_HASH) {
				stringize = true;
				input = input->next;
				if (!input) {
					// TODO: Report error
					break;
				}
			} else if (input->token.kind == TOKEN_HASH_HASH) {
				concat = true;
				input = input->next;
				while (input && input->token.kind == TOKEN_WHITESPACE) {
					input = input->next;
				}

				if (!prev || !input) {
					// TODO: Report error
					break;
				}
			}

			i32 param_index = -1;
			b32 is_param = false;
			if (input->token.kind == TOKEN_IDENT) {
				for (str_list *param = m->params; param; param = param->next) {
					param_index++;
					if (equals(param->value, input->token.value)) {
						is_param = true;
						break;
					}
				}
			}

			if (concat) {
				// TODO: Replace parameter
				ASSERT(!is_param);

				// TODO: Non-identifier concatenation
				ASSERT(prev->token.kind == TOKEN_IDENT);
				ASSERT(input->token.kind == TOKEN_IDENT);

				str a = prev->token.value;
				str b = input->token.value;
				str result = {0};
				result.length = a.length + b.length;
				result.at = ALLOC(arena, result.length, char);
				copy(substr(result, 0, a.length), a);
				copy(substr(result, a.length, -1), b);

				*out = ALLOC(arena, 1, token_list);
				(*out)->token.kind = TOKEN_IDENT;
				(*out)->token.value = result;
			} else if (stringize) {
				if (!is_param) {
					// TODO: Report error
					continue;
				}

				str result = {0};
				result.length = 2;
				for (token_list *list = params[param_index]; list; list = list->next) {
					// TODO: Implement stringize for string and char literals.
					ASSERT(list->token.kind != TOKEN_LITERAL_STRING);
					ASSERT(list->token.kind != TOKEN_LITERAL_CHAR);
					if (list->token.kind == TOKEN_WHITESPACE) {
						result.length += 1;
					} else {
						result.length += list->token.value.length;
					}
				}

				result.at = ALLOC(arena, result.length, char);
				isize start = 1;
				for (token_list *list = params[param_index]; list; list = list->next) {
					if (list->token.kind == TOKEN_WHITESPACE) {
						result.at[start++] = ' ';
					} else {
						isize end = start + list->token.value.length;
						copy(substr(result, start, end), list->token.value);
						start = end;
					}
				}

				result.at[0] = '"';
				result.at[result.length - 1] = '"';
				*out = ALLOC(arena, 1, token_list);
				(*out)->token.kind = TOKEN_LITERAL_STRING;
				(*out)->token.value = result;
			} else if (is_param) {
				ASSERT(params != NULL);
				*out = expand(params[param_index], expanded_macros, macros, arena);
			} else {
				*out = ALLOC(arena, 1, token_list);
				(*out)->token = input->token;
			}

			while (*out) {
				if ((*out)->token.kind != TOKEN_WHITESPACE) {
					prev = *out;
				}

				out = &(*out)->next;
			}
		}

		// Add the expanded macro to the list.
		str_list tmp = {0};
		tmp.value = m->name;
		tmp.next = expanded_macros;
		expanded_macros = &tmp;

		// Paint any tokens that have been expanded blue.
		for (token_list *list = new_list; list; list = list->next) {
			if (list->token.kind != TOKEN_IDENT || list->was_expanded) {
				continue;
			}

			for (str_list *s = expanded_macros; s; s = s->next) {
				if (equals(s->value, list->token.value)) {
					list->was_expanded = true;
					break;
				}
			}
		}

		// Rescan
		new_list = expand(new_list, expanded_macros, macros, arena);

		// Insert the list back into the token list
		token_list **end = NULL;
		for (end = &new_list; *end; end = &(*end)->next) {}
		*end = next;
		*curr = new_list;
	}

	return list;
}

static token_list *
read_line(lexer *lexer, arena *arena)
{
	token_list *result = NULL;
	token_list **p = &result;

	for (;;) {
		token token = cpp_peek_token(lexer);
		if (token.kind == TOKEN_NEWLINE || token.kind == TOKEN_EOF) {
			break;
		}

		cpp_get_token(lexer);
		*p = ALLOC(arena, 1, token_list);
		(*p)->token = token;
		p = &(*p)->next;
	}

	return result;
}

static i64
cpp_parse_expr_rec(cpp_state *cpp, precedence prev_prec)
{
	i64 result = 0;
	lexer *lexer = &cpp->lexer;

	skip_whitespace(lexer);
	token token = cpp_get_token(lexer);
	switch (token.kind) {
	case TOKEN_IDENT:
		break;
	case TOKEN_PLUS:
		result = +cpp_parse_expr_rec(cpp, PREC_PRIMARY);
		break;
	case TOKEN_MINUS:
		result = -cpp_parse_expr_rec(cpp, PREC_PRIMARY);
		break;
	case TOKEN_BANG:
		result = !cpp_parse_expr_rec(cpp, PREC_PRIMARY);
		break;
	case TOKEN_LITERAL_INT:
		while (token.value.length-- > 0) {
			char c = *token.value.at++;
			if (!is_digit(c)) {
				break;
			}

			result *= 10;
			result += c - '0';
		}

		break;
	case TOKEN_LPAREN:
		result = cpp_parse_expr_rec(cpp, PREC_ASSIGN);
		skip_whitespace(lexer);
		cpp_expect(lexer, TOKEN_RPAREN);
		break;
	default:
		errorf(get_location(lexer), "Invalid expression: %s",
			get_token_name(token.kind));
	}

	while (!lexer->error) {
		skip_whitespace(lexer);
		token = cpp_peek_token(lexer);
		if (token.kind == TOKEN_EOF || token.kind == TOKEN_NEWLINE) {
			break;
		}

		precedence prec = get_precedence(token.kind);
		if (prec == PREC_NONE || prec < prev_prec) {
			break;
		}

		cpp_get_token(lexer);
		i64 lhs = result;
		i64 rhs = 0;
		if (token.kind == TOKEN_QMARK) {
			lhs = cpp_parse_expr_rec(cpp, prec);
			skip_whitespace(lexer);
			token = cpp_get_token(lexer);
			if (token.kind != TOKEN_COLON) {
				errorf(get_location(lexer), "Expected ':'");
			}

			skip_whitespace(lexer);
			rhs = cpp_parse_expr_rec(cpp, prec);
			token.kind = TOKEN_QMARK;
		} else {
			rhs = cpp_parse_expr_rec(cpp, prec);
		}

		switch (token.kind) {
		case TOKEN_AMP:           result = lhs  & rhs;  break;
		case TOKEN_AMP_AMP:       result = lhs && rhs; break;
		case TOKEN_BAR:           result = lhs  | rhs;  break;
		case TOKEN_BAR_BAR:       result = lhs || rhs; break;
		case TOKEN_CARET:         result = lhs  ^ rhs;  break;
		case TOKEN_EQUAL_EQUAL:   result = lhs == rhs; break;
		case TOKEN_GREATER:       result = lhs  > rhs;  break;
		case TOKEN_GREATER_EQUAL: result = lhs >= rhs; break;
		case TOKEN_RSHIFT:        result = lhs >> rhs; break;
		case TOKEN_LESS:          result = lhs  < rhs;  break;
		case TOKEN_LESS_EQUAL:    result = lhs <= rhs; break;
		case TOKEN_LSHIFT:        result = lhs << rhs; break;
		case TOKEN_MINUS:         result = lhs  - rhs; break;
		case TOKEN_PERCENT:       result = lhs  % rhs; break;
		case TOKEN_PLUS:          result = lhs  + rhs; break;
		case TOKEN_SLASH:         result = lhs  / rhs; break;
		case TOKEN_STAR:          result = lhs  * rhs; break;
		case TOKEN_QMARK:         result = result ? lhs : rhs; break;
		default:
			errorf(get_location(lexer), "Invalid operator");
		}
	}

	return result;
}

static i64
cpp_parse_expr(cpp_state *cpp)
{
	lexer *lexer = &cpp->lexer;

	skip_whitespace(lexer);
	lexer->tokens = read_line(lexer, cpp->arena);

	// Remove all defined expressions from the token list.
	// TODO: This should only be done once and not in each recursive step.
	token_list *tmp = NULL;
	token_list **p = &tmp;
	while (lexer->tokens) {
		token token = cpp_peek_token(lexer);
		if (equals(token.value, S("defined"))) {
			cpp_get_token(lexer);
			skip_whitespace(lexer);
			if (cpp_accept(lexer, TOKEN_LPAREN)) {
				skip_whitespace(lexer);
				token = cpp_peek_token(lexer);
				cpp_expect(lexer, TOKEN_IDENT);
				skip_whitespace(lexer);
				cpp_expect(lexer, TOKEN_RPAREN);
			} else {
				token = cpp_peek_token(lexer);
				cpp_expect(lexer, TOKEN_IDENT);
			}

			static str token_values[] = { S("0"), S("1") };
			macro *m = upsert_macro(&cpp->macros, token.value, NULL);
			token_list *node = ALLOC(cpp->arena, 1, token_list);
			node->token.kind = TOKEN_LITERAL_INT;
			node->token.value = token_values[m != NULL];
			*p = node;
		} else {
			*p = lexer->tokens;
			cpp_get_token(lexer);
		}

		p = &(*p)->next;
	}

	// Expand the tokens
	lexer->tokens = expand(tmp, NULL, cpp->macros, cpp->arena);

	// Parse the expression recursively.
	i64 result = cpp_parse_expr_rec(cpp, PREC_ASSIGN);
	return result;
}

static void
push_if(cpp_state *cpp, b32 value)
{
	cpp->lexer.file.if_depth++;
	if (cpp->lexer.file.if_depth > LENGTH(cpp->lexer.file.if_state)) {
		fatalf(get_location(&cpp->lexer), "Internal error: Too many #ifs");
	}

	cpp->lexer.file.if_state[cpp->lexer.file.if_depth - 1] = 0;
	cpp->lexer.file.if_loc[cpp->lexer.file.if_depth - 1] = get_location(&cpp->lexer);
	if (!cpp->ignore_token && value) {
		cpp->lexer.file.if_state[cpp->lexer.file.if_depth - 1] = IF_TRUE;
	} else {
		cpp->ignore_token = true;
	}
}

static token
get_token(lexer *lexer)
{
	cpp_state *cpp = (cpp_state *)lexer;
	token tmp, token = {TOKEN_INVALID};
	b32 at_line_start = (lexer->file.offset == 0);
	do {
		b32 was_expanded = (lexer->tokens != NULL);
		token = cpp_get_token(lexer);

		if (at_line_start && !was_expanded && token.kind == TOKEN_HASH) {
			skip_whitespace(lexer);
			token = cpp_get_token(lexer);
			if (token.kind != TOKEN_IDENT) {
				fatalf(get_location(lexer), "Invalid preprocessing directive");
			}

			if_state curr_state = IF_TRUE;
			if_state prev_state = IF_TRUE;
			if (cpp->lexer.file.if_depth > 0) {
				curr_state = cpp->lexer.file.if_state[cpp->lexer.file.if_depth - 1];
				if (cpp->lexer.file.if_depth > 1) {
					prev_state = cpp->lexer.file.if_state[cpp->lexer.file.if_depth - 2];
				}
			}

			skip_whitespace(lexer);
			if (equals(token.value, S("if"))) {
				i64 value = 0;
				if (!cpp->ignore_token) {
					value = cpp_parse_expr(cpp);
				} else {
					skip_line(lexer);
				}

				warnf(get_location(lexer), "#if");
				push_if(cpp, value);
			} else if (equals(token.value, S("ifdef"))) {
				warnf(get_location(lexer), "#ifdef");
				token = cpp_get_token(lexer);
				if (token.kind != TOKEN_IDENT) {
					fatalf(get_location(lexer), "Expected identifier");
				}

				macro *m = upsert_macro(&cpp->macros, token.value, NULL);
				b32 is_defined = (m != NULL);
				push_if(cpp, is_defined);
			} else if (equals(token.value, S("ifndef"))) {
				token = cpp_get_token(lexer);
				if (token.kind != TOKEN_IDENT) {
					fatalf(get_location(lexer), "Expected identifier");
				}

				macro *m = upsert_macro(&cpp->macros, token.value, NULL);
				b32 is_defined = (m != NULL);
				push_if(cpp, !is_defined);
			} else if (equals(token.value, S("elif"))) {
				if (cpp->lexer.file.if_depth <= 0) {
					fatalf(get_location(lexer), "#elif without #if");
				}

				if (curr_state & IF_HAS_ELSE) {
					fatalf(get_location(lexer), "#elif after #else");
				}

				cpp->ignore_token = true;
				if (!(curr_state & IF_TRUE) && (prev_state & IF_TRUE)) {
					i64 value = cpp_parse_expr(cpp);
					if (value) {
						cpp->lexer.file.if_state[cpp->lexer.file.if_depth - 1] |= IF_TRUE;
						cpp->ignore_token = false;
					}
				}

				skip_line(lexer);
			} else if (equals(token.value, S("else"))) {
				if (cpp->lexer.file.if_depth <= 0) {
					fatalf(get_location(lexer), "#else without #if");
				}

				if (curr_state & IF_HAS_ELSE) {
					fatalf(get_location(lexer), "#else after #else");
				}

				cpp->ignore_token = true;
				cpp->lexer.file.if_state[cpp->lexer.file.if_depth - 1] |= IF_HAS_ELSE;
				if (!(curr_state & IF_TRUE) && (prev_state & IF_TRUE)) {
					cpp->lexer.file.if_state[cpp->lexer.file.if_depth - 1] |= IF_TRUE;
					cpp->ignore_token = false;
				}

				skip_line(lexer);
			} else if (equals(token.value, S("endif"))) {
				if (cpp->lexer.file.if_depth <= 0) {
					fatalf(get_location(lexer), "#endif without #if");
				}

				cpp->ignore_token = !(prev_state & IF_TRUE);
				cpp->lexer.file.if_depth--;

				skip_line(lexer);
			} else if (cpp->ignore_token) {
				skip_line(lexer);
				token.kind = TOKEN_NEWLINE;
			} else if (equals(token.value, S("include"))) {
				str path = {0};
				char end_char = '\0';
				b32 is_system_header = false;
				if (lexer->file.contents.at[lexer->file.offset] == '<') {
					is_system_header = true;
					end_char = '>';
				} else if (lexer->file.contents.at[lexer->file.offset] == '"') {
					end_char = '"';
				} else {
					ASSERT(!"Macro filenames have not been implement yet");
				}

				advance(lexer, 1);
				isize start = lexer->file.offset;
				do {
					advance(lexer, 1);
				} while (lexer->at[0] != '\n' && lexer->at[0] != end_char);
				isize end = lexer->file.offset - 1;

				path.at = lexer->file.contents.at + start;
				path.length = end - start;
				if (path.length == 0) {
					fatalf(get_location(lexer), "Invalid header path");
				}

				token.kind = TOKEN_NEWLINE;
				skip_line(lexer);

				str filename = {0};
				str contents = {0};
				b32 found_header = false;

				static struct { str filename, contents; } internal_headers[] = {
					{
						S("stddef.h"),
						S(
							"#ifndef __STDDEF_H\n"
							"#define __STDDEF_H\n"
							"typedef unsigned long size_t;\n"
							"typedef int wchar_t;\n"
							"typedef long ptrdiff_t;\n"
							"#define NULL ((void *)0)\n"
							"#define offsetof(s, m) __builtin_offsetof(s, m)\n"
							"#endif /* __STDDEF_H */\n"
						)
					},
					{
						S("stdbool.h"),
						S(
							"#ifndef __STDBOOL_H\n"
							"#define __STDBOOL_H\n"
							"#define __bool_true_false_are_defined 1\n"
							"#define bool _Bool\n"
							"#define true 1\n"
							"#define false 0\n"
							"#endif /* __STDBOOL_H */\n"
						)
					},
					{
						S("stdarg.h"),
						S(
							"#ifndef __STDARG_H\n"
							"#define __STDARG_H\n"
							"typedef __builtin_va_list va_list;\n"
							"#define va_start(ap, ptr) __builtin_va_start(ap, ptr)\n"
							"#define va_end(ap)        __builtin_va_end(ap)\n"
							"#define va_arg(ap, type)  __builtin_va_arg(ap, type)\n"
							"#define va_copy(dst, src) __builtin_va_copy(dst, src)\n"
							"#endif /* __STDARG_H */\n"
						)
					},
				};

				for (isize i = 0; i < LENGTH(internal_headers); i++) {
					if (equals(internal_headers[i].filename, path)) {
						found_header = true;
						filename = internal_headers[i].filename;
						contents = internal_headers[i].contents;
						break;
					}
				}

				if (!found_header && !is_system_header) {
					b32 is_relative_path = path.length <= 0 || path.at[0] != '/';
					if (is_relative_path) {
						errno = 0;
						str dir = dirname(make_str(cpp->lexer.file.name));
						filename = concat_paths(dir, path, cpp->arena);
						contents = read_file(filename.at, cpp->arena);
						i32 error_value = errno;
						if (error_value == 0) {
							found_header = true;
						}
					}

					// TODO: Search for directories passed with -I option
				}

				if (!found_header) {
					static str system_include_dirs[] = {
						S("/usr/local/include"),
						S("/usr/include")
					};

					for (u32 i = 0; i < LENGTH(system_include_dirs); i++) {
						errno = 0;
						str dir = system_include_dirs[i];
						filename = concat_paths(dir, path, cpp->arena);
						contents = read_file(filename.at, cpp->arena);
						i32 error_value = errno;
						if (error_value == 0) {
							found_header = true;
							break;
						}
					}
				}

				if (found_header) {
					file *f = push_file(cpp);
					f->name = cstr(filename, cpp->arena);
					f->contents = contents;
					f->offset = 0;
				} else {
					fatalf(get_location(lexer), "File not found");
				}
			} else if (equals(token.value, S("define"))) {
				token = cpp_get_token(lexer);
				if (token.kind != TOKEN_IDENT) {
					fatalf(get_location(lexer), "Expected identifier");
				}

				str name = token.value;
				macro *m = upsert_macro(&cpp->macros, token.value, cpp->arena);
				m->param_count = -1;
				if (m->name.at != NULL) {
					warnf(get_location(lexer), "Macro was already defined");
				}

				if (cpp_accept(lexer, TOKEN_LPAREN)) {
					str_list **ptr = &m->params;
					m->param_count = 0;
					skip_whitespace(lexer);
					if (!cpp_accept(lexer, TOKEN_RPAREN)) {
						while (!cpp_accept(lexer, TOKEN_EOF)) {
							token = cpp_get_token(lexer);
							if (token.kind == TOKEN_IDENT) {
								m->param_count++;
								*ptr = ALLOC(cpp->arena, 1, str_list);
								(*ptr)->value = token.value;
								ptr = &(*ptr)->next;
							}

							skip_whitespace(lexer);
							if (!cpp_accept(lexer, TOKEN_COMMA)) {
								break;
							}

							skip_whitespace(lexer);
						}

						cpp_expect(lexer, TOKEN_RPAREN);
					}
				}

				m->name = name;
				m->tokens = read_line(lexer, cpp->arena);
			} else if (equals(token.value, S("undef"))) {
				token = cpp_get_token(lexer);
				macro *m = upsert_macro(&cpp->macros, token.value, NULL);
				if (m != NULL) {
					m->name = tombstone;
				}

				skip_line(lexer);
			} else if (equals(token.value, S("pragma"))) {
				skip_line(lexer);
			} else if (equals(token.value, S("error"))) {
				// TODO: Print the message
				errorf(get_location(lexer), "Error directive");
				skip_line(lexer);
			} else {
				fatalf(get_location(lexer), "Invalid preprocessor directive");
			}

			token.kind = TOKEN_NEWLINE;
		}

		if (token.kind == TOKEN_BACKSLASH) {
			token = cpp_peek_token(lexer);
			if (token.kind == TOKEN_NEWLINE) {
				cpp_get_token(lexer);
			} else {
				token.kind = TOKEN_INVALID;
			}
		} else if (token.kind == TOKEN_EOF) {
			if (cpp->lexer.file.if_depth > 0) {
				fatalf(cpp->lexer.file.if_loc[cpp->lexer.file.if_depth - 1], "Unterminated #if: %d", cpp->lexer.file.if_depth);
			}

			if (cpp->lexer.file.prev != NULL) {
				token.kind = TOKEN_NEWLINE;
				at_line_start = true;
				cpp->lexer.file = *cpp->lexer.file.prev;
				skip_whitespace(lexer);
			}
		} else if (token.kind == TOKEN_NEWLINE) {
			at_line_start = true;
			skip_whitespace(lexer);
		} else if (!cpp->ignore_token && !was_expanded && token.kind == TOKEN_IDENT) {
			ASSERT(lexer->tokens == NULL);

			token_list *list = ALLOC(cpp->arena, 1, token_list);
			list->token = token;

			token_list **p = &list->next;
			while (cpp_accept(lexer, TOKEN_LPAREN)) {
				*p = ALLOC(cpp->arena, 1, token_list);
				(*p)->token.kind = TOKEN_LPAREN;
				(*p)->token.value = S("(");
				p = &(*p)->next;

				i32 paren_depth = 1;
				for (;;) {
					token = cpp_get_token(lexer);
					if (token.kind == TOKEN_RPAREN) {
						paren_depth--;
					} else if (token.kind == TOKEN_LPAREN) {
						paren_depth++;
					} else if (token.kind == TOKEN_EOF) {
						break;
					}

					*p = ALLOC(cpp->arena, 1, token_list);
					(*p)->token = token;
					p = &(*p)->next;
					if (paren_depth <= 0) {
						break;
					}
				}
			}

			lexer->tokens = expand(list, NULL, cpp->macros, cpp->arena);
			token.kind = TOKEN_WHITESPACE;
		}
	} while (cpp->ignore_token || token.kind == TOKEN_WHITESPACE
		|| token.kind == TOKEN_NEWLINE || token.kind == TOKEN_COMMENT);

	if (token.kind == TOKEN_IDENT) {
		struct {
			token_kind token;
			str str;
		} keywords[] = {
			{ TOKEN_AUTO,         S("auto")          },
			{ TOKEN_BREAK,        S("break")         },
			{ TOKEN_CASE,         S("case")          },
			{ TOKEN_CHAR,         S("char")          },
			{ TOKEN_CONST,        S("const")         },
			{ TOKEN_CONTINUE,     S("continue")      },
			{ TOKEN_DO,           S("do")            },
			{ TOKEN_DEFAULT,      S("default")       },
			{ TOKEN_ELSE,         S("else")          },
			{ TOKEN_ENUM,         S("enum")          },
			{ TOKEN_EXTERN,       S("extern")        },
			{ TOKEN_FLOAT,        S("float")         },
			{ TOKEN_FOR,          S("for")           },
			{ TOKEN_GOTO,         S("goto")          },
			{ TOKEN_IF,           S("if")            },
			{ TOKEN_INT,          S("int")           },
			{ TOKEN_LONG,         S("long")          },
			{ TOKEN_REGISTER,     S("register")      },
			{ TOKEN_RESTRICT,     S("restrict")      },
			{ TOKEN_RETURN,       S("return")        },
			{ TOKEN_SIGNED,       S("signed")        },
			{ TOKEN_SHORT,        S("short")         },
			{ TOKEN_STATIC,       S("static")        },
			{ TOKEN_STRUCT,       S("struct")        },
			{ TOKEN_SWITCH,       S("switch")        },
			{ TOKEN_THREAD_LOCAL, S("_Thread_local") },
			{ TOKEN_TYPEDEF,      S("typedef")       },
			{ TOKEN_UNSIGNED,     S("unsigned")      },
			{ TOKEN_VOID,         S("void")          },
			{ TOKEN_VOLATILE,     S("volatile")      },
			{ TOKEN_WHILE,        S("while")         },
		};

		for (usize i = 0; i < LENGTH(keywords); i++) {
			if (equals(token.value, keywords[i].str)) {
				token.kind = keywords[i].token;
				break;
			}
		}
	}

	tmp = token;
	token = lexer->peek[0];
	lexer->peek[0] = lexer->peek[1];
	lexer->peek[1] = tmp;
	return token;
}

static cpp_state
tokenize(char *filename, str src, arena *arena)
{
	cpp_state cpp = {0};
	cpp.arena = arena;
	cpp.lexer.file.name = filename;
	cpp.lexer.file.contents = src;

#if 0
	static struct { str name, value; } predefined_macros[] = {
		{ S("__STDC_VERSION__"), S("201710L") },
		{ S("__STDC__"),         S("1")       },
		{ S("__STDC_NO_VLA__"),  S("1")       },
	};

	for (i32 i = 0; i < LENGTH(predefined_macros); i++) {
		str name = predefined_macros[i].name;
		macro *m = upsert_macro(&cpp.macros, name, cpp.arena);
		m->name = name;
		m->value.name = "(internal)";
		m->value.contents = predefined_macros[i].value;
		m->value.offset = 0;
	}
#endif

	get_token(&cpp.lexer);
	get_token(&cpp.lexer);
	return cpp;
}
