#include <stdarg.h>

static void
verrorf(location loc, char *fmt, va_list ap)
{
	fprintf(stderr, "%s[%ld]: ", loc.file, loc.offset);
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
fatalf(location loc, char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	verrorf(loc, fmt, ap);
	va_end(ap);

	exit(EXIT_FAILURE);
}

static char
advance(lexer *lexer)
{
	for (isize i = 0; i < LENGTH(lexer->at); i++) {
		if (lexer->file.offset + i < lexer->file.contents.length) {
			lexer->at[i] = lexer->file.contents.at[lexer->file.offset + i];
		} else {
			lexer->at[i] = '\0';
		}
	}

	if (lexer->file.offset < lexer->file.contents.length) {
		lexer->file.offset++;
	}

	return lexer->at[0];
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

static token_kind
eat1(lexer *lexer, token_kind a, char c, token_kind b)
{
	if (lexer->at[1] == c) {
		advance(lexer);
		return b;
	} else {
		return a;
	}
}

static token
cpp_get_token(lexer *lexer)
{
	token token = {0};
	isize start = lexer->file.offset;

	char c = advance(lexer);
	switch (c) {
	case '(':  token.kind = TOKEN_LPAREN;    break;
	case ')':  token.kind = TOKEN_RPAREN;    break;
	case '[':  token.kind = TOKEN_LBRACKET;  break;
	case ']':  token.kind = TOKEN_RBRACKET;  break;
	case '{':  token.kind = TOKEN_LBRACE;    break;
	case '}':  token.kind = TOKEN_RBRACE;    break;
	case '?':  token.kind = TOKEN_QMARK;     break;
	case ',':  token.kind = TOKEN_COMMA;     break;
	case ':':  token.kind = TOKEN_COLON;     break;
	case ';':  token.kind = TOKEN_SEMICOLON; break;
	case '~':  token.kind = TOKEN_TILDE;     break;
	case '^':  token.kind = TOKEN_CARET;     break;
	case '!':  token.kind = TOKEN_BANG;      break;
	case '\n': token.kind = TOKEN_NEWLINE;   break;
	case '\0': token.kind = TOKEN_EOF;       break;
	case '\\':
		if (lexer->at[1] == '\n') {
			advance(lexer);
			token.kind = TOKEN_WHITESPACE;
		} else {
			token.kind = TOKEN_BACKSLASH;
		}
		break;
	case '.':
		if (lexer->at[1] == '.' && lexer->at[2] == '.') {
			token.kind = TOKEN_ELLIPSIS;
			advance(lexer);
			advance(lexer);
		} else {
			token.kind = TOKEN_DOT;
		}
		break;
	case '&':
		{
			if (lexer->at[1] == '&') {
				token.kind = TOKEN_AMP_AMP;
				advance(lexer);
			} else if (lexer->at[1] == '=') {
				token.kind = TOKEN_AMP_EQUAL;
				advance(lexer);
			} else {
				token.kind = TOKEN_AMP;
			}
		} break;
	case '|':
		{
			if (lexer->at[1] == '|') {
				token.kind = TOKEN_BAR_BAR;
				advance(lexer);
			} else if (lexer->at[1] == '=') {
				token.kind = TOKEN_BAR_EQUAL;
				advance(lexer);
			} else {
				token.kind = TOKEN_BAR;
			}
		} break;
	case '+':
		{
			if (lexer->at[1] == '+') {
				token.kind = TOKEN_PLUS_PLUS;
				advance(lexer);
			} else if (lexer->at[1] == '=') {
				token.kind = TOKEN_PLUS_EQUAL;
				advance(lexer);
			} else {
				token.kind = TOKEN_PLUS;
			}
		} break;
	case '-':
		{
			if (lexer->at[1] == '-') {
				token.kind = TOKEN_MINUS_MINUS;
				advance(lexer);
			} else if (lexer->at[1] == '>') {
				token.kind = TOKEN_ARROW;
				advance(lexer);
			} else if (lexer->at[1] == '=') {
				token.kind = TOKEN_MINUS_EQUAL;
				advance(lexer);
			} else {
				token.kind = TOKEN_MINUS;
			}
		} break;
	case '*':
		{
			if (lexer->at[1] == '=') {
				token.kind = TOKEN_STAR_EQUAL;
				advance(lexer);
			} else {
				token.kind = TOKEN_STAR;
			}
		} break;
	case '/':
		{
			if (lexer->at[1] == '=') {
				token.kind = TOKEN_SLASH_EQUAL;
				advance(lexer);
			} else if (lexer->at[1] == '/') {
				token.kind = TOKEN_COMMENT;
				while (lexer->at[1] != '\n' && lexer->at[1] != '\0') {
					advance(lexer);
				}
			} else if (lexer->at[1] == '*') {
				token.kind = TOKEN_COMMENT;
				while (!(lexer->at[1] == '*' && lexer->at[2] == '/')
					&& lexer->at[1] != '\0')
				{
					advance(lexer);
				}

				advance(lexer);
				advance(lexer);
			} else {
				token.kind = TOKEN_SLASH;
			}
		} break;
	case '%':
		{
			if (lexer->at[1] == '=') {
				token.kind = TOKEN_PERCENT_EQUAL;
				advance(lexer);
			} else {
				token.kind = TOKEN_PERCENT;
			}
		} break;
	case '=':
		{
			token.kind = eat1(lexer, TOKEN_EQUAL, '=', TOKEN_EQUAL_EQUAL);
		} break;
	case '<':
		{
			if (lexer->at[1] == '=') {
				token.kind = TOKEN_LESS_EQUAL;
				advance(lexer);
			} else if (lexer->at[1] == '<') {
				token.kind = TOKEN_LSHIFT;
				advance(lexer);
			} else {
				token.kind = TOKEN_LESS;
			}
		} break;
	case '>':
		{
			if (lexer->at[1] == '=') {
				token.kind = TOKEN_GREATER_EQUAL;
				advance(lexer);
			} else if (lexer->at[1] == '>') {
				token.kind = TOKEN_RSHIFT;
				advance(lexer);
			} else {
				token.kind = TOKEN_GREATER;
			}
		} break;
	case '#':
		token.kind = eat1(lexer, TOKEN_HASH, '#', TOKEN_HASH_HASH);
		break;
	case '\r':
		{
			token.kind = eat1(lexer, TOKEN_NEWLINE, '\n', TOKEN_NEWLINE);
		} break;
	case ' ': case '\t': case '\v': case '\f':
		{
			token.kind = TOKEN_WHITESPACE;
			while (is_whitespace(lexer->at[1])) {
				advance(lexer);
			}
		} break;
	case '\'':
	case '"':
		{
			while (lexer->at[1] != c
				&& lexer->at[1] != '\n'
				&& lexer->at[1] != '\0')
			{
				if (lexer->at[1] == '\\') {
					advance(lexer);
				}

				advance(lexer);
			}

			if (lexer->at[1] == c) {
				token.kind = c == '"' ? TOKEN_LITERAL_STRING : TOKEN_LITERAL_CHAR;
				advance(lexer);
			}
		} break;
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
		{
			token.kind = TOKEN_LITERAL_INT;
			while (is_digit(lexer->at[1])) {
				advance(lexer);
			}

			if (lexer->at[1] == '.') {
				advance(lexer);
				token.kind = TOKEN_LITERAL_FLOAT;
				while (is_digit(lexer->at[1])) {
					advance(lexer);
				}
			}

			while (is_ident(lexer->at[1])) {
				advance(lexer);
			}
		} break;
	default:
		{
			if (is_alpha(c) || c == '_') {
				token.kind = TOKEN_IDENT;
				while (is_ident(lexer->at[1])) {
					advance(lexer);
				}
			}
		} break;
	}

	isize end = lexer->file.offset;
	token.value = substr(lexer->file.contents, start, end);
	ASSERT(token.kind != TOKEN_EOF || token.value.length == 0);
	return token;
}

static token get_token(lexer *lexer);

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
	if (!cpp_accept(lexer, expected_token)) {
		ASSERT(!"Invalid token");
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
	f->if_depth = 0;
	lexer->file.prev = f;
	return &lexer->file;
}

static str tombstone = S("(deleted)");

static macro *
upsert_macro(macro_table *table, str name, arena *perm)
{
	macro **p = &table->macros;
	macro *m = NULL;

	if (!table) {
		return NULL;
	}

	macro *tmp = table->macros;
	for (u64 h = hash(name); *p; h <<= 2) {
		if (equals(name, (*p)->name)) {
			return *p;
		} else if ((*p)->name.at == tombstone.at) {
			m = *p;
		}

		p = &(*p)->child[h >> 62];
	}

	if (!perm) {
		return upsert_macro(table->parent, name, NULL);
	} else if (m == NULL) {
		*p = m = ALLOC(perm, 1, macro);
	}

	ASSERT(table->macros == tmp || tmp == NULL);
	return m;
}

static token
expand(cpp_state *cpp, token token)
{
	lexer *lexer = &cpp->lexer;
	if (cpp->ignore_token) {
		return token;
	}

	for (;;) {
		while (token.kind == TOKEN_EOF && cpp->macros->parent) {
			cpp->lexer.file = *cpp->lexer.file.prev;
			macro_table *prev = cpp->macros;
			cpp->macros = prev->parent;
			prev->parent = NULL;

			skip_whitespace(lexer);
			token = cpp_get_token(lexer);
		}

		macro *m = upsert_macro(cpp->macros, token.value, NULL);
		b32 can_expand_macro = (m != NULL && m->params.parent == NULL);
		if (!can_expand_macro) {
			break;
		}

		skip_whitespace(lexer);
		b32 has_params = (m->params.macros != NULL);
		if (has_params) {
			if (!cpp_accept(lexer, TOKEN_LPAREN)) {
				break;
			}

			macro *param = m->params.macros;
			while (!cpp_accept(lexer, TOKEN_EOF)) {
				skip_whitespace(lexer);

				param->value = lexer->file;
				param->value.prev = NULL;

				token = cpp_peek_token(lexer);
				while (token.kind != TOKEN_EOF
					&& token.kind != TOKEN_COMMA
					&& token.kind != TOKEN_RPAREN)
				{
					cpp_get_token(lexer);
					token = cpp_peek_token(lexer);
				}

				param->value.contents.length = lexer->file.offset;
				param = param->next;

				if (token.kind != TOKEN_COMMA) {
					break;
				}

				cpp_get_token(lexer);
				if (param == NULL) {
					fatalf(get_location(lexer), "Too many parameters");
				}
			}

			if (param != NULL) {
				fatalf(get_location(lexer), "Too few parameters");
			}

			cpp_expect(lexer, TOKEN_RPAREN);
		}

		m->params.parent = cpp->macros;
		cpp->macros = &m->params;

		file *f = push_file(cpp);
		f->name = m->value.name;
		f->contents = m->value.contents;
		f->offset = m->value.offset;
		skip_whitespace(lexer);
		token = cpp_get_token(lexer);
	}

	return token;
}

static i64
cpp_parse_expr(cpp_state *cpp, precedence prev_prec)
{
	i64 result = 0;
	lexer *lexer = &cpp->lexer;

	skip_whitespace(lexer);
	token token = expand(cpp, cpp_get_token(lexer));
	switch (token.kind) {
	case TOKEN_IDENT:
		if (equals(token.value, S("defined"))) {
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

			macro *m = upsert_macro(cpp->macros, token.value, NULL);
			result = (m != NULL);
		} else {
			result = 0;
		}
		break;
	case TOKEN_PLUS:
		result = +cpp_parse_expr(cpp, PREC_PRIMARY);
		break;
	case TOKEN_MINUS:
		result = -cpp_parse_expr(cpp, PREC_PRIMARY);
		break;
	case TOKEN_BANG:
		result = !cpp_parse_expr(cpp, PREC_PRIMARY);
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
		result = cpp_parse_expr(cpp, PREC_ASSIGN);
		skip_whitespace(lexer);
		cpp_expect(lexer, TOKEN_RPAREN);
		break;
	default:
		errorf(get_location(lexer), "Invalid expression");
	}

	while (!lexer->error) {
		skip_whitespace(lexer);
		token = expand(cpp, cpp_peek_token(lexer));
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
			lhs = cpp_parse_expr(cpp, prec);
			skip_whitespace(lexer);
			token = expand(cpp, cpp_get_token(lexer));
			if (token.kind != TOKEN_COLON) {
				errorf(get_location(lexer), "Expected ':'");
			}

			skip_whitespace(lexer);
			rhs = cpp_parse_expr(cpp, prec);
			token.kind = TOKEN_QMARK;
		} else {
			rhs = cpp_parse_expr(cpp, prec);
		}

		switch (token.kind) {
		case TOKEN_AMP:           result = lhs & rhs;  break;
		case TOKEN_AMP_AMP:       result = lhs && rhs; break;
		case TOKEN_BAR:           result = lhs | rhs;  break;
		case TOKEN_BAR_BAR:       result = lhs || rhs; break;
		case TOKEN_CARET:         result = lhs ^ rhs;  break;
		case TOKEN_EQUAL_EQUAL:   result = lhs == rhs; break;
		case TOKEN_GREATER:       result = lhs > rhs;  break;
		case TOKEN_GREATER_EQUAL: result = lhs >= rhs; break;
		case TOKEN_RSHIFT:        result = lhs >> rhs; break;
		case TOKEN_LESS:          result = lhs < rhs;  break;
		case TOKEN_LESS_EQUAL:    result = lhs <= rhs; break;
		case TOKEN_LSHIFT:        result = lhs << rhs; break;
		case TOKEN_MINUS:         result = lhs - rhs;  break;
		case TOKEN_PERCENT:       result = lhs % rhs;  break;
		case TOKEN_PLUS:          result = lhs + rhs;  break;
		case TOKEN_SLASH:         result = lhs / rhs;  break;
		case TOKEN_STAR:          result = lhs * rhs;  break;
		case TOKEN_QMARK:         result = result ? lhs : rhs; break;
		default:
			errorf(get_location(lexer), "Invalid operator");
		}
	}

	return result;
}

static void
push_if(cpp_state *cpp, b32 value)
{
	cpp->lexer.file.if_depth++;
	if (cpp->lexer.file.if_depth > LENGTH(cpp->if_state)) {
		fatalf(get_location(&cpp->lexer), "Internal error: Too many #ifs");
	}

	cpp->if_state[cpp->lexer.file.if_depth - 1] = 0;
	if (!cpp->ignore_token && value) {
		cpp->if_state[cpp->lexer.file.if_depth - 1] = IF_TRUE;
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
		token = cpp_get_token(lexer);

		if (at_line_start && token.kind == TOKEN_HASH) {
			skip_whitespace(lexer);
			token = cpp_get_token(lexer);
			if (token.kind != TOKEN_IDENT) {
				fatalf(get_location(lexer), "Invalid preprocessing directive");
			}

			if_state curr_state = IF_TRUE;
			if_state prev_state = IF_TRUE;
			if (cpp->lexer.file.if_depth > 0) {
				curr_state = cpp->if_state[cpp->lexer.file.if_depth - 1];
				if (cpp->lexer.file.if_depth > 1) {
					prev_state = cpp->if_state[cpp->lexer.file.if_depth - 2];
				}
			}

			skip_whitespace(lexer);
			if (equals(token.value, S("if"))) {
				i64 value = cpp_parse_expr(cpp, PREC_ASSIGN);
				push_if(cpp, value);
				skip_line(lexer);
			} else if (equals(token.value, S("ifdef"))) {
				token = cpp_get_token(lexer);
				if (token.kind != TOKEN_IDENT) {
					fatalf(get_location(lexer), "Expected identifier");
				}

				macro *m = upsert_macro(cpp->macros, token.value, NULL);
				b32 is_defined = (m != NULL);
				push_if(cpp, is_defined);
			} else if (equals(token.value, S("ifndef"))) {
				token = cpp_get_token(lexer);
				if (token.kind != TOKEN_IDENT) {
					fatalf(get_location(lexer), "Expected identifier");
				}

				macro *m = upsert_macro(cpp->macros, token.value, NULL);
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
					i64 value = cpp_parse_expr(cpp, PREC_ASSIGN);
					if (value) {
						cpp->if_state[cpp->lexer.file.if_depth - 1] |= IF_TRUE;
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
				cpp->if_state[cpp->lexer.file.if_depth - 1] |= IF_HAS_ELSE;
				if (!(curr_state & IF_TRUE) && (prev_state & IF_TRUE)) {
					cpp->if_state[cpp->lexer.file.if_depth - 1] |= IF_TRUE;
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

				advance(lexer);
				char c = '\0';
				isize start = lexer->file.offset;
				do {
					c = advance(lexer);
				} while (c != '\n' && c != end_char);
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
					{ S("stddef.h"), S(
						"typedef unsigned long size_t;\n"
						"typedef int wchar_t;\n"
						"typedef long ptrdiff_t;\n"
						"#define offsetof(s, m) __builtin_offsetof(s, m)\n"),
					}
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
				macro *m = upsert_macro(cpp->macros, token.value, cpp->arena);
				if (m->name.at != NULL) {
					errorf(get_location(lexer), "Macro was already defined");
				}

				if (cpp_accept(lexer, TOKEN_LPAREN)) {
					macro *params = NULL;
					macro **ptr = &params;
					skip_whitespace(lexer);
					while (!cpp_accept(lexer, TOKEN_EOF)) {
						token = cpp_get_token(lexer);
						if (token.kind == TOKEN_IDENT) {
							*ptr = upsert_macro(&m->params, token.value, cpp->arena);
							(*ptr)->name = token.value;
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

				m->name = name;
				m->value = lexer->file;
				m->value.prev = NULL;
				skip_line(lexer);
				m->value.contents.length = lexer->file.offset;
			} else if (equals(token.value, S("undef"))) {
				token = cpp_get_token(lexer);
				macro *m = upsert_macro(cpp->macros, token.value, NULL);
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
		} else {
			token = expand(cpp, token);
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
				fatalf(get_location(lexer), "Unterminated #if");
			}

			if (cpp->lexer.file.prev != NULL) {
				ASSERT(cpp->macros->parent == NULL);

				token.kind = TOKEN_NEWLINE;
				at_line_start = true;
				cpp->lexer.file = *cpp->lexer.file.prev;
				skip_whitespace(lexer);
			}
		} else if (token.kind == TOKEN_NEWLINE) {
			at_line_start = true;
			skip_whitespace(lexer);
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
tokenize_str(str src, char *filename, arena *perm)
{
	cpp_state cpp = {0};
	cpp.arena = perm;
	cpp.lexer.file.name = filename;
	cpp.lexer.file.contents = src;
	cpp.macros = ALLOC(perm, 1, macro_table);
	get_token(&cpp.lexer);
	get_token(&cpp.lexer);
	return cpp;
}

static cpp_state
tokenize(char *filename, arena *perm)
{
	str src = read_file(filename, perm);
	cpp_state cpp = tokenize_str(src, filename, perm);
	return cpp;
}
