static char
advance(lexer *lexer)
{
	for (isize i = 0; i < LENGTH(lexer->at); i++) {
		if (lexer->pos + i < lexer->source.length) {
			lexer->at[i] = lexer->source.at[lexer->pos + i];
		} else {
			lexer->at[i] = '\0';
		}
	}

	if (lexer->pos < lexer->source.length) {
		lexer->pos++;
	}

	lexer->loc.column++;
	if (lexer->at[0] == '\n') {
		lexer->loc.line++;
		lexer->loc.column = 0;
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
get_raw_token(lexer *lexer)
{
	token token = {0};
	isize start = lexer->pos;

	char c = advance(lexer);
	switch (c) {
	case '(':  token.kind = TOKEN_LPAREN;    break;
	case ')':  token.kind = TOKEN_RPAREN;    break;
	case '[':  token.kind = TOKEN_LBRACKET;  break;
	case ']':  token.kind = TOKEN_RBRACKET;  break;
	case '{':  token.kind = TOKEN_LBRACE;    break;
	case '}':  token.kind = TOKEN_RBRACE;    break;
	case ',':  token.kind = TOKEN_COMMA;     break;
	case ':':  token.kind = TOKEN_COLON;     break;
	case ';':  token.kind = TOKEN_SEMICOLON; break;
	case '~':  token.kind = TOKEN_TILDE;     break;
	case '^':  token.kind = TOKEN_CARET;     break;
	case '!':  token.kind = TOKEN_BANG;      break;
	case '\\': token.kind = TOKEN_BACKSLASH; break;
	case '\n': token.kind = TOKEN_NEWLINE;   break;
	case '\0': token.kind = TOKEN_EOF;       break;
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
	case '"':
		{
			while (lexer->at[1] != '"'
				&& lexer->at[1] != '\n'
				&& lexer->at[1] != '\0')
			{
				if (lexer->at[1] == '\\') {
					advance(lexer);
				}

				advance(lexer);
			}

			if (lexer->at[1] == '"') {
				token.kind = TOKEN_LITERAL_STRING;
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

	isize end = lexer->pos;
	token.value = substr(lexer->source, start, end);
	ASSERT(token.kind != TOKEN_EOF || token.value.length == 0);
	return token;
}

static token
peek_raw_token(lexer *t)
{
	lexer tmp = *t;
	token token = get_raw_token(&tmp);
	return token;
}

static void
eat_whitespace(lexer *lexer)
{
	while (peek_raw_token(lexer).kind == TOKEN_WHITESPACE) {
		get_raw_token(lexer);
	}
}

static b32
is_relative_path(str path)
{
	b32 result = path.length <= 0 || path.at[0] != '/';
	return result;
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

	str_copy(substr(result, 0, a.length), a);
	result.at[a.length] = '/';
	str_copy(substr(result, a.length + 1, -1), b);
	return result;
}

static void
push_file(lexer *t, str path, b32 system_header)
{
	if (path.length == 0) {
		// TODO: report error: Invalid header path
		t->error = 1;
		return;
	}

	file *f = ALLOC(t->arena, 1, file);
	f->contents = t->source;
	f->pos = t->pos;
	f->prev = t->files;
	f->name = t->filename;
	f->loc = t->loc;
	t->files = f;

	static str system_include_dirs[] = {
		S("/usr/local/include"),
		S("/usr/include")
	};

	str filename = {0};
	str contents = {0};
	b32 found_header = false;
	if (!system_header) {
		if (is_relative_path(path)) {
			str dir = dirname(str_from(t->filename));
			filename = concat_paths(dir, path, t->arena);
			contents = read_file(filename.at, t->arena);
			if (errno == 0) {
				found_header = true;
			}
		}

		// TODO: Search for directories passed with -I option
	}

	if (!found_header) {
		for (u32 i = 0; i < LENGTH(system_include_dirs); i++) {
			str dir = system_include_dirs[i];
			filename = concat_paths(dir, path, t->arena);
			contents = read_file(filename.at, t->arena);
			if (errno == 0) {
				found_header = true;
				break;
			}
		}
	}

	if (found_header) {
		t->filename = filename.at;
		t->source = contents;
		t->pos = 0;
	}
}

static b32
pop_file(lexer *t)
{
	file *f = t->files;
	b32 result = f != NULL;
	if (result) {
		t->source = f->contents;
		t->pos = f->pos;
		t->loc = f->loc;
		t->filename = f->name;
		t->files = f->prev;
	}

	return result;
}

static token
get_token(lexer *lexer)
{
	token tmp, token = {TOKEN_INVALID};
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

	b32 at_line_start = (lexer->pos == 0);
	do {
		token = get_raw_token(lexer);
		if (at_line_start && token.kind == TOKEN_HASH) {
			token = get_raw_token(lexer);
			if (token.kind == TOKEN_IDENT) {
				if (str_equals(token.value, S("include"))) {
					eat_whitespace(lexer);

					str filename = {0};
					b32 is_system_header = false;
					if (lexer->source.at[lexer->pos] == '<') {
						is_system_header = true;
						advance(lexer);

						char c;
						isize start = lexer->pos;
						do {
							c = advance(lexer);
						} while (c != '\n' && c != '>');
						isize end = lexer->pos - 1;

						filename.at = lexer->source.at + start;
						filename.length = end - start;
						printf("%.*s\n", (int)filename.length, filename.at);
					} else if (lexer->source.at[lexer->pos] == '"') {
						advance(lexer);

						char c;
						isize start = lexer->pos;
						do {
							c = advance(lexer);
						} while (c != '\n' && c != '"');
						isize end = lexer->pos - 1;

						filename.at = lexer->source.at + start;
						filename.length = end - start;
						printf("%.*s\n", (int)filename.length, filename.at);
					} else {
						ASSERT(!"Macro filenames have not been implement yet");
					}

					while (token.kind != TOKEN_NEWLINE) {
						token = get_raw_token(lexer);
						if (token.kind == TOKEN_BACKSLASH) {
							token = peek_raw_token(lexer);
							if (token.kind == TOKEN_NEWLINE) {
								get_raw_token(lexer);
							}
						}
					}

					push_file(lexer, filename, is_system_header);
				}
			}
		}

		if (token.kind == TOKEN_BACKSLASH) {
			token = peek_raw_token(lexer);
			if (token.kind == TOKEN_NEWLINE) {
				get_raw_token(lexer);
			} else {
				token.kind = TOKEN_INVALID;
			}
		} else if (token.kind == TOKEN_EOF) {
			if (pop_file(lexer)) {
				token.kind = TOKEN_WHITESPACE;
			}
		} else if (token.kind == TOKEN_NEWLINE) {
			at_line_start = true;
			eat_whitespace(lexer);
		} else if (token.kind == TOKEN_IDENT) {
			for (usize i = 0; i < LENGTH(keywords); i++) {
				if (str_equals(token.value, keywords[i].str)) {
					token.kind = keywords[i].token;
					break;
				}
			}
		}
	} while (token.kind == TOKEN_WHITESPACE
		|| token.kind == TOKEN_NEWLINE);

	tmp = token;
	token = lexer->peek[0];
	lexer->peek[0] = lexer->peek[1];
	lexer->peek[1] = tmp;
	return token;
}

static token
peek_token(lexer *t)
{
	token result = t->peek[0];
	return result;
}

static lexer
tokenize_str(str src, arena *perm)
{
	lexer lexer = {0};
	lexer.arena = perm;
	lexer.filename = "(no file)";
	lexer.loc.file = "(no file)";
	lexer.loc.line = 1;
	lexer.source = src;
	get_token(&lexer);
	get_token(&lexer);
	return lexer;
}

static lexer
tokenize(char *filename, arena *perm)
{
	str src = read_file(filename, perm);
	lexer t = tokenize_str(src, perm);
	t.loc.file = t.filename = filename;
	return t;
}
