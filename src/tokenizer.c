static char
advance(tokenizer *tokenizer)
{
	char c = '\0';

	if (tokenizer->pos < tokenizer->source.length) {
		c = tokenizer->source.at[tokenizer->pos++];
		tokenizer->loc.column++;
		if (c == '\n') {
			tokenizer->loc.line++;
			tokenizer->loc.column = 0;
		}
	}

	return c;
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
is_whitespace(char c)
{
	b32 result = (c == ' ' || c == '\n' || c == '\t' || c == '\r' || c == '\f' || c == '\v');
	return result;
}

static token_kind
eat1(tokenizer *tokenizer, token_kind default_kind, char c1, token_kind kind1)
{
	char c = advance(tokenizer);
	if (c == c1) {
		return kind1;
	} else {
		tokenizer->pos--;
		return default_kind;
	}
}

static token
get_raw_token(tokenizer *tokenizer)
{
	token token;
	token.kind = TOKEN_INVALID;
	token.value.at = tokenizer->source.at + tokenizer->pos;
	token.value.length = 0;

	if (tokenizer->pos >= tokenizer->source.length) {
		token.kind = TOKEN_EOF;
		return token;
	}

	char c = advance(tokenizer);
	switch (c) {
	case '.': token.kind = TOKEN_DOT; break;
	case '+': token.kind = TOKEN_PLUS; break;
	case '-': token.kind = TOKEN_MINUS; break;
	case '*': token.kind = TOKEN_STAR; break;
	case '/': token.kind = TOKEN_SLASH; break;
	case '%': token.kind = TOKEN_PERCENT; break;
	case '(': token.kind = TOKEN_LPAREN; break;
	case ')': token.kind = TOKEN_RPAREN; break;
	case '[': token.kind = TOKEN_LBRACKET; break;
	case ']': token.kind = TOKEN_RBRACKET; break;
	case '{': token.kind = TOKEN_LBRACE; break;
	case '}': token.kind = TOKEN_RBRACE; break;
	case ',': token.kind = TOKEN_COMMA; break;
	case ';': token.kind = TOKEN_SEMICOLON; break;
	case '&': token.kind = TOKEN_AMPERSAND; break;
	case '=':
		token.kind = eat1(tokenizer, TOKEN_EQUAL, '=', TOKEN_EQUAL_EQUAL);
		break;
	case '<':
		token.kind = eat1(tokenizer, TOKEN_LT, '=', TOKEN_LEQ);
		break;
	case '>':
		token.kind = eat1(tokenizer, TOKEN_GT, '=', TOKEN_GEQ);
		break;
	case '#':
		token.kind = eat1(tokenizer, TOKEN_HASH, '#', TOKEN_HASH_HASH);
		break;
	case '\n':
		token.kind = TOKEN_NEWLINE;
		break;
	case '\r':
		token.kind = eat1(tokenizer, TOKEN_NEWLINE, '\n', TOKEN_NEWLINE);
		break;
	case ' ': case '\t': case '\v': case '\f':
		token.kind = TOKEN_WHITESPACE;
		break;
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
		token.kind = TOKEN_LITERAL_INT;
		do {
			c = advance(tokenizer);
		} while (is_digit(c));
		tokenizer->pos--;
		break;
	default:
		if (is_alpha(c) || c == '_') {
			token.kind = TOKEN_IDENT;
			do {
				c = advance(tokenizer);
			} while (is_alpha(c) || is_digit(c) || c == '_');
			tokenizer->pos--;
		}

		break;
	}

	token.value.length = tokenizer->source.at + tokenizer->pos - token.value.at;
	return token;
}

static token
peek_raw_token(tokenizer *t)
{
	tokenizer tmp = *t;
	token token = get_raw_token(&tmp);
	return token;
}

static void
eat_whitespace(tokenizer *tokenizer)
{
	while (peek_raw_token(tokenizer).kind == TOKEN_WHITESPACE) {
		get_raw_token(tokenizer);
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
push_file(tokenizer *t, str path, b32 system_header)
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
pop_file(tokenizer *t)
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
get_token(tokenizer *tokenizer)
{
	token tmp, token = {TOKEN_INVALID};
	struct {
		token_kind token;
		str str;
	} keywords[] = {
		{ TOKEN_BREAK,    S("break")    },
		{ TOKEN_CHAR,     S("char")     },
		{ TOKEN_CONTINUE, S("continue") },
		{ TOKEN_ELSE,     S("else")     },
		{ TOKEN_FOR,      S("for")      },
		{ TOKEN_IF,       S("if")       },
		{ TOKEN_INT,      S("int")      },
		{ TOKEN_PRINT,    S("print")    },
		{ TOKEN_RETURN,   S("return")   },
		{ TOKEN_STRUCT,   S("struct")   },
		{ TOKEN_VOID,     S("void")     },
		{ TOKEN_WHILE,    S("while")    },
	};

	b32 at_line_start = (tokenizer->pos == 0);
	do {
		token = get_raw_token(tokenizer);
		if (at_line_start && token.kind == TOKEN_HASH) {
			token = get_raw_token(tokenizer);
			if (token.kind == TOKEN_IDENT) {
				if (str_equals(token.value, S("include"))) {
					eat_whitespace(tokenizer);

					str filename = {0};
					b32 is_system_header = false;
					if (tokenizer->source.at[tokenizer->pos] == '<') {
						is_system_header = true;
						advance(tokenizer);

						char c;
						isize start = tokenizer->pos;
						do {
							c = advance(tokenizer);
						} while (c != '\n' && c != '>');
						isize end = tokenizer->pos - 1;

						filename.at = tokenizer->source.at + start;
						filename.length = end - start;
						printf("%.*s\n", (int)filename.length, filename.at);
					} else if (tokenizer->source.at[tokenizer->pos] == '"') {
						advance(tokenizer);

						char c;
						isize start = tokenizer->pos;
						do {
							c = advance(tokenizer);
						} while (c != '\n' && c != '"');
						isize end = tokenizer->pos - 1;

						filename.at = tokenizer->source.at + start;
						filename.length = end - start;
						printf("%.*s\n", (int)filename.length, filename.at);
					} else {
						ASSERT(!"Macro filenames have not been implement yet");
					}

					while (token.kind != TOKEN_NEWLINE) {
						token = get_raw_token(tokenizer);
					}

					push_file(tokenizer, filename, is_system_header);
				}
			}
		}

		if (token.kind == TOKEN_EOF) {
			if (pop_file(tokenizer)) {
				token.kind = TOKEN_WHITESPACE;
			}
		} else if (token.kind == TOKEN_NEWLINE) {
			at_line_start = true;
			eat_whitespace(tokenizer);
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
	token = tokenizer->lookahead[0];
	tokenizer->lookahead[0] = tokenizer->lookahead[1];
	tokenizer->lookahead[1] = tmp;
	return token;
}

static token
peek_token(tokenizer *t)
{
	token result = t->lookahead[0];
	return result;
}

static tokenizer
tokenize(char *filename, arena *arena)
{
	tokenizer tokenizer = {0};
	tokenizer.arena = arena;
	tokenizer.filename = filename;
	tokenizer.loc.file = filename;
	tokenizer.loc.line = 1;
	tokenizer.source = read_file(filename, arena);
	get_token(&tokenizer);
	get_token(&tokenizer);
	return tokenizer;
}
