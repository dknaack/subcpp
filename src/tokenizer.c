static string
read_file(char *filename, arena *arena)
{
	string result = {0};
	FILE *file = fopen(filename, "rb");
	if (file) {
		fseek(file, 0, SEEK_END);
		result.length = ftell(file);
		fseek(file, 0, SEEK_SET);

		result.at = ALLOC(arena, result.length + 1, char);
		if (result.at) {
			fread(result.at, result.length, 1, file);
			result.at[result.length] = '\0';
		}

		fclose(file);
	}

	return result;
}

static tokenizer
tokenize(char *filename, arena *arena)
{
	tokenizer tokenizer = {0};
	tokenizer.loc.file = filename;
	tokenizer.loc.line = 1;
	tokenizer.source = read_file(filename, arena);
	return tokenizer;
}

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
	case '+': token.kind = TOKEN_ADD; break;
	case '-': token.kind = TOKEN_SUB; break;
	case '*': token.kind = TOKEN_MUL; break;
	case '/': token.kind = TOKEN_DIV; break;
	case '%': token.kind = TOKEN_MOD; break;
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
		token.kind = eat1(tokenizer, TOKEN_ASSIGN, '=', TOKEN_EQUALS);
		break;
	case '<':
		token.kind = eat1(tokenizer, TOKEN_LT, '=', TOKEN_LEQ);
		break;
	case '>':
		token.kind = eat1(tokenizer, TOKEN_GT, '=', TOKEN_GEQ);
		break;
	case ' ': case '\t': case '\n': case '\r': case '\v': case '\f':
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

static b32
string_equals(string a, string b)
{
	if (a.length != b.length) {
		return false;
	}

	while (a.length-- > 0) {
		if (*a.at++ != *b.at++) {
			return false;
		}
	}

	return true;
}

static token
get_token(tokenizer *tokenizer)
{
	token token = {TOKEN_INVALID};
	struct {
		token_kind token;
		string str;
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

	do {
		token = get_raw_token(tokenizer);
		if (token.kind == TOKEN_IDENT) {
			for (usize i = 0; i < LENGTH(keywords); i++) {
				if (string_equals(token.value, keywords[i].str)) {
					token.kind = keywords[i].token;
					break;
				}
			}
		}
	} while (token.kind == TOKEN_WHITESPACE);

	return token;
}

static token
peek_token(tokenizer *t)
{
	tokenizer tmp = *t;
	token token = get_token(&tmp);
	return token;
}
