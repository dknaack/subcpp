static struct tokenizer
tokenize(struct string source)
{
	struct tokenizer tokenizer = {0};
	tokenizer.source = source;
	return tokenizer;
}

static char
advance(struct tokenizer *tokenizer)
{
	char c = '\0';

	if (tokenizer->pos < tokenizer->source.length) {
		c = tokenizer->source.at[tokenizer->pos++];
	}

	return c;
}

static bool
is_alpha(char c)
{
	bool result = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z');
	return result;
}

static bool
is_digit(char c)
{
	bool result = ('0' <= c && c <= '9');
	return result;
}

static struct token
get_raw_token(struct tokenizer *tokenizer)
{
	struct token token;
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
	case '=': token.kind = TOKEN_ASSIGN; break;
	case ';': token.kind = TOKEN_SEMICOLON; break;
	case ' ': case '\t': case '\n': case '\r': case '\v': case '\f':
		token.kind = TOKEN_WHITESPACE;
		break;
	default:
		if (is_alpha(c) || c == '_') {
			token.kind = TOKEN_IDENTIFIER;
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

static struct token
get_token(struct tokenizer *tokenizer)
{
	struct token token = {0};

	do {
		token = get_raw_token(tokenizer);
	} while (token.kind == TOKEN_WHITESPACE);

	return token;
}

static struct token
peek_token(struct tokenizer *tokenizer)
{
	struct tokenizer tmp = *tokenizer;
	struct token token = get_token(&tmp);
	return token;
}
