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
	token.pos = tokenizer->pos;

	if (token.pos >= tokenizer->source.length) {
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
	case ' ': case '\t': case '\n': case '\r': case '\v': case '\f':
		token.kind = TOKEN_WHITESPACE;
		break;
	default:
		if (is_alpha(c) || c == '_') {
			token.kind = TOKEN_IDENTIFIER;
			do {
				c = advance(tokenizer);
			} while (is_alpha(c) || is_digit(c) || c == '_');
		}

		break;
	}

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

static char *
token_name(enum token_kind kind)
{
	switch (kind) {
	case TOKEN_EOF:        return "EOF";
	case TOKEN_ADD:        return "'+'";
	case TOKEN_SUB:        return "'-'";
	case TOKEN_MUL:        return "'*'";
	case TOKEN_DIV:        return "'/'";
	case TOKEN_MOD:        return "'%'";
	case TOKEN_ASSIGN:     return "'='";
	case TOKEN_INT:        return "int";
	case TOKEN_IDENTIFIER: return "identifier";
	case TOKEN_WHITESPACE: return "whitespace";
	default:               return "(invalid)";
	}
}


