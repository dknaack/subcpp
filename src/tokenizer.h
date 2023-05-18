enum token_kind {
	TOKEN_INVALID,
	TOKEN_EOF,
	TOKEN_ADD,
	TOKEN_SUB,
	TOKEN_MUL,
	TOKEN_DIV,
	TOKEN_MOD,
	TOKEN_ASSIGN,
	TOKEN_INT,
	TOKEN_IDENTIFIER,
	TOKEN_WHITESPACE,
	TOKEN_COUNT
};

struct token {
	enum token_kind kind;
	size_t pos;
};

struct tokenizer {
	struct string source;
	size_t pos;
};
