enum token_kind {
	TOKEN_INVALID,
	TOKEN_EOF,
	TOKEN_COMMA,
	TOKEN_ADD,
	TOKEN_SUB,
	TOKEN_MUL,
	TOKEN_DIV,
	TOKEN_MOD,
	TOKEN_ASSIGN,
	TOKEN_IDENT,
	TOKEN_WHITESPACE,
	TOKEN_SEMICOLON,
	TOKEN_LPAREN,
	TOKEN_RPAREN,
	TOKEN_LBRACE,
	TOKEN_RBRACE,
	TOKEN_LITERAL_INT,
	TOKEN_BREAK,
	TOKEN_CONTINUE,
	TOKEN_ELSE,
	TOKEN_FOR,
	TOKEN_IF,
	TOKEN_INT,
	TOKEN_PRINT,
	TOKEN_RETURN,
	TOKEN_VOID,
	TOKEN_WHILE,
	TOKEN_COUNT
};

struct token {
	enum token_kind kind;
	struct string value;
};

struct tokenizer {
	struct string source;
	char *filename;
	size_t pos;
	bool error;
};
