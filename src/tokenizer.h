typedef enum {
	TOKEN_INVALID,
	TOKEN_EOF,
	TOKEN_COMMA,
	TOKEN_AMPERSAND,
	TOKEN_ADD,
	TOKEN_SUB,
	TOKEN_MUL,
	TOKEN_DIV,
	TOKEN_MOD,
	TOKEN_EQUALS,
	TOKEN_LT,
	TOKEN_GT,
	TOKEN_LEQ,
	TOKEN_GEQ,
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
	TOKEN_CHAR,
	TOKEN_ELSE,
	TOKEN_FOR,
	TOKEN_IF,
	TOKEN_INT,
	TOKEN_PRINT,
	TOKEN_RETURN,
	TOKEN_VOID,
	TOKEN_WHILE,
	TOKEN_COUNT
} token_kind;

typedef struct {
	token_kind kind;
	string value;
} token;

typedef struct {
	char *file;
	u32 line;
	u32 column;
} location;

typedef struct {
	location loc;
	string source;
	usize pos;
	b32 error;
} tokenizer;
