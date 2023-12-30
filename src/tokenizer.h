typedef enum {
	TOKEN_INVALID,
	TOKEN_EOF,
	TOKEN_COMMA,
	TOKEN_AMPERSAND,
	TOKEN_DOT,
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
	TOKEN_LBRACKET,
	TOKEN_RBRACKET,
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
	TOKEN_STRUCT,
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

static char *
get_token_name(token_kind kind)
{
	switch (kind) {
	case TOKEN_EOF:         return "EOF";
	case TOKEN_COMMA:       return "','";
	case TOKEN_DOT:         return "'.'";
	case TOKEN_ADD:         return "'+'";
	case TOKEN_SUB:         return "'-'";
	case TOKEN_MUL:         return "'*'";
	case TOKEN_DIV:         return "'/'";
	case TOKEN_MOD:         return "'%'";
	case TOKEN_EQUALS:      return "'=='";
	case TOKEN_ASSIGN:      return "'='";
	case TOKEN_IDENT:       return "identifier";
	case TOKEN_WHITESPACE:  return "whitespace";
	case TOKEN_SEMICOLON:   return "';'";
	case TOKEN_LPAREN:      return "'('";
	case TOKEN_RPAREN:      return "')'";
	case TOKEN_LBRACKET:    return "'['";
	case TOKEN_RBRACKET:    return "']'";
	case TOKEN_LBRACE:      return "'{'";
	case TOKEN_RBRACE:      return "'}'";
	case TOKEN_LITERAL_INT: return "integer";
	case TOKEN_BREAK:       return "'break'";
	case TOKEN_CHAR:        return "'char'";
	case TOKEN_CONTINUE:    return "'continue'";
	case TOKEN_ELSE:        return "'else'";
	case TOKEN_FOR:         return "'for'";
	case TOKEN_IF:          return "'if'";
	case TOKEN_INT:         return "'int'";
	case TOKEN_PRINT:       return "'print'";
	case TOKEN_RETURN:      return "'return'";
	case TOKEN_VOID:        return "'void'";
	case TOKEN_WHILE:       return "'while'";
	default:                return "(invalid)";
	}
}
