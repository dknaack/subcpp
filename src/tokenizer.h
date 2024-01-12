typedef enum {
	TOKEN_INVALID,
	TOKEN_EOF,
	TOKEN_NEWLINE,
	TOKEN_WHITESPACE,

	// symbols
	TOKEN_AMPERSAND,
	TOKEN_COMMA,
	TOKEN_DOT,
	TOKEN_EQUAL,
	TOKEN_EQUAL_EQUAL,
	TOKEN_GEQ,
	TOKEN_GT,
	TOKEN_HASH,
	TOKEN_HASH_HASH,
	TOKEN_IDENT,
	TOKEN_LBRACE,
	TOKEN_LBRACKET,
	TOKEN_LEQ,
	TOKEN_LPAREN,
	TOKEN_LT,
	TOKEN_MINUS,
	TOKEN_PERCENT,
	TOKEN_PLUS,
	TOKEN_RBRACE,
	TOKEN_RBRACKET,
	TOKEN_RPAREN,
	TOKEN_SEMICOLON,
	TOKEN_SLASH,
	TOKEN_STAR,

	// literals
	TOKEN_LITERAL_INT,

	// keywords
	TOKEN_BREAK,
	TOKEN_CHAR,
	TOKEN_CONTINUE,
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
	str value;
} token;

typedef struct {
	char *file;
	u32 line;
	u32 column;
} location;

typedef struct {
	token lookahead[2];
	location loc;
	str source;
	isize pos;
	b32 error;
} tokenizer;

static char *
get_token_name(token_kind kind)
{
	switch (kind) {
	case TOKEN_INVALID:     return "invalid token";
	case TOKEN_EOF:         return "eof";
	case TOKEN_COMMA:       return "','";
	case TOKEN_AMPERSAND:   return "'&'";
	case TOKEN_DOT:         return "'.'";
	case TOKEN_PLUS:        return "'+'";
	case TOKEN_MINUS:       return "'-'";
	case TOKEN_STAR:        return "'*'";
	case TOKEN_SLASH:       return "'/'";
	case TOKEN_PERCENT:     return "'%'";
	case TOKEN_EQUAL_EQUAL: return "'=='";
	case TOKEN_LT:          return "'<'";
	case TOKEN_GT:          return "'>'";
	case TOKEN_LEQ:         return "'<='";
	case TOKEN_GEQ:         return "'>='";
	case TOKEN_HASH:        return "'#'";
	case TOKEN_HASH_HASH:   return "'##'";
	case TOKEN_EQUAL:       return "'='";
	case TOKEN_IDENT:       return "identifier";
	case TOKEN_NEWLINE:     return "newline";
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
	case TOKEN_CONTINUE:    return "'continue'";
	case TOKEN_CHAR:        return "'char'";
	case TOKEN_ELSE:        return "'else'";
	case TOKEN_FOR:         return "'for'";
	case TOKEN_IF:          return "'if'";
	case TOKEN_INT:         return "'int'";
	case TOKEN_PRINT:       return "'print'";
	case TOKEN_RETURN:      return "'return'";
	case TOKEN_STRUCT:      return "'struct'";
	case TOKEN_VOID:        return "'void'";
	case TOKEN_WHILE:       return "'while'";
	case TOKEN_COUNT:       return "invalid token";
	}

	return "invalid token";
}
