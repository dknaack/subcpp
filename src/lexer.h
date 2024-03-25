typedef enum {
	TOKEN_INVALID,
	TOKEN_EOF,
	TOKEN_NEWLINE,
	TOKEN_WHITESPACE,
	TOKEN_COMMENT,

	// symbols
	TOKEN_AMP,
	TOKEN_AMP_AMP,
	TOKEN_AMP_EQUAL,
	TOKEN_ARROW,
	TOKEN_BACKSLASH,
	TOKEN_BANG,
	TOKEN_BAR,
	TOKEN_BAR_BAR,
	TOKEN_BAR_EQUAL,
	TOKEN_CARET,
	TOKEN_CARET_EQUAL,
	TOKEN_COLON,
	TOKEN_COMMA,
	TOKEN_DOT,
	TOKEN_ELLIPSIS,
	TOKEN_EQUAL,
	TOKEN_EQUAL_EQUAL,
	TOKEN_GREATER,
	TOKEN_GREATER_EQUAL,
	TOKEN_HASH,
	TOKEN_HASH_HASH,
	TOKEN_IDENT,
	TOKEN_LBRACE,
	TOKEN_LBRACKET,
	TOKEN_LESS,
	TOKEN_LESS_EQUAL,
	TOKEN_LPAREN,
	TOKEN_LSHIFT,
	TOKEN_MINUS,
	TOKEN_MINUS_EQUAL,
	TOKEN_MINUS_MINUS,
	TOKEN_PERCENT,
	TOKEN_PERCENT_EQUAL,
	TOKEN_PLUS,
	TOKEN_PLUS_EQUAL,
	TOKEN_PLUS_PLUS,
	TOKEN_QMARK,
	TOKEN_RBRACE,
	TOKEN_RBRACKET,
	TOKEN_RPAREN,
	TOKEN_RSHIFT,
	TOKEN_SEMICOLON,
	TOKEN_SLASH,
	TOKEN_SLASH_EQUAL,
	TOKEN_STAR,
	TOKEN_STAR_EQUAL,
	TOKEN_TILDE,

	// literals
	TOKEN_LITERAL_CHAR,
	TOKEN_LITERAL_FLOAT,
	TOKEN_LITERAL_INT,
	TOKEN_LITERAL_STRING,

	// keywords
	TOKEN_AUTO,
	TOKEN_BREAK,
	TOKEN_CASE,
	TOKEN_CHAR,
	TOKEN_CONST,
	TOKEN_CONTINUE,
	TOKEN_DEFAULT,
	TOKEN_DO,
	TOKEN_ELSE,
	TOKEN_ENUM,
	TOKEN_EXTERN,
	TOKEN_FLOAT,
	TOKEN_FOR,
	TOKEN_GOTO,
	TOKEN_IF,
	TOKEN_INT,
	TOKEN_LONG,
	TOKEN_REGISTER,
	TOKEN_RESTRICT,
	TOKEN_RETURN,
	TOKEN_SHORT,
	TOKEN_SIGNED,
	TOKEN_STATIC,
	TOKEN_STRUCT,
	TOKEN_SWITCH,
	TOKEN_THREAD_LOCAL,
	TOKEN_TYPEDEF,
	TOKEN_UNSIGNED,
	TOKEN_VOID,
	TOKEN_VOLATILE,
	TOKEN_WHILE,

	TOKEN_COUNT
} token_kind;

typedef struct {
	token_kind kind;
	str value;
} token;

typedef struct {
	char *file;
	isize offset;
} location;

typedef struct file file;
struct file {
	file *prev;
	char *name;
	str contents;
	isize pos;
	location loc;
};

typedef struct macro_param macro_param;
struct macro_param {
	macro_param *next;
	str name;
};

typedef struct macro macro;
struct macro {
	str name;
	str value;
	macro_param *params;
	macro *child[4];
};

typedef struct {
	arena *arena;
	file *files;
	macro *macros;
	location loc;
	str source;
	isize pos;
	b32 error;
	b32 preprocess;
	token peek[2];
	char at[4];
} lexer;

static char *
get_token_name(token_kind kind)
{
	switch (kind) {
	case TOKEN_INVALID:        return "invalid token";
	case TOKEN_EOF:            return "eof";
	case TOKEN_NEWLINE:        return "newline";
	case TOKEN_COMMENT:        return "comment";
	case TOKEN_WHITESPACE:     return "whitespace";
	case TOKEN_AMP:            return "'&'";
	case TOKEN_AMP_AMP:        return "'&&'";
	case TOKEN_AMP_EQUAL:      return "'&='";
	case TOKEN_ARROW:          return "'->'";
	case TOKEN_BACKSLASH:      return "'\\'";
	case TOKEN_BANG:           return "'!'";
	case TOKEN_BAR:            return "'|'";
	case TOKEN_BAR_BAR:        return "'||'";
	case TOKEN_BAR_EQUAL:      return "'|='";
	case TOKEN_CARET:          return "'^'";
	case TOKEN_CARET_EQUAL:    return "'^='";
	case TOKEN_COLON:          return "': '";
	case TOKEN_COMMA:          return "','";
	case TOKEN_DOT:            return "'.'";
	case TOKEN_ELLIPSIS:       return "'...'";
	case TOKEN_EQUAL:          return "'='";
	case TOKEN_EQUAL_EQUAL:    return "'=='";
	case TOKEN_GREATER:        return "'>'";
	case TOKEN_GREATER_EQUAL:  return "'>='";
	case TOKEN_HASH:           return "'#'";
	case TOKEN_HASH_HASH:      return "'##'";
	case TOKEN_IDENT:          return "identifier";
	case TOKEN_LBRACE:         return "'{'";
	case TOKEN_LBRACKET:       return "'['";
	case TOKEN_LESS:           return "'<'";
	case TOKEN_LESS_EQUAL:     return "'<='";
	case TOKEN_LPAREN:         return "'('";
	case TOKEN_LSHIFT:         return "'<<'";
	case TOKEN_MINUS:          return "'-'";
	case TOKEN_MINUS_EQUAL:    return "'-='";
	case TOKEN_MINUS_MINUS:    return "'--'";
	case TOKEN_PERCENT:        return "'%'";
	case TOKEN_PERCENT_EQUAL:  return "'%='";
	case TOKEN_PLUS:           return "'+'";
	case TOKEN_PLUS_EQUAL:     return "'+='";
	case TOKEN_PLUS_PLUS:      return "'++'";
	case TOKEN_QMARK:          return "'?'";
	case TOKEN_RBRACE:         return "'}'";
	case TOKEN_RBRACKET:       return "']'";
	case TOKEN_RPAREN:         return "')'";
	case TOKEN_RSHIFT:         return "'>>'";
	case TOKEN_SEMICOLON:      return "';'";
	case TOKEN_SLASH:          return "'/'";
	case TOKEN_SLASH_EQUAL:    return "'/='";
	case TOKEN_STAR:           return "'*'";
	case TOKEN_STAR_EQUAL:     return "'*='";
	case TOKEN_TILDE:          return "'~'";
	case TOKEN_LITERAL_CHAR:   return "char";
	case TOKEN_LITERAL_FLOAT:  return "float";
	case TOKEN_LITERAL_INT:    return "integer";
	case TOKEN_LITERAL_STRING: return "string";
	case TOKEN_AUTO:           return "'auto'";
	case TOKEN_BREAK:          return "'break'";
	case TOKEN_CASE:           return "'case'";
	case TOKEN_CHAR:           return "'char'";
	case TOKEN_CONST:          return "'const'";
	case TOKEN_CONTINUE:       return "'continue'";
	case TOKEN_DEFAULT:        return "'default'";
	case TOKEN_DO:             return "'do'";
	case TOKEN_ELSE:           return "'else'";
	case TOKEN_ENUM:           return "'enum'";
	case TOKEN_EXTERN:         return "'extern'";
	case TOKEN_FLOAT:          return "'float'";
	case TOKEN_FOR:            return "'for'";
	case TOKEN_GOTO:           return "'goto'";
	case TOKEN_IF:             return "'if'";
	case TOKEN_INT:            return "'int'";
	case TOKEN_LONG:           return "'long'";
	case TOKEN_REGISTER:       return "'register'";
	case TOKEN_RESTRICT:       return "'restrict'";
	case TOKEN_RETURN:         return "'return'";
	case TOKEN_SHORT:          return "'short'";
	case TOKEN_SIGNED:         return "'signed'";
	case TOKEN_STATIC:         return "'static'";
	case TOKEN_STRUCT:         return "'struct'";
	case TOKEN_SWITCH:         return "'switch'";
	case TOKEN_THREAD_LOCAL:   return "'_Thread_local'";
	case TOKEN_TYPEDEF:        return "'typedef'";
	case TOKEN_UNSIGNED:       return "'unsigned'";
	case TOKEN_VOID:           return "'void'";
	case TOKEN_VOLATILE:       return "'volatile'";
	case TOKEN_WHILE:          return "'while'";
	case TOKEN_COUNT:          return "invalid token";
	}

	return "invalid token";
}
