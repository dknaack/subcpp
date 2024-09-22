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
	TOKEN_BANG_EQUAL,
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
	TOKEN_ASM,
	TOKEN_AUTO,
	TOKEN_BREAK,
	TOKEN_BOOL,
	TOKEN_CASE,
	TOKEN_CHAR,
	TOKEN_CONST,
	TOKEN_CONTINUE,
	TOKEN_DEFAULT,
	TOKEN_DO,
	TOKEN_DOUBLE,
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
	TOKEN_SIZEOF,
	TOKEN_STATIC,
	TOKEN_STRUCT,
	TOKEN_SWITCH,
	TOKEN_THREAD_LOCAL,
	TOKEN_TYPEDEF,
	TOKEN_UNION,
	TOKEN_UNSIGNED,
	TOKEN_VOID,
	TOKEN_VOLATILE,
	TOKEN_WHILE,

	// builtins
	TOKEN_BUILTIN_VA_LIST,
	TOKEN_BUILTIN_VA_ARG,
	TOKEN_BUILTIN_VA_START,
	TOKEN_BUILTIN_VA_END,

	TOKEN_COUNT
} token_kind;

typedef struct {
	i32 value;
} file_id;

typedef struct {
	file_id file;
	isize offset;
	char *filename;
} location;

typedef struct {
	token_kind kind;
	str value;
	location loc;
} token;

typedef struct token_list token_list;
struct token_list {
	token_list *next;
	b32 was_expanded;
	token token;
};

typedef struct str_list str_list;
struct str_list {
	str value;
	str_list *next;
};

typedef struct macro macro;
struct macro {
	macro *next;

	str name;
	token_list *tokens;
	i32 param_count;
	str_list *params;
	location loc;
};

typedef struct {
	char *filename;
} lexer;

typedef struct file file;
struct file {
	char *name;
	file_id id;
	str contents;
	file *next;
	file *prev;
};

typedef enum {
	DIR_OTHER,
	DIR_INCLUDE,
	DIR_IF_FALSE,
	DIR_IF_TRUE,
	DIR_ELIF_FALSE,
	DIR_ELIF_TRUE,
	DIR_ELSE,

	DIR_TRUE = 1,
} directive;

typedef struct lexer_state lexer_state;
struct lexer_state {
	str data;
	isize pos;
	char at[4];
	b8 ignore_token;
	char *filename;
	file_id file;
	directive last_directive;
	lexer_state *prev;
};

typedef struct parse_context parse_context;
struct parse_context {
	lexer_state *lexer;
	token peek[2];
	b8 error;

	/* internal */
	arena *arena;
	token_list *tokens;
	macro *macros;
	file *files_head;
	file *files_tail;
	i32 file_count;
};

static location
get_location(parse_context *ctx)
{
	location loc = {0};
	loc.file = ctx->lexer->file;
	loc.offset = ctx->lexer->pos;
	return loc;
}

static char *
get_token_name(token_kind kind)
{
	switch (kind) {
	case TOKEN_INVALID:
		return "invalid token";
	case TOKEN_EOF:
		return "eof";
	case TOKEN_NEWLINE:
		return "newline";
	case TOKEN_COMMENT:
		return "comment";
	case TOKEN_WHITESPACE:
		return "whitespace";
	case TOKEN_AMP:
		return "'&'";
	case TOKEN_AMP_AMP:
		return "'&&'";
	case TOKEN_AMP_EQUAL:
		return "'&='";
	case TOKEN_ARROW:
		return "'->'";
	case TOKEN_BACKSLASH:
		return "'\\'";
	case TOKEN_BANG:
		return "'!'";
	case TOKEN_BANG_EQUAL:
		return "'!='";
	case TOKEN_BAR:
		return "'|'";
	case TOKEN_BAR_BAR:
		return "'||'";
	case TOKEN_BAR_EQUAL:
		return "'|='";
	case TOKEN_CARET:
		return "'^'";
	case TOKEN_CARET_EQUAL:
		return "'^='";
	case TOKEN_COLON:
		return "': '";
	case TOKEN_COMMA:
		return "','";
	case TOKEN_DOT:
		return "'.'";
	case TOKEN_ELLIPSIS:
		return "'...'";
	case TOKEN_EQUAL:
		return "'='";
	case TOKEN_EQUAL_EQUAL:
		return "'=='";
	case TOKEN_GREATER:
		return "'>'";
	case TOKEN_GREATER_EQUAL:
		return "'>='";
	case TOKEN_HASH:
		return "'#'";
	case TOKEN_HASH_HASH:
		return "'##'";
	case TOKEN_IDENT:
		return "identifier";
	case TOKEN_LBRACE:
		return "'{'";
	case TOKEN_LBRACKET:
		return "'['";
	case TOKEN_LESS:
		return "'<'";
	case TOKEN_LESS_EQUAL:
		return "'<='";
	case TOKEN_LPAREN:
		return "'('";
	case TOKEN_LSHIFT:
		return "'<<'";
	case TOKEN_MINUS:
		return "'-'";
	case TOKEN_MINUS_EQUAL:
		return "'-='";
	case TOKEN_MINUS_MINUS:
		return "'--'";
	case TOKEN_PERCENT:
		return "'%'";
	case TOKEN_PERCENT_EQUAL:
		return "'%='";
	case TOKEN_PLUS:
		return "'+'";
	case TOKEN_PLUS_EQUAL:
		return "'+='";
	case TOKEN_PLUS_PLUS:
		return "'++'";
	case TOKEN_QMARK:
		return "'?'";
	case TOKEN_RBRACE:
		return "'}'";
	case TOKEN_RBRACKET:
		return "']'";
	case TOKEN_RPAREN:
		return "')'";
	case TOKEN_RSHIFT:
		return "'>>'";
	case TOKEN_SEMICOLON:
		return "';'";
	case TOKEN_SLASH:
		return "'/'";
	case TOKEN_SLASH_EQUAL:
		return "'/='";
	case TOKEN_STAR:
		return "'*'";
	case TOKEN_STAR_EQUAL:
		return "'*='";
	case TOKEN_TILDE:
		return "'~'";
	case TOKEN_LITERAL_CHAR:
		return "char";
	case TOKEN_LITERAL_FLOAT:
		return "float";
	case TOKEN_LITERAL_INT:
		return "integer";
	case TOKEN_LITERAL_STRING:
		return "string";
	case TOKEN_ASM:
		return "'asm'";
	case TOKEN_AUTO:
		return "'auto'";
	case TOKEN_BREAK:
		return "'break'";
	case TOKEN_BOOL:
		return "'_Bool'";
	case TOKEN_CASE:
		return "'case'";
	case TOKEN_CHAR:
		return "'char'";
	case TOKEN_CONST:
		return "'const'";
	case TOKEN_CONTINUE:
		return "'continue'";
	case TOKEN_DEFAULT:
		return "'default'";
	case TOKEN_DO:
		return "'do'";
	case TOKEN_DOUBLE:
		return "'double'";
	case TOKEN_ELSE:
		return "'else'";
	case TOKEN_ENUM:
		return "'enum'";
	case TOKEN_EXTERN:
		return "'extern'";
	case TOKEN_FLOAT:
		return "'float'";
	case TOKEN_FOR:
		return "'for'";
	case TOKEN_GOTO:
		return "'goto'";
	case TOKEN_IF:
		return "'if'";
	case TOKEN_INT:
		return "'int'";
	case TOKEN_LONG:
		return "'long'";
	case TOKEN_REGISTER:
		return "'register'";
	case TOKEN_RESTRICT:
		return "'restrict'";
	case TOKEN_RETURN:
		return "'return'";
	case TOKEN_SHORT:
		return "'short'";
	case TOKEN_SIGNED:
		return "'signed'";
	case TOKEN_SIZEOF:
		return "'sizeof'";
	case TOKEN_STATIC:
		return "'static'";
	case TOKEN_STRUCT:
		return "'struct'";
	case TOKEN_SWITCH:
		return "'switch'";
	case TOKEN_THREAD_LOCAL:
		return "'_Thread_local'";
	case TOKEN_TYPEDEF:
		return "'typedef'";
	case TOKEN_UNION:
		return "'union'";
	case TOKEN_UNSIGNED:
		return "'unsigned'";
	case TOKEN_VOID:
		return "'void'";
	case TOKEN_VOLATILE:
		return "'volatile'";
	case TOKEN_WHILE:
		return "'while'";
	case TOKEN_BUILTIN_VA_ARG:
		return "'__builtin_va_arg'";
	case TOKEN_BUILTIN_VA_END:
		return "'__builtin_va_end'";
	case TOKEN_BUILTIN_VA_LIST:
		return "'__builtin_va_list'";
	case TOKEN_BUILTIN_VA_START:
		return "'__builtin_va_start'";
	case TOKEN_COUNT:
		return "invalid token";
	}

	return "invalid token";
}
