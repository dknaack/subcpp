#define AST_NIL ((ast_node *)&ast_nil)

typedef struct ast_node ast_node;
typedef struct type type;

typedef enum {
	AST_INVALID,
	AST_FUNCTION,      // {declaration, body}
	AST_ROOT,          // {...(declaration|function)}

	AST_DECL,          // {type_specifier, ...declarators}
	AST_DECL_INIT,     // {declarator, initializer}
	AST_DECL_POINTER,  // {declarator}
	AST_DECL_ARRAY,    // {declarator, size}
	AST_DECL_FUNC,     // {declarator, ...parameters}
	AST_DECL_IDENT,

	AST_EXPR_BINARY,   // {lhs, rhs}
	AST_EXPR_CALL,     // {called, ...params}
	AST_EXPR_IDENT,
	AST_EXPR_INT,
	AST_EXPR_UNARY,    // {operand}
	AST_EXPR_POSTFIX,  // {operand}

	AST_STMT_BREAK,
	AST_STMT_COMPOUND, // {...statements}
	AST_STMT_CONTINUE,
	AST_STMT_DO_WHILE, // {cond, body}
	AST_STMT_EMPTY,
	AST_STMT_FOR,      // {init, cond, post, body}
	AST_STMT_IF,       // {cond, if_branch, else_branch?}
	AST_STMT_PRINT,    // {expr}
	AST_STMT_RETURN,   // {expr?}
	AST_STMT_WHILE,    // {cond, body}

	AST_TYPE_CHAR,
	AST_TYPE_INT,
	AST_TYPE_VOID,
	AST_TYPE_STRUCT,
	AST_TYPE_STRUCT_DEF, // {...declarations}
} ast_node_kind;

typedef enum {
	AST_AUTO         = 1 <<  0,
	AST_CONST        = 1 <<  1,
	AST_LONG         = 1 <<  2,
	AST_LLONG        = 1 <<  3,
	AST_REGISTER     = 1 <<  4,
	AST_RESTRICT     = 1 <<  5,
	AST_SHORT        = 1 <<  6,
	AST_SIGNED       = 1 <<  7,
	AST_STATIC       = 1 <<  8,
	AST_THREAD_LOCAL = 1 <<  9,
	AST_TYPEDEF      = 1 << 10,
	AST_UNSIGNED     = 1 << 11,
} ast_node_flags;

struct ast_node {
	ast_node_kind kind;
	ast_node_flags flags;
	ast_node *next;
	ast_node *children;
	location loc;
	type *type;

	union {
		token_kind op;
		intmax_t i;
		str s;
	} value;
};

static const ast_node ast_nil = {AST_INVALID, 0, AST_NIL, AST_NIL};
