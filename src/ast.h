#define AST_NIL ((ast_node *)&ast_nil)

typedef struct ast_node ast_node;
typedef struct type type;

typedef enum {
	AST_INVALID,
	AST_FUNCTION,      // {body, ...params}
	AST_ROOT,          // {...(declaration|function)}

	AST_DECL,          // {type_specifier, ...declarators}
	AST_DECL_INIT,     // {declarator, initializer?}
	AST_DECL_POINTER,  // {declarator}
	AST_DECL_ARRAY,    // {declarator, size}
	AST_DECL_FUNC,     // {declarator, ...parameters}
	AST_DECL_IDENT,

	AST_EXPR_BINARY,   // {lhs, rhs}
	AST_EXPR_CALL,     // {called, ...params}
	AST_EXPR_IDENT,
	AST_EXPR_INT,
	AST_EXPR_UNARY,    //  {operand}

	AST_STMT_BREAK,
	AST_STMT_COMPOUND, // {...statements}
	AST_STMT_CONTINUE,
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

struct ast_node {
	ast_node_kind kind;
	ast_node *next;
	ast_node *children;
	location loc;
	type *type;

	union {
		intmax_t i;
		str s;
	} value;
};

static const ast_node ast_nil = {AST_INVALID, AST_NIL, AST_NIL};
