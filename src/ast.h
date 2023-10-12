struct ast_bin_expr {
	enum token_kind op;
	struct ast_node *lhs;
	struct ast_node *rhs;
};

struct ast_call_expr {
	struct ast_node *called;
	struct ast_node *parameters;
};

struct ast_decl {
	struct string name;
	struct ast_node *expr;
};

struct ast_if_stmt {
	struct ast_node *cond;
	struct ast_node *then;
	struct ast_node *otherwise;
};

struct ast_while_stmt {
	struct ast_node *cond;
	struct ast_node *body;
};

struct ast_for_stmt {
	struct ast_node *init;
	struct ast_node *cond;
	struct ast_node *post;
	struct ast_node *body;
};

struct ast_function {
	struct string name;
	struct ast_node *parameters;
	struct ast_node *body;
};

enum ast_node_kind {
	AST_INVALID,
	AST_ROOT,
	AST_DECL,
	AST_FUNCTION,

	/* NOTE: expressions */
	AST_BINARY,
	AST_CALL,
	AST_IDENT,
	AST_INT,

	/* NOTE: statements */
	AST_BREAK,
	AST_COMPOUND,
	AST_CONTINUE,
	AST_DECL_STMT,
	AST_EMPTY,
	AST_FOR,
	AST_IF,
	AST_PRINT,
	AST_WHILE,
	AST_RETURN,
};

struct ast_node {
	enum ast_node_kind kind;
	struct ast_node *next;

	union {
		struct ast_for_stmt for_stmt;
		struct ast_if_stmt if_stmt;
		struct ast_while_stmt while_stmt;
		struct ast_bin_expr bin_expr;
		struct ast_call_expr call_expr;
		struct ast_function function;
		struct ast_decl decl;
		struct ast_node *children;
		struct string ident;
		intmax_t ival;
	} u;
};
