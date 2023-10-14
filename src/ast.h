struct ast_expr_bin {
	enum token_kind op;
	struct ast_node *lhs;
	struct ast_node *rhs;
};

struct ast_expr_call {
	struct ast_node *called;
	struct ast_node *params;
};

struct ast_decl {
	struct string name;
	struct ast_node *expr;
	struct ast_node *type;
};

struct ast_stmt_if {
	struct ast_node *cond;
	struct ast_node *then;
	struct ast_node *otherwise;
};

struct ast_stmt_while {
	struct ast_node *cond;
	struct ast_node *body;
};

struct ast_stmt_for {
	struct ast_node *init;
	struct ast_node *cond;
	struct ast_node *post;
	struct ast_node *body;
};

struct ast_function {
	struct string name;
	struct ast_node *params;
	struct ast_node *body;
	struct ast_node *return_type;
};

enum ast_node_kind {
	AST_INVALID,
	AST_DECL,
	AST_FUNCTION,
	AST_ROOT,

	AST_EXPR_BINARY,
	AST_EXPR_CALL,
	AST_EXPR_IDENT,
	AST_EXPR_INT,

	AST_STMT_BREAK,
	AST_STMT_COMPOUND,
	AST_STMT_CONTINUE,
	AST_STMT_DECL,
	AST_STMT_EMPTY,
	AST_STMT_FOR,
	AST_STMT_IF,
	AST_STMT_PRINT,
	AST_STMT_RETURN,
	AST_STMT_WHILE,

	AST_TYPE_CHAR,
	AST_TYPE_INT,
	AST_TYPE_VOID,
};

struct ast_node {
	enum ast_node_kind kind;
	struct ast_node *next;
	struct location loc;

	union {
		struct ast_stmt_for for_stmt;
		struct ast_stmt_if if_stmt;
		struct ast_stmt_while while_stmt;
		struct ast_expr_bin bin_expr;
		struct ast_expr_call call_expr;
		struct ast_function function;
		struct ast_decl decl;
		struct ast_node *children;
		struct string ident;
		intmax_t ival;
	} u;
};

static char *
get_ast_name(enum ast_node_kind kind)
{
	switch (kind) {
	case AST_INVALID:       return "INVALID";
	case AST_ROOT:          return "ROOT";
	case AST_DECL:          return "DECL";
	case AST_FUNCTION:      return "FUNCTION";
	case AST_TYPE_VOID:     return "VOID";
	case AST_TYPE_CHAR:     return "CHAR";
	case AST_TYPE_INT:      return "INT";
	case AST_EXPR_BINARY:   return "BINARY";
	case AST_EXPR_CALL:     return "CALL";
	case AST_EXPR_IDENT:    return "IDENT";
	case AST_EXPR_INT:      return "EXPR_INT";
	case AST_STMT_BREAK:    return "BREAK";
	case AST_STMT_COMPOUND: return "COMPOUND";
	case AST_STMT_CONTINUE: return "CONTINUE";
	case AST_STMT_DECL:     return "DECL_STMT";
	case AST_STMT_EMPTY:    return "EMPTY";
	case AST_STMT_FOR:      return "FOR";
	case AST_STMT_IF:       return "IF";
	case AST_STMT_PRINT:    return "PRINT";
	case AST_STMT_WHILE:    return "WHILE";
	case AST_STMT_RETURN:   return "RETURN";
	}

	return "(invalid)";
}
