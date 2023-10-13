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
	struct ast_node *type;
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

	/* NOTE: types */
	AST_VOID,
	AST_CHAR,
	AST_INT,

	/* NOTE: expressions */
	AST_BINARY,
	AST_CALL,
	AST_IDENT,
	AST_LITERAL_INT,

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
	struct location loc;

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

static char *
get_ast_name(enum ast_node_kind kind)
{
	switch (kind) {
	case AST_INVALID:     return "INVALID";
	case AST_ROOT:        return "ROOT";
	case AST_DECL:        return "DECL";
	case AST_FUNCTION:    return "FUNCTION";
	case AST_VOID:        return "VOID";
	case AST_CHAR:        return "CHAR";
	case AST_INT:         return "INT";
	case AST_BINARY:      return "BINARY";
	case AST_CALL:        return "CALL";
	case AST_IDENT:       return "IDENT";
	case AST_LITERAL_INT: return "LITERAL_INT";
	case AST_BREAK:       return "BREAK";
	case AST_COMPOUND:    return "COMPOUND";
	case AST_CONTINUE:    return "CONTINUE";
	case AST_DECL_STMT:   return "DECL_STMT";
	case AST_EMPTY:       return "EMPTY";
	case AST_FOR:         return "FOR";
	case AST_IF:          return "IF";
	case AST_PRINT:       return "PRINT";
	case AST_WHILE:       return "WHILE";
	case AST_RETURN:      return "RETURN";
	}

	return "(invalid)";
}
