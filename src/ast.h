typedef struct ast_node ast_node;

typedef struct ast_expr_bin {
	token_kind op;
	ast_node *lhs;
	ast_node *rhs;
} ast_expr_bin;

typedef struct ast_expr_unary {
	token_kind op;
	ast_node *operand;
} ast_expr_unary;

typedef struct ast_expr_call {
	ast_node *called;
	ast_node *params;
} ast_expr_call;

typedef struct ast_decl {
	string name;
	ast_node *expr;
	ast_node *type;
} ast_decl;

typedef struct ast_stmt_if {
	ast_node *cond;
	ast_node *then;
	ast_node *otherwise;
} ast_stmt_if;

typedef struct ast_stmt_while {
	ast_node *cond;
	ast_node *body;
} ast_stmt_while;

typedef struct ast_stmt_for {
	ast_node *init;
	ast_node *cond;
	ast_node *post;
	ast_node *body;
} ast_stmt_for;

typedef struct ast_type_pointer {
	ast_node *target;
} ast_type_pointer;

typedef struct ast_function {
	string name;
	ast_node *params;
	ast_node *body;
	ast_node *return_type;
} ast_function;

typedef enum ast_node_kind {
	AST_INVALID,
	AST_DECL,
	AST_FUNCTION,
	AST_ROOT,

	AST_EXPR_BINARY,
	AST_EXPR_CALL,
	AST_EXPR_IDENT,
	AST_EXPR_INT,
	AST_EXPR_UNARY,

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
	AST_TYPE_POINTER,
	AST_TYPE_VOID,
} ast_node_kind;

struct ast_node {
	ast_node_kind kind;
	ast_node *next;
	location loc;
	struct type *type;

	union {
		ast_stmt_for for_stmt;
		ast_stmt_if if_stmt;
		ast_stmt_while while_stmt;
		ast_expr_bin bin_expr;
		ast_expr_call call_expr;
		ast_expr_unary unary_expr;
		ast_type_pointer pointer_type;
		ast_function function;
		ast_decl decl;
		ast_node *children;
		string ident;
		intmax_t ival;
	} u;
};

static char *
get_ast_name(ast_node_kind kind)
{
	switch (kind) {
	case AST_INVALID:       return "INVALID";
	case AST_ROOT:          return "ROOT";
	case AST_DECL:          return "DECL";
	case AST_FUNCTION:      return "FUNCTION";
	case AST_TYPE_VOID:     return "VOID";
	case AST_TYPE_CHAR:     return "CHAR";
	case AST_TYPE_INT:      return "INT";
	case AST_TYPE_POINTER:  return "POINTER";
	case AST_EXPR_BINARY:   return "BINARY";
	case AST_EXPR_CALL:     return "CALL";
	case AST_EXPR_IDENT:    return "IDENT";
	case AST_EXPR_INT:      return "EXPR_INT";
	case AST_EXPR_UNARY:    return "UNARY";
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
