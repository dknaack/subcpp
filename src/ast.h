#define AST_NIL ((ast_node *)&ast_nil)

typedef struct ast_node ast_node;
typedef struct type type;

typedef struct {
	token_kind op;
	ast_node *lhs;
	ast_node *rhs;
} ast_expr_bin;

typedef struct {
	token_kind op;
	ast_node *operand;
} ast_expr_unary;

typedef struct {
	ast_node *called;
	ast_node *params;
} ast_expr_call;

typedef struct {
	ast_node *declarator;
	u32 qualifiers;
} ast_decl_pointer;

typedef struct {
	ast_node *declarator;
	ast_node *size;
	u32 qualifiers;
} ast_decl_array;

typedef struct {
	ast_node *declarator;
	ast_node *initializer;
} ast_decl_list;

typedef struct {
	ast_node *list;
	ast_node *type_specifier;
} ast_decl;

typedef struct {
	ast_node *cond;
	ast_node *then;
	ast_node *otherwise;
} ast_stmt_if;

typedef struct {
	ast_node *cond;
	ast_node *body;
} ast_stmt_while;

typedef struct {
	ast_node *init;
	ast_node *cond;
	ast_node *post;
	ast_node *body;
} ast_stmt_for;

typedef struct {
	string name;
	ast_node *params;
	ast_node *body;
	ast_node *return_type;
} ast_function;

typedef enum {
	AST_INVALID,
	AST_FUNCTION,
	AST_ROOT,

	AST_DECL,
	AST_DECL_LIST,
	AST_DECL_POINTER,
	AST_DECL_ARRAY,
	AST_DECL_IDENT,

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
	AST_TYPE_VOID,
	AST_TYPE_STRUCT,
	AST_TYPE_STRUCT_DEF,
	AST_TYPE_STRUCT_ANON,
} ast_node_kind;

struct ast_node {
	ast_node_kind kind;
	ast_node *next;
	ast_node *children;
	location loc;
	type *type;

	union {
		ast_function function;
		ast_decl decl;
		ast_decl_list decl_list;
		ast_decl_pointer decl_pointer;
		ast_decl_array decl_array;
		string ident;
		intmax_t ival;
	} u;

	union {
		intmax_t i;
	} value;
};

static ast_node ast_nil = {AST_INVALID, &ast_nil, &ast_nil};

static char *
get_ast_name(ast_node_kind kind)
{
	switch (kind) {
	case AST_INVALID:          return "INVALID";
	case AST_ROOT:             return "ROOT";
	case AST_DECL:             return "DECL";
	case AST_DECL_LIST:        return "DECL_LIST";
	case AST_DECL_POINTER:     return "DECL_POINTER";
	case AST_DECL_ARRAY:       return "DECL_ARRAY";
	case AST_DECL_IDENT:       return "DECL_IDENT";
	case AST_FUNCTION:         return "FUNCTION";
	case AST_TYPE_VOID:        return "VOID";
	case AST_TYPE_CHAR:        return "CHAR";
	case AST_TYPE_INT:         return "INT";
	case AST_TYPE_STRUCT_ANON: return "STRUCT_ANON";
	case AST_TYPE_STRUCT_DEF:  return "STRUCT_DEF";
	case AST_TYPE_STRUCT:      return "STRUCT";
	case AST_EXPR_BINARY:      return "BINARY";
	case AST_EXPR_CALL:        return "CALL";
	case AST_EXPR_IDENT:       return "IDENT";
	case AST_EXPR_INT:         return "EXPR_INT";
	case AST_EXPR_UNARY:       return "UNARY";
	case AST_STMT_BREAK:       return "BREAK";
	case AST_STMT_COMPOUND:    return "COMPOUND";
	case AST_STMT_CONTINUE:    return "CONTINUE";
	case AST_STMT_DECL:        return "DECL_STMT";
	case AST_STMT_EMPTY:       return "EMPTY";
	case AST_STMT_FOR:         return "FOR";
	case AST_STMT_IF:          return "IF";
	case AST_STMT_PRINT:       return "PRINT";
	case AST_STMT_WHILE:       return "WHILE";
	case AST_STMT_RETURN:      return "RETURN";
	}

	return "(invalid)";
}
