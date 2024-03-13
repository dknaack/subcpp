#define AST_NIL ((ast_node *)&ast_nil)

typedef struct ast_node ast_node;
typedef struct type type;

typedef enum {
	AST_INVALID,
	AST_EXTERN_DEF,    // {type_specifier, ...declarators}
	AST_INIT_LIST,     // {expr, list}
	AST_DECL_LIST,     // {decl, decl_list}
	AST_DECL,          // {type, value?}

	// expressions
	AST_EXPR_BINARY,   // {lhs, rhs}
	AST_EXPR_CALL,     // {called: expr, params: expr_list}
	AST_EXPR_IDENT,    // value.s
	AST_EXPR_INT,      // value.i
	AST_EXPR_LIST,     // {expr, list}
	AST_EXPR_MEMBER,   // {operand}
	AST_EXPR_POSTFIX,  // {operand}
	AST_EXPR_UNARY,    // {operand}

	// statements
	AST_STMT_BREAK,    // {}
	AST_STMT_CASE,     // {expr, stmt}
	AST_STMT_CONTINUE, // {}
	AST_STMT_DEFAULT,  // {stmt}
	AST_STMT_DO_WHILE, // {cond, body}
	AST_STMT_EMPTY,    // {}
	AST_STMT_FOR_INIT, // {init, cond}
	AST_STMT_FOR_COND, // {cond, post}
	AST_STMT_FOR_POST, // {post, body}
	AST_STMT_IF_COND,  // {cond, if_else}
	AST_STMT_IF_ELSE,  // {if, else?}
	AST_STMT_LIST,     // {stmt, stmt_list}
	AST_STMT_PRINT,    // {expr}
	AST_STMT_SWITCH,   // {expr, stmt}
	AST_STMT_RETURN,   // {expr?}
	AST_STMT_WHILE,    // {cond, body}

	// types
	AST_TYPE_ARRAY,      // {size_expr, type}
	AST_TYPE_CHAR,       // {}
	AST_TYPE_FLOAT,      // {}
	AST_TYPE_FUNC,       // {param_list, return_type}
	AST_TYPE_INT,        // {}
	AST_TYPE_POINTER,    // {type}
	AST_TYPE_STRUCT,     // {}
	AST_TYPE_STRUCT_DEF, // {...declarations}
	AST_TYPE_VOID,       // {}
} ast_node_kind;

typedef enum {
	AST_AUTO         = 1 <<  0,
	AST_CONST        = 1 <<  1,
	AST_EXTERN       = 1 <<  2,
	AST_LONG         = 1 <<  3,
	AST_LLONG        = 1 <<  4,
	AST_REGISTER     = 1 <<  5,
	AST_RESTRICT     = 1 <<  6,
	AST_SHORT        = 1 <<  7,
	AST_SIGNED       = 1 <<  8,
	AST_STATIC       = 1 <<  9,
	AST_THREAD_LOCAL = 1 << 10,
	AST_TYPEDEF      = 1 << 11,
	AST_UNSIGNED     = 1 << 12,
} ast_node_flags;

struct ast_node {
	ast_node_kind kind;
	ast_node_flags flags;
	ast_node *child[2];
	location loc;
	type *type;
	symbol_id symbol_id;

	union {
		token_kind op;
		intmax_t i;
		str s;
	} value;
};

static const ast_node ast_nil = {AST_INVALID, 0, {AST_NIL, AST_NIL}};

static b32
is_statement(ast_node_kind node_kind)
{
	switch (node_kind) {
	case AST_STMT_BREAK:
	case AST_STMT_CONTINUE:
	case AST_STMT_DO_WHILE:
	case AST_STMT_EMPTY:
	case AST_STMT_FOR_INIT:
	case AST_STMT_FOR_COND:
	case AST_STMT_FOR_POST:
	case AST_STMT_IF_COND:
	case AST_STMT_IF_ELSE:
	case AST_STMT_LIST:
	case AST_STMT_PRINT:
	case AST_STMT_RETURN:
	case AST_STMT_WHILE:
		return true;
	default:
		return false;
	}
}
