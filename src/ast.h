#define AST_NIL ((ast_node *)&ast_nil)

typedef struct ast_node ast_node;
typedef struct type type;

typedef enum {
	AST_INVALID,
	AST_LIST,
	AST_EXTERN_DEF,    // {type, name}
	AST_ENUMERATOR,    // {value, next}
	AST_INIT,          // {first, last}
	AST_DECL,          // {type, name}

	// expressions
	AST_EXPR_BINARY,   // {lhs, rhs}
	AST_EXPR_CALL,     // {called: expr, params: expr_list}
	AST_EXPR_CAST,     // {type, expr}
	AST_EXPR_COMPOUND, // {type, expr}
	AST_EXPR_IDENT,    // value.s
	AST_EXPR_MEMBER,   // {operand}
	AST_EXPR_MEMBER_PTR, // {operand}
	AST_EXPR_POSTFIX,  // {operand}
	AST_EXPR_SIZEOF,
	AST_EXPR_TERNARY1, // {expr, ternary2}
	AST_EXPR_TERNARY2, // {expr, expr}
	AST_EXPR_UNARY,    // {operand}
	AST_EXPR_LITERAL,

	// statements
	AST_STMT_BREAK,    // {}
	AST_STMT_CASE,     // {expr, stmt}
	AST_STMT_CONTINUE, // {}
	AST_STMT_DEFAULT,  // {stmt}
	AST_STMT_DO_WHILE, // {cond, body}
	AST_STMT_EMPTY,    // {}
	AST_STMT_GOTO,     // {}
	AST_STMT_FOR1,     // {init, for2}
	AST_STMT_FOR2,     // {cond, for3}
	AST_STMT_FOR3,     // {post, body}
	AST_STMT_IF1,      // {cond, if2}
	AST_STMT_IF2,      // {if, else?}
	AST_STMT_LABEL,    // {stmt}
	AST_STMT_SWITCH,   // {expr, stmt}
	AST_STMT_RETURN,   // {expr?}
	AST_STMT_WHILE,    // {cond, body}

	// types
	AST_TYPE_ARRAY,    // {size_expr, type}
	AST_TYPE_BITFIELD, // {expr, type}
	AST_TYPE_CHAR,     // {}
	AST_TYPE_ENUM,     // {...enumerators}
	AST_TYPE_FLOAT,    // {}
	AST_TYPE_FUNC,     // {param_list, return_type}
	AST_TYPE_IDENT,
	AST_TYPE_INT,      // {}
	AST_TYPE_POINTER,  // {_, type}
	AST_TYPE_STRUCT,   // {...declarations}
	AST_TYPE_UNION,    // {...declarations}
	AST_TYPE_VOID,     // {}
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
	AST_VARIADIC     = 1 << 13,
	AST_VOLATILE     = 1 << 14,
	AST_OPAQUE       = 1 << 15,
} ast_node_flags;

typedef struct {
	ast_id first, last;
} ast_list;

struct ast_node {
	ast_node_kind kind;
	ast_node_flags flags;
	token token;
	ast_id child[2];
};

typedef struct {
	ast_node *nodes;
	type *types;
	ast_id root;
	isize cap;
	isize size;
	b32 error;
} ast_pool;

static const ast_node ast_nil = {0};
static const ast_id ast_id_nil = {0};

typedef enum {
	PREC_NONE    =  0,
	PREC_COMMA   =  2,
	PREC_ASSIGN  =  4,
	PREC_TERNARY =  6,
	PREC_LOR     =  8,
	PREC_LAND    = 10,
	PREC_BOR     = 12,
	PREC_XOR     = 14,
	PREC_BAND    = 16,
	PREC_EQUAL   = 18,
	PREC_COMPARE = 20,
	PREC_SHIFT   = 22,
	PREC_TERM    = 24,
	PREC_FACTOR  = 26,
	PREC_PRIMARY = 28,
} precedence;

/* NOTE: An operator with negative precedence is right associative. */
static precedence
get_precedence(token_kind token)
{
	switch (token) {
	case TOKEN_COMMA:
		return PREC_COMMA;
	case TOKEN_QMARK:
		return PREC_TERNARY;
	case TOKEN_EQUAL:
	case TOKEN_PLUS_EQUAL:
	case TOKEN_MINUS_EQUAL:
	case TOKEN_STAR_EQUAL:
	case TOKEN_SLASH_EQUAL:
	case TOKEN_PERCENT_EQUAL:
	case TOKEN_AMP_EQUAL:
	case TOKEN_BAR_EQUAL:
	case TOKEN_CARET_EQUAL:
		return PREC_ASSIGN;
	case TOKEN_BAR_BAR:
		return PREC_LOR;
	case TOKEN_AMP_AMP:
		return PREC_LAND;
	case TOKEN_BAR:
		return PREC_BOR;
	case TOKEN_CARET:
		return PREC_XOR;
	case TOKEN_AMP:
		return PREC_BAND;
	case TOKEN_LSHIFT:
	case TOKEN_RSHIFT:
		return PREC_SHIFT;
	case TOKEN_PLUS:
	case TOKEN_MINUS:
		return PREC_TERM;
	case TOKEN_STAR:
	case TOKEN_SLASH:
	case TOKEN_PERCENT:
		return PREC_FACTOR;
	case TOKEN_EQUAL_EQUAL:
	case TOKEN_BANG_EQUAL:
		return PREC_EQUAL;
	case TOKEN_LESS:
	case TOKEN_GREATER:
	case TOKEN_LESS_EQUAL:
	case TOKEN_GREATER_EQUAL:
		return PREC_COMPARE;
	case TOKEN_LBRACKET:
	case TOKEN_DOT:
	case TOKEN_PLUS_PLUS:
	case TOKEN_MINUS_MINUS:
		return PREC_PRIMARY;
	default:
		return PREC_NONE;
	}
}

static ast_node *
get_node(ast_pool *p, ast_id id)
{
	if (0 < id.value && id.value < p->size) {
		return p->nodes + id.value;
	}

	ASSERT(!"ID is out of bounds");
	return NULL;
}

static ast_node *
get_node_of_kind(ast_pool *p, ast_id id, ast_node_kind kind)
{
	ast_node *node = get_node(p, id);
	ASSERT(node->kind == kind);
	return node;
}
