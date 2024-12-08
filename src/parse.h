typedef enum {
	AST_INVALID,         // {}
	AST_NONE,            // {}
	AST_BUILTIN,         // {}
	AST_DECL,            // {type, expr?}
	AST_ENUMERATOR,      // {expr?}
	AST_EXTERN_DEF,      // {type, expr?}
	AST_INIT,            // {(expr|init)}

	// expressions
	AST_EXPR_BINARY,     // {expr, expr}
	AST_EXPR_CALL,       // {expr, expr*}
	AST_EXPR_CAST,       // {type, expr}
	AST_EXPR_COMPOUND,   // {type, (expr|init)*}
	AST_EXPR_IDENT,      // {decl} IMPORTANT: Can be cyclic!
	AST_EXPR_LITERAL,    // {}
	AST_EXPR_MEMBER,     // {expr}
	AST_EXPR_MEMBER_PTR, // {expr}
	AST_EXPR_POSTFIX,    // {expr}
	AST_EXPR_SIZEOF,     // {(expr|type)}
	AST_EXPR_TERNARY,    // {expr, expr, expr}
	AST_EXPR_UNARY,      // {expr}

	// statements
	AST_STMT_ASM,        // {}
	AST_STMT_BREAK,      // {}
	AST_STMT_CASE,       // {expr, stmt}
	AST_STMT_COMPOUND,   // {stmt*}
	AST_STMT_CONTINUE,   // {}
	AST_STMT_DECL,
	AST_STMT_DEFAULT,    // {stmt}
	AST_STMT_DO_WHILE,   // {expr, stmt}
	AST_STMT_FOR,        // {(expr|decl)?, expr?, expr?, stmt}
	AST_STMT_GOTO,       // {}
	AST_STMT_IF,         // {expr, stmt, stmt?}
	AST_STMT_LABEL,      // {stmt}
	AST_STMT_RETURN,     // {expr?}
	AST_STMT_SWITCH,     // {expr, stmt}
	AST_STMT_WHILE,      // {expr, stmt}

	// types
	AST_TYPE_BASIC,      // {}
	AST_TYPE_ARRAY,      // {type, expr?}
	AST_TYPE_BITFIELD,   // {type, expr?}
	AST_TYPE_ENUM,       // {enumerator*}
	AST_TYPE_FUNC,       // {type, decl*}
	AST_TYPE_IDENT,      // {decl}
	AST_TYPE_POINTER,    // {type}
	AST_TYPE_STRUCT,
	AST_TYPE_UNION,
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

typedef struct {
	ast_node_kind kind;
	ast_node_flags flags;
	token token;
	ast_id next;
	ast_id children;
} ast_node;

typedef struct ast_pool {
	ast_node *nodes;
	ast_id root;
	isize cap;
	isize size;
	b32 error;
} ast_pool;

static const ast_id ast_id_nil = {0};

typedef struct parse_scope_entry parse_scope_entry;
struct parse_scope_entry {
	str name;
	b32 is_type;
	parse_scope_entry *next;
};

typedef struct parse_scope parse_scope;
struct parse_scope {
	parse_scope *parent;
	parse_scope_entry *entries;
};

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

static ast_node
get_node(ast_pool *p, ast_id id)
{
	ast_node result = {0};

	if (0 < id.value && id.value < p->size) {
		result = p->nodes[id.value];
	} else {
		ASSERT(!"Out of bounds");
	}

	return result;
}

static i32
get_children(ast_pool *p, ast_id id, ast_id *children, i32 max_count)
{
	i32 count = 0;
	ast_node node = get_node(p, id);

	ast_id child_id = node.children;
	for (count = 0; child_id.value != 0 && count < max_count; count++) {
		ast_node child = get_node(p, child_id);
		children[count] = child_id;
		child_id = child.next;
	}

	return count;
}
