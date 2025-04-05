typedef enum {
	// Represents a node that is not valid.
	AST_INVALID,

	// An empty node, used as a placeholder in for statements, for example.
	AST_NONE,

	// A builtin function, like `printf` or `malloc`.
	AST_BUILTIN,

	// A declaration of a variable inside a function. The node contains at most
	// two children. The first child is the type, the second (optional) child
	// is the initializer. The identifier is stored in the token of the node.
	AST_DECL,

	// An external declaration, like a function prototype or a global variable.
	// The node contains at most two children. The first child is the type,
	// the second child contains the initializer or the function body.
	AST_EXTERN_DEF,

	// An enumerator in an enum declaration. The first child is the value of the
	// enumerator, the identifier is stored in the token of the node.
	AST_ENUMERATOR,

	// An initializer list for a variable or a struct, consisting of a list of
	// expressions or nested initializer lists.
	AST_INIT,

	//
	// Expessions
	//

	// A binary expression, like `+` or `==`. It contains two expressions for
	// the left and right operands. The operator is stored in the token of the
	// node.
	AST_EXPR_BINARY,

	// A call to a function. The first child is the function expression, the
	// following children are expressions for the arguments.
	AST_EXPR_CALL,

	// A cast expression, like `(int) 3`. The first child is the type, the
	// second child is the expression to cast.
	AST_EXPR_CAST,

	// A compound expression, like `{1, 2, 3}` or `{.x = 1, .y = 2}`. This node
	// contains exactly two children. The first child is the type, the second
	// must be an initializer list.
	AST_EXPR_COMPOUND,

	// An identifier, like `x` or `foo`. The identifier is stored in the token.
	AST_EXPR_IDENT,

	// A literal, like `42` or `3.14`. The value is stored in the token.
	// Different kinds of literals are distinguished by the token kind.
	AST_EXPR_LITERAL,

	// A member access expression, like `foo.bar`. The first child is the
	// expression, the identifier for the member is stored in the token.
	AST_EXPR_MEMBER,
	AST_EXPR_MEMBER_PTR,

	// A postfix expression, like `foo++` or `bar--`. The first child is the
	// expression, the operator is stored in the token.
	AST_EXPR_POSTFIX,

	// A sizeof expression, like `sizeof(int)` or `sizeof foo`. The first child
	// is the expression or type to get the size of.
	AST_EXPR_SIZEOF,

	// A ternary expression, like `a ? b : c`. The first child is the condition,
	// the second child is the expression if the condition is true, the third
	// child is the expression if the condition is false.
	AST_EXPR_TERNARY,

	// A unary expression, like `!a` or `++b`. The first child is the expression,
	// the operator is stored in the token.
	AST_EXPR_UNARY,

	//
	// Statements
	//

	// An assembly statement, like `asm(...)`. The first child must be a string
	// literal with the assembly code.
	AST_STMT_ASM,

	// A break statement. This node has no children.
	AST_STMT_BREAK,

	// A case statement in a switch statement. The first child is the expression
	// to compare the switch expression to, the second child is the statement.
	AST_STMT_CASE,

	// A compound statement, like `{a; b;}`. This node contains a list of
	// statements as children.
	AST_STMT_COMPOUND,

	// A continue statement. This node has no children.
	AST_STMT_CONTINUE,

	// A declaration statement, like `int x = 42, y = 3;`. This node contains a
	// list of declarations as children.
	AST_STMT_DECL,

	// A default statement in a switch statement. The first child is the statement.
	AST_STMT_DEFAULT,

	// A do-while statement. The first child is the expression, the second child
	// is the statement.
	AST_STMT_DO_WHILE,

	// A for statement. The children are the initialization, the condition, the
	// increment and the statement. Each of these might be an empty node,
	// except for the statement.
	AST_STMT_FOR,        // {(expr|decl)?, expr?, expr?, stmt}

	// A goto statement. The label is stored in the token.
	AST_STMT_GOTO,

	// An if statement. The children are the condition, the statement and the
	// optional else statement.
	AST_STMT_IF,

	// A labeled statement, like `foo: bar;`. The first child is the statement,
	// the label is stored in the token.
	AST_STMT_LABEL,

	// A return statement. The first optional child is the expression to return.
	AST_STMT_RETURN,

	// A switch statement. The children are the expression and the statement.
	AST_STMT_SWITCH,

	// A while statement. The children are the expression and the statement.
	AST_STMT_WHILE,

	//
	// Types
	//

	// A basic type, like `int` or `void`. The type is stored in the token.
	AST_TYPE_BASIC,

	// An array type, like `int[10]` or `char[]`. The first child is the type,
	// the second child is an expression for the size of the array.
	AST_TYPE_ARRAY,

	// A bitfield type, like `int:10`. The first child is the type, the second
	// child is an expression for the size of the bitfield.
	AST_TYPE_BITFIELD,

	// An enum type, like `enum { ... }`. It contains a list of enumerators.
	AST_TYPE_ENUM,

	// A function type, like `int (*)(int, int)`. The first child is the return
	// type, the following children are the parameter types stored as
	// declarations.
	AST_TYPE_FUNC,

	// An identifier for a type, like in a typedef. The identifier is stored in
	// the token.
	AST_TYPE_IDENT,

	// A pointer type, like `int *` or `void **`. The only child is the base type.
	AST_TYPE_POINTER,

	// A struct or union type. It contains a list of declarations as children.
	// Opaque structs and unions contain no children. The tag is stored in the
	// token.
	AST_TYPE_STRUCT,
	AST_TYPE_UNION,
} ast_node_kind;

// Flags for each node, mostly used for type qualifiers in declarations.
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

typedef union {
	// Reference to another node, for switch statements and identifiers
	ast_id ref;
	// Reference to another node, which represents the type of this node.
	// Usually for expressions.
	type_id type;
	// Could be the value of a literal or the size of an array.
	i64 i;
	// Could be the value of a floating point literal.
	f64 f;
} ast_info;

// The AST is a tree of tokens, where each node has a reference to its children
// and the next node in the list of children of the parent node.
typedef struct {
	ast_node_kind kind;
	ast_node_flags flags;
	token token;
	ast_id next;
	ast_id children;
	// Additional information about the node, depending on the kind of node.
	// For example, the type of an expression or the value of a literal.
	ast_info info;
} ast_node;

typedef struct ast_pool {
	ast_node *nodes;
	ast_id root;
	isize max_size;
	isize size;
	b32 error;
} ast_pool;

static const ast_id ast_nil = {0};

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
