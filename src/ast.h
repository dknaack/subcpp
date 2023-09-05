enum expr_kind {
	EXPR_BINARY,
	EXPR_CALL,
	EXPR_IDENTIFIER,
	EXPR_INT,
};

struct expr_binary {
	enum token_kind op;
	struct expr *lhs;
	struct expr *rhs;
};

struct expr_call {
	struct expr *called;
};

struct expr {
	enum expr_kind kind;

	union {
		struct expr_binary binary;
		struct expr_call call;
		struct string identifier;
		intmax_t ival;
	} u;
};

struct decl {
	struct decl *next;
	struct string name;
	struct expr *expr;
};

enum stmt_kind {
	STMT_BREAK,
	STMT_COMPOUND,
	STMT_CONTINUE,
	STMT_DECL,
	STMT_EMPTY,
	STMT_EXPR,
	STMT_FOR,
	STMT_IF,
	STMT_PRINT,
	STMT_WHILE,
	STMT_RETURN,
};

struct stmt_if {
	struct expr *condition;
	struct stmt *then;
	struct stmt *otherwise;
};

struct stmt_while {
	struct expr *condition;
	struct stmt *body;
};

struct stmt_for {
	/* TODO: replace with declaration */
	struct expr *init;
	struct expr *condition;
	struct expr *post;
	struct stmt *body;
};

struct stmt {
	enum stmt_kind kind;
	struct stmt *next;

	union {
		struct stmt_if _if;
		struct stmt_while _while;
		struct stmt_for _for;
		struct expr *expr;
		struct decl *decl;
		struct stmt *compound;
	} u;
};

struct function {
	struct function *next;
	struct string name;
	struct stmt *body;
};
