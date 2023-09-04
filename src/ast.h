enum expr_kind {
	EXPR_BINARY,
	EXPR_IDENTIFIER,
	EXPR_INT,
};

struct expr_binary {
	enum token_kind op;
	struct expr *lhs;
	struct expr *rhs;
};

struct expr {
	enum expr_kind kind;

	union {
		struct expr_binary binary;
		struct string identifier;
		intmax_t ival;
	} u;
};

enum stmt_kind {
	STMT_COMPOUND,
	STMT_EMPTY,
	STMT_EXPR,
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

struct stmt {
	enum stmt_kind kind;
	struct stmt *next;

	union {
		struct stmt_if _if;
		struct stmt_while _while;
		struct expr *expr;
		struct stmt *compound;
	} u;
};
