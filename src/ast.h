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
