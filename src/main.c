#include <stdio.h>
#include <stdlib.h>

#include "main.h"
#include "memory.h"
#include "tokenizer.h"
#include "ast.h"

#include "tokenizer.c"
#include "parser.c"

struct string
read_file(char *filename, struct arena *arena)
{
	struct string result = {0};
	FILE *file = fopen(filename, "rb");
	if (file) {
		fseek(file, 0, SEEK_END);
		result.length = ftell(file);
		fseek(file, 0, SEEK_SET);

		result.at = ALLOC(arena, result.length + 1, char);
		if (result.at) {
			fread(result.at, result.length, 1, file);
			result.at[result.length] = '\0';
		}

		fclose(file);
	}

	return result;
}

static char *
get_token_name(enum token_kind kind)
{
	switch (kind) {
	case TOKEN_EOF:        return "EOF";
	case TOKEN_ADD:        return "'+'";
	case TOKEN_SUB:        return "'-'";
	case TOKEN_MUL:        return "'*'";
	case TOKEN_DIV:        return "'/'";
	case TOKEN_MOD:        return "'%'";
	case TOKEN_ASSIGN:     return "'='";
	case TOKEN_INT:        return "int";
	case TOKEN_IDENTIFIER: return "identifier";
	case TOKEN_WHITESPACE: return "whitespace";
	default:               return "(invalid)";
	}
}


static void
print_expr(struct expr *expr)
{
	switch (expr->kind) {
	case EXPR_BINARY:
		printf("(");
		print_expr(expr->u.binary.lhs);
		switch (expr->u.binary.op) {
		case TOKEN_ADD:    printf(" + ");  break;
		case TOKEN_SUB:    printf(" - ");  break;
		case TOKEN_MUL:    printf(" * ");  break;
		case TOKEN_DIV:    printf(" / ");  break;
		case TOKEN_MOD:    printf(" %% "); break;
		case TOKEN_ASSIGN: printf(" = ");  break;
		default:
			printf(" (invalid operation) ");
		}

		print_expr(expr->u.binary.rhs);
		printf(")");
		break;
	case EXPR_IDENTIFIER:
		printf("%.*s", (int)expr->u.identifier.length, expr->u.identifier.at);
		break;
	default:
		printf("(invalid)");
	}
}

int
main(int argc, char *argv[])
{
	if (argc < 2) {
		fprintf(stderr, "Usage: %s FILE\n", argv[0]);
		return 1;
	}

	struct arena *arena = arena_create(1000 * 1000);
	struct string contents = read_file(argv[1], arena);
	struct tokenizer tokenizer = tokenize(contents);
	struct expr *expr = parse_assign_expr(&tokenizer, arena);
	print_expr(expr);
	printf("\n");
	free(arena);
	return 0;
}
