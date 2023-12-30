#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

#include "main.h"

#include "debug.c"
#include "tokenizer.c"
#include "parser.c"
#include "check.c"
#include "ir.c"
#include "optimize.c"
#include "regalloc.c"
#include "stream.c"
#include "x86.c"

static void
print_node(ast_node *node, int indent)
{
	for (int i = 0; i < indent; i++) {
		printf("    ");
	}

	switch (node->kind) {
	case AST_EXPR_BINARY:
		printf("(");
		print_node(node->u.bin_expr.lhs, 0);
		switch (node->u.bin_expr.op) {
		case TOKEN_ADD:    printf(" + ");  break;
		case TOKEN_SUB:    printf(" - ");  break;
		case TOKEN_MUL:    printf(" * ");  break;
		case TOKEN_DIV:    printf(" / ");  break;
		case TOKEN_MOD:    printf(" %% "); break;
		case TOKEN_ASSIGN: printf(" = ");  break;
		default:
			printf(" (invalid operation) ");
		}

		print_node(node->u.bin_expr.rhs, 0);
		printf(")");
		break;
	case AST_EXPR_IDENT:
		printf("%.*s", (int)node->u.ident.length, node->u.ident.at);
		break;
	case AST_EXPR_INT:
		printf("%jd", node->u.ival);
		break;
	case AST_STMT_BREAK:
		printf("break;\n");
		break;
	case AST_STMT_CONTINUE:
		printf("continue;\n");
		break;
	case AST_DECL:
		// TODO: print declarations and declarators
		break;
	case AST_STMT_EMPTY:
		printf(";\n");
		break;
	case AST_STMT_FOR:
		printf("for (");
		print_node(node->u.for_stmt.init, 0);
		printf("; ");
		print_node(node->u.for_stmt.cond, 0);
		printf("; ");
		print_node(node->u.for_stmt.post, 0);
		printf(")\n");
		break;
	case AST_STMT_COMPOUND:
		printf("{\n");
		for (node = node->u.children; node; node = node->next) {
			print_node(node, indent + 1);
		}
		printf("}\n");
		break;
	case AST_STMT_IF:
		printf("if (");
		print_node(node->u.if_stmt.cond, 0);
		printf(")\n");
		print_node(node->u.if_stmt.then, indent);
		if (node->u.if_stmt.otherwise) {
			printf("else\n");
			print_node(node->u.if_stmt.otherwise, indent);
		}

		break;
	case AST_STMT_WHILE:
		printf("while (");
		print_node(node->u.while_stmt.cond, 0);
		printf(")\n");
		print_node(node->u.while_stmt.body, indent);
		break;
	case AST_STMT_RETURN:
		printf("return ");
		print_node(node->u.children, 0);
		printf(";\n");
		break;
	case AST_STMT_PRINT:
		printf("print ");
		print_node(node->u.children, 0);
		printf(";\n");
		break;
	default:
		printf("(invalid)");
	}
}

static void
run_command(char **args)
{
	int status;
	switch (fork()) {
	case -1:
		perror("fork");
		exit(1);
		break;
	case 0:
		execvp(args[0], args);
		exit(1);
		break;
	default:
		wait(&status);
	}
}

static void
run_assembler(char *input, char *output)
{
	char *args[128] = {0};
	char **arg = args;

	*arg++ = "nasm";
	*arg++ = "-felf64";
	*arg++ = "-o";
	*arg++ = output;
	*arg++ = input;
	*arg++ = NULL;

	run_command(args);
}

static void
run_linker(char *input, char *output)
{
	char *args[128] = {0};
	char **arg = args;

	*arg++ = "ld";
	*arg++ = "-dynamic-linker";
	*arg++ = "/usr/lib/ld-linux-x86-64.so.2";
	*arg++ = "-o";
	*arg++ = output;
	*arg++ = "/lib/crt1.o";
	*arg++ = "/lib/crti.o";
	*arg++ = "-lc";
	*arg++ = input;
	*arg++ = "/lib/crtn.o";
	*arg++ = NULL;

	run_command(args);
}

int
main(int argc, char *argv[])
{
	if (argc < 2) {
		fprintf(stderr, "Usage: %s FILE\n", argv[0]);
		return 1;
	}

	arena *arena = new_arena(1000 * 1000);
	symbol_table symbols = {0};
	tokenizer tokenizer = tokenize(argv[1], arena);
	ast_node *root = parse(&tokenizer, arena);
	check_node(root, &symbols, arena);
	if (tokenizer.error) {
		free(arena);
		return 1;
	}

	ir_program ir_program = translate(root, arena);
	optimize(ir_program, arena);
	machine_program machine_program = x86_select_instructions(ir_program, arena);
	allocation_info *info = allocate_registers(machine_program, arena);
	stream out = stream_open("/tmp/out.s", 1024, arena);
	x86_generate(&out, machine_program, info);
	stream_close(&out);
	run_assembler("/tmp/out.s", "/tmp/out.o");
	run_linker("/tmp/out.o", "./a.out");

	free(arena);
	return 0;
}
