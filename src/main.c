#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

#include "main.h"

static void
print_program(struct ir_program program)
{
	for (uint32_t i = 0; i < program.instr_count; i++) {
		printf("%2d| ", i);
		struct ir_instr instr = program.instrs[i];

		char *c = "";
		switch (instr.size) {
		case 1: c = "b"; break;
		case 2: c = "w"; break;
		case 4: c = "d"; break;
		case 8: c = "q"; break;
		}

		uint32_t dst = i;
		uint32_t op0 = instr.op0;
		uint32_t op1 = instr.op1;
		switch (instr.opcode) {
		case IR_NOP:   printf("\tnop\n"); break;
		case IR_CONST: printf("\tMOV\tr%d%s, %d\n", dst, c, op0); break;
		case IR_MOV:   printf("\tMOV\tr%d%s, r%d\n", op0, c, op1); break;
		case IR_LOAD:  printf("\tLOAD\tr%d%s, r%d\n", dst, c, op0); break;
		case IR_STORE: printf("\tSTORE\tr%d%s, r%d\n", op0, c, op1); break;
		case IR_ADD:   printf("\tADD\tr%d%s, r%d, r%d\n",  dst, c, op0, op1); break;
		case IR_SUB:   printf("\tSUB\tr%d%s, r%d, r%d\n",  dst, c, op0, op1); break;
		case IR_MUL:   printf("\tMUL\tr%d%s, r%d, r%d\n",  dst, c, op0, op1); break;
		case IR_DIV:   printf("\tDIV\tr%d%s, r%d, r%d\n",  dst, c, op0, op1); break;
		case IR_MOD:   printf("\tMOD\tr%d%s, r%d, r%d\n", dst, c, op0, op1); break;
		case IR_EQL:   printf("\tEQL\tr%d%s, r%d,= r%d\n", dst, c, op0, op1); break;
		case IR_LEQ:   printf("\tLEQ\tr%d%s, r%d, r%d\n", dst, c, op0, op1); break;
		case IR_GEQ:   printf("\tGEQ\tr%d%s, r%d, r%d\n", dst, c, op0, op1); break;
		case IR_LT:    printf("\tLT\tr%d%s, r%d, r%d\n",  dst, c, op0, op1); break;
		case IR_GT:    printf("\tGT\tr%d%s, r%d, r%d\n",  dst, c, op0, op1); break;
		case IR_JMP:   printf("\tGOTO\tL%d\n", op0); break;
		case IR_JIZ:   printf("\tJIZ\tr%d, L%d\n", op0, op1); break;
		case IR_RET:   printf("\tRET\tr%d\n", op0); break;
		case IR_CALL:  printf("\tCALL\tr%d, L%d, %d\n", dst, op0, op1); break;
		case IR_PRINT: printf("\tPRINT\tr%d\n", op0); break;
		case IR_PARAM: printf("\tPARAM\tr%d\n", op0); break;
		case IR_VAR:   printf("\tALLOCA\tr%d, 8\n", dst); break;
		case IR_LABEL: printf("L%d:\n", op0); break;
		}
	}

	fflush(stdout);
}

#include "tokenizer.c"
#include "parser.c"
#include "check.c"
#include "ir.c"
#include "optimize.c"
#include "regalloc.c"
#include "stream.c"
#include "x86.c"

static void
print_node(struct ast_node *node, int indent)
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
		printf("%.*s", (int)node->u.decl.name.length, node->u.decl.name.at);
		if (node->u.decl.expr) {
			printf(" = ");
			print_node(node->u.decl.expr, 0);
		}

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

	struct arena *arena = arena_create(1000 * 1000);
	struct symbol_table symbols = {0};
	struct tokenizer tokenizer = tokenize(argv[1], arena);
	struct ast_node *root = parse(&tokenizer, arena);
	check_node(root, &symbols, arena);
	if (tokenizer.error) {
		free(arena);
		return 1;
	}

	struct ir_program ir_program = ir_generate(root, arena);
	optimize(ir_program, arena);
	struct machine_program machine_program = x86_select_instructions(ir_program, arena);
	struct allocation_info *info = allocate_registers(machine_program, arena);
	struct stream out = stream_open("/tmp/out.s", 1024, arena);
	x86_generate(&out, machine_program, info);
	stream_close(&out);
	run_assembler("/tmp/out.s", "/tmp/out.o");
	run_linker("/tmp/out.o", "./a.out");

	free(arena);
	return 0;
}
