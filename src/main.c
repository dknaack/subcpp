#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

#include "main.h"

static void
print_program(struct ir_program program, uint32_t *usage_count)
{
	for (uint32_t i = 0; i < program.instr_count; i++) {
		printf("(%d) %2d| ", usage_count[i], i);
		struct ir_instr instr = program.instrs[i];

		uint32_t dst = i;
		uint32_t op0 = instr.op0;
		uint32_t op1 = instr.op1;
		switch (instr.opcode) {
		case IR_NOP:   printf("\tnop\n"); break;
		case IR_SET:   printf("\tset r%d, %d\n", dst, op0); break;
		case IR_MOV:   printf("\tmov r%d, r%d\n", op0, op1); break;
		case IR_ADD:   printf("\tadd r%d, r%d, r%d\n", dst, op0, op1); break;
		case IR_SUB:   printf("\tsub r%d, r%d, r%d\n", dst, op0, op1); break;
		case IR_MUL:   printf("\tmul r%d, r%d, r%d\n", dst, op0, op1); break;
		case IR_DIV:   printf("\tdiv r%d, r%d, r%d\n", dst, op0, op1); break;
		case IR_MOD:   printf("\tmod r%d, r%d, r%d\n", dst, op0, op1); break;
		case IR_JMP:   printf("\tjmp L%d\n", op0); break;
		case IR_JIZ:   printf("\tjiz r%d, L%d\n", op0, op1); break;
		case IR_RET:   printf("\tret r%d\n", op0); break;
		case IR_CALL:  printf("\tcall r%d, L%d\n", dst, op0); break;
		case IR_PRINT: printf("\tprint r%d\n", op0); break;
		case IR_PARAM: printf("\tparam r%d\n", dst); break;
		case IR_VAR:   printf("\tvar r%d\n", dst); break;
		case IR_LABEL: printf("L%d:\n", op0); break;
		}
	}

	fflush(stdout);
}

#include "tokenizer.c"
#include "parser.c"
#include "ir.c"
#include "regalloc.c"
#include "stream.c"
#include "x86.c"

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
	case TOKEN_IDENT:      return "identifier";
	case TOKEN_WHITESPACE: return "whitespace";
	default:               return "(invalid)";
	}
}


static void
print_node(struct ast_node *node, int indent)
{
	for (int i = 0; i < indent; i++) {
		printf("    ");
	}

	switch (node->kind) {
	case AST_BINARY:
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
	case AST_IDENT:
		printf("%.*s", (int)node->u.ident.length, node->u.ident.at);
		break;
	case AST_INT:
		printf("%jd", node->u.ival);
		break;
	case AST_BREAK:
		printf("break;\n");
		break;
	case AST_CONTINUE:
		printf("continue;\n");
		break;
	case AST_DECL:
		printf("%.*s", (int)node->u.decl.name.length, node->u.decl.name.at);
		if (node->u.decl.expr) {
			printf(" = ");
			print_node(node->u.decl.expr, 0);
		}

		break;
	case AST_EMPTY:
		printf(";\n");
		break;
	case AST_FOR:
		printf("for (");
		print_node(node->u.for_stmt.init, 0);
		printf("; ");
		print_node(node->u.for_stmt.cond, 0);
		printf("; ");
		print_node(node->u.for_stmt.post, 0);
		printf(")\n");
		break;
	case AST_COMPOUND:
		printf("{\n");
		for (node = node->u.children; node; node = node->next) {
			print_node(node, indent + 1);
		}
		printf("}\n");
		break;
	case AST_IF:
		printf("if (");
		print_node(node->u.if_stmt.cond, 0);
		printf(")\n");
		print_node(node->u.if_stmt.then, indent);
		if (node->u.if_stmt.otherwise) {
			printf("else\n");
			print_node(node->u.if_stmt.otherwise, indent);
		}

		break;
	case AST_WHILE:
		printf("while (");
		print_node(node->u.while_stmt.cond, 0);
		printf(")\n");
		print_node(node->u.while_stmt.body, indent);
		break;
	case AST_RETURN:
		printf("return ");
		print_node(node->u.children, 0);
		printf(";\n");
		break;
	case AST_PRINT:
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
	struct string contents = read_file(argv[1], arena);
	struct tokenizer tokenizer = tokenize(contents);
	struct ast_node *root = parse(&tokenizer, arena);
	struct ir_program ir_program = ir_generate(root, arena);
	struct machine_program machine_program =
	    x86_select_instructions(ir_program, arena);
	struct stream out = stream_open(NULL, 1024, arena);
	x86_generate(&out, machine_program);
	stream_close(&out);
	allocate_registers(machine_program, X86_REGISTER_COUNT, arena);
	out = stream_open("/tmp/out.s", 1024, arena);
	x86_generate(&out, machine_program);
	run_assembler("/tmp/out.s", "/tmp/out.o");
	run_linker("/tmp/out.o", "./a.out");

	stream_close(&out);
	free(arena);
	return 0;
}
