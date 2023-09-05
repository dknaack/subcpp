#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

#include "main.h"

static void
print_ir(struct ir_instruction instruction)
{
	uint32_t dst = instruction.dst;
	uint32_t op0 = instruction.op0;
	uint32_t op1 = instruction.op1;

	switch (instruction.opcode) {
	case IR_NOP:   printf("\tnop\n"); break;
	case IR_SET:   printf("\tset r%d, %d\n", dst, op0); break;
	case IR_MOV:   printf("\tmov r%d, r%d\n", dst, op0); break;
	case IR_ADD:   printf("\tadd r%d, r%d, r%d\n", dst, op0, op1); break;
	case IR_SUB:   printf("\tsub r%d, r%d, r%d\n", dst, op0, op1); break;
	case IR_MUL:   printf("\tmul r%d, r%d, r%d\n", dst, op0, op1); break;
	case IR_DIV:   printf("\tdiv r%d, r%d, r%d\n", dst, op0, op1); break;
	case IR_MOD:   printf("\tmod r%d, r%d, r%d\n", dst, op0, op1); break;
	case IR_JMP:   printf("\tjmp L%d\n", dst); break;
	case IR_JIZ:   printf("\tjiz r%d, L%d\n", op0, op1); break;
	case IR_RET:   printf("\tret r%d\n", op0); break;
	case IR_CALL:  printf("\tcall r%d, L%d\n", dst, op0); break;
	case IR_PRINT: printf("\tprint r%d\n", op0); break;
	case IR_LABEL: printf("L%d:\n", op0); return;
	}
}

static void
print_program(struct ir_instruction *instructions, uint32_t instruction_count)
{
	for (uint32_t i = 0; i < instruction_count; i++) {
		printf("%2d| ", i);
		print_ir(*instructions++);
	}

	fflush(stdout);
}

#include "tokenizer.c"
#include "parser.c"
#include "codegen.c"
#include "regalloc.c"
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
	case EXPR_INT:
		printf("%jd", expr->u.ival);
		break;
	default:
		printf("(invalid)");
	}
}

static void
print_decl(struct decl *decl)
{
	printf("int ");
	while (decl) {
		printf("%.*s", (int)decl->name.length, decl->name.at);
		if (decl->expr) {
			printf(" = ");
			print_expr(decl->expr);
		}

		decl = decl->next;
		if (decl) {
			printf(", ");
		}
	}
}

static void
print_stmt(struct stmt *stmt, int indent)
{
	for (int i = 0; i < indent; i++) {
		printf("    ");
	}

	switch (stmt->kind) {
	case STMT_BREAK:
		printf("break;\n");
		break;
	case STMT_CONTINUE:
		printf("continue;\n");
		break;
	case STMT_DECL:
		break;
	case STMT_EMPTY:
		printf(";\n");
		break;
	case STMT_EXPR:
		print_expr(stmt->u.expr);
		printf(";\n");
		break;
	case STMT_FOR_EXPR:
	case STMT_FOR_DECL:
		printf("for (");
		if (stmt->kind == STMT_FOR_EXPR) {
			print_expr(stmt->u._for.init.expr);
		} else {
			print_decl(stmt->u._for.init.decl);
		}
		printf("; ");
		print_expr(stmt->u._for.condition);
		printf("; ");
		print_expr(stmt->u._for.post);
		printf(")\n");
		break;
	case STMT_COMPOUND:
		printf("{\n");
		for (stmt = stmt->u.compound; stmt; stmt = stmt->next) {
			print_stmt(stmt, indent + 1);
		}
		printf("}\n");
		break;
	case STMT_IF:
		printf("if (");
		print_expr(stmt->u._if.condition);
		printf(")\n");
		print_stmt(stmt->u._if.then, indent);
		if (stmt->u._if.otherwise) {
			printf("else\n");
			print_stmt(stmt->u._if.otherwise, indent);
		}

		break;
	case STMT_WHILE:
		printf("while (");
		print_expr(stmt->u._while.condition);
		printf(")\n");
		print_stmt(stmt->u._while.body, indent);
		break;
	case STMT_RETURN:
		printf("return ");
		print_expr(stmt->u.expr);
		printf(";\n");
		break;
	case STMT_PRINT:
		printf("print ");
		print_expr(stmt->u.expr);
		printf(";\n");
		break;
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
	struct function *function = parse(&tokenizer, arena);
	struct ir_program program = ir_generate(function, arena);
	x86_generate(program, arena);
	run_assembler("/tmp/out.s", "/tmp/out.o");
	run_linker("/tmp/out.o", "./a.out");

	free(arena);
	return 0;
}
