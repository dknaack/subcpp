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

		uint32_t dst = i;
		uint32_t op0 = instr.op0;
		uint32_t op1 = instr.op1;
		switch (instr.opcode) {
		case IR_NOP:   printf("\tnop\n"); break;
		case IR_CONST: printf("\tr%d = %d\n", dst, op0); break;
		case IR_MOV:   printf("\tr%d = r%d\n", op0, op1); break;
		case IR_ADD:   printf("\tr%d = r%d + r%d\n", dst, op0, op1); break;
		case IR_SUB:   printf("\tr%d = r%d - r%d\n", dst, op0, op1); break;
		case IR_MUL:   printf("\tr%d = r%d * r%d\n", dst, op0, op1); break;
		case IR_DIV:   printf("\tr%d = r%d / r%d\n", dst, op0, op1); break;
		case IR_MOD:   printf("\tr%d = r%d %% r%d\n", dst, op0, op1); break;
		case IR_EQL:   printf("\tr%d = r%d == r%d\n", dst, op0, op1); break;
		case IR_LT:    printf("\tr%d = r%d < r%d\n", dst, op0, op1); break;
		case IR_GT:    printf("\tr%d = r%d > r%d\n", dst, op0, op1); break;
		case IR_LEQ:   printf("\tr%d = r%d <= r%d\n", dst, op0, op1); break;
		case IR_GEQ:   printf("\tr%d = r%d >= r%d\n", dst, op0, op1); break;
		case IR_JMP:   printf("\tgoto L%d\n", op0); break;
		case IR_JIZ:   printf("\tif r%d == 0 goto L%d\n", op0, op1); break;
		case IR_RET:   printf("\tret r%d\n", op0); break;
		case IR_CALL:  printf("\tr%d = call L%d, %d\n", dst, op0, op1); break;
		case IR_PRINT: printf("\tprint r%d\n", op0); break;
		case IR_PARAM: printf("\tparam r%d\n", op0); break;
		case IR_VAR:   printf("\tr%d = var\n", dst); break;
		case IR_LABEL: printf("L%d:\n", op0); break;
		}
	}

	fflush(stdout);
}

#include "tokenizer.c"
#include "parser.c"
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

static void
add_variable(struct symbol_table *table, struct string name,
	struct type *type, struct arena *arena)
{
	struct symbol *symbol = table->free_symbols;
	if (symbol) {
		table->free_symbols = symbol->next;
	} else {
		symbol = ALLOC(arena, 1, struct symbol);
	}

	ASSERT(name.at == scope_marker.at || type->kind != TYPE_VOID);
	symbol->name = name;
	symbol->type = type;
	symbol->next = table->symbols;
	table->symbols = symbol;
}

static struct type *
get_variable(struct symbol_table *table, struct string name)
{
	struct type *type = &type_void;

	for (struct symbol *symbol = table->symbols; symbol; symbol = symbol->next) {
		if (string_equals(symbol->name, name)) {
			type = symbol->type;
			break;
		}
	}

	return type;
}

static void
push_scope(struct symbol_table *table, struct arena *arena)
{
	add_variable(table, scope_marker, &type_void, arena);
}

static void
pop_scope(struct symbol_table *table)
{
	struct symbol *symbol;

	for (symbol = table->symbols; symbol; symbol = table->symbols) {
		table->symbols = symbol->next;
		symbol->next = table->free_symbols;
		table->free_symbols = symbol;

		if (symbol->name.at == scope_marker.at) {
			break;
		}
	}
}

static bool
type_equals(struct type *lhs, struct type *rhs)
{
	if (lhs->kind == TYPE_VOID || rhs->kind == TYPE_VOID) {
		return false;
	}

	if (lhs->kind != rhs->kind) {
		return false;
	}

	switch (lhs->kind) {
	case TYPE_FUNCTION:
		if (!type_equals(lhs, rhs)) {
			return false;
		}

		lhs = lhs->u.function.param_types;
		rhs = rhs->u.function.param_types;
		while (lhs && rhs) {
			if (!type_equals(lhs, rhs)) {
				return false;
			}

			lhs = lhs->next;
			rhs = rhs->next;
		}

		bool result = (lhs == NULL && rhs == NULL);
		return result;
	default:
		return true;
	}
}

static struct type *
check_node(struct ast_node *node, struct symbol_table *symbols, struct arena *arena)
{
	struct type *lhs, *rhs, *type, **param_type;
	struct ast_node *param;
	if (!node) {
		return &type_void;
	}

	struct ast_node *orig_node = node;
	switch (node->kind) {
	case AST_INVALID:
		ASSERT(!"Invalid node");
		break;
	case AST_ROOT:
		type = &type_void;
		for (node = node->u.children; node; node = node->next) {
			check_node(node, symbols, arena);
		}
		break;
	case AST_EXPR_BINARY:
		lhs = check_node(node->u.bin_expr.lhs, symbols, arena);
		rhs = check_node(node->u.bin_expr.rhs, symbols, arena);
		if (!type_equals(lhs, rhs)) {
			errorf(node->loc, "Incompatible types: %s, %s",
				type_get_name(lhs->kind), type_get_name(rhs->kind));
		}

		type = lhs;
		break;
	case AST_EXPR_CALL:
		type = check_node(node->u.call_expr.called, symbols, arena);
		if (type->kind != TYPE_FUNCTION) {
			errorf(node->loc, "Not a function: %s", type_get_name(type->kind));
			break;
		}

		rhs = type->u.function.param_types;
		for (param = node->u.call_expr.params; param; param = param->next) {
			lhs = check_node(param, symbols, arena);
			if (!type_equals(lhs, rhs)) {
				errorf(node->loc, "Incompatible types: %s, %s",
					type_get_name(lhs->kind), type_get_name(rhs->kind));
			}

			rhs = rhs->next;
		}

		type = type->u.function.return_type;
		break;
	case AST_EXPR_IDENT:
		type = get_variable(symbols, node->u.ident);
		break;
	case AST_STMT_BREAK:
		type = type_create(TYPE_VOID, arena);
		break;
	case AST_STMT_COMPOUND:
		type = type_create(TYPE_VOID, arena);
		push_scope(symbols, arena);
		for (node = node->u.children; node; node = node->next) {
			check_node(node, symbols, arena);
		}

		pop_scope(symbols);
		break;
	case AST_DECL:
		lhs = check_node(node->u.decl.type, symbols, arena);
		if (node->u.decl.expr) {
			rhs = check_node(node->u.decl.expr, symbols, arena);
			if (!type_equals(lhs, rhs)) {
				errorf(node->loc, "Incompatible types: %s, %s",
					type_get_name(lhs->kind), type_get_name(rhs->kind));
			}
		}

		add_variable(symbols, node->u.decl.name, lhs, arena);
		type = lhs;
		break;
	case AST_STMT_DECL:
		type = type_create(TYPE_VOID, arena);
		for (node = node->u.children; node; node = node->next) {
			check_node(node, symbols, arena);
		}

		break;
	case AST_STMT_CONTINUE:
		type = &type_void;
		break;
	case AST_STMT_EMPTY:
		type = &type_void;
		break;
	case AST_STMT_FOR:
		type = &type_void;
		check_node(node->u.for_stmt.init, symbols, arena);
		check_node(node->u.for_stmt.cond, symbols, arena);
		check_node(node->u.for_stmt.post, symbols, arena);
		check_node(node->u.for_stmt.body, symbols, arena);
		break;
	case AST_STMT_IF:
		type = &type_void;
		check_node(node->u.if_stmt.cond, symbols, arena);
		check_node(node->u.if_stmt.then, symbols, arena);
		check_node(node->u.if_stmt.otherwise, symbols, arena);
		break;
	case AST_STMT_PRINT:
		type = &type_void;
		check_node(node->u.children, symbols, arena);
		break;
	case AST_STMT_WHILE:
		type = &type_void;
		check_node(node->u.while_stmt.cond, symbols, arena);
		check_node(node->u.while_stmt.body, symbols, arena);
		break;
	case AST_STMT_RETURN:
		type = &type_void;
		check_node(node->u.children, symbols, arena);
		break;
	case AST_FUNCTION:
		type = type_create(TYPE_FUNCTION, arena);
		type->u.function.return_type = type_create(TYPE_INT, arena);
		param_type = &type->u.function.param_types;
		for (param = node->u.function.params; param; param = param->next) {
			*param_type = check_node(param, symbols, arena);
			param_type = &(*param_type)->next;
		}

		add_variable(symbols, node->u.function.name, type, arena);
		push_scope(symbols, arena);
		param_type = &type->u.function.param_types;
		for (param = node->u.function.params; param; param = param->next) {
			add_variable(symbols, param->u.decl.name, *param_type, arena);
			param_type = &(*param_type)->next;
		}

		for (node = node->u.function.body; node; node = node->next) {
			check_node(node, symbols, arena);
		}
		pop_scope(symbols);
		break;
	case AST_TYPE_VOID:
		type = type_create(TYPE_VOID, arena);
		break;
	case AST_TYPE_CHAR:
		type = type_create(TYPE_CHAR, arena);
		break;
	case AST_TYPE_INT:
	case AST_EXPR_INT:
		type = type_create(TYPE_INT, arena);
		break;
	}

	orig_node->type = type;
	return type;
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
