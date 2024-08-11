#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

#include "main.h"

#include "lexer.c"
#include "parser.c"
#include "check.c"
#include "ir.c"
#include "optimize.c"
#include "debug.c"
#include "regalloc.c"
#include "stream.c"
#include "x86.c"

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
	char *output = "a.out";
	char *input = NULL;

	for (int i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			switch (argv[i][1]) {
			case 'o':
				if (argv[i][2]) {
					output = argv[i] + 2;
				} else if (i + 1 < argc) {
					output = argv[i + 1];
					i++;
				} else {
					fprintf(stderr, "Expected argument\n");
					return 1;
				}

				break;
			}
		} else if (!input) {
			input = argv[i];
		} else {
			fprintf(stderr, "Invalid argument\n");
			return 1;
		}
	}

	if (!input) {
		fprintf(stderr, "Usage: %s FILE\n", argv[0]);
		return 1;
	}

	arena *arena = new_arena(1024 * 1024 * 1024);
	str src = read_file(input, arena);
	parse_context pc = tokenize(input, src, arena);

	ast_pool pool = parse(&pc, arena);
	printf("parsing done\n");

	semantic_info info = check(&pool, arena);
	printf("type checking done\n");

	if (!pool.error) {
		ir_program ir_program = translate(&pool, &info, arena);
		print_ir_program(ir_program);
		optimize(ir_program, arena);
		print_ir_program(ir_program);

		machine_program machine_program = x86_select_instructions(ir_program, arena);
		machine_program.symtab = &info.symtab;
		allocation_info *info = allocate_registers(machine_program, arena);
		stream out = stream_open("/tmp/out.s", 1024 * 1024, arena);
		x86_generate(&out, machine_program, info);
		stream_close(&out);

		run_assembler("/tmp/out.s", "/tmp/out.o");
		run_linker("/tmp/out.o", output);
	}

	free(arena);
	return 0;
}
