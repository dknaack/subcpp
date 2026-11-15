#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

#include "main.h"

#include "lex.c"
#include "parse.c"
#include "sema.c"
#include "debug.c"
#include "ir.c"
#include "optimize.c"
#include "regalloc.c"
#include "io.c"
#include "x86.c"

int
main(int argc, char *argv[])
{
	arena *arena = new_arena(1024 * 1024);

	str input = S("char *const foo = \"1234\";");
	syntax_tree tree = parse("test", input, arena);
	print_ast(&tree);

	free(arena);
	return 0;
}
