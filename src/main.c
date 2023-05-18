#include <stdio.h>
#include <stdlib.h>

#include "main.h"
#include "memory.h"
#include "tokenizer.h"

#include "tokenizer.c"

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

	for (;;) {
		struct token token = get_token(&tokenizer);
		if (token.kind == TOKEN_EOF || token.kind == TOKEN_INVALID) {
			break;
		}

		printf("%s\n", token_name(token.kind));
	}

	free(arena);
	return 0;
}
