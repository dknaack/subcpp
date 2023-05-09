#include <stdio.h>
#include <stdlib.h>

struct string {
	char *at;
	size_t length;
};

struct string
read_file(char *filename)
{
	struct string result = {0};
	FILE *file = fopen(filename, "rb");
	if (file) {
		fseek(file, 0, SEEK_END);
		result.length = ftell(file);
		fseek(file, 0, SEEK_SET);

		result.at = malloc(result.length + 1);
		if (result.at) {
			fread(result.at, result.length, 1, file);
			result.at[result.length] = '\0';
		}

		fclose(file);
	}

	return result;
}

int
main(void)
{
	struct string contents = read_file("src/main.c");
	fwrite(contents.at, 1, contents.length, stdout);
	free(contents.at);
	return 0;
}
