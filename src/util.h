static string
read_file(char *filename, arena *arena)
{
	string result = {0};
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

static b32
string_equals(string a, string b)
{
	if (a.length != b.length) {
		return false;
	}

	while (a.length-- > 0) {
		if (*a.at++ != *b.at++) {
			return false;
		}
	}

	return true;
}

static u64
hash(string str)
{
	u64 h = 0xcbf29ce484222325ull;

	while (str.length-- > 0) {
		h *= 0x00000100000001B3;
		h ^= *str.at++;
	}

	return h;
}
