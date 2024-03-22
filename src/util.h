static str
read_file(char *filename, arena *arena)
{
	str result = {0};
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

static str
make_str(char *cstr)
{
	str result;
	result.at = cstr;
	result.length = 0;
	while (result.at[result.length]) {
		result.length++;
	}

	return result;
}

static b32
equals(str a, str b)
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

static char *
cstr(str s, arena *perm)
{
	char *c = ALLOC(perm, 1 + s.length, char);

	for (isize i = 0; i < s.length; i++) {
		c[i] = s.at[i];
	}

	return c;
}

static str
substr(str s, isize start, isize end)
{
	str result;
	result.at = s.at + start;
	result.length = end < 0 ? s.length - start : end - start;

	ASSERT(end < 0 || start <= end);
	ASSERT(start <= s.length && end <= s.length);
	return result;
}

static void
copy(str dst, str src)
{
	for (isize i = 0; i < MIN(dst.length, src.length); i++) {
		dst.at[i] = src.at[i];
	}
}

static u64
hash(str str)
{
	u64 h = 0xcbf29ce484222325ull;

	while (str.length-- > 0) {
		h *= 0x00000100000001B3;
		h ^= *str.at++;
	}

	return h;
}
