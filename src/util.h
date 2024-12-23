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

static i32
compare(str a, str b)
{
	i32 result = 0;

	while (result == 0 && a.length-- > 0 && b.length-- > 0) {
		result = (*a.at > *b.at) - (*a.at < *b.at);
	}

	return result + (a.length > 0) - (b.length > 0);
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

static b32
starts_with(str s, str prefix)
{
	b32 result = false;

	if (s.length >= prefix.length) {
		s = substr(s, 0, prefix.length);
		result = equals(s, prefix);
	}

	return result;
}

static void
copy(str dst, str src)
{
	for (isize i = 0; i < MIN(dst.length, src.length); i++) {
		dst.at[i] = src.at[i];
	}
}

#define HASH_INIT 0xcbf29ce484222325ull

static void
hash(u64 *h, void *data, isize size)
{
	char *byte = data;
	while (size-- > 0) {
		*h *= 0x00000100000001B3;
		*h ^= *byte++;
	}
}

static char *
cstr(str s, arena *a)
{
	char *c = ALLOC(a, s.length + 1, char);
	memcpy(c, s.at, s.length);
	return c;
}
