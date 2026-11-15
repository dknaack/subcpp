typedef struct {
	char *buffer;
	isize size;
	isize used;
	int error;
	int fd;
} writer;

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

static void
copy(str dst, str src)
{
	for (isize i = 0; i < MIN(dst.length, src.length); i++) {
		dst.at[i] = src.at[i];
	}
}
