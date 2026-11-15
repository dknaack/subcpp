#include <fcntl.h>
#include <stdio.h>

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
starts_with(str s, str prefix)
{
	b32 result = false;

	if (s.length >= prefix.length) {
		s = substr(s, 0, prefix.length);
		result = equals(s, prefix);
	}

	return result;
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

static writer
new_writer(char *filename, isize size, arena *arena)
{
	writer w = {0};
	if (filename) {
		w.fd = open(filename, O_WRONLY|O_TRUNC|O_CREAT, 0666);
	} else {
		w.fd = STDOUT_FILENO;
	}

	w.size = size;
	w.buffer = ALLOC(arena, size, char);
	return w;
}

static void
wflush(writer *w)
{
	w->error |= (w->fd < 0);
	if (!w->error && w->used > 0) {
		w->error |= write(w->fd, w->buffer, w->used);
		w->used = 0;
	}
}

static void
wclose(writer *w)
{
	wflush(w);
	close(w->fd);
	w->fd = -1;
}

static void
write_u8(writer *w, u8 byte)
{
	if (w->used == w->size) {
		wflush(w);
	}

	if (w->used != w->size) {
		w->buffer[w->used++] = byte;
	}
}

static void
print_str(writer *w, str str)
{
	while (str.length-- > 0) {
		write_u8(w, *str.at++);
	}
}

static void
print_cstr(writer *w, char *str)
{
	while (*str) {
		write_u8(w, *str++);
	}
}

static void
print_u32(writer *w, u32 value)
{
	char number[64] = {0};
	char *end = number + sizeof(number);
	char *at = end;
	*--at = '\0';
	do {
		*--at = '0' + (value % 10);
		value /= 10;
	} while (value > 0);

	print_cstr(w, at);
}

static void
print_hex(writer *w, u32 value)
{
	char hex_number[64] = {0};
	char *end = hex_number + sizeof(hex_number);
	char *at = end;
	*--at = '\0';
	do {
		u8 hex_digit = value % 16;
		*--at = (hex_digit >= 10 ? 'a' - 10 : '0') + hex_digit;
		value /= 16;
	} while (value > 0);
	*--at = 'x';
	*--at = '0';

	print_cstr(w, at);
}
