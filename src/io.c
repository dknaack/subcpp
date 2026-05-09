#include <fcntl.h>

typedef struct {
	char *buffer;
	isize size;
	isize used;
	int error;
	int fd;
} writer;

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
