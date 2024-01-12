#include <fcntl.h>

typedef struct {
	char *buffer;
	usize size;
	usize used;
	int error;
	int fd;
} stream;

static stream
stream_open(char *filename, usize size, arena *arena)
{
	stream stream = {0};
	if (filename) {
		stream.fd = open(filename, O_WRONLY|O_TRUNC|O_CREAT, 0666);
	} else {
		stream.fd = 1;
	}

	stream.size = size;
	stream.buffer = ALLOC(arena, size, char);
	return stream;
}

static void
stream_flush(stream *stream)
{
	stream->error |= (stream->fd < 0);
	if (!stream->error && stream->used > 0) {
		stream->error |= write(stream->fd, stream->buffer, stream->used);
		stream->used = 0;
	}
}

static void
stream_close(stream *stream)
{
	stream_flush(stream);
	stream->fd = -1;
}

static void
stream_write(stream *stream, u8 byte)
{
	if (stream->used == stream->size) {
		stream_flush(stream);
	}

	if (stream->used != stream->size) {
		stream->buffer[stream->used++] = byte;
	}
}

static void
stream_prints(stream *stream, str str)
{
	while (str.length-- > 0) {
		stream_write(stream, *str.at++);
	}
}

static void
stream_print(stream *stream, char *str)
{
	while (*str) {
		stream_write(stream, *str++);
	}
}

static void
stream_printu(stream *stream, u32 value)
{
	char number[64] = {0};
	char *end = number + sizeof(number);
	char *at = end;
	*--at = '\0';
	do {
		*--at = '0' + (value % 10);
		value /= 10;
	} while (value > 0);

	stream_print(stream, at);
}

static void
stream_print_hex(stream *stream, u32 value)
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

	stream_print(stream, at);
}
