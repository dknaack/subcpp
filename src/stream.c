#include <fcntl.h>

struct stream {
	uint8_t *buffer;
	size_t size;
	size_t used;
	int error;
	int fd;
};

static struct stream
stream_open(char *filename, size_t size, struct arena *arena)
{
	struct stream stream = {0};
	if (filename) {
		stream.fd = open(filename, O_WRONLY|O_TRUNC|O_CREAT, 0666);
	} else {
		stream.fd = 1;
	}

	stream.size = size;
	stream.buffer = zalloc(arena, size, 1);
	return stream;
}

static void
stream_flush(struct stream *stream)
{
	stream->error |= (stream->fd < 0);
	if (!stream->error && stream->used > 0) {
		stream->error |= write(stream->fd, stream->buffer, stream->used);
		stream->used = 0;
	}
}

static void
stream_close(struct stream *stream)
{
	stream_flush(stream);
	stream->fd = -1;
}

static void
stream_write(struct stream *stream, uint8_t byte)
{
	if (stream->used == stream->size) {
		stream_flush(stream);
	}

	if (stream->used != stream->size) {
		stream->buffer[stream->used++] = byte;
	}
}

static void
stream_prints(struct stream *stream, struct string str)
{
	while (str.length-- > 0) {
		stream_write(stream, *str.at++);
	}
}

static void
stream_print(struct stream *stream, char *str)
{
	while (*str) {
		stream_write(stream, *str++);
	}
}

static void
stream_printu(struct stream *stream, uint32_t value)
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
stream_print_hex(struct stream *stream, uint32_t value)
{
	char hex_number[64] = {0};
	char *end = hex_number + sizeof(hex_number);
	char *at = end;
	*--at = '\0';
	do {
		uint8_t hex_digit = value % 16;
		*--at = (hex_digit >= 10 ? 'a' - 10 : '0') + hex_digit;
		value /= 16;
	} while (value > 0);
	*--at = 'x';
	*--at = '0';

	stream_print(stream, at);
}


