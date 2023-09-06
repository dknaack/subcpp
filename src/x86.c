#include <fcntl.h>

enum x86_register {
	X86_R12,
	X86_R13,
	X86_R14,
	X86_R15,
	X86_REGISTER_COUNT,

	X86_R8,
	X86_R9,
	X86_R10,
	X86_R11,
	X86_RAX,
	X86_RBX,
	X86_RCX,
	X86_RDX,
	X86_RSI,
	X86_RDI,
	X86_RSP,
	X86_RBP,
};

enum x86_opcode {
	X86_MOV,
	X86_JMP,
	X86_JZ,
	X86_ADD,
	X86_SUB,
	X86_MUL,
};

struct stream {
	uint8_t *buffer;
	size_t size;
	size_t used;
	int error;
	int fd;
};

static uint32_t x86_register_size = 8;

static struct stream
stream_open(char *filename, size_t size, struct arena *arena)
{
	struct stream stream = {0};
	stream.fd = open(filename, O_WRONLY|O_TRUNC|O_CREAT, 0666);
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

static char *
x86_get_register_name(enum x86_register reg)
{
	switch (reg) {
	case X86_R8:  return "r8";
	case X86_R9:  return "r9";
	case X86_R10: return "r10";
	case X86_R11: return "r11";
	case X86_R12: return "r12";
	case X86_R13: return "r13";
	case X86_R14: return "r14";
	case X86_R15: return "r15";
	case X86_RAX: return "rax";
	case X86_RBX: return "rbx";
	case X86_RCX: return "rcx";
	case X86_RDX: return "rdx";
	case X86_RSI: return "rsi";
	case X86_RDI: return "rdi";
	default:      return "(invalid)";
	}
}

static void
x86_emit_location(struct stream *out, struct location loc)
{
	switch (loc.type) {
	case LOC_STACK:
		stream_print(out, "qword[rsp+");
		stream_printu(out, loc.address * x86_register_size);
		stream_print(out, "]");
		break;
	case LOC_REGISTER:
		stream_print(out, x86_get_register_name(loc.address));
		break;
	case LOC_CONST:
		stream_print_hex(out, loc.address);
		break;
	case LOC_LABEL:
		stream_print(out, "L");
		stream_printu(out, loc.address);
		break;
	default:
		ASSERT(!"Invalid location");
	}
}

static void
x86_emit0(struct stream *out, char *op)
{
	stream_print(out, "\t");
	stream_print(out, op);
	stream_print(out, "\n");
}

static void
x86_emit1(struct stream *out, char *op, struct location dst)
{
	stream_print(out, "\t");
	stream_print(out, op);
	stream_print(out, " ");
	x86_emit_location(out, dst);
	stream_print(out, "\n");
}

static void
x86_emit2(struct stream *out, char *op, struct location dst, struct location op0)
{
	stream_print(out, "\t");
	stream_print(out, op);
	stream_print(out, " ");
	x86_emit_location(out, dst);
	stream_print(out, ", ");
	x86_emit_location(out, op0);
	stream_print(out, "\n");
}

static bool
location_equals(struct location a, struct location b)
{
	bool result = (a.type == b.type && a.address == b.address);
	return result;
}

static void
x86_mov(struct stream *out, struct location dst, struct location src)
{
	if (!location_equals(dst, src)) {
		if (dst.type == LOC_STACK && src.type == LOC_STACK) {
			stream_print(out, "\tmov rax, ");
			x86_emit_location(out, src);
			src = register_location(X86_RAX);
		}

		stream_print(out, "\tmov ");
		x86_emit_location(out, dst);
		stream_print(out, ", ");
		x86_emit_location(out, src);
		stream_print(out, "\n");
	}
}

static void
x86_generate_basic_block(struct stream *out,
    struct ir_program program, uint32_t block_index,
    struct location *locations, uint32_t stack_size, char *postamble)
{
	struct ir_instruction *instructions = program.instructions;
	struct ir_block block = program.blocks[block_index];
	for (uint32_t i = block.start; i < block.start + block.size; i++) {
		struct location rax = register_location(X86_RAX);
		struct location rsi = register_location(X86_RSI);
		struct location rdi = register_location(X86_RDI);
		struct location rdx = register_location(X86_RDX);
		struct location temp = rax;

		struct location dst = const_location(instructions[i].dst);
		struct location op0 = const_location(instructions[i].op0);
		struct location op1 = const_location(instructions[i].op1);
		switch (instructions[i].opcode) {
		case IR_MOV:
		case IR_ADD:
		case IR_SUB:
		case IR_MUL:
		case IR_DIV:
		case IR_MOD:
			op1 = locations[instructions[i].op1];
			op0 = locations[instructions[i].op0];
			/* fallthrough */
		case IR_SET:
		case IR_CALL:
		case IR_PARAM:
			dst = locations[instructions[i].dst];
		case IR_JMP:
		case IR_LABEL:
		case IR_NOP:
			break;
		case IR_JIZ:
		case IR_RET:
		case IR_PRINT:
			op0 = locations[instructions[i].op0];
			break;
		}

		if (dst.type != LOC_STACK && !location_equals(dst, op1)) {
			temp = dst;
		}

		switch (instructions[i].opcode) {
		case IR_SET:
			x86_mov(out, dst, op0);
			break;
		case IR_MOV:
			x86_mov(out, dst, op0);
			break;
		case IR_ADD:
			x86_mov(out, temp, op0);
			x86_emit2(out, "add", temp, op1);
			x86_mov(out, dst, temp);
			break;
		case IR_SUB:
			x86_mov(out, temp, op0);
			x86_emit2(out, "sub", temp, op1);
			x86_mov(out, dst, temp);
			break;
		case IR_MUL:
			x86_mov(out, rax, op0);
			x86_emit1(out, "imul", op1);
			x86_mov(out, dst, rax);
			break;
		case IR_DIV:
			x86_mov(out, rax, op0);
			x86_mov(out, rdx, const_location(0));
			x86_emit1(out, "idiv", op1);
			x86_mov(out, dst, rax);
			break;
		case IR_MOD:
			x86_mov(out, rax, op0);
			x86_mov(out, rdx, const_location(0));
			x86_emit1(out, "idiv", op1);
			x86_mov(out, dst, rdx);
			break;
		case IR_JMP:
			dst = label_location(dst.address);
			x86_emit1(out, "jmp", dst);
			stream_print(out, "\n");
			break;
		case IR_JIZ:
			dst = label_location(dst.address);
			if (op0.type == LOC_STACK) {
				x86_mov(out, rax, op0);
				op0 = rax;
			}

			x86_emit2(out, "test", op0, op0);
			x86_emit1(out, "jz", dst);
			stream_print(out, "\n");
			break;
		case IR_RET:
			x86_mov(out, rax, op0);
			if (stack_size > 0) {
				stream_print(out, "\tadd rsp, ");
				stream_printu(out, stack_size);
				stream_print(out, "\n");
			}

			stream_print(out, postamble);
			break;
		case IR_CALL:
			op0 = label_location(op0.address);
			stream_print(out, "\tcall ");
			stream_prints(out, program.functions[op0.address].name);
			stream_print(out, "\n");
			x86_mov(out, dst, rax);
			break;
		case IR_PARAM:
			x86_mov(out, rdi, dst);
			break;
		case IR_PRINT:
			stream_print(out, "\tmov rdi, fmt\n");
			x86_mov(out, rsi, op0);
			x86_mov(out, rax, const_location(0));
			stream_print(out, "\tcall printf wrt ..plt\n");
			break;
		case IR_LABEL:
			stream_print(out, "L");
			stream_printu(out, op0.address);
			stream_print(out, ":\n");
		case IR_NOP:
			break;
		}
	}
}

static void
x86_generate_function(struct stream *out, struct ir_program program,
    uint32_t function_index, struct location *locations, uint32_t stack_size)
{
	struct ir_function function = program.functions[function_index];
	uint32_t last_block = function.block_index + function.block_count;

	stream_prints(out, function.name);
	stream_print(out, ":\n");
	stream_print(out,
	    "\tpush r12\n"
	    "\tpush r13\n"
	    "\tpush r14\n"
	    "\tpush r15\n"
	);

	char *postamble = "\n"
	    "\tpop r15\n"
	    "\tpop r14\n"
	    "\tpop r13\n"
	    "\tpop r12\n"
	    "\tret\n";

	for (uint32_t i = function.block_index; i < last_block; i++) {
		x86_generate_basic_block(out, program, i, locations, stack_size, postamble);
	}
}

static void
x86_generate(struct ir_program program, struct arena *arena)
{
	struct location *locations = allocate_registers(program, X86_REGISTER_COUNT, arena);

	// TODO: choose a random file for output
	struct stream out = stream_open("/tmp/out.s", 4096, arena);
	if (!out.fd) {
		return;
	}

	uint32_t stack_size = 0;
	for (uint32_t i = 0; i < program.register_count; i++) {
		if (locations[i].type == LOC_STACK) {
			locations[i].address = stack_size;
			stack_size += x86_register_size;
		}
	}

	stream_print(&out,
	    "global main\n"
	    "extern printf\n\n"
	    "section .data\n"
	    "fmt: db \"%d\", 0x0A, 0\n\n"
	    "section .text\n"
	);

	if (stack_size > 0) {
		stream_print(&out, "\tsub rsp, ");
		stream_printu(&out, stack_size);
		stream_print(&out, "\n");
	}

	for (uint32_t i = 0; i < program.function_count; i++) {
		x86_generate_function(&out, program, i, locations, stack_size);
	}

	if (stack_size > 0) {
		stream_print(&out, "\tadd rsp, ");
		stream_printu(&out, stack_size);
		stream_print(&out, "\n\tret\n");
	}

	stream_close(&out);
}
