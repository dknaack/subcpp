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

		if (src.type == LOC_CONST && src.address == 0) {
			x86_emit2(out, "xor", dst, dst);
		} else {
			x86_emit2(out, "mov", dst, src);
		}
	}
}

struct x86_program {
	struct ir_program ir;
	struct location *locations;
	uint32_t *usage_count;
	uint32_t stack_size;
	char *postamble;
};

static void
x86_generate_instruction(struct stream *out, struct x86_program program,
    uint32_t instruction_index, struct location dst)
{
	struct ir_instruction *instr = program.ir.instructions;
	struct location *locations = program.locations;
	uint32_t stack_size = program.stack_size;

	enum ir_opcode opcode = instr[instruction_index].opcode;
	uint32_t op0 = instr[instruction_index].op0;
	uint32_t op1 = instr[instruction_index].op1;

	struct location rax = register_location(X86_RAX);
	struct location rcx = register_location(X86_RCX);
	struct location rdx = register_location(X86_RDX);
	struct location rdi = register_location(X86_RDI);
	struct location rsi = register_location(X86_RSI);

	switch (opcode) {
	case IR_SET:
		x86_mov(out, dst, const_location(op0));
		break;
	case IR_VAR:
		x86_mov(out, dst, locations[instruction_index]);
		break;
	case IR_MOV:
		ASSERT(location_equals(dst, program.locations[op0]));
		x86_generate_instruction(out, program, op1, program.locations[op0]);
		break;
	case IR_ADD:
		if (instr[op1].opcode == IR_SET && instr[op1].op0 == 1) {
			x86_generate_instruction(out, program, op0, dst);
			x86_emit1(out, "inc", dst);
		} else if (instr[op1].opcode == IR_SET) {
			op1 = instr[op1].op0;
			x86_generate_instruction(out, program, op0, dst);
			x86_emit2(out, "add", dst, const_location(op1));
		} else {
			x86_generate_instruction(out, program, op0, dst);
			x86_generate_instruction(out, program, op1, locations[op1]);
			x86_emit2(out, "add", dst, locations[op1]);
		}
		break;
	case IR_SUB:
		if (instr[op1].opcode == IR_SET && instr[op1].op0 == 1) {
			x86_generate_instruction(out, program, op0, dst);
			x86_emit1(out, "dec", dst);
		} else if (instr[op1].opcode == IR_SET) {
			op1 = instr[op1].op0;
			x86_generate_instruction(out, program, op0, dst);
			x86_emit2(out, "sub", dst, const_location(op1));
		} else {
			x86_generate_instruction(out, program, op0, dst);
			x86_generate_instruction(out, program, op1, locations[op1]);
			x86_emit2(out, "sub", dst, locations[op1]);
		}
		break;
	case IR_MUL:
		if (instr[op1].opcode == IR_SET && instr[op1].op0 == 1) {
			x86_generate_instruction(out, program, op0, dst);
		} else if (instr[op1].opcode == IR_SET && instr[op1].op0 == 2) {
			x86_generate_instruction(out, program, op0, dst);
			x86_emit2(out, "add", dst, dst);
		} else {
			x86_generate_instruction(out, program, op0, rax);
			x86_generate_instruction(out, program, op1, locations[op1]);
			x86_emit1(out, "imul", locations[op1]);
			x86_mov(out, dst, rax);
		}
		break;
	case IR_DIV:
		x86_generate_instruction(out, program, op0, rax);
		x86_generate_instruction(out, program, op1, rcx);
		x86_mov(out, rdx, const_location(0));
		x86_emit1(out, "idiv", rcx);
		x86_mov(out, dst, rax);
		break;
	case IR_MOD:
		x86_generate_instruction(out, program, op0, rax);
		x86_generate_instruction(out, program, op1, rcx);
		x86_mov(out, rdx, const_location(0));
		x86_emit1(out, "idiv", rcx);
		x86_mov(out, dst, rdx);
		break;
	case IR_JMP:
		op0 = instr[op0].op0;
		x86_emit1(out, "jmp", label_location(op0));
		stream_print(out, "\n");
		break;
	case IR_JIZ:
		x86_generate_instruction(out, program, op0, rax);
		if (instr[op0].opcode != IR_SUB) {
			x86_emit2(out, "test", rax, rax);
		}

		op1 = instr[op1].op0;
		x86_emit1(out, "jz", label_location(op1));
		stream_print(out, "\n");
		break;
	case IR_RET:
		x86_generate_instruction(out, program, op0, rax);

		if (stack_size > 0) {
			stream_print(out, "\tadd rsp, ");
			stream_printu(out, stack_size);
			stream_print(out, "\n");
		}

		stream_print(out, program.postamble);
		break;
	case IR_CALL:
		stream_print(out, "\tcall ");
		stream_prints(out, program.ir.functions[op0].name);
		stream_print(out, "\n");
		x86_mov(out, dst, rax);
		break;
	case IR_PARAM:
		x86_generate_instruction(out, program, op0, rdi);
		break;
	case IR_PRINT:
		x86_generate_instruction(out, program, op0, rsi);
		stream_print(out, "\tmov rdi, fmt\n");
		x86_mov(out, rax, const_location(0));
		stream_print(out, "\tcall printf wrt ..plt\n");
		break;
	case IR_LABEL:
		stream_print(out, "L");
		stream_printu(out, op0);
		stream_print(out, ":\n");
		break;
	case IR_NOP:
		break;
	}
}

static void
x86_generate_function(struct stream *out,
    struct x86_program program, uint32_t function_index)
{
	struct ir_function function = program.ir.functions[function_index];
	uint32_t last_block = function.block_index + function.block_count;

	stream_prints(out, function.name);
	stream_print(out, ":\n");
	stream_print(out,
	    "\tpush r12\n"
	    "\tpush r13\n"
	    "\tpush r14\n"
	    "\tpush r15\n"
	);

	program.postamble = "\n"
	    "\tpop r15\n"
	    "\tpop r14\n"
	    "\tpop r13\n"
	    "\tpop r12\n"
	    "\tret\n";

	for (uint32_t i = function.block_index; i < last_block; i++) {
		struct ir_block block = program.ir.blocks[i];
		for (uint32_t i = block.start; i < block.start + block.size; i++) {
			struct ir_instruction instr = program.ir.instructions[i];
			if (instr.opcode == IR_LABEL) {
				if (program.usage_count[i] == 0) {
					continue;
				}
			} else if (program.usage_count[i] != 0) {
				continue;
			}

			struct location dst = program.locations[i];
			if (instr.opcode == IR_MOV) {
				dst = program.locations[instr.op0];
			}

			x86_generate_instruction(out, program, i, dst);
		}
	}
}

static void
x86_generate(struct stream *out, struct ir_program program,
    struct location *locations, uint32_t *usage_count)
{
	struct x86_program x86_program = {0};
	x86_program.ir = program;
	x86_program.locations = locations;
	x86_program.usage_count = usage_count;
	x86_program.stack_size = 0;

	for (uint32_t i = 0; i < program.register_count; i++) {
		if (locations[i].type == LOC_STACK) {
			locations[i].address = x86_program.stack_size;
			x86_program.stack_size += x86_register_size;
		}
	}

	stream_print(out,
	    "global main\n"
	    "extern printf\n\n"
	    "section .data\n"
	    "fmt: db \"%d\", 0x0A, 0\n\n"
	    "section .text\n"
	);

	if (x86_program.stack_size > 0) {
		stream_print(out, "\tsub rsp, ");
		stream_printu(out, x86_program.stack_size);
		stream_print(out, "\n");
	}

	for (uint32_t i = 0; i < program.function_count; i++) {
		x86_generate_function(out, x86_program, i);
	}

	if (x86_program.stack_size > 0) {
		stream_print(out, "\tadd rsp, ");
		stream_printu(out, x86_program.stack_size);
		stream_print(out, "\n\tret\n");
	}
}
