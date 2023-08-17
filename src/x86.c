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
};

static uint32_t x86_register_size = 8;
static FILE *x86_output = NULL;

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
x86_emit_location(struct location loc)
{
	switch (loc.type) {
	case LOCATION_STACK:
		fprintf(x86_output, "qword[rsp+%d]", loc.address * x86_register_size);
		break;
	case LOCATION_REGISTER:
		fprintf(x86_output, "%s", x86_get_register_name(loc.address));
		break;
	case LOCATION_CONST:
		fprintf(x86_output, "%#x", loc.address);
		break;
	case LOCATION_LABEL:
		fprintf(x86_output, "L%d", loc.address);
		break;
	}
}

static void
x86_emit0(char *op)
{
	fprintf(x86_output, "\t%s\n", op);
}

static void
x86_emit1(char *op, struct location dst)
{
	fprintf(x86_output, "\t%s ", op);
	x86_emit_location(dst);
	fprintf(x86_output, "\n");
}

static void
x86_emit2(char *op, struct location dst, struct location op0)
{
	fprintf(x86_output, "\t%s ", op);
	x86_emit_location(dst);
	fprintf(x86_output, ", ");
	x86_emit_location(op0);
	fprintf(x86_output, "\n");
}

static bool
location_equals(struct location a, struct location b)
{
	bool result = (a.type == b.type && a.address == b.address);
	return result;
}

static void
x86_mov(struct location dst, struct location src)
{
	if (!location_equals(dst, src)) {
		if (dst.type == LOCATION_STACK && src.type == LOCATION_STACK) {
			fprintf(x86_output, "\tmov rax, ");
			x86_emit_location(src);
			src = register_location(X86_RAX);
		}

		fprintf(x86_output, "\tmov ");
		x86_emit_location(dst);
		fprintf(x86_output, ", ");
		x86_emit_location(src);
		fprintf(x86_output, "\n");
	}
}

static void
x86_generate(struct ir_program program, struct arena *arena)
{
	struct location *locations = allocate_registers(program, X86_REGISTER_COUNT, arena);

	x86_output = fopen("/tmp/out.s", "w");
	if (!x86_output) {
		return;
	}

	uint32_t stack_size = 0;
	for (uint32_t i = 0; i < program.register_count; i++) {
		if (locations[i].type == LOCATION_STACK) {
			locations[i].address = stack_size;
			stack_size += x86_register_size;
		}
	}

	fprintf(x86_output, "global main\nmain:\n");
	if (stack_size > 0) {
		fprintf(x86_output, "\tsub rsp, %d\n", stack_size);
	}

	struct ir_instruction *instructions = program.instructions;
	for (uint32_t i = 0; i < program.instruction_count; i++) {
		struct location rax = register_location(X86_RAX);
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
			dst = locations[instructions[i].dst];
		case IR_JMP:
		case IR_LABEL:
			break;
		case IR_JIZ:
		case IR_RET:
			op0 = locations[instructions[i].op0];
			break;
		}

		if (dst.type != LOCATION_STACK) {
			temp = dst;
		}

		switch (instructions[i].opcode) {
		case IR_SET:
			x86_mov(dst, op0);
			break;
		case IR_MOV:
			x86_mov(dst, op0);
			break;
		case IR_ADD:
			x86_mov(temp, op0);
			x86_emit2("add", temp, op1);
			x86_mov(dst, temp);
			break;
		case IR_SUB:
			x86_mov(temp, op0);
			x86_emit2("sub", temp, op1);
			x86_mov(dst, temp);
			break;
		case IR_MUL:
			x86_mov(rax, op0);
			x86_emit1("imul", op1);
			x86_mov(dst, rax);
			break;
		case IR_DIV:
			x86_mov(rax, op0);
			x86_emit1("idiv", op1);
			x86_mov(dst, rax);
			break;
		case IR_MOD:
			x86_mov(rax, op0);
			x86_mov(rdx, const_location(0));
			x86_emit1("idiv", op1);
			x86_mov(dst, rdx);
			break;
		case IR_JMP:
			op0 = label_location(op0.address);
			x86_emit1("jmp", op0);
			fprintf(x86_output, "\n");
			break;
		case IR_JIZ:
			op1 = label_location(op1.address);
			if (op0.type == LOCATION_STACK) {
				x86_mov(rax, op0);
				op0 = rax;
			}

			x86_emit2("test", op0, op0);
			x86_emit1("jz", op1);
			fprintf(x86_output, "\n");
			break;
		case IR_RET:
			x86_mov(rax, op0);
			if (stack_size > 0) {
				fprintf(x86_output, "\tadd rsp, %d\n", stack_size);
			}
			x86_emit0("ret");
			break;
		case IR_LABEL:
			fprintf(x86_output, "L%d:\n", op0.address);
		}
	}

	if (stack_size > 0) {
		fprintf(x86_output, "\tadd rsp, %d\n", stack_size);
		fprintf(x86_output, "\tret\n");
	}

	fclose(x86_output);
}
