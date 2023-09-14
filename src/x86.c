static char *
x86_get_opcode_name(enum x86_opcode opcode)
{
	switch (opcode) {
	case X86_ADD:   return "add";
	case X86_CALL:  return "call";
	case X86_CMP:   return "cmp";
	case X86_DEC:   return "dec";
	case X86_IDIV:  return "idiv";
	case X86_IMUL:  return "imul";
	case X86_INC:   return "inc";
	case X86_JMP:   return "jmp";
	case X86_JZ:    return "jz";
	case X86_JL:    return "jl";
	case X86_JG:    return "jg";
	case X86_JLE:   return "jle";
	case X86_JGE:   return "jge";
	case X86_MOV:   return "mov";
	case X86_RET:   return "ret";
	case X86_SETZ:  return "setz";
	case X86_SETL:  return "setl";
	case X86_SETG:  return "setg";
	case X86_SETLE: return "setle";
	case X86_SETGE: return "setge";
	case X86_SUB:   return "sub";
	case X86_TEST:  return "test";
	case X86_XOR:   return "xor";
	case X86_LABEL: return "label";
	case X86_PRINT: return "print";
	}

	return "(invalid)";
}

static char *
x86_get_byte_register_name(enum x86_register reg)
{
	switch (reg) {
	case X86_R8:  return "r8b";
	case X86_R9:  return "r9b";
	case X86_R10: return "r10b";
	case X86_R11: return "r11b";
	case X86_R12: return "r12b";
	case X86_R13: return "r13b";
	case X86_R14: return "r14b";
	case X86_R15: return "r15b";
	case X86_RAX: return "al";
	case X86_RBX: return "bl";
	case X86_RCX: return "cl";
	case X86_RDX: return "dl";
	case X86_RSI: return "sil";
	case X86_RDI: return "dil";
	default:      return "(invalid)";
	}
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
x86_select0(struct machine_program *out, enum x86_opcode opcode)
{
	push_instr(out, opcode, 0);
}

static void
x86_select1(struct machine_program *out, enum x86_opcode opcode,
    struct machine_operand dst)
{
	struct machine_operand op0, op1;
	switch (opcode) {
	case X86_IDIV:
		push_instr(out, opcode, 3);
		dst.flags |= MOP_USE;
		push_operand(out, dst);
		op0 = make_mreg(X86_RAX);
		op0.flags |= MOP_DEF | MOP_USE;
		push_operand(out, op0);
		op1 = make_mreg(X86_RDX);
		op1.flags |= MOP_DEF | MOP_USE;
		push_operand(out, op1);
		break;
	case X86_IMUL:
		push_instr(out, opcode, 3);
		dst.flags |= MOP_USE;
		push_operand(out, dst);
		op0 = make_mreg(X86_RAX);
		op0.flags |= MOP_DEF | MOP_USE;
		push_operand(out, op0);
		op1 = make_mreg(X86_RDX);
		op1.flags |= MOP_DEF;
		push_operand(out, op1);
		break;
	default:
		push_instr(out, opcode, 1);
		dst.flags |= MOP_USE | MOP_DEF;
		push_operand(out, dst);
	}
}

static void
x86_select2(struct machine_program *out, enum x86_opcode opcode,
    struct machine_operand dst, struct machine_operand src)
{
	switch (opcode) {
	case X86_MOV:
		if (src.kind == MOP_IMMEDIATE && src.value == 0) {
			push_instr(out, X86_XOR, 2);
			dst.flags |= MOP_DEF | MOP_USE;
			push_operand(out, dst);
			push_operand(out, dst);
		} else if (!machine_operand_equals(dst, src)) {
			push_instr(out, opcode, 2);
			dst.flags |= MOP_DEF;
			push_operand(out, dst);
			src.flags |= MOP_USE;
			push_operand(out, src);
		}
		break;
	default:
		push_instr(out, opcode, 2);
		dst.flags |= MOP_DEF | MOP_USE;
		push_operand(out, dst);
		src.flags |= MOP_USE;
		push_operand(out, src);
	}
}

static bool
x86_is_comparison_opcode(enum ir_opcode ir_opcode)
{
	switch (ir_opcode) {
	case IR_EQL:
	case IR_LT:
	case IR_GT:
	case IR_LEQ:
	case IR_GEQ:
		return true;
	default:
		return false;
	}
}

static enum x86_opcode
x86_get_setcc_opcode(enum ir_opcode ir_opcode)
{
	switch (ir_opcode) {
	case IR_EQL: return X86_SETZ;
	case IR_LT:  return X86_SETL;
	case IR_GT:  return X86_SETG;
	case IR_LEQ: return X86_SETLE;
	case IR_GEQ: return X86_SETGE;
	default:
		ASSERT(!"Not a comparison operator");
		return X86_SETZ;
	}
}

static enum x86_opcode
x86_get_jcc_opcode(enum ir_opcode ir_opcode)
{
	switch (ir_opcode) {
	case IR_EQL: return X86_JZ;
	case IR_LT:  return X86_JGE;
	case IR_GT:  return X86_JLE;
	case IR_LEQ: return X86_JG;
	case IR_GEQ: return X86_JL;
	default:
		ASSERT(!"Not a comparison operator");
		return X86_SETZ;
	}
}

static void x86_select_instr(struct machine_program *out,
    struct ir_program program, uint32_t instr_index, struct machine_operand dst);

static struct machine_operand
x86_select_immediate(struct machine_program *out, struct ir_program program, uint32_t instr_index)
{
	struct machine_operand result;
	if (program.instrs[instr_index].opcode == IR_SET) {
		result = make_immediate(program.instrs[instr_index].op0);
	} else {
		result = make_vreg(instr_index);
		x86_select_instr(out, program, instr_index, result);
	}

	return result;
}

static void
x86_select_instr(struct machine_program *out, struct ir_program program,
    uint32_t instr_index, struct machine_operand dst)
{
	struct ir_instr *instr = program.instrs;

	enum x86_opcode x86_opcode = (enum x86_opcode)0;
	enum ir_opcode opcode = instr[instr_index].opcode;
	uint32_t op0 = instr[instr_index].op0;
	uint32_t op1 = instr[instr_index].op1;

	struct machine_operand rax = make_mreg(X86_RAX);
	struct machine_operand rcx = make_mreg(X86_RCX);
	struct machine_operand rdx = make_mreg(X86_RDX);
	struct machine_operand rdi = make_mreg(X86_RDI);
	struct machine_operand rsi = make_mreg(X86_RSI);
	struct machine_operand src = {0};

	switch (opcode) {
	case IR_SET:
		x86_select2(out, X86_MOV, dst, make_immediate(op0));
		break;
	case IR_VAR:
		x86_select2(out, X86_MOV, dst, make_vreg(instr_index));
		break;
	case IR_MOV:
		x86_select_instr(out, program, op1, dst);
		break;
	case IR_ADD:
		if (instr[op1].opcode == IR_SET && instr[op1].op0 == 1) {
			x86_select_instr(out, program, op0, dst);
			x86_select1(out, X86_INC, dst);
		} else if (instr[op1].opcode == IR_SET) {
			x86_select_instr(out, program, op0, dst);
			op1 = instr[op1].op0;
			x86_select2(out, X86_ADD, dst, make_immediate(op1));
		} else if (instr[op0].opcode == IR_SET) {
			x86_select_instr(out, program, op1, dst);
			op0 = instr[op0].op0;
			x86_select2(out, X86_ADD, dst, make_immediate(op0));
		} else {
			src = make_vreg(op1);
			x86_select_instr(out, program, op0, dst);
			x86_select_instr(out, program, op1, src);
			x86_select2(out, X86_ADD, dst, src);
		}
		break;
	case IR_SUB:
		if (instr[op1].opcode == IR_SET && instr[op1].op0 == 1) {
			x86_select_instr(out, program, op0, dst);
			x86_select1(out, X86_DEC, dst);
		} else if (instr[op1].opcode == IR_SET) {
			op1 = instr[op1].op0;
			x86_select_instr(out, program, op0, dst);
			x86_select2(out, X86_SUB, dst, make_immediate(op1));
		} else {
			src = make_vreg(op1);
			x86_select_instr(out, program, op0, dst);
			x86_select_instr(out, program, op1, src);
			x86_select2(out, X86_SUB, dst, src);
		}
		break;
	case IR_MUL:
		if (instr[op1].opcode == IR_SET && instr[op1].op0 == 1) {
			x86_select_instr(out, program, op0, dst);
		} else if (instr[op1].opcode == IR_SET && instr[op1].op0 == 2) {
			x86_select_instr(out, program, op0, dst);
			x86_select2(out, X86_ADD, dst, dst);
		} else {
			src = make_vreg(op1);
			x86_select_instr(out, program, op0, rax);
			x86_select_instr(out, program, op1, src);
			x86_select1(out, X86_IMUL, src);
			x86_select2(out, X86_MOV, dst, rax);
		}
		break;
	case IR_DIV:
		x86_select_instr(out, program, op0, rax);
		x86_select_instr(out, program, op1, rcx);
		x86_select2(out, X86_MOV, rdx, make_immediate(0));
		x86_select1(out, X86_IDIV, rcx);
		x86_select2(out, X86_MOV, dst, rax);
		break;
	case IR_MOD:
		x86_select_instr(out, program, op0, rax);
		x86_select_instr(out, program, op1, rcx);
		x86_select2(out, X86_MOV, rdx, make_immediate(0));
		x86_select1(out, X86_IDIV, rcx);
		x86_select2(out, X86_MOV, dst, rdx);
		break;
	case IR_EQL:
	case IR_LT:
	case IR_GT:
	case IR_GEQ:
	case IR_LEQ:
		x86_select_instr(out, program, op0, dst);
		x86_select_instr(out, program, op1, make_vreg(op1));
		x86_select2(out, X86_CMP, dst, make_vreg(op1));
		x86_select1(out, x86_get_setcc_opcode(opcode), dst);
		break;
	case IR_JMP:
		op0 = instr[op0].op0;
		x86_select1(out, X86_JMP, make_label(op0));
		break;
	case IR_JIZ:
		x86_opcode = X86_JZ;
		if (x86_is_comparison_opcode(instr[op0].opcode)) {
			dst = make_vreg(instr[op0].op0);
			x86_select_instr(out, program, instr[op0].op0, dst);
			src = x86_select_immediate(out, program, instr[op0].op1);
			x86_select2(out, X86_CMP, dst, src);
			x86_opcode = x86_get_jcc_opcode(instr[op0].opcode);
		} else if (instr[op0].opcode == IR_SUB) {
			x86_select_instr(out, program, op0, src);
		} else {
			src = make_vreg(op0);
			x86_select_instr(out, program, op0, src);
			x86_select2(out, X86_TEST, src, src);
		}

		op1 = instr[op1].op0;
		x86_select1(out, x86_opcode, make_label(op1));
		break;
	case IR_RET:
		x86_select_instr(out, program, op0, rax);
		x86_select0(out, X86_RET);
		break;
	case IR_CALL:
		for (uint32_t i = 1; i <= op1; i++) {
			ASSERT(instr[instr_index - i].opcode == IR_PARAM);
			src = make_vreg(instr[instr_index - i].op0);
			uint32_t parameter_index = i - 1;
			switch (parameter_index) {
			case 0:
				x86_select_instr(out, program, src.value, rdi);
				break;
			case 1:
				x86_select_instr(out, program, src.value, rsi);
				break;
			case 2:
				x86_select_instr(out, program, src.value, rdx);
				break;
			case 3:
				x86_select_instr(out, program, src.value, rcx);
				break;
			default:
				ASSERT(!"Too many arguments");
				break;
			}
		}

		x86_select1(out, X86_CALL, make_func(op0));
		x86_select2(out, X86_MOV, dst, rax);
		break;
	case IR_PRINT:
		x86_select_instr(out, program, op0, rsi);
		x86_select2(out, X86_MOV, rax, make_immediate(0));
		x86_select2(out, X86_PRINT, rdi, rsi);
		break;
	case IR_LABEL:
		x86_select1(out, X86_LABEL, make_immediate(op0));
		break;
	case IR_NOP:
	case IR_PARAM:
		break;
	}
}

static struct machine_program
x86_select_instructions(struct ir_program program, struct arena *arena)
{
	struct machine_program out = {0};
	out.max_size = 1024 * 8;
	out.functions = ZALLOC(arena, program.function_count, struct machine_function);
	out.function_count = program.function_count;
	out.code = alloc(arena, out.max_size, 1);
	out.vreg_count = program.register_count;
	out.mreg_count = X86_REGISTER_COUNT;

	for (uint32_t f = 0; f < program.function_count; f++) {
		struct ir_function ir_function = program.functions[f];
		struct machine_function *function = &out.functions[f];
		function->name = ir_function.name;
		function->start = out.size;

		uint32_t first_block = ir_function.block_index;
		uint32_t last_block = first_block + ir_function.block_count;
		for (uint32_t b = first_block; b < last_block; b++) {
			struct ir_block block = program.blocks[b];
			for (uint32_t i = block.start; i < block.start + block.size; i++) {
				uint32_t instr_index = program.toplevel_instr_indices[i];
				struct ir_instr instr = program.instrs[instr_index];
				struct machine_operand dst = make_vreg(instr_index);
				if (instr.opcode == IR_MOV) {
					dst = make_vreg(instr.op0);
				}

				x86_select_instr(&out, program, instr_index, dst);
			}
		}
	}

	return out;
}

static void
x86_emit_operand(struct stream *out, struct machine_operand operand,
    struct machine_function *functions)
{
	enum x86_register reg;
	switch (operand.kind) {
	case MOP_SPILL:
		ASSERT(!"Spilled variables have not been implemented yet");
		break;
	case MOP_LABEL:
		stream_print(out, "L");
		stream_printu(out, operand.value);
		break;
	case MOP_MREG:
		reg = (enum x86_register)operand.value;
		stream_print(out, x86_get_register_name(reg));
		break;
	case MOP_IMMEDIATE:
		stream_printu(out, operand.value);
		break;
	case MOP_FUNC:
		stream_prints(out, functions[operand.value].name);
		break;
	case MOP_VREG:
		stream_print(out, "v");
		stream_printu(out, operand.value);
		//ASSERT(!"Cannot use virtual register during code generation");
		break;
	default:
		stream_print(out, "(invalid operand)");
	}
}

static bool
x86_is_setcc(enum x86_opcode opcode)
{
	switch (opcode) {
	case X86_SETZ:
	case X86_SETL:
	case X86_SETG:
	case X86_SETLE:
	case X86_SETGE:
		return true;
	default:
		return false;
	}
}

static void
x86_generate(struct stream *out, struct machine_program program)
{
	stream_print(out,
	    "global main\n"
	    "extern printf\n\n"
	    "section .data\n"
	    "fmt: db \"%d\", 0x0A, 0\n\n"
	    "section .text\n");

	char *start = (char *)program.code;
	char *code = start;
	char *end = code + program.size;
	struct machine_function *function = program.functions;
	while (code < end) {
		while (start + function->start < code) {
			stream_prints(out, function->name);
			stream_print(out, ":\n");
			function++;
		}

		struct machine_instr *instr = (struct machine_instr *)code;
		struct machine_operand *operands
		    = (struct machine_operand *)(instr + 1);
		enum x86_opcode opcode = (enum x86_opcode)instr->opcode;
		uint32_t operand_count = instr->operand_count;
		code += sizeof(*instr) + operand_count * sizeof(*operands);

		if (opcode == X86_PRINT) {
			stream_print(out,
			    "\tmov rdi, fmt\n"
			    "\tcall printf wrt ..plt\n");
		} else if (opcode == X86_LABEL) {
			stream_print(out, "L");
			x86_emit_operand(out, operands[0], program.functions);
			stream_print(out, ":\n");
		} else if (x86_is_setcc(opcode)) {
			ASSERT(operands[0].kind == MOP_MREG);
			stream_print(out, "\t");
			stream_print(out, x86_get_opcode_name(opcode));
			stream_print(out, " ");
			char *name = x86_get_byte_register_name(operands[0].value);
			stream_print(out, name);
			stream_print(out, "\n");
		} else {
			if (opcode == X86_IMUL || opcode == X86_IDIV) {
				operand_count -= 2;
			}

			stream_print(out, "\t");
			stream_print(out, x86_get_opcode_name(opcode));
			stream_print(out, " ");
			while (operand_count-- > 0) {
				x86_emit_operand(out, *operands++, program.functions);
				if (operand_count > 0) {
					stream_print(out, ", ");
				}
			}

			stream_print(out, "\n");
		}
	}
}
