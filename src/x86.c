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
	case X86_CMP:
		push_instr(out, opcode, 2);
		dst.flags |= MOP_USE;
		push_operand(out, dst);
		src.flags |= MOP_USE;
		push_operand(out, src);
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
    struct ir_instr *instr, uint32_t instr_index, struct machine_operand dst);

static struct machine_operand
x86_select_immediate(struct machine_program *out,
	struct ir_instr *instr, uint32_t instr_index)
{
	struct machine_operand result;
	if (instr[instr_index].opcode == IR_CONST) {
		result = make_immediate(instr[instr_index].op0);
	} else {
		result = make_vreg(instr_index);
		x86_select_instr(out, instr, instr_index, result);
	}

	return result;
}

static void
x86_alloc(struct machine_program *out, uint32_t id, uint32_t size)
{
	(void)out;
	(void)id;
	(void)size;
}

static void
x86_select_instr(struct machine_program *out, struct ir_instr *instr,
    uint32_t instr_index, struct machine_operand dst)
{
	dst.size = MIN(dst.size, instr[instr_index].size);

	uint32_t op0 = instr[instr_index].op0;
	uint32_t op1 = instr[instr_index].op1;
	enum ir_opcode opcode = instr[instr_index].opcode;
	switch (opcode) {
	case IR_CONST:
		{
			struct machine_operand src = make_immediate(op0);
			x86_select2(out, X86_MOV, dst, src);
		} break;
	case IR_ALLOC:
		{
			struct machine_operand src = make_vreg(instr_index);
			x86_select2(out, X86_MOV, dst, src);
		} break;
	case IR_MOV:
		x86_select_instr(out, instr, op1, dst);
		break;
	case IR_LOAD:
	case IR_STORE:
		break;
	case IR_ADD:
		if (instr[op1].opcode == IR_CONST && instr[op1].op0 == 1) {
			x86_select_instr(out, instr, op0, dst);
			x86_select1(out, X86_INC, dst);
		} else if (instr[op1].opcode == IR_CONST) {
			x86_select_instr(out, instr, op0, dst);
			op1 = instr[op1].op0;
			x86_select2(out, X86_ADD, dst, make_immediate(op1));
		} else if (instr[op0].opcode == IR_CONST) {
			x86_select_instr(out, instr, op1, dst);
			op0 = instr[op0].op0;
			x86_select2(out, X86_ADD, dst, make_immediate(op0));
		} else {
			struct machine_operand src = make_vreg(op1);
			x86_select_instr(out, instr, op0, dst);
			x86_select_instr(out, instr, op1, src);
			x86_select2(out, X86_ADD, dst, src);
		}
		break;
	case IR_SUB:
		if (instr[op1].opcode == IR_CONST && instr[op1].op0 == 1) {
			x86_select_instr(out, instr, op0, dst);
			x86_select1(out, X86_DEC, dst);
		} else if (instr[op1].opcode == IR_CONST) {
			op1 = instr[op1].op0;
			x86_select_instr(out, instr, op0, dst);
			x86_select2(out, X86_SUB, dst, make_immediate(op1));
		} else {
			struct machine_operand src = make_vreg(op1);
			x86_select_instr(out, instr, op0, dst);
			x86_select_instr(out, instr, op1, src);
			x86_select2(out, X86_SUB, dst, src);
		}
		break;
	case IR_MUL:
		if (instr[op1].opcode == IR_CONST && instr[op1].op0 == 1) {
			x86_select_instr(out, instr, op0, dst);
		} else if (instr[op1].opcode == IR_CONST && instr[op1].op0 == 2) {
			x86_select_instr(out, instr, op0, dst);
			x86_select2(out, X86_ADD, dst, dst);
		} else {
			struct machine_operand rax = make_mreg(X86_RAX);
			struct machine_operand src = make_vreg(op1);
			x86_select_instr(out, instr, op0, rax);
			x86_select_instr(out, instr, op1, src);
			x86_select1(out, X86_IMUL, src);
			x86_select2(out, X86_MOV, dst, rax);
		}
		break;
	case IR_DIV:
	case IR_MOD:
		{
			struct machine_operand rax = make_mreg(X86_RAX);
			struct machine_operand rcx = make_mreg(X86_RCX);
			struct machine_operand rdx = make_mreg(X86_RDX);

			x86_select_instr(out, instr, op0, rax);
			x86_select_instr(out, instr, op1, rcx);
			x86_select2(out, X86_MOV, rdx, make_immediate(0));
			x86_select1(out, X86_IDIV, rcx);
			x86_select2(out, X86_MOV, dst, opcode == IR_DIV ? rax : rdx);
		} break;
	case IR_EQL:
	case IR_LT:
	case IR_GT:
	case IR_GEQ:
	case IR_LEQ:
		x86_select_instr(out, instr, op0, dst);
		x86_select_instr(out, instr, op1, make_vreg(op1));
		x86_select2(out, X86_CMP, dst, make_vreg(op1));
		x86_select1(out, x86_get_setcc_opcode(opcode), dst);
		break;
	case IR_JMP:
		op0 = instr[op0].op0;
		x86_select1(out, X86_JMP, make_label(op0));
		break;
	case IR_JIZ:
		{
			enum x86_opcode x86_opcode = X86_JZ;
			if (x86_is_comparison_opcode(instr[op0].opcode)) {
				dst = make_vreg(instr[op0].op0);
				x86_select_instr(out, instr, instr[op0].op0, dst);
				struct machine_operand src = x86_select_immediate(out, instr, instr[op0].op1);
				x86_select2(out, X86_CMP, dst, src);
				x86_opcode = x86_get_jcc_opcode(instr[op0].opcode);
			} else if (instr[op0].opcode == IR_SUB) {
				struct machine_operand src = {0};
				x86_select_instr(out, instr, op0, src);
			} else {
				struct machine_operand src = make_vreg(op0);
				x86_select_instr(out, instr, op0, src);
				x86_select2(out, X86_TEST, src, src);
			}

			op1 = instr[op1].op0;
			x86_select1(out, x86_opcode, make_label(op1));
		} break;
	case IR_RET:
		{
			struct machine_operand rax = make_mreg(X86_RAX);
			x86_select_instr(out, instr, op0, rax);
			x86_select0(out, X86_RET);
		} break;
	case IR_CALL:
		{
			for (uint32_t i = 1; i <= op1; i++) {
				ASSERT(instr[instr_index - i].opcode == IR_PARAM);
				struct machine_operand src = make_vreg(instr[instr_index - i].op0);
				uint32_t parameter_index = i - 1;
				switch (parameter_index) {
				case 0:
					{
						struct machine_operand rdi = make_mreg(X86_RDI);
						x86_select_instr(out, instr, src.value, rdi);
					} break;
				case 1:
					{
						struct machine_operand rsi = make_mreg(X86_RSI);
						x86_select_instr(out, instr, src.value, rsi);
					} break;
				case 2:
					{
						struct machine_operand rdx = make_mreg(X86_RDX);
						x86_select_instr(out, instr, src.value, rdx);
					} break;
				case 3:
					{
						struct machine_operand rcx = make_mreg(X86_RCX);
						x86_select_instr(out, instr, src.value, rcx);
					} break;
				default:
					ASSERT(!"Too many arguments");
					break;
				}
			}

			struct machine_operand rax = make_mreg(X86_RAX);
			x86_select1(out, X86_CALL, make_func(op0));
			x86_select2(out, X86_MOV, dst, rax);
		} break;
	case IR_PRINT:
		{
			struct machine_operand rsi = make_mreg(X86_RSI);
			struct machine_operand rdi = make_mreg(X86_RDI);
			struct machine_operand rax = make_mreg(X86_RAX);

			x86_select_instr(out, instr, op0, rsi);
			x86_select2(out, X86_MOV, rax, make_immediate(0));
			x86_select2(out, X86_PRINT, rdi, rsi);
		} break;
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
	out.blocks = ZALLOC(arena, program.block_count, struct machine_block);
	out.functions = ZALLOC(arena, program.function_count, struct machine_function);
	out.function_count = program.function_count;
	out.block_count = program.block_count;
	out.code = alloc(arena, out.max_size, 1);
	out.vreg_count = program.register_count;
	out.mreg_count = X86_REGISTER_COUNT;

	out.temp_mregs = x86_temp_regs;
	out.temp_mreg_count = LENGTH(x86_temp_regs);

	for (uint32_t f = 0; f < program.function_count; f++) {
		struct ir_function ir_function = program.functions[f];
		out.functions[f].name = ir_function.name;
		out.functions[f].block_index = ir_function.block_index;
		out.functions[f].instr_index = out.size;

		for (uint32_t i = 0; i < ir_function.parameter_count; i++) {
			struct ir_instr instr = program.instrs[ir_function.instr_index+i];
			ASSERT(instr.opcode == IR_ALLOC);
			struct machine_operand dst = make_vreg(ir_function.instr_index+i);
			struct machine_operand src;
			switch (i) {
			case 0:
				src = make_mreg(X86_RDI);
				x86_select2(&out, X86_MOV, dst, src);
				break;
			case 1:
				src = make_mreg(X86_RSI);
				x86_select2(&out, X86_MOV, dst, src);
				break;
			case 2:
				src = make_mreg(X86_RDX);
				x86_select2(&out, X86_MOV, dst, src);
				break;
			case 3:
				src = make_mreg(X86_RCX);
				x86_select2(&out, X86_MOV, dst, src);
				break;
			default:
				ASSERT(!"Too many parameters");
			}
		}

		uint32_t first_block = ir_function.block_index;
		uint32_t last_block = first_block + ir_function.block_count;
		for (uint32_t b = first_block; b < last_block; b++) {
			struct ir_block block = program.blocks[b];
			out.blocks[b].instr_index = out.size;
			for (uint32_t i = block.start; i < block.start + block.size; i++) {
				uint32_t instr_index = program.toplevel_instr_indices[i];
				struct ir_instr instr = program.instrs[instr_index];
				struct machine_operand dst = make_vreg(instr_index);
				if (instr.opcode == IR_MOV) {
					dst = make_vreg(instr.op0);
				}

				x86_select_instr(&out, program.instrs, instr_index, dst);
			}
		}
	}

	out.instr_count = 0;
	char *start = out.code;
	char *code = start;
	char *end = start + out.size;
	while (code < end) {
		out.instr_count++;
		code += get_instr_size(*(struct machine_instr *)code);
	}

	out.instr_offsets = ALLOC(arena, out.instr_count, uint32_t);
	uint32_t instr_index = 0;
	code = start;
	while (code < end) {
		out.instr_offsets[instr_index++] = code - start;
		code += get_instr_size(*(struct machine_instr *)code);
	}

	/* Convert instruction offset to instruction index */
	instr_index = 0;
	for (uint32_t i = 0; i < out.block_count; i++) {
		uint32_t offset = out.blocks[i].instr_index;
		while (out.instr_offsets[instr_index] < offset) {
			instr_index++;
		}

		out.blocks[i].instr_index = instr_index;
	}

	instr_index = 0;
	for (uint32_t i = 0; i < out.function_count; i++) {
		uint32_t offset = out.functions[i].instr_index;
		while (out.instr_offsets[instr_index] < offset) {
			instr_index++;
		}

		out.functions[i].instr_index = instr_index;
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
		switch (operand.size) {
		case 1:
			stream_print(out, "byte");
			break;
		case 2:
			stream_print(out, "word");
			break;
		case 4:
			stream_print(out, "dword");
			break;
		case 8:
		case 0:
			stream_print(out, "qword");
			break;
		default:
			ASSERT(!"Invalid size");
		}

		stream_print(out, "[rsp+");
		stream_printu(out, operand.value * 8 + 8);
		stream_print(out, "]");
		break;
	case MOP_LABEL:
		stream_print(out, "L");
		stream_printu(out, operand.value);
		break;
	case MOP_MREG:
		reg = (enum x86_register)operand.value;
		stream_print(out, x86_get_register_name(reg, operand.size));
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
x86_generate(struct stream *out, struct machine_program program,
    struct allocation_info *info)
{
	stream_print(out,
	    "global main\n"
	    "extern printf\n\n"
	    "section .data\n"
	    "fmt: db \"%d\", 0x0A, 0\n\n"
	    "section .text\n");

	for (uint32_t function_index = 0; function_index < program.function_count; function_index++) {
		uint32_t first_instr = program.functions[function_index].instr_index;
		uint32_t last_instr = program.instr_count;
		if (function_index + 1 < program.function_count) {
			last_instr = program.functions[function_index+1].instr_index;
		}

		stream_prints(out, program.functions[function_index].name);
		stream_print(out, ":\n");

		for (uint32_t j = 0; j < LENGTH(x86_preserved_regs); j++) {
			uint32_t mreg = x86_preserved_regs[j];
			if (info[function_index].used[mreg]) {
				stream_print(out, "\tpush ");
				x86_emit_operand(out, make_mreg(mreg), program.functions);
				stream_print(out, "\n");
			}
		}

		uint32_t stack_size = info[function_index].spill_count * 8;
		if (stack_size > 0) {
			stream_print(out, "\tsub rsp, ");
			stream_printu(out, stack_size);
			stream_print(out, "\n");
		}

		for (uint32_t i = first_instr; i < last_instr; i++) {
			struct machine_instr *instr = get_instr(program, i);
			struct machine_operand *operands = (struct machine_operand *)(instr + 1);
			enum x86_opcode opcode = (enum x86_opcode)instr->opcode;
			uint32_t operand_count = instr->operand_count;

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

				if (opcode == X86_RET) {
					uint32_t j = LENGTH(x86_preserved_regs);
					while (j-- > 0) {
						uint32_t mreg = x86_preserved_regs[j];
						if (info[function_index].used[mreg]) {
							stream_print(out, "\tpop ");
							x86_emit_operand(out, make_mreg(mreg), program.functions);
							stream_print(out, "\n");
						}
					}

					if (stack_size > 0) {
						stream_print(out, "\tadd rsp, ");
						stream_printu(out, stack_size);
						stream_print(out, "\n");
					}
				}

				if (opcode == X86_MOV && machine_operand_equals(operands[0], operands[1])) {
					continue;
				}

				if (operands[0].kind == MOP_SPILL
				    && operands[1].kind == MOP_SPILL) {
					stream_print(out, "\tmov rax, ");
					x86_emit_operand(out, operands[1], program.functions);
					operands[1] = make_mreg(X86_RAX);
					stream_print(out, "\n");
				}

				stream_print(out, "\t");
				stream_print(out, x86_get_opcode_name(opcode));
				while (operand_count-- > 0) {
					stream_print(out, " ");
					x86_emit_operand(out, *operands++, program.functions);
					if (operand_count > 0) {
						stream_print(out, ",");
					}
				}

				stream_print(out, "\n");
			}
		}
	}
}
