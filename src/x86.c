static void
push_instr(machine_program *program, u32 opcode, u32 operand_count)
{
	machine_instr instr = {0};
	instr.opcode = opcode;
	instr.operand_count = operand_count;
	ASSERT(program->size + sizeof(instr) + operand_count
		* sizeof(machine_operand) <= program->max_size);
	memcpy((char *)program->code + program->size, &instr, sizeof(instr));
	program->size += sizeof(instr);
}

static void
push_operand(machine_program *program, machine_operand operand)
{
	memcpy((char *)program->code + program->size, &operand, sizeof(operand));
	program->size += sizeof(operand);
}

static void
x86_select0(machine_program *out, x86_opcode opcode)
{
	push_instr(out, opcode, 0);
}

static void
x86_select1(machine_program *out, x86_opcode opcode, machine_operand dst)
{
	dst.flags |= MOP_DEF | MOP_USE;

	switch (opcode) {
	case X86_IDIV:
		{
			push_instr(out, opcode, 3);
			push_operand(out, dst);

			machine_operand op0 = make_mreg(X86_RAX);
			op0.flags |= MOP_DEF | MOP_USE;
			push_operand(out, op0);

			machine_operand op1 = make_mreg(X86_RDX);
			op1.flags |= MOP_DEF | MOP_USE;
			push_operand(out, op1);
		} break;
	case X86_IMUL:
		{
			push_instr(out, opcode, 3);
			push_operand(out, dst);

			machine_operand op0 = make_mreg(X86_RAX);
			op0.flags |= MOP_DEF | MOP_USE;
			push_operand(out, op0);

			machine_operand op1 = make_mreg(X86_RDX);
			op1.flags |= MOP_DEF;
			push_operand(out, op1);
		} break;
	default:
		{
			push_instr(out, opcode, 1);
			push_operand(out, dst);
		} break;
	}
}

static void
x86_select2(machine_program *out, x86_opcode opcode,
	machine_operand dst, machine_operand src)
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
			if (dst.kind == MOP_VREG || dst.kind == MOP_MREG) {
				dst.flags |= MOP_DEF;
			} else {
				dst.flags |= MOP_USE;
			}
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

static void
x86_select3(machine_program *out, x86_opcode opcode,
	machine_operand dst, machine_operand base, machine_operand offset)
{
	if (opcode == X86_STORE) {
		dst.flags |= MOP_USE;
	} else {
		dst.flags |= MOP_DEF;
	}

	base.flags |= MOP_USE;
	offset.flags |= MOP_USE;

	push_instr(out, opcode, 3);
	push_operand(out, dst);
	push_operand(out, base);
	push_operand(out, offset);
}

static b32
x86_is_comparison_opcode(ir_opcode ir_opcode)
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

static x86_opcode
x86_get_setcc_opcode(ir_opcode ir_opcode)
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

static x86_opcode
x86_get_jcc_opcode(ir_opcode ir_opcode)
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

static void x86_select_instr(machine_program *out,
	ir_instr *instr, u32 instr_index, machine_operand dst);

static machine_operand
x86_select_immediate(machine_program *out,
	ir_instr *instr, u32 instr_index)
{
	machine_operand result;
	if (instr[instr_index].opcode == IR_CONST) {
		result = make_immediate(instr[instr_index].op0);
	} else {
		result = make_vreg(instr_index);
		x86_select_instr(out, instr, instr_index, result);
	}

	return result;
}

static void
x86_alloc(machine_program *out, u32 id, u32 size)
{
	(void)out;
	(void)id;
	(void)size;
}

static void
x86_select_instr(machine_program *out, ir_instr *instr,
	u32 instr_index, machine_operand dst)
{
	u32 size = instr[instr_index].size;
	u32 op0 = instr[instr_index].op0;
	u32 op1 = instr[instr_index].op1;
	ir_opcode opcode = instr[instr_index].opcode;
	switch (opcode) {
	case IR_VAR:
		{
			machine_operand src = make_vreg(instr_index);
			x86_select2(out, X86_MOV, dst, src);
		} break;
	case IR_CONST:
		{
			machine_operand src = make_immediate(op0);
			x86_select2(out, X86_MOV, dst, src);
		} break;
	case IR_ALLOC:
		{
			machine_operand src = make_spill(op1);
			x86_select2(out, X86_LEA, dst, src);
		} break;
	case IR_COPY:
		{
			x86_select_instr(out, instr, op0, dst);
		} break;
	case IR_MOV:
		{
			x86_select_instr(out, instr, op1, dst);
		} break;
	case IR_LOAD:
		{
			machine_operand src = make_vreg(op0);
			ASSERT(!machine_operand_equals(src, dst));
			if (instr[op0].opcode == IR_ALLOC) {
				u32 addr = instr[op0].op1;
				src = make_spill(addr);
				src.size = size;
				x86_select2(out, X86_MOV, dst, src);
			} else if (instr[op0].opcode == IR_ADD
				&& instr[instr[op0].op0].opcode == IR_ALLOC
				&& instr[instr[op0].op1].opcode == IR_CONST)
			{
				u32 base = instr[instr[op0].op0].op1;
				u32 offset = instr[instr[op0].op1].op0;
				src = make_spill(base + offset);
				src.size = size;
				x86_select2(out, X86_MOV, dst, src);
			} else {
				x86_select_instr(out, instr, op0, src);
				src.size = dst.size = size;
				x86_select2(out, X86_LOAD, dst, src);
			}
		} break;
	case IR_STORE:
		{
			machine_operand src = make_vreg(op1);
			ASSERT(!machine_operand_equals(src, dst));
			if (instr[op1].opcode != IR_CONST) {
				x86_select_instr(out, instr, op1, src);
			} else {
				src = make_immediate(instr[op1].op0);
			}

			if (instr[op0].opcode == IR_ADD
				&& instr[instr[op0].op0].opcode == IR_ALLOC
				&& instr[instr[op0].op1].opcode == IR_CONST)
			{
				u32 base = instr[instr[op0].op0].op1;
				u32 offset = instr[instr[op0].op1].op0;
				machine_operand addr = make_spill(base + offset);
				addr.size = size;
				x86_select2(out, X86_MOV, addr, src);
			} else if (instr[op0].opcode == IR_ALLOC) {
				u32 offset = instr[op0].op1;
				machine_operand addr = make_spill(offset);
				addr.size = size;
				x86_select2(out, X86_MOV, addr, src);
			} else {
				x86_select_instr(out, instr, op0, dst);
				x86_select2(out, X86_STORE, dst, src);
			}
		} break;
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
			machine_operand src = make_vreg(op1);
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
			machine_operand src = make_vreg(op1);
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
			machine_operand rax = make_mreg(X86_RAX);
			machine_operand src = make_vreg(op1);
			x86_select_instr(out, instr, op0, rax);
			x86_select_instr(out, instr, op1, src);
			x86_select1(out, X86_IMUL, src);
			x86_select2(out, X86_MOV, dst, rax);
		}
		break;
	case IR_DIV:
	case IR_MOD:
		{
			machine_operand rax = make_mreg(X86_RAX);
			machine_operand rcx = make_mreg(X86_RCX);
			machine_operand rdx = make_mreg(X86_RDX);

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
	case IR_SHL:
	case IR_SHR:
		{
			machine_operand shift = make_mreg(X86_RCX);
			shift.size = 1;

			x86_select_instr(out, instr, op0, dst);
			x86_select_instr(out, instr, op1, shift);
			x86_select2(out, opcode == IR_SHL ? X86_SHL : X86_SHR, dst, shift);
		} break;
	case IR_JMP:
		op0 = instr[op0].op0;
		x86_select1(out, X86_JMP, make_label(op0));
		break;
	case IR_JIZ:
	case IR_JNZ:
		{
			x86_opcode x86_opcode = opcode == IR_JIZ ? X86_JZ : X86_JNZ;
			if (x86_is_comparison_opcode(instr[op0].opcode)) {
				dst = make_vreg(instr[op0].op0);
				x86_select_instr(out, instr, instr[op0].op0, dst);
				machine_operand src = x86_select_immediate(out, instr, instr[op0].op1);
				x86_select2(out, X86_CMP, dst, src);
				x86_opcode = x86_get_jcc_opcode(instr[op0].opcode);
			} else if (instr[op0].opcode == IR_SUB) {
				machine_operand src = {0};
				x86_select_instr(out, instr, op0, src);
			} else {
				machine_operand src = make_vreg(op0);
				x86_select_instr(out, instr, op0, src);
				x86_select2(out, X86_TEST, src, src);
			}

			op1 = instr[op1].op0;
			x86_select1(out, x86_opcode, make_label(op1));
		} break;
	case IR_RET:
		{
			machine_operand rax = make_mreg(X86_RAX);
			x86_select_instr(out, instr, op0, rax);
			x86_select0(out, X86_RET);
		} break;
	case IR_CALL:
		{
			for (u32 i = 1; i <= op1; i++) {
				ASSERT(instr[instr_index - i].opcode == IR_PARAM);
				machine_operand src = make_vreg(instr[instr_index - i].op0);
				u32 parameter_index = i - 1;
				switch (parameter_index) {
				case 0:
					{
						machine_operand rdi = make_mreg(X86_RDI);
						x86_select_instr(out, instr, src.value, rdi);
					} break;
				case 1:
					{
						machine_operand rsi = make_mreg(X86_RSI);
						x86_select_instr(out, instr, src.value, rsi);
					} break;
				case 2:
					{
						machine_operand rdx = make_mreg(X86_RDX);
						x86_select_instr(out, instr, src.value, rdx);
					} break;
				case 3:
					{
						machine_operand rcx = make_mreg(X86_RCX);
						x86_select_instr(out, instr, src.value, rcx);
					} break;
				default:
					ASSERT(!"Too many arguments");
					break;
				}
			}

			machine_operand rax = make_mreg(X86_RAX);
			x86_select1(out, X86_CALL, make_func(op0));
			x86_select2(out, X86_MOV, dst, rax);
		} break;
	case IR_PRINT:
		{
			machine_operand rsi = make_mreg(X86_RSI);
			machine_operand rdi = make_mreg(X86_RDI);
			machine_operand rax = make_mreg(X86_RAX);

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

static machine_program
x86_select_instructions(ir_program program, arena *arena)
{
	machine_program out = {0};
	out.max_size = 1024 * 8;
	out.blocks = ALLOC(arena, program.block_count, machine_block);
	out.functions = ALLOC(arena, program.function_count, machine_function);
	out.function_count = program.function_count;
	out.block_count = program.block_count;
	out.code = alloc(arena, out.max_size, 1);
	out.vreg_count = program.register_count;
	out.mreg_count = X86_REGISTER_COUNT;

	out.temp_mregs = x86_temp_regs;
	out.temp_mreg_count = LENGTH(x86_temp_regs);

	b8 *is_toplevel = get_toplevel_instructions(program, arena);

	for (u32 f = 0; f < program.function_count; f++) {
		ir_function ir_function = program.functions[f];
		out.functions[f].name = ir_function.name;
		out.functions[f].block_index = ir_function.block_index;
		out.functions[f].instr_index = out.size;
		out.functions[f].stack_size = ir_function.stack_size;

		for (u32 i = 0; i < ir_function.parameter_count; i++) {
			machine_operand dst = make_vreg(ir_function.instr_index+i);
			machine_operand src;
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

		u32 first_block = ir_function.block_index;
		u32 last_block = first_block + ir_function.block_count;
		for (u32 b = first_block; b < last_block; b++) {
			ir_block block = program.blocks[b];
			out.blocks[b].instr_index = out.size;
			for (u32 i = block.start; i < block.start + block.size; i++) {
				ir_instr instr = program.instrs[i];
				machine_operand dst = make_vreg(i);
				if (instr.opcode == IR_MOV || instr.opcode == IR_STORE) {
					dst = make_vreg(instr.op0);
				}

				if (is_toplevel[i]) {
					x86_select_instr(&out, program.instrs, i, dst);
				}
			}
		}
	}

	out.instr_count = 0;
	char *start = out.code;
	char *code = start;
	char *end = start + out.size;
	while (code < end) {
		out.instr_count++;
		code += get_instr_size(*(machine_instr *)code);
	}

	out.instr_offsets = ALLOC(arena, out.instr_count, u32);
	u32 instr_index = 0;
	code = start;
	while (code < end) {
		out.instr_offsets[instr_index++] = code - start;
		code += get_instr_size(*(machine_instr *)code);
	}

	/* Convert instruction offset to instruction index */
	instr_index = 0;
	for (u32 i = 0; i < out.block_count; i++) {
		u32 offset = out.blocks[i].instr_index;
		while (out.instr_offsets[instr_index] < offset) {
			instr_index++;
		}

		out.blocks[i].instr_index = instr_index;
	}

	instr_index = 0;
	for (u32 i = 0; i < out.function_count; i++) {
		u32 offset = out.functions[i].instr_index;
		while (out.instr_offsets[instr_index] < offset) {
			instr_index++;
		}

		out.functions[i].instr_index = instr_index;
	}


	return out;
}

static void
x86_emit_operand(stream *out, machine_operand operand,
	machine_function *functions)
{
	x86_register reg;

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
		stream_printu(out, operand.value);
		stream_print(out, "]");
		break;
	case MOP_LABEL:
		stream_print(out, "L");
		stream_printu(out, operand.value);
		break;
	case MOP_MREG:
		reg = (x86_register)operand.value;
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

static void
x86_emit_operand_indirect(stream *out, machine_operand operand,
	machine_function *functions)
{
	switch (operand.kind) {
	case MOP_MREG:
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
			stream_print(out, "qword");
			break;
		default:
			ASSERT(!"Invalid size");
		}

		stream_print(out, "[");
		operand.size = 8;
		x86_emit_operand(out, operand, functions);
		stream_print(out, "]");
		break;
	default:
		x86_emit_operand(out, operand, functions);
	}
}

static b32
x86_is_setcc(x86_opcode opcode)
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
x86_generate(stream *out, machine_program program, allocation_info *info)
{
	stream_print(out,
		"global main\n"
		"extern printf\n\n"
		"section .data\n"
		"fmt: db \"%d\", 0x0A, 0\n\n"
		"section .text\n");

	for (u32 function_index = 0; function_index < program.function_count; function_index++) {
		u32 first_instr = program.functions[function_index].instr_index;
		u32 last_instr = program.instr_count;
		if (function_index + 1 < program.function_count) {
			last_instr = program.functions[function_index+1].instr_index;
		}

		stream_prints(out, program.functions[function_index].name);
		stream_print(out, ":\n");

		u32 used_volatile_register_count = 0;
		for (u32 j = 0; j < LENGTH(x86_preserved_regs); j++) {
			u32 mreg = x86_preserved_regs[j];
			if (info[function_index].used[mreg]) {
				stream_print(out, "\tpush ");
				x86_emit_operand(out, make_mreg(mreg), program.functions);
				stream_print(out, "\n");
				used_volatile_register_count++;
			}
		}

		u32 stack_size = program.functions[function_index].stack_size;
		stack_size += 8 * info[function_index].spill_count;

		u32 total_stack_size = (stack_size + 8 * used_volatile_register_count);
		b32 stack_is_aligned = ((total_stack_size & 15) == 0);
		if (!stack_is_aligned) {
			stack_size += (total_stack_size & 15);
		}

		if (stack_size > 0) {
			stream_print(out, "\tsub rsp, ");
			stream_printu(out, stack_size);
			stream_print(out, "\n");
		}

		for (u32 i = first_instr; i < last_instr; i++) {
			machine_instr *instr = get_instr(program, i);
			machine_operand *operands = (machine_operand *)(instr + 1);
			x86_opcode opcode = (x86_opcode)instr->opcode;
			u32 operand_count = instr->operand_count;

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
					if (stack_size > 0) {
						stream_print(out, "\tadd rsp, ");
						stream_printu(out, stack_size);
						stream_print(out, "\n");
					}

					u32 j = LENGTH(x86_preserved_regs);
					while (j-- > 0) {
						u32 mreg = x86_preserved_regs[j];
						if (info[function_index].used[mreg]) {
							stream_print(out, "\tpop ");
							x86_emit_operand(out, make_mreg(mreg), program.functions);
							stream_print(out, "\n");
						}
					}
				} else if (opcode == X86_MOV) {
					if  (machine_operand_equals(operands[0], operands[1])) {
						continue;
					}
				}

				if (opcode == X86_LOAD && operand_count == 3) {
					stream_print(out, "\tmov ");
					x86_emit_operand(out, operands[0], program.functions);
					stream_print(out, ", [");
					x86_emit_operand(out, operands[1], program.functions);
					stream_print(out, " + ");
					x86_emit_operand(out, operands[1], program.functions);
					stream_print(out, "]\n");
				} else if (opcode == X86_STORE && operand_count == 3) {
					stream_print(out, "\tmov [");
					x86_emit_operand(out, operands[0], program.functions);
					stream_print(out, " + ");
					x86_emit_operand(out, operands[1], program.functions);
					stream_print(out, "], ");
					x86_emit_operand(out, operands[1], program.functions);
					stream_print(out, "\n");
				} else if (opcode == X86_STORE) {
					stream_print(out, "\tmov ");
					x86_emit_operand_indirect(out, operands[0], program.functions);
					stream_print(out, ", ");
					x86_emit_operand(out, operands[1], program.functions);
					stream_print(out, "\n");
				} else if (opcode == X86_LOAD) {
					stream_print(out, "\tmov ");
					x86_emit_operand(out, operands[0], program.functions);
					stream_print(out, ", ");
					x86_emit_operand_indirect(out, operands[1], program.functions);
					stream_print(out, "\n");
				} else {
					if (operands[0].kind == MOP_SPILL
						&& operands[1].kind == MOP_SPILL)
					{
						stream_print(out, "\tmov rax, ");
						x86_emit_operand(out, operands[1], program.functions);
						operands[1] = make_mreg(X86_RAX);
						stream_print(out, "\n");
					}

					stream_print(out, "\t");
					stream_print(out, x86_get_opcode_name(opcode));
					for (u32 j = 0; j < operand_count; j++) {
						stream_print(out, " ");
						x86_emit_operand(out, operands[j], program.functions);
						b32 is_last_operand = (j + 1 == operand_count);
						if (!is_last_operand) {
							stream_print(out, ",");
						}
					}

					stream_print(out, "\n");
				}
			}
		}
	}
}
