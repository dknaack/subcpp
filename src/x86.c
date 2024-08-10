static void
push_inst(machine_program *program, u32 opcode, u32 operand_count)
{
	machine_inst inst = {0};
	inst.opcode = opcode;
	inst.operand_count = operand_count;
	ASSERT(program->size + sizeof(inst) + operand_count
		* sizeof(machine_operand) <= program->max_size);
	memcpy((char *)program->code + program->size, &inst, sizeof(inst));
	program->size += sizeof(inst);
	program->inst_count++;
}

static void
push_operand(machine_program *program, machine_operand operand)
{
	machine_function *func = &program->functions[program->function_count - 1];
	ASSERT(operand.kind != MOP_VREG || operand.value < func->register_count);
	memcpy((char *)program->code + program->size, &operand, sizeof(operand));
	program->size += sizeof(operand);
}

static void
x86_select1(machine_program *out, x86_opcode opcode, machine_operand dst)
{
	dst.flags |= MOP_DEF | MOP_USE;

	switch (opcode) {
	case X86_IDIV:
		{
			push_inst(out, opcode, 3);
			push_operand(out, dst);

			machine_operand op0 = make_operand(MOP_MREG, X86_RAX, dst.size);
			op0.flags |= MOP_DEF | MOP_USE | MOP_IMPLICIT;
			push_operand(out, op0);

			machine_operand op1 = make_operand(MOP_MREG, X86_RDX, dst.size);
			op1.flags |= MOP_DEF | MOP_USE | MOP_IMPLICIT;
			push_operand(out, op1);
		} break;
	case X86_IMUL:
		{
			push_inst(out, opcode, 3);
			push_operand(out, dst);

			machine_operand op0 = make_operand(MOP_MREG, X86_RAX, dst.size);
			op0.flags |= MOP_DEF | MOP_USE | MOP_IMPLICIT;
			push_operand(out, op0);

			machine_operand op1 = make_operand(MOP_MREG, X86_RDX, dst.size);
			op1.flags |= MOP_DEF | MOP_IMPLICIT;
			push_operand(out, op1);
		} break;
	default:
		{
			push_inst(out, opcode, 1);
			push_operand(out, dst);
		} break;
	}
}

static void
x86_select2(machine_program *out, x86_opcode opcode,
	machine_operand dst, machine_operand src)
{
	ASSERT(dst.kind != 0 && src.kind != 0);
	ASSERT(dst.size > 0 && src.size > 0);

	switch (opcode) {
	case X86_MOV:
		if (!equals_operand(dst, src)) {
			push_inst(out, opcode, 2);
			if (dst.flags & MOP_INDIRECT) {
				dst.flags |= MOP_USE;
			} else {
				dst.flags |= MOP_DEF;
			}

			push_operand(out, dst);
			src.flags |= MOP_USE;
			push_operand(out, src);
			// Why was this here?
			//ASSERT(!(dst.flags & MOP_INDIRECT));
		}
		break;
	case X86_CMP:
		push_inst(out, opcode, 2);
		dst.flags |= MOP_USE;
		push_operand(out, dst);
		src.flags |= MOP_USE;
		push_operand(out, src);
		break;
	default:
		push_inst(out, opcode, 2);
		dst.flags |= MOP_DEF | MOP_USE;
		push_operand(out, dst);
		src.flags |= MOP_USE;
		push_operand(out, src);
	}
}

static x86_opcode
x86_get_setcc_opcode(ir_opcode ir_opcode)
{
	switch (ir_opcode) {
	case IR_EQL:  return X86_SETZ;
	case IR_LT:   return X86_SETL;
	case IR_GT:   return X86_SETG;
	case IR_LEQ:  return X86_SETLE;
	case IR_GEQ:  return X86_SETGE;
	case IR_LTU:  return X86_SETB;
	case IR_GTU:  return X86_SETA;
	case IR_LEQU: return X86_SETAE;
	case IR_GEQU: return X86_SETBE;
	default:
		ASSERT(!"Not a comparison operator");
		return X86_SETZ;
	}
}

static x86_opcode
x86_get_jcc_opcode(ir_opcode ir_opcode, bool is_jiz)
{
	if (is_jiz) {
		switch (ir_opcode) {
		case IR_EQL:  return X86_JNZ;
		case IR_LT:   return X86_JGE;
		case IR_GT:   return X86_JLE;
		case IR_LEQ:  return X86_JG;
		case IR_GEQ:  return X86_JL;
		case IR_LTU:  return X86_JAE;
		case IR_GTU:  return X86_JBE;
		case IR_LEQU: return X86_JA;
		case IR_GEQU: return X86_JB;
		default:
			ASSERT(!"Not a comparison operator");
			return X86_SETZ;
		}
	} else {
		switch (ir_opcode) {
		case IR_EQL:  return X86_JZ;
		case IR_LT:   return X86_JL;
		case IR_GT:   return X86_JG;
		case IR_LEQ:  return X86_JLE;
		case IR_GEQ:  return X86_JGE;
		case IR_LTU:  return X86_JB;
		case IR_GTU:  return X86_JA;
		case IR_LEQU: return X86_JBE;
		case IR_GEQU: return X86_JAE;
		default:
			ASSERT(!"Not a comparison operator");
			return X86_SETZ;
		}
	}
}

static void x86_select_inst(machine_program *out,
	ir_inst *inst, u32 inst_index, machine_operand dst);

static machine_operand
x86_select_immediate(machine_program *out,
	ir_inst *inst, u32 inst_index)
{
	machine_operand result;
	u32 size = ir_sizeof(inst[inst_index].type);
	if (inst[inst_index].opcode == IR_CONST) {
		result = make_operand(MOP_IMMEDIATE, inst[inst_index].op0, size);
	} else {
		result = make_operand(MOP_VREG, inst_index, size);
		x86_select_inst(out, inst, inst_index, result);
	}

	return result;
}

static void
x86_select_inst(machine_program *out, ir_inst *inst,
	u32 inst_index, machine_operand dst)
{
	ir_type type = inst[inst_index].type;
	u32 size = ir_sizeof(type);
	u32 op0 = inst[inst_index].op0;
	u32 op1 = inst[inst_index].op1;
	ir_opcode opcode = inst[inst_index].opcode;
	b32 is_float = (type == IR_F32 || type == IR_F64);

	switch (opcode) {
	case IR_GLOBAL:
		{
			machine_operand src = make_global(op0);
			x86_select2(out, X86_MOV, dst, src);
		} break;
	case IR_VAR:
		{
			machine_operand src = make_operand(MOP_VREG, inst_index, size);
			src.size = size;
			if (type == IR_F32 || type == IR_F64) {
				x86_select2(out, X86_MOVSS, dst, src);
			} else {
				x86_select2(out, X86_MOV, dst, src);
			}
		} break;
	case IR_CAST:
	case IR_CASTU:
		{
			ir_type op0_type = inst[op0].type;
			machine_operand src = make_operand(MOP_VREG, op0, ir_sizeof(op0_type));

			x86_select_inst(out, inst, op0, src);
			if (type == IR_F32) {
				x86_select2(out, X86_CVTSI2SS, dst, src);
			} else if (type == IR_F64) {
				x86_select2(out, X86_CVTSI2SD, dst, src);
			} else if (op0_type == IR_F32) {
				src.size = 8;
				x86_select2(out, X86_CVTTSS2SI, dst, src);
			} else if (op0_type == IR_F64) {
				src.size = 8;
				x86_select2(out, X86_CVTTSD2SI, dst, src);
			}

			if (type == IR_I8) {
			}
		} break;
	case IR_CONST:
		{
			if (type == IR_F32 || type == IR_F64) {
				machine_operand src = make_float(op0);
				x86_select2(out, X86_MOVSS, dst, src);
			} else {
				machine_operand src = make_operand(MOP_IMMEDIATE, op0, size);
				x86_select2(out, X86_MOV, dst, src);
			}
		} break;
	case IR_BUILTIN:
		{
			ASSERT(!"Builtins should be handled in their respective instruction");
		} break;
	case IR_ALLOC:
		{
			machine_operand src = make_spill(op1);
			x86_select2(out, X86_LEA, dst, src);
		} break;
	case IR_COPY:
		{
			x86_select_inst(out, inst, op0, dst);
		} break;
	case IR_MOV:
		{
			x86_select_inst(out, inst, op1, dst);
		} break;
	case IR_LOAD:
		{
			machine_operand src = make_operand(MOP_VREG, op0, ir_sizeof(inst[op0].type));
			b32 is_float = (type == IR_F32 || type == IR_F64);
			x86_opcode x86_opcode = (is_float ? X86_MOVSS : X86_MOV);
			if (is_float) {
				dst.flags |= MOP_ISFLOAT;
			}

			ASSERT(!equals_operand(src, dst));
			if (inst[op0].opcode == IR_ALLOC) {
				u32 addr = inst[op0].op1;
				src = make_spill(addr);
				src.size = size;

				x86_select2(out, x86_opcode, dst, src);
			} else if (inst[op0].opcode == IR_ADD
				&& inst[inst[op0].op0].opcode == IR_ALLOC
				&& inst[inst[op0].op1].opcode == IR_CONST)
			{
				u32 base = inst[inst[op0].op0].op1;
				u32 offset = inst[inst[op0].op1].op0;
				src = make_spill(base + offset);
				src.size = size;

				x86_select2(out, x86_opcode, dst, src);
			} else {
				x86_select_inst(out, inst, op0, src);
				src.size = dst.size = size;
				src.flags |= MOP_INDIRECT;
				x86_select2(out, x86_opcode, dst, src);
			}
		} break;
	case IR_STORE:
		{
			machine_operand src = make_operand(MOP_VREG, op1, ir_sizeof(inst[op1].type));
			x86_opcode x86_opcode = (is_float ? X86_MOVSS : X86_MOV);

			ASSERT(!equals_operand(src, dst));
			if (inst[op1].opcode != IR_CONST) {
				x86_select_inst(out, inst, op1, src);
			} else {
				src = make_operand(MOP_IMMEDIATE, inst[op1].op0, size);
			}

			src.size = ir_sizeof(inst[op1].type);
			if (inst[op0].opcode == IR_ADD
				&& inst[inst[op0].op0].opcode == IR_ALLOC
				&& inst[inst[op0].op1].opcode == IR_CONST)
			{
				u32 base = inst[inst[op0].op0].op1;
				u32 offset = inst[inst[op0].op1].op0;
				machine_operand addr = make_spill(base + offset);
				addr.size = size;
				x86_select2(out, x86_opcode, addr, src);
			} else if (inst[op0].opcode == IR_SUB
				&& inst[inst[op0].op0].opcode == IR_ALLOC
				&& inst[inst[op0].op1].opcode == IR_CONST)
			{
				u32 base = inst[inst[op0].op0].op1;
				u32 offset = inst[inst[op0].op1].op0;
				machine_operand addr = make_spill(base - offset);
				addr.size = size;
				x86_select2(out, x86_opcode, addr, src);
			} else if (inst[op0].opcode == IR_ALLOC) {
				u32 offset = inst[op0].op1;
				machine_operand addr = make_spill(offset);
				addr.size = size;
				x86_select2(out, x86_opcode, addr, src);
			} else {
				x86_select_inst(out, inst, op0, dst);
				dst.flags |= MOP_INDIRECT;
				dst.size = src.size;
				x86_select2(out, x86_opcode, dst, src);
			}
		} break;
	case IR_ADD:
		{
			if (is_float) {
				machine_operand src = make_operand(MOP_VREG, op1, ir_sizeof(inst[op1].type));
				src.flags |= MOP_ISFLOAT;
				dst.flags |= MOP_ISFLOAT;

				x86_select_inst(out, inst, op0, dst);
				x86_select_inst(out, inst, op1, src);
				x86_select2(out, X86_ADDSS, dst, src);
			} else {
				if (inst[op1].opcode == IR_CONST && inst[op1].op0 == 1) {
					x86_select_inst(out, inst, op0, dst);
					x86_select1(out, X86_INC, dst);
				} else if (inst[op1].opcode == IR_CONST) {
					x86_select_inst(out, inst, op0, dst);
					op1 = inst[op1].op0;
					machine_operand src = make_operand(MOP_IMMEDIATE, op1, size);
					x86_select2(out, X86_ADD, dst, src);
				} else if (inst[op0].opcode == IR_CONST) {
					x86_select_inst(out, inst, op1, dst);
					op0 = inst[op0].op0;
					machine_operand src = make_operand(MOP_IMMEDIATE, op0, size);
					x86_select2(out, X86_ADD, dst, src);
				} else {
					machine_operand src = make_operand(MOP_VREG, op1, ir_sizeof(inst[op1].type));
					x86_select_inst(out, inst, op0, dst);
					x86_select_inst(out, inst, op1, src);
					x86_select2(out, X86_ADD, dst, src);
				}
			}
		}
		break;
	case IR_SUB:
		if (is_float) {
			ASSERT(opcode != IR_MOD);

			machine_operand src = make_operand(MOP_VREG, op1, ir_sizeof(inst[op1].type));
			dst.flags |= MOP_ISFLOAT;
			src.flags |= MOP_ISFLOAT;
			x86_select2(out, X86_SUBSS, dst, src);
		} else {
			if (inst[op1].opcode == IR_CONST && inst[op1].op0 == 1) {
				x86_select_inst(out, inst, op0, dst);
				x86_select1(out, X86_DEC, dst);
			} else if (inst[op0].opcode == IR_CONST && inst[op0].op0 == 0) {
				x86_select_inst(out, inst, op1, dst);
				x86_select1(out, X86_NEG, dst);
			} else if (inst[op1].opcode == IR_CONST) {
				isize src_size = ir_sizeof(inst[op1].type);
				op1 = inst[op1].op0;
				x86_select_inst(out, inst, op0, dst);
				machine_operand src = make_operand(MOP_IMMEDIATE, op1, src_size);
				x86_select2(out, X86_SUB, dst, src);
			} else {
				machine_operand src = make_operand(MOP_VREG, op1, ir_sizeof(inst[op1].type));
				x86_select_inst(out, inst, op0, dst);
				x86_select_inst(out, inst, op1, src);
				x86_select2(out, X86_SUB, dst, src);
			}
		}
		break;
	case IR_MUL:
		if (is_float) {
			machine_operand src = make_operand(MOP_VREG, op1, ir_sizeof(inst[op1].type));
			dst.flags |= MOP_ISFLOAT;
			src.flags |= MOP_ISFLOAT;
			x86_select2(out, X86_MULSS, dst, src);
		} else {
			if (inst[op1].opcode == IR_CONST && inst[op1].op0 == 1) {
				x86_select_inst(out, inst, op0, dst);
			} else if (inst[op1].opcode == IR_CONST && inst[op1].op0 == 2) {
				x86_select_inst(out, inst, op0, dst);
				x86_select2(out, X86_ADD, dst, dst);
			} else {
				machine_operand rax = make_operand(MOP_MREG, X86_RAX, ir_sizeof(inst[op0].type));
				machine_operand src = make_operand(MOP_VREG, op1, ir_sizeof(inst[op1].type));

				x86_select_inst(out, inst, op0, rax);
				x86_select_inst(out, inst, op1, src);
				x86_select1(out, X86_IMUL, src);
				x86_select2(out, X86_MOV, dst, rax);
			}
		}
		break;
	case IR_DIV:
	case IR_MOD:
		if (is_float) {
			ASSERT(opcode != IR_MOD);

			machine_operand src = make_operand(MOP_VREG, op1, ir_sizeof(inst[op1].type));
			dst.flags |= MOP_ISFLOAT;
			src.flags |= MOP_ISFLOAT;
			x86_select2(out, X86_DIVSS, dst, src);
		} else {
			machine_operand rax = make_operand(MOP_MREG, X86_RAX, ir_sizeof(inst[op0].type));
			machine_operand rcx = make_operand(MOP_MREG, X86_RCX, ir_sizeof(inst[op1].type));
			machine_operand rdx = make_operand(MOP_MREG, X86_RDX, dst.size);
			machine_operand zero = make_operand(MOP_IMMEDIATE, 0, dst.size);

			x86_select_inst(out, inst, op0, rax);
			x86_select_inst(out, inst, op1, rcx);
			x86_select2(out, X86_MOV, rdx, zero);
			x86_select1(out, X86_IDIV, rcx);
			x86_select2(out, X86_MOV, dst, opcode == IR_DIV ? rax : rdx);
		} break;
	case IR_EQL:
	case IR_LT:
	case IR_GT:
	case IR_GEQ:
	case IR_LEQ:
	case IR_LTU:
	case IR_GTU:
	case IR_GEQU:
	case IR_LEQU:
		{
			x86_opcode x86_opcode = is_float ? X86_COMISS : X86_CMP;
			machine_operand dst_byte = dst;
			dst_byte.size = 1;

			machine_operand src = make_operand(MOP_VREG, op1, ir_sizeof(inst[op1].type));
			if (is_float) {
				dst.flags |= MOP_ISFLOAT;
				src.flags |= MOP_ISFLOAT;
			}

			x86_select_inst(out, inst, op0, dst);
			x86_select_inst(out, inst, op1, src);
			x86_select2(out, x86_opcode, dst, src);
			x86_select1(out, x86_get_setcc_opcode(opcode), dst_byte);
			x86_select2(out, X86_MOVZX, dst, dst_byte);
		} break;
	case IR_SHL:
	case IR_SHR:
		{
			machine_operand shift = make_operand(MOP_MREG, X86_RCX, 1);

			x86_select_inst(out, inst, op0, dst);
			x86_select_inst(out, inst, op1, shift);
			x86_select2(out, opcode == IR_SHL ? X86_SHL : X86_SHR, dst, shift);
		} break;
	case IR_AND:
	case IR_OR:
	case IR_XOR:
		{
			machine_operand src = make_operand(MOP_VREG, op1, ir_sizeof(inst[op1].type));
			x86_opcode x86_opcode =
				opcode == IR_AND ? X86_AND :
				opcode == IR_OR ? X86_OR : X86_XOR;

			if (inst[op1].opcode == IR_CONST) {
				src = make_operand(MOP_IMMEDIATE, inst[op1].op0, src.size);
				x86_select_inst(out, inst, op0, dst);
				x86_select2(out, x86_opcode, dst, src);
			} else if (inst[op0].opcode == IR_CONST) {
				src = make_operand(MOP_IMMEDIATE, inst[op0].op0, src.size);
				x86_select_inst(out, inst, op1, dst);
				x86_select2(out, x86_opcode, dst, src);
			} else {
				x86_select_inst(out, inst, op0, dst);
				x86_select_inst(out, inst, op1, src);
				x86_select2(out, x86_opcode, dst, src);
			}
		} break;
	case IR_NOT:
		{
			x86_select_inst(out, inst, op0, dst);
			x86_select1(out, X86_NOT, dst);
		} break;
	case IR_JMP:
		x86_select1(out, X86_JMP, make_label(op0));
		break;
	case IR_JIZ:
	case IR_JNZ:
		{
			b32 is_jiz = opcode == IR_JIZ;
			x86_opcode jcc = is_jiz ? X86_JZ : X86_JNZ;
			if (is_comparison_opcode(inst[op0].opcode)) {
				dst = make_operand(MOP_VREG, inst[op0].op0, ir_sizeof(inst[op0].type));
				x86_select_inst(out, inst, inst[op0].op0, dst);
				machine_operand src = x86_select_immediate(out, inst, inst[op0].op1);
				src.size = dst.size;
				if (is_float) {
					dst.flags |= MOP_ISFLOAT;
					src.flags |= MOP_ISFLOAT;
				}

				x86_opcode cmp_opcode = (is_float ? X86_COMISS : X86_CMP);
				x86_select2(out, cmp_opcode, dst, src);
				jcc = x86_get_jcc_opcode(inst[op0].opcode, is_jiz);
			} else {
				machine_operand src = make_operand(MOP_VREG, op0, ir_sizeof(inst[op0].type));
				x86_select_inst(out, inst, op0, src);
				ASSERT(src.size > 0 && src.size <= 8);
				x86_select2(out, X86_TEST, src, src);
			}

			x86_select1(out, jcc, make_label(op1));
		} break;
	case IR_RET:
		{
			machine_operand rax = make_operand(MOP_MREG, X86_RAX, size);
			x86_select_inst(out, inst, op0, rax);
			rax.flags |= MOP_IMPLICIT;
			x86_select1(out, X86_RET, rax);
		} break;
	case IR_SEXT:
		{
			machine_operand src = make_operand(MOP_VREG, op0, ir_sizeof(inst[op0].type));
			ASSERT(src.size <= dst.size);

			x86_select_inst(out, inst, op0, src);
			x86_select2(out, X86_MOVSX, dst, src);
		} break;
	case IR_ZEXT:
		{
			machine_operand src = make_operand(MOP_VREG, op0, ir_sizeof(inst[op0].type));
			ASSERT(src.size <= dst.size);

			x86_select_inst(out, inst, op0, src);
			x86_select2(out, X86_MOVZX, dst, src);
		} break;
	case IR_TRUNC:
		{
			dst.size = ir_sizeof(inst[op0].type);
			x86_select_inst(out, inst, op0, dst);
		} break;
	case IR_CALL:
		{
			machine_operand called = make_operand(MOP_VREG, op0, 8);
			if (inst[op0].opcode == IR_BUILTIN) {
				machine_operand src = {0};
				ir_builtin builtin = inst[op0].op0;
				switch (builtin) {
				case BUILTIN_POPCOUNT:
					src = make_operand(MOP_VREG, inst[inst_index - 1].op0, size);
					x86_select2(out, X86_POPCNT, dst, src);
					break;
				default:
					ASSERT(!"Builtin is not supported");
				}
			} else {
				if (inst[op0].opcode == IR_GLOBAL) {
					called = make_func(inst[op0].op0);
				} else {
					x86_select_inst(out, inst, op0, called);
				}

				i32 param_offset = op1;
				while (param_offset > 0) {
					ASSERT(inst[inst_index - param_offset].opcode == IR_PARAM);
					ir_inst param_inst = inst[inst_index - param_offset];
					isize param_size = ir_sizeof(param_inst.type);
					i32 param_index = op1 - param_offset;
					switch (param_index) {
					case 0:
						{
							machine_operand rdi = make_operand(MOP_MREG, X86_RDI, param_size);
							x86_select_inst(out, inst, param_inst.op0, rdi);
						} break;
					case 1:
						{
							machine_operand rsi = make_operand(MOP_MREG, X86_RSI, param_size);
							x86_select_inst(out, inst, param_inst.op0, rsi);
						} break;
					case 2:
						{
							machine_operand rdx = make_operand(MOP_MREG, X86_RDX, param_size);
							x86_select_inst(out, inst, param_inst.op0, rdx);
						} break;
					case 3:
						{
							machine_operand rcx = make_operand(MOP_MREG, X86_RCX, param_size);
							x86_select_inst(out, inst, param_inst.op0, rcx);
						} break;
					case 4:
						{
							machine_operand r8 = make_operand(MOP_MREG, X86_R8, param_size);
							x86_select_inst(out, inst, param_inst.op0, r8);
						} break;
					case 5:
						{
							machine_operand r9 = make_operand(MOP_MREG, X86_R9, param_size);
							x86_select_inst(out, inst, param_inst.op0, r9);
						} break;
					default:
						ASSERT(!"Too many arguments");
						break;
					}

					param_offset--;
				}

				machine_operand rax = make_operand(MOP_MREG, X86_RAX, size);
				x86_select1(out, X86_CALL, called);
				x86_select2(out, X86_MOV, dst, rax);
			}
		} break;
	case IR_LABEL:
		{
			isize src_size = ir_sizeof(inst[op0].type);
			machine_operand src = make_operand(MOP_IMMEDIATE, op0, src_size);
			x86_select1(out, X86_LABEL, src);
		} break;
	case IR_NOP:
	case IR_PARAM:
		break;
	}
}

static machine_program
x86_select_instructions(ir_program program, arena *arena)
{
	machine_program out = {0};
	out.max_size = 8 * 1024 * 1024;
	out.functions = ALLOC(arena, program.function_count, machine_function);
	out.code = alloc(arena, out.max_size, 1);
	out.vreg_count = program.register_count;
	out.register_info.register_count = X86_REGISTER_COUNT;
	out.register_info.int_register_count = X86_INT_REGISTER_COUNT;
	out.register_info.volatile_registers = x86_temp_regs;
	out.register_info.volatile_register_count = LENGTH(x86_temp_regs);

	machine_function *mach_func = out.functions;
	for (isize f = 0; f < program.function_count; f++) {
		ir_function *ir_func = &program.functions[f];
		b8 *is_toplevel = get_toplevel_instructions(ir_func, program.insts + ir_func->inst_index, arena);

		out.function_count++;
		mach_func->name = ir_func->name;
		mach_func->stack_size = ir_func->stack_size;
		mach_func->register_count = ir_func->inst_count;
		u32 first_inst_offset = out.size;
		u32 first_inst_index = out.inst_count;
		ir_inst *inst = program.insts + ir_func->inst_index;

		i32 float_count = 0;
		for (u32 i = 0; i < ir_func->inst_count; i++) {
			if (inst[i].opcode == IR_CONST
				&& (inst[i].type == IR_F32 || inst[i].type == IR_F64))
			{
				float_count++;
			}
		}

		f32 *floats = alloc(arena, 1, sizeof(*floats));
		i32 j = 0;
		for (u32 i = 0; i < ir_func->inst_count; i++) {
			if (inst[i].opcode == IR_CONST
				&& (inst[i].type == IR_F32 || inst[i].type == IR_F64))
			{
				floats[j] = inst[i].op0;
				inst[i].op0 = j++;
			}
		}

		mach_func->float_count = float_count;
		mach_func->floats = (i32 *)floats;

		for (u32 i = 0; i < ir_func->parameter_count; i++) {
			// TODO: Set the correct size of the parameters
			machine_operand dst = make_operand(MOP_VREG, i+1, 8);
			machine_operand src;
			switch (i) {
			case 0:
				src = make_operand(MOP_MREG, X86_RDI, 8);
				x86_select2(&out, X86_MOV, dst, src);
				break;
			case 1:
				src = make_operand(MOP_MREG, X86_RSI, 8);
				x86_select2(&out, X86_MOV, dst, src);
				break;
			case 2:
				src = make_operand(MOP_MREG, X86_RDX, 8);
				x86_select2(&out, X86_MOV, dst, src);
				break;
			case 3:
				src = make_operand(MOP_MREG, X86_RCX, 8);
				x86_select2(&out, X86_MOV, dst, src);
				break;
			case 4:
				src = make_operand(MOP_MREG, X86_R8, 8);
				x86_select2(&out, X86_MOV, dst, src);
				break;
			case 5:
				src = make_operand(MOP_MREG, X86_R9, 8);
				x86_select2(&out, X86_MOV, dst, src);
				break;
			default:
				ASSERT(!"Too many parameters");
			}
		}

		for (u32 i = ir_func->parameter_count; i < ir_func->inst_count; i++) {
			machine_operand dst = make_operand(MOP_VREG, i, ir_sizeof(inst[i].type));
			if (inst[i].opcode == IR_MOV || inst[i].opcode == IR_STORE) {
				dst = make_operand(MOP_VREG, inst[i].op0, ir_sizeof(inst[inst[i].op0].type));
				if (inst[i].type == IR_F32 || inst[i].type == IR_F64) {
					dst.flags |= MOP_ISFLOAT;
				}
			}

			if (is_toplevel[i]) {
				x86_select_inst(&out, inst, i, dst);
			}
		}

		// NOTE: Compute instruction offsets
		mach_func->inst_count = out.inst_count - first_inst_index;
		mach_func->inst_offsets = ALLOC(arena, mach_func->inst_count, u32);
		char *code = (char *)out.code + first_inst_offset;
		for (u32 i = 0; i < mach_func->inst_count; i++) {
			mach_func->inst_offsets[i] = code - (char *)out.code;
			machine_inst *inst = (machine_inst *)code;
			code += sizeof(*inst) + inst->operand_count * sizeof(machine_operand);
		}


		// NOTE: Compute the instruction index of each label
		u32 *label_indices = ALLOC(arena, ir_func->label_count, u32);
		for (u32 i = 0; i < mach_func->inst_count; i++) {
			machine_inst *inst = (machine_inst *)((char *)out.code
				+ mach_func->inst_offsets[i]);
			machine_operand *operands = (machine_operand *)(inst + 1);
			if (inst->opcode == X86_LABEL) {
				// A label should only have one operand: The index of the label.
				ASSERT(operands[0].kind == MOP_IMMEDIATE);
				ASSERT(operands[0].value < ir_func->label_count);

				label_indices[operands[0].value] = i;
			}
		}

		// Replace label operands with the instruction index
		for (u32 i = 0; i < mach_func->inst_count; i++) {
			machine_inst *inst = (machine_inst *)((char *)out.code
				+ mach_func->inst_offsets[i]);
			machine_operand *operands = (machine_operand *)(inst + 1);
			for (u32 j = 0; j < inst->operand_count; j++) {
				if (operands[j].kind == MOP_LABEL) {
					operands[j].value = label_indices[operands[j].value];
				}
			}
		}

		mach_func++;
	}

	return out;
}

static void
x86_emit_operand(stream *out, machine_operand operand, symbol_table *symtab)
{
	x86_register reg;

	b32 print_size = (operand.kind == MOP_SPILL
		|| (operand.flags & MOP_INDIRECT));
	if (print_size) {
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
	}

	switch (operand.kind) {
	case MOP_GLOBAL:
		{
			ast_id node_id = {operand.value};
			switch (symtab->kind[node_id.value]) {
			case SYM_DECL:
				{
					decl_symbol *sym = get_decl_symbol(*symtab, node_id);
					if (equals(sym->name, S("wait"))) {
						stream_print(out, "_wait");
					} else {
						stream_prints(out, sym->name);
					}
				} break;
			case SYM_STRING:
				{
					stream_print(out, "str#");
					symbol_id sym_id = symtab->symbols[node_id.value];
					stream_printu(out, sym_id.value);
				} break;
			default:
				ASSERT(!"Invalid symbol");
			}
		} break;
	case MOP_SPILL:
		stream_print(out, "[rsp+");
		stream_printu(out, operand.value);
		stream_print(out, "]");
		break;
	case MOP_LABEL:
		stream_print(out, ".L");
		stream_printu(out, operand.value);
		break;
	case MOP_MREG:
		if (operand.flags & MOP_INDIRECT) {
			stream_print(out, "[");
			operand.size = 8;
		}

		reg = (x86_register)operand.value;
		stream_print(out, x86_get_register_name(reg, operand.size));

		if (operand.flags & MOP_INDIRECT) {
			stream_print(out, "]");
		}
		break;
	case MOP_IMMEDIATE:
		stream_printu(out, operand.value);
		break;
	case MOP_FLOAT:
		stream_print(out, "[float#");
		stream_printu(out, operand.value);
		stream_print(out, "]");
		break;
	case MOP_FUNC:
		{
			ast_id node_id = {operand.value};
			decl_symbol *sym = get_decl_symbol(*symtab, node_id);
			if (equals(sym->name, S("wait"))) {
				stream_print(out, "_wait");
			} else {
				stream_prints(out, sym->name);
			}
		} break;
	case MOP_VREG:
		stream_print(out, "v");
		stream_printu(out, operand.value);
		//ASSERT(!"Cannot use virtual register during code generation");
		break;
	default:
		ASSERT(false);
		stream_print(out, "(invalid operand)");
	}
}

static void
x86_generate(stream *out, machine_program program, allocation_info *info)
{
	stream_print(out, "section .data\n");

	for (u32 i = 0; i < program.function_count; i++) {
		machine_function *function = &program.functions[i];
		for (i32 j = 0; j < function->float_count; j++) {
			stream_print(out, "float#");
			stream_printu(out, j);
			stream_print(out, ": dd ");
			stream_printu(out, function->floats[j]);
			stream_print(out, "\n");
		}
	}

	stream_print(out, "\n");

	for (u32 i = 0; i < program.symtab->decl_count; i++) {
		decl_symbol *sym = &program.symtab->decls[i];
		if (sym->is_function && sym->linkage != LINK_STATIC) {
			if (sym->linkage == LINK_EXTERN || sym->definition.value == 0) {
				stream_print(out, "extern ");
			} else {
				stream_print(out, "global ");
			}

			stream_prints(out, sym->name);
			stream_print(out, "\n");
		}
	}

	for (u32 i = 1; i < program.symtab->string_count; i++) {
		str sym = program.symtab->strings[i];
		stream_print(out, "str#");
		stream_printu(out, i);
		stream_print(out, ": db \"");
		for (isize j = 0; j < sym.length; j++) {
			b32 is_printable = (0x20 <= sym.at[j] && sym.at[j] < 0x7F);
			if (is_printable && sym.at[j] != '"') {
				stream_write(out, sym.at[j]);
			} else {
				stream_print(out, "\", ");
				stream_print_hex(out, sym.at[j]);
				stream_print(out, ", \"");
			}
		}

		stream_print(out, "\", 0x0\n");
	}

	stream_print(out, "\nsection .bss\n");
	for (u32 i = 0; i < program.symtab->decl_count; i++) {
		decl_symbol *sym = &program.symtab->decls[i];
		if (sym->is_global && sym->linkage != LINK_EXTERN && !sym->is_function) {
			stream_print(out, "\t");
			stream_prints(out, sym->name);
			stream_print(out, " resb ");
			u32 size = type_sizeof(sym->type);
			stream_printu(out, size);
			stream_print(out, "\n");
		}
	}

	stream_print(out, "\nsection .text\n");
	for (u32 function_index = 0; function_index < program.function_count; function_index++) {
		machine_function *function = &program.functions[function_index];
		stream_prints(out, function->name);
		stream_print(out, ":\n");

		u32 used_volatile_register_count = 0;
		for (u32 j = 0; j < LENGTH(x86_preserved_regs); j++) {
			u32 mreg = x86_preserved_regs[j];
			if (info[function_index].used[mreg]) {
				stream_print(out, "\tpush ");
				x86_emit_operand(out, make_operand(MOP_MREG, mreg, 8), program.symtab);
				stream_print(out, "\n");
				used_volatile_register_count++;
			}
		}

		u32 stack_size = function->stack_size;
		stack_size += 8 * info[function_index].spill_count;

		u32 total_stack_size = (stack_size + 8 * used_volatile_register_count);
		b32 stack_is_aligned = ((total_stack_size & 15) == 8);
		if (!stack_is_aligned) {
			stack_size += 24 - (total_stack_size & 15);
		}

		if (stack_size > 0) {
			stream_print(out, "\tsub rsp, ");
			stream_printu(out, stack_size);
			stream_print(out, "\n");
		}

		// Convert label instruction indices back to label indices
		char *code = (char *)program.code;
		for (u32 i = 0; i < function->inst_count; i++) {
			machine_inst *inst = (machine_inst *)(code + function->inst_offsets[i]);
			machine_operand *operands = (machine_operand *)(inst + 1);

			for (u32 j = 0; j < inst->operand_count; j++) {
				if (operands[j].kind == MOP_LABEL) {
					isize label_offset = function->inst_offsets[operands[j].value];
					machine_inst *label = (machine_inst *)(code + label_offset);
					machine_operand *label_index = (machine_operand *)(label + 1);
					operands[j].value = label_index->value;
				}
			}
		}

		for (u32 i = 0; i < function->inst_count; i++) {
			machine_inst *inst = get_inst(program.code, function->inst_offsets, i);
			machine_operand *operands = (machine_operand *)(inst + 1);
			x86_opcode opcode = (x86_opcode)inst->opcode;
			u32 operand_count = inst->operand_count;

			if (opcode == X86_PRINT) {
				stream_print(out,
					"\tmov rdi, fmt\n"
					"\tcall printf wrt ..plt\n");
			} else if (opcode == X86_LABEL) {
				stream_print(out, ".L");
				x86_emit_operand(out, operands[0], program.symtab);
				stream_print(out, ":\n");
			} else if (opcode == X86_RET) {
				stream_print(out, "\tjmp .exit\n");
			} else {
				if (opcode == X86_MOV || opcode == X86_MOVSS) {
					if  (equals_operand(operands[0], operands[1])) {
						continue;
					}
				}

				if (operands[0].kind == MOP_SPILL
					&& operands[1].kind == MOP_SPILL)
				{
					machine_operand rax = make_operand(MOP_MREG, X86_RAX, operands[0].size);
					stream_print(out, "\tmov ");
					x86_emit_operand(out, rax, program.symtab);
					stream_print(out, ", ");
					x86_emit_operand(out, operands[1], program.symtab);
					operands[1] = rax;
					stream_print(out, "\n");
				}

				stream_print(out, "\t");
				stream_print(out, x86_get_opcode_name(opcode));
				stream_print(out, " ");
				for (u32 j = 0; j < operand_count; j++) {
					if (operands[j].flags & MOP_IMPLICIT) {
						continue;
					}

					if (j != 0) {
						stream_print(out, ", ");
					}

					x86_emit_operand(out, operands[j], program.symtab);
				}

				stream_print(out, "\n");
			}
		}

		stream_print(out, ".exit:\n");
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
				x86_emit_operand(out, make_operand(MOP_MREG, mreg, 8), program.symtab);
				stream_print(out, "\n");
			}
		}

		stream_print(out, "\tret\n\n");
	}
}
