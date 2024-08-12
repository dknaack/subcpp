static void
x86_emit1(mach_program *out, x86_opcode opcode, mach_operand dst)
{
	dst.flags |= MOP_DEF | MOP_USE;

	switch (opcode) {
	case X86_IDIV:
		{
			push_inst(out, opcode, 3);
			push_operand(out, dst);

			mach_operand op0 = make_operand(MOP_MREG, X86_RAX, dst.size);
			op0.flags |= MOP_DEF | MOP_USE | MOP_IMPLICIT;
			push_operand(out, op0);

			mach_operand op1 = make_operand(MOP_MREG, X86_RDX, dst.size);
			op1.flags |= MOP_DEF | MOP_USE | MOP_IMPLICIT;
			push_operand(out, op1);
		} break;
	case X86_IMUL:
		{
			push_inst(out, opcode, 3);
			push_operand(out, dst);

			mach_operand op0 = make_operand(MOP_MREG, X86_RAX, dst.size);
			op0.flags |= MOP_DEF | MOP_USE | MOP_IMPLICIT;
			push_operand(out, op0);

			mach_operand op1 = make_operand(MOP_MREG, X86_RDX, dst.size);
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
x86_emit2(mach_program *out, x86_opcode opcode,
	mach_operand dst, mach_operand src)
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

static void x86_select_inst(mach_program *out,
	ir_inst *inst, isize inst_index, mach_operand dst);

static mach_operand
x86_select_const(mach_program *out, ir_inst *inst, isize inst_index)
{
	mach_operand result;
	u32 size = ir_sizeof(inst[inst_index].type);
	if (inst[inst_index].opcode == IR_CONST) {
		result = make_operand(MOP_CONST, inst[inst_index].op0, size);
	} else {
		result = make_operand(MOP_VREG, inst_index, size);
		x86_select_inst(out, inst, inst_index, result);
	}

	return result;
}

static void
x86_select_inst(mach_program *out, ir_inst *inst,
	isize inst_index, mach_operand dst)
{
	ir_type type = inst[inst_index].type;
	u32 size = ir_sizeof(type);
	u32 op0 = inst[inst_index].op0;
	u32 op1 = inst[inst_index].op1;
	ir_opcode opcode = inst[inst_index].opcode;
	b32 is_float = (type == IR_F32 || type == IR_F64);

	switch (opcode) {
	case IR_SEQ:
		{
			ASSERT(!"Should have been handled by x86_select");
		} break;
	case IR_GLOBAL:
		{
			mach_operand src = make_global(op0);
			x86_emit2(out, X86_MOV, dst, src);
		} break;
	case IR_VAR:
		{
			mach_operand src = make_operand(MOP_VREG, inst_index, size);
			src.size = size;
			if (type == IR_F32 || type == IR_F64) {
				x86_emit2(out, X86_MOVSS, dst, src);
			} else {
				x86_emit2(out, X86_MOV, dst, src);
			}
		} break;
	case IR_CAST:
	case IR_CASTU:
		{
			ir_type op0_type = inst[op0].type;
			mach_operand src = make_operand(MOP_VREG, op0, ir_sizeof(op0_type));

			x86_select_inst(out, inst, op0, src);
			if (type == IR_F32) {
				x86_emit2(out, X86_CVTSI2SS, dst, src);
			} else if (type == IR_F64) {
				x86_emit2(out, X86_CVTSI2SD, dst, src);
			} else if (op0_type == IR_F32) {
				src.size = 8;
				x86_emit2(out, X86_CVTTSS2SI, dst, src);
			} else if (op0_type == IR_F64) {
				src.size = 8;
				x86_emit2(out, X86_CVTTSD2SI, dst, src);
			}

			if (type == IR_I8) {
			}
		} break;
	case IR_CONST:
		{
			mach_operand src = make_operand(MOP_CONST, op0, size);
			x86_emit2(out, X86_MOV, dst, src);
		} break;
	case IR_BUILTIN:
		{
			ASSERT(!"Builtins should be handled in their respective instruction");
		} break;
	case IR_ALLOC:
		{
			mach_operand src = make_spill(op1);
			x86_emit2(out, X86_LEA, dst, src);
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
			mach_operand src = make_operand(MOP_VREG, op0, ir_sizeof(inst[op0].type));
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

				x86_emit2(out, x86_opcode, dst, src);
			} else if (inst[op0].opcode == IR_ADD
				&& inst[inst[op0].op0].opcode == IR_ALLOC
				&& inst[inst[op0].op1].opcode == IR_CONST)
			{
				u32 base = inst[inst[op0].op0].op1;
				u32 offset = inst[inst[op0].op1].op0;
				src = make_spill(base + offset);
				src.size = size;

				x86_emit2(out, x86_opcode, dst, src);
			} else {
				x86_select_inst(out, inst, op0, src);
				src.size = dst.size = size;
				src.flags |= MOP_INDIRECT;
				x86_emit2(out, x86_opcode, dst, src);
			}
		} break;
	case IR_STORE:
		{
			mach_operand src = make_operand(MOP_VREG, op1, ir_sizeof(inst[op1].type));
			x86_opcode x86_opcode = (is_float ? X86_MOVSS : X86_MOV);

			ASSERT(!equals_operand(src, dst));
			if (inst[op1].opcode != IR_CONST) {
				x86_select_inst(out, inst, op1, src);
			} else {
				src = make_operand(MOP_CONST, inst[op1].op0, size);
			}

			src.size = ir_sizeof(inst[op1].type);
			if (inst[op0].opcode == IR_ADD
				&& inst[inst[op0].op0].opcode == IR_ALLOC
				&& inst[inst[op0].op1].opcode == IR_CONST)
			{
				u32 base = inst[inst[op0].op0].op1;
				u32 offset = inst[inst[op0].op1].op0;
				mach_operand addr = make_spill(base + offset);
				addr.size = size;
				x86_emit2(out, x86_opcode, addr, src);
			} else if (inst[op0].opcode == IR_SUB
				&& inst[inst[op0].op0].opcode == IR_ALLOC
				&& inst[inst[op0].op1].opcode == IR_CONST)
			{
				u32 base = inst[inst[op0].op0].op1;
				u32 offset = inst[inst[op0].op1].op0;
				mach_operand addr = make_spill(base - offset);
				addr.size = size;
				x86_emit2(out, x86_opcode, addr, src);
			} else if (inst[op0].opcode == IR_ALLOC) {
				u32 offset = inst[op0].op1;
				mach_operand addr = make_spill(offset);
				addr.size = size;
				x86_emit2(out, x86_opcode, addr, src);
			} else {
				x86_select_inst(out, inst, op0, dst);
				dst.flags |= MOP_INDIRECT;
				dst.size = src.size;
				x86_emit2(out, x86_opcode, dst, src);
			}
		} break;
	case IR_ADD:
		{
			if (is_float) {
				mach_operand src = make_operand(MOP_VREG, op1, ir_sizeof(inst[op1].type));
				src.flags |= MOP_ISFLOAT;
				dst.flags |= MOP_ISFLOAT;

				x86_select_inst(out, inst, op0, dst);
				x86_select_inst(out, inst, op1, src);
				x86_emit2(out, X86_ADDSS, dst, src);
			} else {
				if (inst[op1].opcode == IR_CONST && inst[op1].op0 == 1) {
					x86_select_inst(out, inst, op0, dst);
					x86_emit1(out, X86_INC, dst);
				} else if (inst[op1].opcode == IR_CONST) {
					x86_select_inst(out, inst, op0, dst);
					op1 = inst[op1].op0;
					mach_operand src = make_operand(MOP_CONST, op1, size);
					x86_emit2(out, X86_ADD, dst, src);
				} else if (inst[op0].opcode == IR_CONST) {
					x86_select_inst(out, inst, op1, dst);
					op0 = inst[op0].op0;
					mach_operand src = make_operand(MOP_CONST, op0, size);
					x86_emit2(out, X86_ADD, dst, src);
				} else {
					mach_operand src = make_operand(MOP_VREG, op1, ir_sizeof(inst[op1].type));
					x86_select_inst(out, inst, op0, dst);
					x86_select_inst(out, inst, op1, src);
					x86_emit2(out, X86_ADD, dst, src);
				}
			}
		}
		break;
	case IR_SUB:
		if (is_float) {
			ASSERT(opcode != IR_MOD);

			mach_operand src = make_operand(MOP_VREG, op1, ir_sizeof(inst[op1].type));
			dst.flags |= MOP_ISFLOAT;
			src.flags |= MOP_ISFLOAT;
			x86_emit2(out, X86_SUBSS, dst, src);
		} else {
			if (inst[op1].opcode == IR_CONST && inst[op1].op0 == 1) {
				x86_select_inst(out, inst, op0, dst);
				x86_emit1(out, X86_DEC, dst);
			} else if (inst[op0].opcode == IR_CONST && inst[op0].op0 == 0) {
				x86_select_inst(out, inst, op1, dst);
				x86_emit1(out, X86_NEG, dst);
			} else if (inst[op1].opcode == IR_CONST) {
				isize src_size = ir_sizeof(inst[op1].type);
				op1 = inst[op1].op0;
				x86_select_inst(out, inst, op0, dst);
				mach_operand src = make_operand(MOP_CONST, op1, src_size);
				x86_emit2(out, X86_SUB, dst, src);
			} else {
				mach_operand src = make_operand(MOP_VREG, op1, ir_sizeof(inst[op1].type));
				x86_select_inst(out, inst, op0, dst);
				x86_select_inst(out, inst, op1, src);
				x86_emit2(out, X86_SUB, dst, src);
			}
		}
		break;
	case IR_MUL:
		if (is_float) {
			mach_operand src = make_operand(MOP_VREG, op1, ir_sizeof(inst[op1].type));
			dst.flags |= MOP_ISFLOAT;
			src.flags |= MOP_ISFLOAT;
			x86_emit2(out, X86_MULSS, dst, src);
		} else {
			if (inst[op1].opcode == IR_CONST && inst[op1].op0 == 1) {
				x86_select_inst(out, inst, op0, dst);
			} else if (inst[op1].opcode == IR_CONST && inst[op1].op0 == 2) {
				x86_select_inst(out, inst, op0, dst);
				x86_emit2(out, X86_ADD, dst, dst);
			} else {
				mach_operand rax = make_operand(MOP_MREG, X86_RAX, ir_sizeof(inst[op0].type));
				mach_operand src = make_operand(MOP_VREG, op1, ir_sizeof(inst[op1].type));

				x86_select_inst(out, inst, op0, rax);
				x86_select_inst(out, inst, op1, src);
				x86_emit1(out, X86_IMUL, src);
				x86_emit2(out, X86_MOV, dst, rax);
			}
		}
		break;
	case IR_DIV:
	case IR_MOD:
		if (is_float) {
			ASSERT(opcode != IR_MOD);

			mach_operand src = make_operand(MOP_VREG, op1, ir_sizeof(inst[op1].type));
			dst.flags |= MOP_ISFLOAT;
			src.flags |= MOP_ISFLOAT;
			x86_emit2(out, X86_DIVSS, dst, src);
		} else {
			mach_operand rax = make_operand(MOP_MREG, X86_RAX, ir_sizeof(inst[op0].type));
			mach_operand rcx = make_operand(MOP_MREG, X86_RCX, ir_sizeof(inst[op1].type));
			mach_operand rdx = make_operand(MOP_MREG, X86_RDX, dst.size);
			mach_operand zero = make_operand(MOP_CONST, 0, dst.size);

			x86_select_inst(out, inst, op0, rax);
			x86_select_inst(out, inst, op1, rcx);
			x86_emit2(out, X86_MOV, rdx, zero);
			x86_emit1(out, X86_IDIV, rcx);
			x86_emit2(out, X86_MOV, dst, opcode == IR_DIV ? rax : rdx);
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
			mach_operand dst_byte = dst;
			dst_byte.size = 1;

			mach_operand src = make_operand(MOP_VREG, op1, ir_sizeof(inst[op1].type));
			if (is_float) {
				dst.flags |= MOP_ISFLOAT;
				src.flags |= MOP_ISFLOAT;
			}

			x86_select_inst(out, inst, op0, dst);
			x86_select_inst(out, inst, op1, src);
			x86_emit2(out, x86_opcode, dst, src);
			x86_emit1(out, x86_get_setcc_opcode(opcode), dst_byte);
			x86_emit2(out, X86_MOVZX, dst, dst_byte);
		} break;
	case IR_SHL:
	case IR_SHR:
		{
			mach_operand shift = make_operand(MOP_MREG, X86_RCX, 1);

			x86_select_inst(out, inst, op0, dst);
			x86_select_inst(out, inst, op1, shift);
			x86_emit2(out, opcode == IR_SHL ? X86_SHL : X86_SHR, dst, shift);
		} break;
	case IR_AND:
	case IR_OR:
	case IR_XOR:
		{
			mach_operand src = make_operand(MOP_VREG, op1, ir_sizeof(inst[op1].type));
			x86_opcode x86_opcode =
				opcode == IR_AND ? X86_AND :
				opcode == IR_OR ? X86_OR : X86_XOR;

			if (inst[op1].opcode == IR_CONST) {
				src = make_operand(MOP_CONST, inst[op1].op0, src.size);
				x86_select_inst(out, inst, op0, dst);
				x86_emit2(out, x86_opcode, dst, src);
			} else if (inst[op0].opcode == IR_CONST) {
				src = make_operand(MOP_CONST, inst[op0].op0, src.size);
				x86_select_inst(out, inst, op1, dst);
				x86_emit2(out, x86_opcode, dst, src);
			} else {
				x86_select_inst(out, inst, op0, dst);
				x86_select_inst(out, inst, op1, src);
				x86_emit2(out, x86_opcode, dst, src);
			}
		} break;
	case IR_NOT:
		{
			x86_select_inst(out, inst, op0, dst);
			x86_emit1(out, X86_NOT, dst);
		} break;
	case IR_JMP:
		x86_emit1(out, X86_JMP, make_label(op0));
		break;
	case IR_JIZ:
	case IR_JNZ:
		{
			b32 is_jiz = opcode == IR_JIZ;
			x86_opcode jcc = is_jiz ? X86_JZ : X86_JNZ;
			if (is_comparison_opcode(inst[op0].opcode)) {
				dst = make_operand(MOP_VREG, inst[op0].op0, ir_sizeof(inst[op0].type));
				x86_select_inst(out, inst, inst[op0].op0, dst);
				mach_operand src = x86_select_const(out, inst, inst[op0].op1);
				src.size = dst.size;
				if (is_float) {
					dst.flags |= MOP_ISFLOAT;
					src.flags |= MOP_ISFLOAT;
				}

				x86_opcode cmp_opcode = (is_float ? X86_COMISS : X86_CMP);
				x86_emit2(out, cmp_opcode, dst, src);
				jcc = x86_get_jcc_opcode(inst[op0].opcode, is_jiz);
			} else {
				mach_operand src = make_operand(MOP_VREG, op0, ir_sizeof(inst[op0].type));
				x86_select_inst(out, inst, op0, src);
				ASSERT(src.size > 0 && src.size <= 8);
				x86_emit2(out, X86_TEST, src, src);
			}

			x86_emit1(out, jcc, make_label(op1));
		} break;
	case IR_RET:
		{
			mach_operand rax = make_operand(MOP_MREG, X86_RAX, size);
			x86_select_inst(out, inst, op0, rax);
			rax.flags |= MOP_IMPLICIT;
			x86_emit1(out, X86_RET, rax);
		} break;
	case IR_SEXT:
		{
			mach_operand src = make_operand(MOP_VREG, op0, ir_sizeof(inst[op0].type));
			ASSERT(src.size <= dst.size);

			x86_select_inst(out, inst, op0, src);
			x86_emit2(out, X86_MOVSX, dst, src);
		} break;
	case IR_ZEXT:
		{
			mach_operand src = make_operand(MOP_VREG, op0, ir_sizeof(inst[op0].type));
			ASSERT(src.size <= dst.size);

			x86_select_inst(out, inst, op0, src);
			x86_emit2(out, X86_MOVZX, dst, src);
		} break;
	case IR_TRUNC:
		{
			dst.size = ir_sizeof(inst[op0].type);
			x86_select_inst(out, inst, op0, dst);
		} break;
	case IR_CALL:
		{
			mach_operand called = make_operand(MOP_VREG, op0, 8);
			if (inst[op0].opcode == IR_BUILTIN) {
				mach_operand src = {0};
				ir_builtin builtin = inst[op0].op0;
				switch (builtin) {
				case BUILTIN_POPCOUNT:
					src = make_operand(MOP_VREG, inst[inst_index - 1].op0, size);
					x86_emit2(out, X86_POPCNT, dst, src);
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
							mach_operand rdi = make_operand(MOP_MREG, X86_RDI, param_size);
							x86_select_inst(out, inst, param_inst.op0, rdi);
						} break;
					case 1:
						{
							mach_operand rsi = make_operand(MOP_MREG, X86_RSI, param_size);
							x86_select_inst(out, inst, param_inst.op0, rsi);
						} break;
					case 2:
						{
							mach_operand rdx = make_operand(MOP_MREG, X86_RDX, param_size);
							x86_select_inst(out, inst, param_inst.op0, rdx);
						} break;
					case 3:
						{
							mach_operand rcx = make_operand(MOP_MREG, X86_RCX, param_size);
							x86_select_inst(out, inst, param_inst.op0, rcx);
						} break;
					case 4:
						{
							mach_operand r8 = make_operand(MOP_MREG, X86_R8, param_size);
							x86_select_inst(out, inst, param_inst.op0, r8);
						} break;
					case 5:
						{
							mach_operand r9 = make_operand(MOP_MREG, X86_R9, param_size);
							x86_select_inst(out, inst, param_inst.op0, r9);
						} break;
					default:
						ASSERT(!"Too many arguments");
						break;
					}

					param_offset--;
				}

				mach_operand rax = make_operand(MOP_MREG, X86_RAX, size);
				x86_emit1(out, X86_CALL, called);
				x86_emit2(out, X86_MOV, dst, rax);
			}
		} break;
	case IR_LABEL:
		{
			isize src_size = ir_sizeof(inst[op0].type);
			mach_operand src = make_operand(MOP_CONST, op0, src_size);
			x86_emit1(out, X86_LABEL, src);
		} break;
	case IR_NOP:
	case IR_PARAM:
		break;
	}
}

static mach_program
x86_select(ir_program program, arena *arena)
{
	mach_program out = {0};
	out.functions = ALLOC(arena, program.function_count, mach_function);
	// TODO: This should be a dynamic array
	out.max_size = 8 * 1024 * 1024;
	out.code = alloc(arena, out.max_size, 1);
	out.vreg_count = program.register_count;
	out.register_info.register_count = X86_REGISTER_COUNT;
	out.register_info.int_register_count = X86_INT_REGISTER_COUNT;
	out.register_info.volatile_registers = x86_temp_regs;
	out.register_info.volatile_register_count = LENGTH(x86_temp_regs);

	for (isize i = 0; i < program.function_count; i++) {
		ir_function *ir_func = &program.functions[i];
		mach_function *mach_func = &out.functions[i];

		isize first_inst_index = out.inst_count;
		isize first_inst_offset = out.size;

		// NOTE: Initialize the parameter registers
		for (isize i = 0; i < ir_func->param_count; i++) {
			// TODO: Set the correct size of the parameters
			mach_operand dst = make_operand(MOP_VREG, i+1, 8);
			mach_operand src;
			switch (i) {
			case 0:
				src = make_operand(MOP_MREG, X86_RDI, 8);
				x86_emit2(&out, X86_MOV, dst, src);
				break;
			case 1:
				src = make_operand(MOP_MREG, X86_RSI, 8);
				x86_emit2(&out, X86_MOV, dst, src);
				break;
			case 2:
				src = make_operand(MOP_MREG, X86_RDX, 8);
				x86_emit2(&out, X86_MOV, dst, src);
				break;
			case 3:
				src = make_operand(MOP_MREG, X86_RCX, 8);
				x86_emit2(&out, X86_MOV, dst, src);
				break;
			case 4:
				src = make_operand(MOP_MREG, X86_R8, 8);
				x86_emit2(&out, X86_MOV, dst, src);
				break;
			case 5:
				src = make_operand(MOP_MREG, X86_R9, 8);
				x86_emit2(&out, X86_MOV, dst, src);
				break;
			default:
				// TODO: The function should store the parameter offsets of
				// each argument. Then, we can calculate the offset for this
				// parameter.
				ASSERT(!"Too many parameters");
			}
		}

		// NOTE: Do the instruction selection
		ir_inst *inst = program.insts + ir_func->inst_index;
		for (isize i = 1; i != 0; i = inst[i].op1) {
			ASSERT(inst[i].opcode == IR_SEQ);

			isize j = inst[j].op0;
			mach_operand dst = make_operand(MOP_VREG, j, ir_sizeof(inst[j].type));
			if (inst[j].opcode == IR_MOV || inst[j].opcode == IR_STORE) {
				dst = make_operand(MOP_VREG, inst[j].op0, ir_sizeof(inst[inst[j].op0].type));
				if (inst[j].type == IR_F32 || inst[j].type == IR_F64) {
					dst.flags |= MOP_ISFLOAT;
				}
			}

			x86_select_inst(&out, inst, j, dst);
		}

		// NOTE: Compute instruction offsets
		mach_func->inst_count = out.inst_count - first_inst_index;
		mach_func->inst_offsets = ALLOC(arena, mach_func->inst_count, u32);
		char *code = (char *)out.code + first_inst_offset;
		for (isize i = 0; i < mach_func->inst_count; i++) {
			mach_func->inst_offsets[i] = code - (char *)out.code;
			mach_inst *inst = (mach_inst *)code;
			code += sizeof(*inst) + inst->operand_count * sizeof(mach_operand);
		}


		// NOTE: Compute the instruction index of each label
		u32 *label_indices = ALLOC(arena, ir_func->label_count, u32);
		for (isize i = 0; i < mach_func->inst_count; i++) {
			mach_inst *inst = (mach_inst *)((char *)out.code
				+ mach_func->inst_offsets[i]);
			mach_operand *operands = (mach_operand *)(inst + 1);
			if (inst->opcode == X86_LABEL) {
				// A label should only have one operand: The index of the label.
				ASSERT(operands[0].kind == MOP_CONST);
				ASSERT((i32)operands[0].value < ir_func->label_count);

				label_indices[operands[0].value] = i;
			}
		}

		// Replace label operands with the instruction index
		for (isize i = 0; i < mach_func->inst_count; i++) {
			mach_inst *inst = (mach_inst *)((char *)out.code
				+ mach_func->inst_offsets[i]);
			mach_operand *operands = (mach_operand *)(inst + 1);
			for (isize j = 0; j < inst->operand_count; j++) {
				if (operands[j].kind == MOP_LABEL) {
					operands[j].value = label_indices[operands[j].value];
				}
			}
		}
	}

	return out;
}
