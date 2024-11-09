static void
x86_emit1(x86_context ctx, x86_opcode opcode, mach_operand dst)
{
	mach_program *program = ctx.program;
	dst.flags |= MOP_DEF | MOP_USE;

	switch (opcode) {
	case X86_IDIV:
		{
			push_inst(program, opcode, 3);
			push_operand(program, dst);

			mach_operand op0 = make_operand(MOP_MREG, X86_RAX, dst.size);
			op0.flags |= MOP_DEF | MOP_USE | MOP_IMPLICIT;
			push_operand(program, op0);

			mach_operand op1 = make_operand(MOP_MREG, X86_RDX, dst.size);
			op1.flags |= MOP_DEF | MOP_USE | MOP_IMPLICIT;
			push_operand(program, op1);
		} break;
	case X86_IMUL:
		{
			push_inst(program, opcode, 3);
			push_operand(program, dst);

			mach_operand op0 = make_operand(MOP_MREG, X86_RAX, dst.size);
			op0.flags |= MOP_DEF | MOP_USE | MOP_IMPLICIT;
			push_operand(program, op0);

			mach_operand op1 = make_operand(MOP_MREG, X86_RDX, dst.size);
			op1.flags |= MOP_DEF | MOP_IMPLICIT;
			push_operand(program, op1);
		} break;
	default:
		{
			push_inst(program, opcode, 1);
			push_operand(program, dst);
		} break;
	}
}

static void
x86_emit2(x86_context ctx, x86_opcode opcode,
	mach_operand dst, mach_operand src)
{
	mach_program *program = ctx.program;
	ASSERT(dst.kind != 0 && src.kind != 0);
	ASSERT(dst.size > 0 && src.size > 0);

	switch (opcode) {
	case X86_MOV:
		if (!equals_operand(dst, src)) {
			push_inst(program, opcode, 2);
			if (dst.flags & MOP_INDIRECT) {
				dst.flags |= MOP_USE;
			} else {
				dst.flags |= MOP_DEF;
			}

			push_operand(program, dst);
			src.flags |= MOP_USE;
			push_operand(program, src);
			// Why was this here?
			//ASSERT(!(dst.flags & MOP_INDIRECT));
		}
		break;
	case X86_CMP:
		push_inst(program, opcode, 2);
		dst.flags |= MOP_USE;
		push_operand(program, dst);
		src.flags |= MOP_USE;
		push_operand(program, src);
		break;
	default:
		push_inst(program, opcode, 2);
		dst.flags |= MOP_DEF | MOP_USE;
		push_operand(program, dst);
		src.flags |= MOP_USE;
		push_operand(program, src);
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

static void x86_select_inst(x86_context ctx, isize inst_index, mach_operand dst);

static mach_operand
x86_select_const(x86_context ctx, isize inst_index)
{
	ir_inst *inst = ctx.inst;
	mach_operand result;
	u32 size = ir_sizeof(inst[inst_index].type);
	if (inst[inst_index].opcode == IR_CONST) {
		result = make_operand(MOP_CONST, inst[inst_index].op0, size);
	} else {
		result = make_operand(MOP_VREG, inst_index, size);
		x86_select_inst(ctx, inst_index, result);
	}

	return result;
}

static void
x86_select_inst(x86_context ctx, isize inst_index, mach_operand dst)
{
	ir_inst *inst = ctx.inst;
	ir_type type = inst[inst_index].type;
	u32 size = ir_sizeof(type);
	u32 op0 = inst[inst_index].op0;
	u32 op1 = inst[inst_index].op1;
	ir_opcode opcode = inst[inst_index].opcode;
	b32 is_float = (type == IR_F32 || type == IR_F64);

	switch (opcode) {
	case IR_GLOBAL:
		{
			mach_operand src = make_global(op0);
			x86_emit2(ctx, X86_MOV, dst, src);
		} break;
	case IR_VAR:
		{
			mach_operand src = make_operand(MOP_VREG, inst_index, size);
			src.size = size;
			if (type == IR_F32 || type == IR_F64) {
				x86_emit2(ctx, X86_MOVSS, dst, src);
			} else {
				x86_emit2(ctx, X86_MOV, dst, src);
			}
		} break;
	case IR_PARAM:
		{
			// TODO: Set the correct size of the parameters
			mach_operand src;
			switch (op0) {
			case 0:
				src = make_operand(MOP_MREG, X86_RDI, dst.size);
				x86_emit2(ctx, X86_MOV, dst, src);
				break;
			case 1:
				src = make_operand(MOP_MREG, X86_RSI, dst.size);
				x86_emit2(ctx, X86_MOV, dst, src);
				break;
			case 2:
				src = make_operand(MOP_MREG, X86_RDX, dst.size);
				x86_emit2(ctx, X86_MOV, dst, src);
				break;
			case 3:
				src = make_operand(MOP_MREG, X86_RCX, dst.size);
				x86_emit2(ctx, X86_MOV, dst, src);
				break;
			case 4:
				src = make_operand(MOP_MREG, X86_R8, dst.size);
				x86_emit2(ctx, X86_MOV, dst, src);
				break;
			case 5:
				src = make_operand(MOP_MREG, X86_R9, dst.size);
				x86_emit2(ctx, X86_MOV, dst, src);
				break;
			default:
				// TODO: The function should store the parameter offsets of
				// each argument. Then, we can calculate the offset for this
				// parameter.
				ASSERT(!"Too many parameters");
			}
		} break;
	case IR_CAST:
	case IR_CASTU:
		{
			ir_type op0_type = inst[op0].type;
			mach_operand src = make_operand(MOP_VREG, op0, ir_sizeof(op0_type));

			x86_select_inst(ctx, op0, src);
			if (type == IR_F32) {
				x86_emit2(ctx, X86_CVTSI2SS, dst, src);
			} else if (type == IR_F64) {
				x86_emit2(ctx, X86_CVTSI2SD, dst, src);
			} else if (op0_type == IR_F32) {
				src.size = 8;
				x86_emit2(ctx, X86_CVTTSS2SI, dst, src);
			} else if (op0_type == IR_F64) {
				src.size = 8;
				x86_emit2(ctx, X86_CVTTSD2SI, dst, src);
			}

			if (type == IR_I8) {
			}
		} break;
	case IR_CONST:
		{
			mach_operand src = make_operand(MOP_CONST, op0, size);
			x86_emit2(ctx, X86_MOV, dst, src);
		} break;
	case IR_BUILTIN:
		{
			ASSERT(!"Builtins should be handled in their respective instruction");
		} break;
	case IR_ALLOC:
		{
			mach_operand src = make_spill(op1);
			x86_emit2(ctx, X86_LEA, dst, src);
		} break;
	case IR_COPY:
		{
			x86_select_inst(ctx, op0, dst);
		} break;
	case IR_MOV:
		{
			x86_select_inst(ctx, op1, dst);
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

				x86_emit2(ctx, x86_opcode, dst, src);
			} else if (inst[op0].opcode == IR_ADD
				&& inst[inst[op0].op0].opcode == IR_ALLOC
				&& inst[inst[op0].op1].opcode == IR_CONST)
			{
				u32 base = inst[inst[op0].op0].op1;
				u32 offset = inst[inst[op0].op1].op0;
				src = make_spill(base + offset);
				src.size = size;

				x86_emit2(ctx, x86_opcode, dst, src);
			} else {
				x86_select_inst(ctx, op0, src);
				src.size = dst.size = size;
				src.flags |= MOP_INDIRECT;
				x86_emit2(ctx, x86_opcode, dst, src);
			}
		} break;
	case IR_STORE:
		{
			mach_operand src = make_operand(MOP_VREG, op1, ir_sizeof(inst[op1].type));
			x86_opcode x86_opcode = (is_float ? X86_MOVSS : X86_MOV);

			ASSERT(!equals_operand(src, dst));
			if (inst[op1].opcode != IR_CONST) {
				x86_select_inst(ctx, op1, src);
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
				x86_emit2(ctx, x86_opcode, addr, src);
			} else if (inst[op0].opcode == IR_SUB
				&& inst[inst[op0].op0].opcode == IR_ALLOC
				&& inst[inst[op0].op1].opcode == IR_CONST)
			{
				u32 base = inst[inst[op0].op0].op1;
				u32 offset = inst[inst[op0].op1].op0;
				mach_operand addr = make_spill(base - offset);
				addr.size = size;
				x86_emit2(ctx, x86_opcode, addr, src);
			} else if (inst[op0].opcode == IR_ALLOC) {
				u32 offset = inst[op0].op1;
				mach_operand addr = make_spill(offset);
				addr.size = size;
				x86_emit2(ctx, x86_opcode, addr, src);
			} else {
				x86_select_inst(ctx, op0, dst);
				dst.flags |= MOP_INDIRECT;
				dst.size = src.size;
				x86_emit2(ctx, x86_opcode, dst, src);
			}
		} break;
	case IR_ADD:
		{
			if (is_float) {
				mach_operand src = make_operand(MOP_VREG, op1, ir_sizeof(inst[op1].type));
				src.flags |= MOP_ISFLOAT;
				dst.flags |= MOP_ISFLOAT;

				x86_select_inst(ctx, op0, dst);
				x86_select_inst(ctx, op1, src);
				x86_emit2(ctx, X86_ADDSS, dst, src);
			} else {
				if (inst[op1].opcode == IR_CONST && inst[op1].op0 == 1) {
					x86_select_inst(ctx, op0, dst);
					x86_emit1(ctx, X86_INC, dst);
				} else if (inst[op1].opcode == IR_CONST) {
					x86_select_inst(ctx, op0, dst);
					op1 = inst[op1].op0;
					mach_operand src = make_operand(MOP_CONST, op1, size);
					x86_emit2(ctx, X86_ADD, dst, src);
				} else if (inst[op0].opcode == IR_CONST) {
					x86_select_inst(ctx, op1, dst);
					op0 = inst[op0].op0;
					mach_operand src = make_operand(MOP_CONST, op0, size);
					x86_emit2(ctx, X86_ADD, dst, src);
				} else {
					mach_operand src = make_operand(MOP_VREG, op1, ir_sizeof(inst[op1].type));
					x86_select_inst(ctx, op0, dst);
					x86_select_inst(ctx, op1, src);
					x86_emit2(ctx, X86_ADD, dst, src);
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
			x86_emit2(ctx, X86_SUBSS, dst, src);
		} else {
			if (inst[op1].opcode == IR_CONST && inst[op1].op0 == 1) {
				x86_select_inst(ctx, op0, dst);
				x86_emit1(ctx, X86_DEC, dst);
			} else if (inst[op0].opcode == IR_CONST && inst[op0].op0 == 0) {
				x86_select_inst(ctx, op1, dst);
				x86_emit1(ctx, X86_NEG, dst);
			} else if (inst[op1].opcode == IR_CONST) {
				isize src_size = ir_sizeof(inst[op1].type);
				op1 = inst[op1].op0;
				x86_select_inst(ctx, op0, dst);
				mach_operand src = make_operand(MOP_CONST, op1, src_size);
				x86_emit2(ctx, X86_SUB, dst, src);
			} else {
				mach_operand src = make_operand(MOP_VREG, op1, ir_sizeof(inst[op1].type));
				x86_select_inst(ctx, op0, dst);
				x86_select_inst(ctx, op1, src);
				x86_emit2(ctx, X86_SUB, dst, src);
			}
		}
		break;
	case IR_MUL:
		if (is_float) {
			mach_operand src = make_operand(MOP_VREG, op1, ir_sizeof(inst[op1].type));
			dst.flags |= MOP_ISFLOAT;
			src.flags |= MOP_ISFLOAT;
			x86_emit2(ctx, X86_MULSS, dst, src);
		} else {
			if (inst[op1].opcode == IR_CONST && inst[op1].op0 == 1) {
				x86_select_inst(ctx, op0, dst);
			} else if (inst[op1].opcode == IR_CONST && inst[op1].op0 == 2) {
				x86_select_inst(ctx, op0, dst);
				x86_emit2(ctx, X86_ADD, dst, dst);
			} else {
				mach_operand rax = make_operand(MOP_MREG, X86_RAX, ir_sizeof(inst[op0].type));
				mach_operand src = make_operand(MOP_VREG, op1, ir_sizeof(inst[op1].type));

				x86_select_inst(ctx, op0, rax);
				x86_select_inst(ctx, op1, src);
				x86_emit1(ctx, X86_IMUL, src);
				x86_emit2(ctx, X86_MOV, dst, rax);
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
			x86_emit2(ctx, X86_DIVSS, dst, src);
		} else {
			mach_operand rax = make_operand(MOP_MREG, X86_RAX, ir_sizeof(inst[op0].type));
			mach_operand rcx = make_operand(MOP_MREG, X86_RCX, ir_sizeof(inst[op1].type));
			mach_operand rdx = make_operand(MOP_MREG, X86_RDX, dst.size);
			mach_operand zero = make_operand(MOP_CONST, 0, dst.size);

			x86_select_inst(ctx, op0, rax);
			x86_select_inst(ctx, op1, rcx);
			x86_emit2(ctx, X86_MOV, rdx, zero);
			x86_emit1(ctx, X86_IDIV, rcx);
			x86_emit2(ctx, X86_MOV, dst, opcode == IR_DIV ? rax : rdx);
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

			x86_select_inst(ctx, op0, dst);
			x86_select_inst(ctx, op1, src);
			x86_emit2(ctx, x86_opcode, dst, src);
			x86_emit1(ctx, x86_get_setcc_opcode(opcode), dst_byte);
			x86_emit2(ctx, X86_MOVZX, dst, dst_byte);
		} break;
	case IR_SHL:
	case IR_SHR:
		{
			mach_operand shift = make_operand(MOP_MREG, X86_RCX, 1);

			x86_select_inst(ctx, op0, dst);
			x86_select_inst(ctx, op1, shift);
			x86_emit2(ctx, opcode == IR_SHL ? X86_SHL : X86_SHR, dst, shift);
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
				x86_select_inst(ctx, op0, dst);
				x86_emit2(ctx, x86_opcode, dst, src);
			} else if (inst[op0].opcode == IR_CONST) {
				src = make_operand(MOP_CONST, inst[op0].op0, src.size);
				x86_select_inst(ctx, op1, dst);
				x86_emit2(ctx, x86_opcode, dst, src);
			} else {
				x86_select_inst(ctx, op0, dst);
				x86_select_inst(ctx, op1, src);
				x86_emit2(ctx, x86_opcode, dst, src);
			}
		} break;
	case IR_NOT:
		{
			x86_select_inst(ctx, op0, dst);
			x86_emit1(ctx, X86_NOT, dst);
		} break;
	case IR_JMP:
		x86_emit1(ctx, X86_JMP, make_label(op0));
		break;
	case IR_JIZ:
	case IR_JNZ:
		{
			b32 is_jiz = opcode == IR_JIZ;
			x86_opcode jcc = is_jiz ? X86_JZ : X86_JNZ;
			if (is_comparison_opcode(inst[op0].opcode)) {
				dst = make_operand(MOP_VREG, inst[op0].op0, ir_sizeof(inst[op0].type));
				x86_select_inst(ctx, inst[op0].op0, dst);
				mach_operand src = x86_select_const(ctx, inst[op0].op1);
				src.size = dst.size;
				if (is_float) {
					dst.flags |= MOP_ISFLOAT;
					src.flags |= MOP_ISFLOAT;
				}

				x86_opcode cmp_opcode = (is_float ? X86_COMISS : X86_CMP);
				x86_emit2(ctx, cmp_opcode, dst, src);
				jcc = x86_get_jcc_opcode(inst[op0].opcode, is_jiz);
			} else {
				mach_operand src = make_operand(MOP_VREG, op0, ir_sizeof(inst[op0].type));
				x86_select_inst(ctx, op0, src);
				ASSERT(src.size > 0 && src.size <= 8);
				x86_emit2(ctx, X86_TEST, src, src);
			}

			x86_emit1(ctx, jcc, make_label(op1));
		} break;
	case IR_RET:
		{
			mach_operand rax = make_operand(MOP_MREG, X86_RAX, size);
			x86_select_inst(ctx, op0, rax);
			rax.flags |= MOP_IMPLICIT;
			x86_emit1(ctx, X86_RET, rax);
		} break;
	case IR_SEXT:
		{
			mach_operand src = make_operand(MOP_VREG, op0, ir_sizeof(inst[op0].type));
			ASSERT(src.size <= dst.size);

			x86_select_inst(ctx, op0, src);
			x86_emit2(ctx, X86_MOVSX, dst, src);
		} break;
	case IR_ZEXT:
		{
			mach_operand src = make_operand(MOP_VREG, op0, ir_sizeof(inst[op0].type));
			ASSERT(src.size <= dst.size);

			x86_select_inst(ctx, op0, src);
			x86_emit2(ctx, X86_MOVZX, dst, src);
		} break;
	case IR_TRUNC:
		{
			dst.size = ir_sizeof(inst[op0].type);
			x86_select_inst(ctx, op0, dst);
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
					x86_emit2(ctx, X86_POPCNT, dst, src);
					break;
				default:
					ASSERT(!"Builtin is not supported");
				}
			} else {
				if (inst[op0].opcode == IR_GLOBAL) {
					called = make_global(inst[op0].op0);
				} else {
					x86_select_inst(ctx, op0, called);
				}

				isize param_count = 0;
				for (isize param = op1; param; param = inst[param].op1) {
					param_count++;
				}

				isize param_index = param_count;
				for (isize param = op1; param; param = inst[param].op1) {
					param_index--;
					ir_inst param_inst = inst[param];
					ASSERT(param_inst.opcode == IR_CALL);
					isize param_size = ir_sizeof(param_inst.type);
					switch (param_index) {
					case 0:
						{
							mach_operand rdi = make_operand(MOP_MREG, X86_RDI, param_size);
							x86_select_inst(ctx, param_inst.op0, rdi);
						} break;
					case 1:
						{
							mach_operand rsi = make_operand(MOP_MREG, X86_RSI, param_size);
							x86_select_inst(ctx, param_inst.op0, rsi);
						} break;
					case 2:
						{
							mach_operand rdx = make_operand(MOP_MREG, X86_RDX, param_size);
							x86_select_inst(ctx, param_inst.op0, rdx);
						} break;
					case 3:
						{
							mach_operand rcx = make_operand(MOP_MREG, X86_RCX, param_size);
							x86_select_inst(ctx, param_inst.op0, rcx);
						} break;
					case 4:
						{
							mach_operand r8 = make_operand(MOP_MREG, X86_R8, param_size);
							x86_select_inst(ctx, param_inst.op0, r8);
						} break;
					case 5:
						{
							mach_operand r9 = make_operand(MOP_MREG, X86_R9, param_size);
							x86_select_inst(ctx, param_inst.op0, r9);
						} break;
					default:
						ASSERT(!"Too many arguments");
						break;
					}
				}

				mach_operand rax = make_operand(MOP_MREG, X86_RAX, size);
				x86_emit1(ctx, X86_CALL, called);
				x86_emit2(ctx, X86_MOV, dst, rax);
			}
		} break;
	case IR_LABEL:
		{
			isize src_size = ir_sizeof(inst[op0].type);
			mach_operand src = make_operand(MOP_CONST, op0, src_size);
			x86_emit1(ctx, X86_LABEL, src);
		} break;
	case IR_NOP:
		break;
	}
}

static mach_program
x86_select(ir_program p, arena *arena)
{
	mach_program result = {0};
	result.funcs = ALLOC(arena, p.func_count, mach_function);
	// TODO: This should be a dynamic array
	result.max_size = 8 * 1024 * 1024;
	result.code = alloc(arena, result.max_size, 1);
	result.func_count = p.func_count;
	result.max_vreg_count = p.max_reg_count;
	result.max_label_count = p.max_label_count;
	result.mreg_count = X86_REGISTER_COUNT;
	result.int_mreg_count = X86_INT_REGISTER_COUNT;
	result.tmp_mreg_count = LENGTH(x86_temp_regs);
	result.tmp_mregs = x86_temp_regs;

	symbol_id sym_id = p.symtab.section[SECTION_TEXT];
	while (sym_id.value != 0) {
		symbol *sym = &p.symtab.symbols[sym_id.value];
		ir_function *ir_func = &p.funcs[sym_id.value];
		mach_function *mach_func = &result.funcs[sym_id.value];
		isize first_inst = result.size;

		x86_context ctx = {0};
		ctx.inst = p.insts + ir_func->inst_index;
		ctx.program = &result;

		// NOTE: Do the instruction selection
		ir_inst *inst = p.insts + ir_func->inst_index;
		i32 *ref_count = get_ref_count(inst, ir_func->inst_count, arena);
		for (isize j = 0; j < ir_func->inst_count; j++) {
			ir_opcode opcode = inst[j].opcode;
			if (ref_count[j] == 1 || opcode == IR_VAR) {
				continue;
			}

			isize size = ir_sizeof(inst[j].type);
			mach_operand dst = make_operand(MOP_VREG, j, size);
			if (opcode == IR_MOV || opcode == IR_STORE) {
				isize op0_size = ir_sizeof(inst[inst[j].op0].type);
				dst = make_operand(MOP_VREG, inst[j].op0, op0_size);
				if (inst[j].type == IR_F32 || inst[j].type == IR_F64) {
					dst.flags |= MOP_ISFLOAT;
				}
			}

			x86_select_inst(ctx, j, dst);
		}

		isize last_inst = result.size;
		mach_func->inst_count = last_inst - first_inst;
		sym_id = sym->next;
	}

	return result;
}
