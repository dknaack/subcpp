static void
x86_push_token(x86_context *ctx, mach_token token)
{
	ASSERT(ctx->token_count < ctx->max_token_count);
	ctx->tokens[ctx->token_count++] = token;
}

static mach_token
x86_encode_opcode(x86_opcode opcode,
	x86_operand_kind arg0, x86_operand_size size0,
	x86_operand_kind arg1, x86_operand_size size1)
{
	mach_token result = {0};
	result.kind = MACH_INST;
	result.value |= opcode;
	result.value |= arg0  << 16;
	result.value |= size0 << 20;
	result.value |= arg1  << 24;
	result.value |= size1 << 28;
	return result;
}

static void
x86_emit0(x86_context *ctx, x86_opcode opcode)
{
	mach_token opcode_token = x86_encode_opcode(opcode, 0, 0, 0, 0);
	x86_push_token(ctx, opcode_token);
}

static void
x86_emit1(x86_context *ctx, x86_opcode opcode, mach_token dst)
{
	dst.flags |= MACH_DEF | MACH_USE;

	mach_token opcode_token = x86_encode_opcode(opcode, X86_REG, dst.size, 0, 0);
	x86_push_token(ctx, opcode_token);

	switch (opcode) {
	case X86_IDIV:
		{
			x86_push_token(ctx, dst);

			mach_token op0 = make_mach_token(MACH_REG, X86_RAX, dst.size);
			op0.flags |= MACH_DEF | MACH_USE | MACH_IMPLICIT;
			x86_push_token(ctx, op0);

			mach_token op1 = make_mach_token(MACH_REG, X86_RDX, dst.size);
			op1.flags |= MACH_DEF | MACH_USE | MACH_IMPLICIT;
			x86_push_token(ctx, op1);
		} break;
	case X86_IMUL:
		{
			x86_push_token(ctx, dst);

			mach_token op0 = make_mach_token(MACH_REG, X86_RAX, dst.size);
			op0.flags |= MACH_DEF | MACH_USE | MACH_IMPLICIT;
			x86_push_token(ctx, op0);

			mach_token op1 = make_mach_token(MACH_REG, X86_RDX, dst.size);
			op1.flags |= MACH_DEF | MACH_IMPLICIT;
			x86_push_token(ctx, op1);
		} break;
	default:
		{
			x86_push_token(ctx, dst);
		} break;
	}
}

static void
x86_emit2(x86_context *ctx, x86_opcode opcode, mach_token dst, mach_token src)
{
	ASSERT(dst.kind != 0 && src.kind != 0);
	ASSERT(dst.size > 0 && src.size > 0);

	mach_token opcode_token = x86_encode_opcode(opcode, X86_REG, dst.size, X86_REG, src.size);
	x86_push_token(ctx, opcode_token);

	switch (opcode) {
	case X86_MOV:
		if (!equals_token(dst, src)) {
			if (dst.flags & MACH_INDIRECT) {
				dst.flags |= MACH_USE;
			} else {
				dst.flags |= MACH_DEF;
			}

			x86_push_token(ctx, dst);
			src.flags |= MACH_USE;
			x86_push_token(ctx, src);
			// Why was this here?
			//ASSERT(!(dst.flags & MACH_INDIRECT));
		}
		break;
	case X86_CMP:
		x86_emit0(ctx, opcode);
		dst.flags |= MACH_USE;
		x86_push_token(ctx, dst);
		src.flags |= MACH_USE;
		x86_push_token(ctx, src);
		break;
	default:
		x86_emit0(ctx, opcode);
		dst.flags |= MACH_DEF | MACH_USE;
		x86_push_token(ctx, dst);
		src.flags |= MACH_USE;
		x86_push_token(ctx, src);
	}
}

static x86_opcode
x86_get_setcc_opcode(ir_opcode ir_opcode)
{
	switch (ir_opcode) {
	case IR_EQ:
	case IR_FEQ:
		return X86_SETZ;
	case IR_LT:
	case IR_FLT:
		return X86_SETL;
	case IR_GT:
	case IR_FGT:
		return X86_SETG;
	case IR_LE:
	case IR_FLE:
		return X86_SETLE;
	case IR_GE:
	case IR_FGE:
		return X86_SETGE;
	case IR_LTU:
		return X86_SETB;
	case IR_GTU:
		return X86_SETA;
	case IR_LEU:
		return X86_SETAE;
	case IR_GEU:
		return X86_SETBE;
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
		case IR_EQ:
		case IR_FEQ:
			return X86_JNZ;
		case IR_LT:
		case IR_FLT:
			return X86_JGE;
		case IR_GT:
		case IR_FGT:
			return X86_JLE;
		case IR_LE:
		case IR_FLE:
			return X86_JG;
		case IR_GE:
		case IR_FGE:
			return X86_JL;
		case IR_LTU:
			return X86_JAE;
		case IR_GTU:
			return X86_JBE;
		case IR_LEU:
			return X86_JA;
		case IR_GEU:
			return X86_JB;
		default:
			ASSERT(!"Not a comparison operator");
			return X86_SETZ;
		}
	} else {
		switch (ir_opcode) {
		case IR_EQ:
		case IR_FEQ:
			return X86_JZ;
		case IR_LT:
		case IR_FLT:
			return X86_JL;
		case IR_GT:
		case IR_FGT:
			return X86_JG;
		case IR_LE:
		case IR_FLE:
			return X86_JLE;
		case IR_GE:
		case IR_FGE:
			return X86_JGE;
		case IR_LTU:
			return X86_JB;
		case IR_GTU:
			return X86_JA;
		case IR_LEU:
			return X86_JBE;
		case IR_GEU:
			return X86_JAE;
		default:
			ASSERT(!"Not a comparison operator");
			return X86_SETZ;
		}
	}
}

static void x86_select_inst(x86_context *ctx, isize inst_index, mach_token dst);

static mach_token
x86_vreg(u32 value, u32 size)
{
	mach_token result = make_mach_token(MACH_REG, X86_REGISTER_COUNT + value, size);
	return result;
}

static mach_token
x86_select_const(x86_context *ctx, isize inst_index)
{
	ir_inst *inst = ctx->inst;
	mach_token result;
	u32 size = inst[inst_index].size;
	if (inst[inst_index].opcode == IR_CONST) {
		result = make_mach_token(MACH_CONST, inst[inst_index].op0, size);
	} else {
		result = x86_vreg(inst_index, size);
		x86_select_inst(ctx, inst_index, result);
	}

	return result;
}

static void
x86_select_inst(x86_context *ctx, isize inst_index, mach_token dst)
{
	ir_inst *inst = ctx->inst;
	u32 op0 = inst[inst_index].op0;
	u32 op1 = inst[inst_index].op1;
	u32 size = inst[inst_index].size;
	b32 is_float = false;

	ir_opcode opcode = inst[inst_index].opcode;
	switch (opcode) {
	case IR_NOP:
		break;
	case IR_LABEL:
		{
			isize src_size = inst[op0].size;
			mach_token src = make_mach_token(MACH_CONST, op0, src_size);
			x86_emit1(ctx, X86_LABEL, src);
		} break;
	case IR_GLOBAL:
		{
			mach_token src = make_global(op0);
			x86_emit2(ctx, X86_MOV, dst, src);
		} break;
	case IR_VAR:
		{
			mach_token src = x86_vreg(inst_index, size);
			x86_emit2(ctx, X86_MOV, dst, src);
		} break;
	case IR_PARAM:
		{
			// TODO: Set the correct size of the parameters
			mach_token src;
			switch (op0) {
			case 0:
				src = make_mach_token(MACH_REG, X86_RDI, dst.size);
				x86_emit2(ctx, X86_MOV, dst, src);
				break;
			case 1:
				src = make_mach_token(MACH_REG, X86_RSI, dst.size);
				x86_emit2(ctx, X86_MOV, dst, src);
				break;
			case 2:
				src = make_mach_token(MACH_REG, X86_RDX, dst.size);
				x86_emit2(ctx, X86_MOV, dst, src);
				break;
			case 3:
				src = make_mach_token(MACH_REG, X86_RCX, dst.size);
				x86_emit2(ctx, X86_MOV, dst, src);
				break;
			case 4:
				src = make_mach_token(MACH_REG, X86_R8, dst.size);
				x86_emit2(ctx, X86_MOV, dst, src);
				break;
			case 5:
				src = make_mach_token(MACH_REG, X86_R9, dst.size);
				x86_emit2(ctx, X86_MOV, dst, src);
				break;
			default:
				// TODO: The function should store the parameter offsets of
				// each argument. Then, we can calculate the offset for this
				// parameter.
				ASSERT(!"Too many parameters");
			}
		} break;
	case IR_CVT:
	case IR_FCVT:
		{
#if 0
			mach_token src = x86_vreg(op0, inst[op0].size);

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
#endif
		} break;
	case IR_CONST:
		{
			mach_token src = make_mach_token(MACH_CONST, op0, size);
			x86_emit2(ctx, X86_MOV, dst, src);
		} break;
	case IR_BUILTIN:
		{
			ASSERT(!"Builtins should be handled in their respective instruction");
		} break;
	case IR_ALLOC:
		{
			mach_token src = make_spill(op1);
			x86_emit2(ctx, X86_LEA, dst, src);
		} break;
	case IR_COPY:
	case IR_FCOPY:
		{
			x86_select_inst(ctx, op0, dst);
		} break;
	case IR_MOV:
	case IR_FMOV:
		{
			x86_select_inst(ctx, op1, dst);
		} break;
	case IR_LOAD:
	case IR_FLOAD:
		{
			mach_token src = x86_vreg(op0, inst[op0].size);
			x86_opcode x86_opcode = X86_MOV;
			if (opcode == IR_FLOAD) {
				x86_opcode = X86_MOVSS;
				if (dst.kind == MACH_REG) {
					ctx->is_float[dst.value] = true;
				}
			}

			ASSERT(!equals_token(src, dst));
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
				src.flags |= MACH_INDIRECT;
				x86_emit2(ctx, x86_opcode, dst, src);
			}
		} break;
	case IR_STORE:
	case IR_FSTORE:
		{
			mach_token src = x86_vreg(op1, inst[op1].size);
			x86_opcode x86_opcode = (opcode == IR_FSTORE ? X86_MOVSS : X86_MOV);

			ASSERT(!equals_token(src, dst));
			if (inst[op1].opcode != IR_CONST) {
				x86_select_inst(ctx, op1, src);
			} else {
				src = make_mach_token(MACH_CONST, inst[op1].op0, size);
			}

			src.size = inst[op1].size;
			if (inst[op0].opcode == IR_ADD
				&& inst[inst[op0].op0].opcode == IR_ALLOC
				&& inst[inst[op0].op1].opcode == IR_CONST)
			{
				u32 base = inst[inst[op0].op0].op1;
				u32 offset = inst[inst[op0].op1].op0;
				mach_token addr = make_spill(base + offset);
				addr.size = size;
				x86_emit2(ctx, x86_opcode, addr, src);
			} else if (inst[op0].opcode == IR_SUB
				&& inst[inst[op0].op0].opcode == IR_ALLOC
				&& inst[inst[op0].op1].opcode == IR_CONST)
			{
				u32 base = inst[inst[op0].op0].op1;
				u32 offset = inst[inst[op0].op1].op0;
				mach_token addr = make_spill(base - offset);
				addr.size = size;
				x86_emit2(ctx, x86_opcode, addr, src);
			} else if (inst[op0].opcode == IR_ALLOC) {
				u32 offset = inst[op0].op1;
				mach_token addr = make_spill(offset);
				addr.size = size;
				x86_emit2(ctx, x86_opcode, addr, src);
			} else {
				x86_select_inst(ctx, op0, dst);
				dst.flags |= MACH_INDIRECT;
				dst.size = src.size;
				x86_emit2(ctx, x86_opcode, dst, src);
			}
		} break;
	case IR_ADD:
		{
			if (inst[op1].opcode == IR_CONST && inst[op1].op0 == 1) {
				x86_select_inst(ctx, op0, dst);
				x86_emit1(ctx, X86_INC, dst);
			} else if (inst[op1].opcode == IR_CONST) {
				x86_select_inst(ctx, op0, dst);
				op1 = inst[op1].op0;
				mach_token src = make_mach_token(MACH_CONST, op1, size);
				x86_emit2(ctx, X86_ADD, dst, src);
			} else if (inst[op0].opcode == IR_CONST) {
				x86_select_inst(ctx, op1, dst);
				op0 = inst[op0].op0;
				mach_token src = make_mach_token(MACH_CONST, op0, size);
				x86_emit2(ctx, X86_ADD, dst, src);
			} else {
				mach_token src = x86_vreg(op1, inst[op1].size);
				x86_select_inst(ctx, op0, dst);
				x86_select_inst(ctx, op1, src);
				x86_emit2(ctx, X86_ADD, dst, src);
			}
		} break;
	case IR_SUB:
		{
			if (inst[op1].opcode == IR_CONST && inst[op1].op0 == 1) {
				x86_select_inst(ctx, op0, dst);
				x86_emit1(ctx, X86_DEC, dst);
			} else if (inst[op0].opcode == IR_CONST && inst[op0].op0 == 0) {
				x86_select_inst(ctx, op1, dst);
				x86_emit1(ctx, X86_NEG, dst);
			} else if (inst[op1].opcode == IR_CONST) {
				isize src_size = inst[op1].size;
				op1 = inst[op1].op0;
				x86_select_inst(ctx, op0, dst);
				mach_token src = make_mach_token(MACH_CONST, op1, src_size);
				x86_emit2(ctx, X86_SUB, dst, src);
			} else {
				mach_token src = x86_vreg(op1, inst[op1].size);
				x86_select_inst(ctx, op0, dst);
				x86_select_inst(ctx, op1, src);
				x86_emit2(ctx, X86_SUB, dst, src);
			}
		} break;
	case IR_MUL:
		{
			if (inst[op1].opcode == IR_CONST && inst[op1].op0 == 1) {
				x86_select_inst(ctx, op0, dst);
			} else if (inst[op1].opcode == IR_CONST && inst[op1].op0 == 2) {
				x86_select_inst(ctx, op0, dst);
				x86_emit2(ctx, X86_ADD, dst, dst);
			} else {
				mach_token rax = make_mach_token(MACH_REG, X86_RAX, inst[op0].size);
				mach_token src = x86_vreg(op1, inst[op1].size);

				x86_select_inst(ctx, op0, rax);
				x86_select_inst(ctx, op1, src);
				x86_emit1(ctx, X86_IMUL, src);
				x86_emit2(ctx, X86_MOV, dst, rax);
			}
		} break;
	case IR_DIV:
	case IR_MOD:
		{
			mach_token rax = make_mach_token(MACH_REG, X86_RAX, inst[op0].size);
			mach_token rcx = make_mach_token(MACH_REG, X86_RCX, inst[op1].size);
			mach_token rdx = make_mach_token(MACH_REG, X86_RDX, dst.size);
			mach_token zero = make_mach_token(MACH_CONST, 0, dst.size);

			x86_select_inst(ctx, op0, rax);
			x86_select_inst(ctx, op1, rcx);
			x86_emit2(ctx, X86_MOV, rdx, zero);
			x86_emit1(ctx, X86_IDIV, rcx);
			x86_emit2(ctx, X86_MOV, dst, opcode == IR_DIV ? rax : rdx);
		} break;
	case IR_FEQ:
	case IR_FGT:
	case IR_FGE:
	case IR_FLT:
	case IR_FLE:
		is_float = true;
		/* fallthrough */
	case IR_EQ:
	case IR_LT:
	case IR_GT:
	case IR_GE:
	case IR_LE:
	case IR_LTU:
	case IR_GTU:
	case IR_GEU:
	case IR_LEU:
		{
			x86_opcode x86_opcode = is_float ? X86_COMISS : X86_CMP;
			mach_token dst_byte = dst;
			dst_byte.size = 1;

			mach_token lhs = x86_vreg(op0, inst[op0].size);
			if (is_float && lhs.kind == MACH_REG) {
				ctx->is_float[lhs.value] = true;
			}

			mach_token rhs = x86_vreg(op1, inst[op1].size);
			if (is_float && rhs.kind == MACH_REG) {
				ctx->is_float[rhs.value] = true;
			}

			x86_select_inst(ctx, op0, lhs);
			x86_select_inst(ctx, op1, rhs);
			x86_emit2(ctx, x86_opcode, lhs, rhs);
			x86_emit1(ctx, x86_get_setcc_opcode(opcode), dst_byte);
			x86_emit2(ctx, X86_MOVZX, dst, dst_byte);
		} break;
	case IR_SHL:
	case IR_SHR:
		{
			mach_token shift = make_mach_token(MACH_REG, X86_RCX, 1);

			x86_select_inst(ctx, op0, dst);
			x86_select_inst(ctx, op1, shift);
			x86_emit2(ctx, opcode == IR_SHL ? X86_SHL : X86_SHR, dst, shift);
		} break;
	case IR_AND:
	case IR_OR:
	case IR_XOR:
		{
			mach_token src = x86_vreg(op1, inst[op1].size);
			x86_opcode x86_opcode =
				opcode == IR_AND ? X86_AND :
				opcode == IR_OR ? X86_OR : X86_XOR;

			if (inst[op1].opcode == IR_CONST) {
				src = make_mach_token(MACH_CONST, inst[op1].op0, src.size);
				x86_select_inst(ctx, op0, dst);
				x86_emit2(ctx, x86_opcode, dst, src);
			} else if (inst[op0].opcode == IR_CONST) {
				src = make_mach_token(MACH_CONST, inst[op0].op0, src.size);
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
				dst = x86_vreg(inst[op0].op0, inst[op0].size);
				x86_select_inst(ctx, inst[op0].op0, dst);
				mach_token src = x86_select_const(ctx, inst[op0].op1);
				src.size = dst.size;
				if (is_float && dst.kind == MACH_REG) {
					ctx->is_float[dst.value] = true;
				}

				if (is_float && src.kind == MACH_REG) {
					ctx->is_float[src.value] = true;
				}

				x86_opcode cmp_opcode = (is_float ? X86_COMISS : X86_CMP);
				x86_emit2(ctx, cmp_opcode, dst, src);
				jcc = x86_get_jcc_opcode(inst[op0].opcode, is_jiz);
			} else {
				mach_token src = x86_vreg(op0, inst[op0].size);
				x86_select_inst(ctx, op0, src);
				ASSERT(src.size > 0 && src.size <= 8);
				x86_emit2(ctx, X86_TEST, src, src);
			}

			x86_emit1(ctx, jcc, make_label(op1));
		} break;
	case IR_RET:
	case IR_FRET:
		{
			mach_token return_reg;
			if (opcode == IR_FRET) {
				return_reg = make_mach_token(MACH_REG, X86_XMM0, size);
			} else {
				return_reg = make_mach_token(MACH_REG, X86_RAX, size);
			}

			if (op0 == 0) {
				x86_emit0(ctx, X86_RET);
			} else {
				x86_select_inst(ctx, op0, return_reg);
				return_reg.flags |= MACH_IMPLICIT;
				x86_emit1(ctx, X86_RET, return_reg);
			}
		} break;
	case IR_SEXT:
		{
			mach_token src = x86_vreg(op0, inst[op0].size);
			ASSERT(src.size <= dst.size);

			x86_select_inst(ctx, op0, src);
			x86_emit2(ctx, X86_MOVSX, dst, src);
		} break;
	case IR_ZEXT:
		{
			mach_token src = x86_vreg(op0, inst[op0].size);
			ASSERT(src.size <= dst.size);

			x86_select_inst(ctx, op0, src);
			x86_emit2(ctx, X86_MOVZX, dst, src);
		} break;
	case IR_TRUNC:
		{
			dst.size = inst[op0].size;
			x86_select_inst(ctx, op0, dst);
		} break;
	case IR_CALL:
		{
			mach_token called = x86_vreg(op0, 8);
			if (inst[op0].opcode == IR_BUILTIN) {
				mach_token src = {0};
				ir_builtin builtin = inst[op0].op0;
				switch (builtin) {
				case BUILTIN_POPCOUNT:
					src = x86_vreg(inst[inst_index - 1].op0, size);
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
					isize param_size = param_inst.size;
					switch (param_index) {
					case 0:
						{
							mach_token rdi = make_mach_token(MACH_REG, X86_RDI, param_size);
							x86_select_inst(ctx, param_inst.op0, rdi);
						} break;
					case 1:
						{
							mach_token rsi = make_mach_token(MACH_REG, X86_RSI, param_size);
							x86_select_inst(ctx, param_inst.op0, rsi);
						} break;
					case 2:
						{
							mach_token rdx = make_mach_token(MACH_REG, X86_RDX, param_size);
							x86_select_inst(ctx, param_inst.op0, rdx);
						} break;
					case 3:
						{
							mach_token rcx = make_mach_token(MACH_REG, X86_RCX, param_size);
							x86_select_inst(ctx, param_inst.op0, rcx);
						} break;
					case 4:
						{
							mach_token r8 = make_mach_token(MACH_REG, X86_R8, param_size);
							x86_select_inst(ctx, param_inst.op0, r8);
						} break;
					case 5:
						{
							mach_token r9 = make_mach_token(MACH_REG, X86_R9, param_size);
							x86_select_inst(ctx, param_inst.op0, r9);
						} break;
					default:
						ASSERT(!"Too many arguments");
						break;
					}
				}

				mach_token rax = make_mach_token(MACH_REG, X86_RAX, size);
				x86_emit1(ctx, X86_CALL, called);
				x86_emit2(ctx, X86_MOV, dst, rax);
			}
		} break;
	case IR_FVAR:
		{
			mach_token src = x86_vreg(inst_index, size);
			ctx->is_float[inst_index] = true;
			x86_emit2(ctx, X86_MOVSS, dst, src);
		} break;
	case IR_FADD:
	case IR_FSUB:
	case IR_FMUL:
	case IR_FDIV:
		{
			x86_opcode x86_opcode = X86_NOP;
			switch (opcode) {
			case IR_FADD: x86_opcode = X86_ADDSS; break;
			case IR_FSUB: x86_opcode = X86_SUBSS; break;
			case IR_FMUL: x86_opcode = X86_MULSS; break;
			case IR_FDIV: x86_opcode = X86_DIVSS; break;
			default:
				ASSERT(!"WTF");
			}

			mach_token src = x86_vreg(op1, inst[op1].size);
			ctx->is_float[op1] = true;
			if (dst.kind == MACH_REG) {
				ctx->is_float[dst.value] = true;
			}

			x86_select_inst(ctx, op0, dst);
			x86_select_inst(ctx, op1, src);
			x86_emit2(ctx, x86_opcode, dst, src);
		} break;
		break;
	}
}

static void
x86_emit_token(stream *out, mach_token token, symbol_table symtab)
{
	x86_register reg;

	b32 print_size = (token.kind == MACH_SPILL
		|| (token.flags & MACH_INDIRECT));
	if (print_size) {
		switch (token.size) {
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

	switch (token.kind) {
	case MACH_GLOBAL:
		{
			ASSERT(token.value < symtab.symbol_count);
			symbol *sym = &symtab.symbols[token.value];
			if (sym->name.length > 0) {
				stream_prints(out, sym->name);
			} else {
				stream_print(out, "L#");
				stream_printu(out, token.value);
			}
		} break;
	case MACH_SPILL:
		stream_print(out, "[rsp+");
		stream_printu(out, token.value);
		stream_print(out, "]");
		break;
	case MACH_LABEL:
		stream_print(out, ".L");
		stream_printu(out, token.value);
		break;
	case MACH_REG:
		if (token.flags & MACH_INDIRECT) {
			stream_print(out, "[");
			token.size = 8;
		}

		reg = (x86_register)token.value;
		stream_print(out, x86_get_register_name(reg, token.size));

		if (token.flags & MACH_INDIRECT) {
			stream_print(out, "]");
		}
		break;
	case MACH_CONST:
		stream_printu(out, token.value);
		break;
	default:
		ASSERT(false);
		stream_print(out, "(invalid token)");
	}
}

static void
x86_generate(stream *out, ir_program p, arena *arena)
{
	symbol_table symtab = p.symtab;
	isize max_token_count = 1024 * 1024;
	mach_token *tokens = ALLOC(arena, max_token_count, mach_token);

	stream_print(out, "section .text\n");
	symbol_id sym_id = symtab.section[SECTION_TEXT];
	while (sym_id.value != 0) {
		symbol *sym = &symtab.symbols[sym_id.value];
		ir_function *ir_func = &p.funcs[sym_id.value];

		//
		// 1. Instruction selection
		//

		x86_context ctx = {0};
		ctx.inst = p.insts + ir_func->inst_index;
		ctx.tokens = tokens;
		ctx.max_token_count = max_token_count;
		ctx.is_float = ALLOC(arena, ir_func->inst_count, b32);

		ir_inst *inst = p.insts + ir_func->inst_index;
		i32 *ref_count = get_ref_count(inst, ir_func->inst_count, arena);
		for (isize j = 0; j < ir_func->inst_count; j++) {
			ir_opcode opcode = inst[j].opcode;
			if (ref_count[j] == 1 || opcode == IR_VAR) {
				continue;
			}

			isize size = inst[j].size;
			mach_token dst = x86_vreg(j, size);
			if (opcode == IR_MOV || opcode == IR_STORE
				|| opcode == IR_FMOV || opcode == IR_FSTORE)
			{
				isize op0_size = inst[inst[j].op0].size;
				dst = x86_vreg(inst[j].op0, op0_size);
				if (opcode == IR_FMOV || opcode == IR_FSTORE) {
					ctx.is_float[dst.value] = true;
				}
			}

			x86_select_inst(&ctx, j, dst);
		}

		isize token_count = ctx.token_count;
		if (token_count == 0) {
			// NOTE: Do not generate code for empty functions
			goto next;
		}

		//
		// 2. Register allocation
		//

		regalloc_hints hints = {0};
		hints.is_float = ctx.is_float;
		hints.tmp_mregs = x86_temp_regs;
		hints.int_mreg_count = X86_INT_REGISTER_COUNT;
		hints.tmp_mreg_count = LENGTH(x86_temp_regs);
		hints.mreg_count = X86_REGISTER_COUNT;
		hints.vreg_count = ir_func->inst_count;
		hints.label_count = p.max_label_count;

		regalloc_info info = regalloc(tokens, token_count, hints, arena);

		//
		// 3. Generate the code
		//

		stream_prints(out, sym->name);
		stream_print(out, ":\n");

		// Print function prologue
		isize used_volatile_register_count = 0;
		for (isize j = 0; j < LENGTH(x86_saved_regs); j++) {
			u32 mreg = x86_saved_regs[j];
			if (info.used[mreg]) {
				stream_print(out, "\tpush ");
				x86_emit_token(out, make_mach_token(MACH_REG, mreg, 8), symtab);
				stream_print(out, "\n");
				used_volatile_register_count++;
			}
		}

		// TODO: Set function stack size
		isize stack_size = 0;
#if 0
		char *code = (char *)p.tokens + func->inst_offset;
		mach_inst *first_inst = (mach_inst *)code;
		if (first_inst->opcode == X86_SUB) {
			mach_token *tokens = (mach_token *)(first_inst + 1);
			if (tokens[0].kind == MACH_REG && tokens[0].value == X86_RSP) {
				ASSERT(tokens[1].kind == MACH_CONST);
				stack_size = tokens[1].value;

				stack_size += 8 * info.spill_count;
				stack_size += used_volatile_register_count;
				b32 is_stack_aligned = ((stack_size & 15) == 8);
				if (!is_stack_aligned) {
					stack_size += 24 - (stack_size & 15);
				}

				tokens[1].value = stack_size;
			}
		}
#endif
		b32 first_inst = true;
		b32 first_token = true;

		// TODO: We need to ensure that instructions do not
		// contain two address tokens, e.g. mov [rax], [rax]
		for (isize i = 0; i < token_count; i++) {
			mach_token token = tokens[i];
			if (token.flags & MACH_IMPLICIT) {
				continue;
			}

			switch (token.kind) {
			case MACH_INST:
				if (first_inst) {
					first_inst = false;
				} else {
					stream_print(out, "\n");
				}

				if (token.value == X86_LABEL) {
					if (i + 1 < token_count) {
						stream_print(out, ".L");
						stream_printu(out, tokens[i + 1].value);
						stream_print(out, ":");
						i++;
					}
				} else if (token.value == X86_RET) {
					stream_print(out, "\tjmp .exit\n");
				} else {
					if (i + 2 < token_count
						&& tokens[i + 1].kind == MACH_SPILL
						&& tokens[i + 2].kind == MACH_SPILL)
					{
						isize op1_size = tokens[i + 2].size;
						mach_token new_token = make_mach_token(MACH_REG, X86_RCX, op1_size);

						stream_print(out, "\tmov ");
						x86_emit_token(out, new_token, symtab);
						stream_print(out, ", ");
						x86_emit_token(out, tokens[i + 2], symtab);
						stream_print(out, "\n");

						tokens[i + 2] = new_token;
					}

					stream_print(out, "\t");
					stream_print(out, x86_get_opcode_name(token.value));
					stream_print(out, " ");
				}

				first_token = true;
				break;
			default:
				if (first_token) {
					first_token = false;
				} else {
					stream_print(out, ", ");
				}

				x86_emit_token(out, token, symtab);
				break;
			}
		}

		// Print function epilogue
		stream_print(out, "\n.exit:\n");
		if (stack_size > 0) {
			stream_print(out, "\tadd rsp, ");
			stream_printu(out, stack_size);
			stream_print(out, "\n");
		}

		for (isize j = 0; j < LENGTH(x86_saved_regs); j++) {
			u32 mreg = x86_saved_regs[j];
			if (info.used[mreg]) {
				stream_print(out, "\tpop ");
				x86_emit_token(out, make_mach_token(MACH_REG, mreg, 8), symtab);
				stream_print(out, "\n");
			}
		}

		stream_print(out, "\tret\n\n");
next:
		sym_id = sym->next;
	}

	// Print the remaining sections
	for (isize j = 0; j < SECTION_COUNT; j++) {
		if (j == SECTION_TEXT) {
			// We print the text section again to print the symbol
			// declarations, like `global main`.
			stream_print(out, "section .text\n");
		} else if (j == SECTION_DATA) {
			stream_print(out, "section .data\n");
		} else if (j == SECTION_RODATA) {
			stream_print(out, "section .rodata\n");
		} else if (j == SECTION_BSS) {
			stream_print(out, "section .bss\n");
		} else {
			// Unsupported section
			continue;
		}

		symbol_id sym_id = symtab.section[j];
		while (sym_id.value != 0) {
			symbol *sym = &symtab.symbols[sym_id.value];
			if (sym->linkage == LINK_STATIC) {
				stream_print(out, "static ");
				stream_prints(out, sym->name);
				stream_print(out, "\n");
			} else if (sym->linkage == LINK_EXTERN) {
				stream_print(out, "extern ");
				stream_prints(out, sym->name);
				stream_print(out, "\n");
			} else if (sym->name.length > 0) {
				stream_print(out, "global ");
				stream_prints(out, sym->name);
				stream_print(out, "\n");
			}

			// NOTE: text section was already printed in the loop above
			if (sym->size > 0 && j != SECTION_TEXT) {
				if (sym->name.length > 0) {
					stream_prints(out, sym->name);
				} else {
					stream_print(out, "L#");
					stream_printu(out, sym_id.value);
				}

				if (j == SECTION_DATA || j == SECTION_RODATA) {
					// NOTE: Inside data or rodata section, symbols contain byte data
					if (sym->data) {
						stream_print(out, ": db ");
						u8 *byte = sym->data;
						for (isize i = 0; i < sym->size; i++) {
							if (i != 0) {
								stream_print(out, ", ");
							}

							stream_print_hex(out, byte[i]);
						}

						stream_print(out, "\n");
					} else {
						stream_print(out, ": times ");
						stream_printu(out, sym->size);
						stream_print(out, " db 0\n");
					}
				} else if (j == SECTION_BSS) {
					// NOTE; Inside bss section, symbols have no data
					stream_print(out, " resb ");
					stream_print_hex(out, sym->size);
					stream_print(out, "\n");
				}
			}

			sym_id = sym->next;
		}
	}
}
