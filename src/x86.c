static void
x86_push_token(x86_context *ctx, mach_token token)
{
	ASSERT(ctx->token_count < ctx->max_token_count);
	ctx->tokens[ctx->token_count++] = token;
}

static void
x86_emit(x86_context *ctx, x86_opcode opcode, x86_operand_size size,
	isize arg_count, x86_operand_kind *kind, mach_token *args)
{
	ASSERT(arg_count <= 4);
	if (arg_count > 4) {
		arg_count = 4;
	}

	if (opcode == X86_MOV && equals_token(args[0], args[1])
		&& kind[0] == X86_REG && kind[1] == X86_REG)
	{
		return;
	}

	mach_token inst = {0};
	inst.kind = MACH_INST;
	inst.value |= opcode;
	inst.value |= (size & 0xf) << 16;
	if (opcode == X86_CALL) {
		inst.flags |= MACH_CALL;
	}

	for (isize i = 0; i < arg_count; i++) {
		inst.value |= (kind[i] & 0x7) << (20 + 3 * i);
	}

	x86_push_token(ctx, inst);

	for (isize i = arg_count - 1; i >= 0; i--) {
		mach_token arg = args[i];
		if (kind[i] == X86_REG || kind[i] == X86_BASE || kind[i] == X86_INDEX) {
			if (i == 0) {
				switch (opcode) {
				case X86_CMP:
				case X86_TEST:
					arg.flags = MACH_USE;
					break;
				case X86_MOV:
					arg.flags = MACH_DEF;
					break;
				default:
					arg.flags = MACH_USE | MACH_DEF;
				}
			} else {
				arg.flags = MACH_USE;
			}
		} else {
			arg.flags = 0;
		}

		x86_push_token(ctx, arg);
	}
}

static void
x86_emit0(x86_context *ctx, x86_opcode opcode)
{
	x86_emit(ctx, opcode, 0, 0, NULL, NULL);
}

static void
x86_emit1(x86_context *ctx, x86_opcode opcode, x86_operand_size size,
	x86_operand_kind dst_kind, mach_token dst)
{
	x86_emit(ctx, opcode, size, 1, &dst_kind, &dst);
}

static void
x86_emit2(x86_context *ctx, x86_opcode opcode, x86_operand_size size,
	x86_operand_kind dst_kind, mach_token dst,
	x86_operand_kind src_kind, mach_token src)
{
	x86_operand_kind kind[2] = {dst_kind, src_kind};
	mach_token args[2] = {dst, src};

	x86_emit(ctx, opcode, size, 2, kind, args);
}

static void
x86_emit3(x86_context *ctx, x86_opcode opcode, x86_operand_size size,
	x86_operand_kind arg0_kind, mach_token arg0,
	x86_operand_kind arg1_kind, mach_token arg1,
	x86_operand_kind arg2_kind, mach_token arg2)
{
	x86_operand_kind kind[3] = {arg0_kind, arg1_kind, arg2_kind};
	mach_token args[3] = {arg0, arg1, arg2};

	x86_emit(ctx, opcode, size, 3, kind, args);
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
x86_mreg(x86_register value, u32 size)
{
	mach_token result = make_mach_token(MACH_REG, value, size);
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

// IMPORTANT: Destination must be a register!
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
			x86_emit1(ctx, X86_LABEL, X86_DWORD, X86_IMM, src);
		} break;
	case IR_GLOBAL:
		{
			mach_token src = make_global(op0);
			x86_emit2(ctx, X86_MOV, size, X86_REG, dst, X86_SYM, src);
		} break;
	case IR_VAR:
		{
			mach_token src = x86_vreg(inst_index, size);
			x86_emit2(ctx, X86_MOV, size, X86_REG, dst, X86_REG, src);
		} break;
	case IR_PARAM:
		{
			// TODO: Set the correct size of the parameters
			mach_token src = {0};
			switch (op0) {
			case 0:
				src = x86_mreg(X86_RDI, dst.size);
				break;
			case 1:
				src = x86_mreg(X86_RSI, dst.size);
				break;
			case 2:
				src = x86_mreg(X86_RDX, dst.size);
				break;
			case 3:
				src = x86_mreg(X86_RCX, dst.size);
				break;
			case 4:
				src = x86_mreg(X86_R8, dst.size);
				break;
			case 5:
				src = x86_mreg(X86_R9, dst.size);
				break;
			default:
				// TODO: The function should store the parameter offsets of
				// each argument. Then, we can calculate the offset for this
				// parameter.
				ASSERT(!"Too many parameters");
			}

			x86_emit2(ctx, X86_MOV, size, X86_REG, dst, X86_REG, src);
		} break;
	case IR_CVT:
	case IR_FCVT:
		{
#if 0
			mach_token src = x86_vreg(op0, inst[op0].size);

			x86_select_inst(ctx, op0, src);
			if (type == IR_F32) {
				x86_emit2(ctx, X86_CVTSI2SS, size, dst, src);
			} else if (type == IR_F64) {
				x86_emit2(ctx, X86_CVTSI2SD, size, dst, src);
			} else if (op0_type == IR_F32) {
				src.size = 8;
				x86_emit2(ctx, X86_CVTTSS2SI, size, dst, src);
			} else if (op0_type == IR_F64) {
				src.size = 8;
				x86_emit2(ctx, X86_CVTTSD2SI, size, dst, src);
			}

			if (type == IR_I8) {
			}
#endif
		} break;
	case IR_CONST:
		{
			mach_token src = make_mach_token(MACH_CONST, op0, size);
			x86_emit2(ctx, X86_MOV, size, X86_REG, dst, X86_IMM, src);
		} break;
	case IR_BUILTIN:
		{
			ASSERT(!"Builtins should be handled in their respective instruction");
		} break;
	case IR_ALLOC:
		{
			mach_token src = make_mach_token(MACH_CONST, op1, size);
			mach_token rsp = x86_mreg(X86_RSP, dst.size);

			x86_emit3(ctx, X86_LEA, size, X86_REG, dst, X86_BASE, rsp, X86_DISP_IMM, src);
		} break;
	case IR_COPY:
	case IR_FCOPY:
		{
			x86_select_inst(ctx, op0, dst);
		} break;
	case IR_MOV:
	case IR_FMOV:
		{
			ASSERT(inst[op0].opcode == IR_VAR);
			dst = x86_vreg(op0, inst[op0].size);
			x86_select_inst(ctx, op1, dst);
		} break;
	case IR_LOAD:
	case IR_FLOAD:
		{
			x86_opcode mov = X86_MOV;
			if (opcode == IR_FLOAD) {
				mov = X86_MOVSS;
				if (dst.kind == MACH_REG) {
					ctx->vreg_class[dst.value] = X86_XMM_MASK;
				}
			}

			if (inst[op0].opcode == IR_GLOBAL) {
				mach_token src = make_global(inst[op0].op0);

				x86_emit2(ctx, mov, size, X86_REG, dst, X86_DISP_SYM, src);
			} else if (inst[op0].opcode == IR_ALLOC) {
				mach_token src = make_mach_token(MACH_CONST, op1, size);
				mach_token rsp = x86_mreg(X86_RSP, dst.size);

				x86_emit3(ctx, X86_MOV, size, X86_REG, dst, X86_BASE, rsp, X86_DISP_IMM, src);
			} else {
				mach_token src = x86_vreg(op0, inst[op0].size);

				x86_select_inst(ctx, op0, src);
				x86_emit2(ctx, mov, size, X86_REG, dst, X86_BASE, src);
			}
		} break;
	case IR_STORE:
	case IR_FSTORE:
		{
			mach_token src = x86_vreg(op1, inst[op1].size);
			x86_opcode mov = (opcode == IR_FSTORE ? X86_MOVSS : X86_MOV);

			ASSERT(!equals_token(src, dst));
			if (inst[op1].opcode != IR_CONST) {
				x86_select_inst(ctx, op1, src);
			} else {
				src = make_mach_token(MACH_CONST, inst[op1].op0, size);
			}

			x86_select_inst(ctx, op0, dst);
			x86_emit2(ctx, mov, size, X86_BASE, dst, X86_REG, src);
		} break;
	case IR_ADD:
		{
			if (inst[op1].opcode == IR_CONST && inst[op1].op0 == 1) {
				x86_select_inst(ctx, op0, dst);
				x86_emit1(ctx, X86_INC, size, X86_REG, dst);
			} else if (inst[op1].opcode == IR_CONST) {
				x86_select_inst(ctx, op0, dst);
				op1 = inst[op1].op0;
				mach_token src = make_mach_token(MACH_CONST, op1, size);
				x86_emit2(ctx, X86_ADD, size, X86_REG, dst, X86_REG, src);
			} else if (inst[op0].opcode == IR_CONST) {
				x86_select_inst(ctx, op1, dst);
				op0 = inst[op0].op0;
				mach_token src = make_mach_token(MACH_CONST, op0, size);
				x86_emit2(ctx, X86_ADD, size, X86_REG, dst, X86_REG, src);
			} else {
				mach_token src = x86_vreg(op1, inst[op1].size);
				x86_select_inst(ctx, op0, dst);
				x86_select_inst(ctx, op1, src);
				x86_emit2(ctx, X86_ADD, size, X86_REG, dst, X86_REG, src);
			}
		} break;
	case IR_SUB:
		{
			if (inst[op1].opcode == IR_CONST && inst[op1].op0 == 1) {
				x86_select_inst(ctx, op0, dst);
				x86_emit1(ctx, X86_DEC, size, X86_REG, dst);
			} else if (inst[op0].opcode == IR_CONST && inst[op0].op0 == 0) {
				x86_select_inst(ctx, op1, dst);
				x86_emit1(ctx, X86_NEG, size, X86_REG, dst);
			} else if (inst[op1].opcode == IR_CONST) {
				isize src_size = inst[op1].size;
				op1 = inst[op1].op0;
				x86_select_inst(ctx, op0, dst);
				mach_token src = make_mach_token(MACH_CONST, op1, src_size);
				x86_emit2(ctx, X86_SUB, size, X86_REG, dst, X86_IMM, src);
			} else {
				mach_token src = x86_vreg(op1, inst[op1].size);
				x86_select_inst(ctx, op0, dst);
				x86_select_inst(ctx, op1, src);
				x86_emit2(ctx, X86_SUB, size, X86_REG, dst, X86_REG, src);
			}
		} break;
	case IR_MUL:
		{
			if (inst[op1].opcode == IR_CONST && inst[op1].op0 == 1) {
				x86_select_inst(ctx, op0, dst);
			} else if (inst[op1].opcode == IR_CONST && inst[op1].op0 == 2) {
				x86_select_inst(ctx, op0, dst);
				x86_emit2(ctx, X86_ADD, size, X86_REG, dst, X86_REG, dst);
			} else {
				mach_token rax = x86_mreg(X86_RAX, inst[op0].size);
				mach_token src = x86_vreg(op1, inst[op1].size);

				x86_select_inst(ctx, op0, rax);
				x86_select_inst(ctx, op1, src);
				x86_emit1(ctx, X86_IMUL, src.size, X86_REG, src);
				x86_emit2(ctx, X86_MOV, size, X86_REG, dst, X86_REG, rax);
			}
		} break;
	case IR_DIV:
	case IR_MOD:
		{
			mach_token rax = x86_mreg(X86_RAX, inst[op0].size);
			mach_token rcx = x86_mreg(X86_RCX, inst[op1].size);
			mach_token rdx = x86_mreg(X86_RDX, dst.size);
			mach_token zero = make_mach_token(MACH_CONST, 0, dst.size);
			mach_token src = opcode == IR_DIV ? rax : rdx;

			x86_select_inst(ctx, op0, rax);
			x86_select_inst(ctx, op1, rcx);
			x86_emit2(ctx, X86_MOV, size, X86_REG, rdx, X86_IMM, zero);
			x86_emit1(ctx, X86_IDIV, rcx.size, X86_REG, rcx);
			x86_emit2(ctx, X86_MOV, size, X86_REG, dst, X86_REG, src);
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
			x86_opcode setcc = x86_get_setcc_opcode(opcode);
			x86_opcode cmp = is_float ? X86_COMISS : X86_CMP;
			mach_token dst_byte = dst;
			dst_byte.size = 1;

			mach_token lhs = x86_vreg(op0, inst[op0].size);
			if (is_float && lhs.kind == MACH_REG) {
				ctx->vreg_class[lhs.value] = X86_XMM_MASK;
			}

			mach_token rhs = x86_vreg(op1, inst[op1].size);
			if (is_float && rhs.kind == MACH_REG) {
				ctx->vreg_class[rhs.value] = X86_XMM_MASK;
			}

			x86_select_inst(ctx, op0, lhs);
			x86_select_inst(ctx, op1, rhs);
			x86_emit2(ctx, cmp, size, X86_REG, lhs, X86_REG, rhs);
			x86_emit1(ctx, setcc, X86_BYTE, X86_REG, dst_byte);
			x86_emit2(ctx, X86_MOVZX, size, X86_REG, dst, X86_REG, dst_byte);
		} break;
	case IR_SHL:
	case IR_SHR:
		{
			mach_token rcx = x86_mreg(X86_RCX, 1);
			x86_opcode shift = opcode == IR_SHL ? X86_SHL : X86_SHR;

			x86_select_inst(ctx, op0, dst);
			x86_select_inst(ctx, op1, rcx);
			x86_emit2(ctx, shift, size, X86_REG, dst, X86_REG, rcx);
		} break;
	case IR_AND:
	case IR_OR:
	case IR_XOR:
		{
			mach_token src = x86_vreg(op1, inst[op1].size);
			x86_opcode bitop =
				opcode == IR_AND ? X86_AND :
				opcode == IR_OR ? X86_OR : X86_XOR;

			if (inst[op1].opcode == IR_CONST) {
				src = make_mach_token(MACH_CONST, inst[op1].op0, src.size);
				x86_select_inst(ctx, op0, dst);
			} else if (inst[op0].opcode == IR_CONST) {
				src = make_mach_token(MACH_CONST, inst[op0].op0, src.size);
				x86_select_inst(ctx, op1, dst);
			} else {
				x86_select_inst(ctx, op0, dst);
				x86_select_inst(ctx, op1, src);
			}

			x86_emit2(ctx, bitop, size, X86_REG, dst, X86_REG, src);
		} break;
	case IR_NOT:
		{
			x86_select_inst(ctx, op0, dst);
			x86_emit1(ctx, X86_NOT, size, X86_REG, dst);
		} break;
	case IR_JMP:
		x86_emit1(ctx, X86_JMP, X86_DWORD, X86_IMM, make_label(op0));
		break;
	case IR_JIZ:
	case IR_JNZ:
		{
			b32 is_jiz = opcode == IR_JIZ;
			x86_opcode jcc = is_jiz ? X86_JZ : X86_JNZ;
			if (is_comparison_opcode(inst[op0].opcode)) {
				jcc = x86_get_jcc_opcode(inst[op0].opcode, is_jiz);

				dst = x86_vreg(inst[op0].op0, inst[op0].size);
				x86_select_inst(ctx, inst[op0].op0, dst);
				mach_token src = x86_select_const(ctx, inst[op0].op1);
				src.size = dst.size;

				if (is_float && dst.kind == MACH_REG) {
					ctx->vreg_class[dst.value] = X86_XMM_MASK;
				}

				if (is_float && src.kind == MACH_REG) {
					ctx->vreg_class[src.value] = X86_XMM_MASK;
				}

				x86_opcode cmp = (is_float ? X86_COMISS : X86_CMP);
				x86_emit2(ctx, cmp, size, X86_REG, dst, X86_REG, src);
			} else {
				mach_token src = x86_vreg(op0, inst[op0].size);
				x86_select_inst(ctx, op0, src);
				ASSERT(src.size > 0 && src.size <= 8);
				x86_emit2(ctx, X86_TEST, size, X86_REG, src, X86_REG, src);
			}

			x86_emit1(ctx, jcc, X86_DWORD, X86_IMM, make_label(op1));
		} break;
	case IR_RET:
	case IR_FRET:
		{
			mach_token return_reg;
			if (opcode == IR_FRET) {
				return_reg = x86_mreg(X86_XMM0, size);
			} else {
				return_reg = x86_mreg(X86_RAX, size);
			}

			if (op0 == 0) {
				x86_emit0(ctx, X86_RET);
			} else {
				x86_select_inst(ctx, op0, return_reg);
				x86_emit1(ctx, X86_RET, 0, 0, return_reg);
			}
		} break;
	case IR_SEXT:
		{
			mach_token src = x86_vreg(op0, inst[op0].size);
			ASSERT(src.size <= dst.size);

			x86_select_inst(ctx, op0, src);
			x86_emit2(ctx, X86_MOVSX, size, X86_REG, dst, X86_REG, src);
		} break;
	case IR_ZEXT:
		{
			mach_token src = x86_vreg(op0, inst[op0].size);
			ASSERT(src.size <= dst.size);

			x86_select_inst(ctx, op0, src);
			x86_emit2(ctx, X86_MOVZX, size, X86_REG, dst, X86_REG, src);
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
					x86_emit2(ctx, X86_POPCNT, size, X86_REG, dst, X86_REG, src);
					break;
				default:
					ASSERT(!"Builtin is not supported");
				}
			} else {
				x86_operand_kind called_kind = X86_REG;
				if (inst[op0].opcode == IR_GLOBAL) {
					called = make_global(inst[op0].op0);
					called_kind = X86_SYM;
					ASSERT(called.value < ctx->symtab->symbol_count);
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
							mach_token rdi = x86_mreg(X86_RDI, param_size);
							x86_select_inst(ctx, param_inst.op0, rdi);
						} break;
					case 1:
						{
							mach_token rsi = x86_mreg(X86_RSI, param_size);
							x86_select_inst(ctx, param_inst.op0, rsi);
						} break;
					case 2:
						{
							mach_token rdx = x86_mreg(X86_RDX, param_size);
							x86_select_inst(ctx, param_inst.op0, rdx);
						} break;
					case 3:
						{
							mach_token rcx = x86_mreg(X86_RCX, param_size);
							x86_select_inst(ctx, param_inst.op0, rcx);
						} break;
					case 4:
						{
							mach_token r8 = x86_mreg(X86_R8, param_size);
							x86_select_inst(ctx, param_inst.op0, r8);
						} break;
					case 5:
						{
							mach_token r9 = x86_mreg(X86_R9, param_size);
							x86_select_inst(ctx, param_inst.op0, r9);
						} break;
					default:
						ASSERT(!"Too many arguments");
						break;
					}
				}

				mach_token rax = x86_mreg(X86_RAX, size);
				x86_emit1(ctx, X86_CALL, X86_QWORD, called_kind, called);
				x86_emit2(ctx, X86_MOV, size, X86_REG, dst, X86_REG, rax);
			}
		} break;
	case IR_FVAR:
		{
			mach_token src = x86_vreg(inst_index, size);
			ctx->vreg_class[inst_index] = X86_XMM_MASK;
			x86_emit2(ctx, X86_MOVSS, size, X86_REG, dst, X86_REG, src);
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
			ctx->vreg_class[op1] = X86_XMM_MASK;
			if (dst.kind == MACH_REG) {
				ctx->vreg_class[dst.value] = X86_XMM_MASK;
			}

			x86_select_inst(ctx, op0, dst);
			x86_select_inst(ctx, op1, src);
			x86_emit2(ctx, x86_opcode, size, X86_REG, dst, X86_REG, src);
		} break;
		break;
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
		ctx.vreg_class = ALLOC(arena, X86_REGISTER_COUNT + ir_func->inst_count, u32);
		ctx.symtab = &symtab;

		for (isize i = 0; i < X86_REGISTER_COUNT + ir_func->inst_count; i++) {
			ctx.vreg_class[i] = ~X86_XMM_MASK;
		}

		u32 curr_block = 0;
		basic_block *blocks = ALLOC(arena, p.max_label_count, basic_block);

		ir_inst *inst = p.insts + ir_func->inst_index;
		i32 *ref_count = get_ref_count(inst, ir_func->inst_count, arena);
		for (isize j = 0; j < ir_func->inst_count; j++) {
			ir_opcode opcode = inst[j].opcode;
			if (ref_count[j] == 1 || opcode == IR_VAR) {
				continue;
			}

			isize size = inst[j].size;
			mach_token dst = x86_vreg(j, size);
			if (opcode == IR_LABEL) {
				u32 prev_block = curr_block;
				curr_block = inst[j].op0;
				// If the previous block was a conditional jump
				if (prev_block > 0 && blocks[prev_block].succ[0] == 0) {
					blocks[prev_block].succ[0] = curr_block;
				}

				blocks[curr_block].offset = j;
			} else {
				x86_select_inst(&ctx, j, dst);

				if (opcode == IR_JIZ) {
					u32 next_block = inst[j].op1;
					isize start = blocks[curr_block].offset;
					blocks[curr_block].succ[0] = 0;
					blocks[curr_block].succ[1] = next_block;
					blocks[curr_block].size = ctx.token_count - start;
				} else if (opcode == IR_JMP) {
					u32 next_block = inst[j].op0;
					isize start = blocks[curr_block].offset;
					blocks[curr_block].succ[0] = next_block;
					blocks[curr_block].succ[1] = next_block;
					blocks[curr_block].size = ctx.token_count - start;
				}
			}
		}

		isize block_start = blocks[curr_block].offset;
		blocks[curr_block].size = ctx.token_count - block_start;
		isize token_count = ctx.token_count;
		if (token_count == 0) {
			// NOTE: Do not generate code for empty functions
			goto next;
		}

		print_x86_program(tokens, token_count);

		//
		// 2. Register allocation
		//

		mach_info mach = {0};
		mach.tmp_mreg_count = LENGTH(x86_temp_regs);
		mach.mreg_count = X86_REGISTER_COUNT;
		mach.vreg_count = ir_func->inst_count;
		mach.mreg_class = ALLOC(arena, mach.mreg_count, u32);
		mach.vreg_class = ctx.vreg_class;
		mach.tmp_mregs = x86_temp_regs;
		mach.pool = ALLOC(arena, X86_REGISTER_COUNT, u32);
		for (isize i = 0; i < X86_REGISTER_COUNT; i++) {
			mach.mreg_class[i] = (1 << i);
			if (i != X86_RSP && i != X86_RBP) {
				mach.pool[mach.pool_size] = i;
				mach.pool_size++;
			}
		}

		u32 *reg_table = regalloc(tokens, token_count, blocks,
			p.max_label_count, mach, arena);

		//
		// 3. Generate the code
		//

		stream_prints(out, sym->name);
		stream_print(out, ":\n");

		// Print function prologue
#if 0
		isize used_volatile_register_count = 0;
		for (isize j = 0; j < LENGTH(x86_saved_regs); j++) {
			u32 mreg = x86_saved_regs[j];
			if (reg_table[mreg]) {
				stream_print(out, "\tpush ");
				stream_print(out, x86_get_register_name(mreg, 8));
				stream_print(out, "\n");
				used_volatile_register_count++;
			}
		}
#endif

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
		for (isize i = 0; i < token_count; i++) {
			mach_token token = tokens[i];
			if (token.kind != MACH_INST) {
				continue;
			}

			isize operand_count = 0;
			x86_operand_kind kinds[4] = {0};
			x86_opcode opcode = (token.value & X86_OPCODE_MASK);
			if (opcode == X86_LABEL) {
				stream_print(out, ".L");
				stream_printu(out, tokens[i + 1].value);
				stream_print(out, ":");
				operand_count = 0;
			} else if (opcode == X86_RET) {
				stream_print(out, "    jmp .exit");
			} else {
				stream_print(out, "    ");
				stream_print(out, x86_get_opcode_name(opcode));

				// Count the number of operands and read their type
				for (isize i = 0; i < 4; i++) {
					kinds[i] = (token.value >> (20 + 3 * i)) & 0x7;
					if (kinds[i] == X86_NIL) {
						break;
					}

					operand_count++;
				}
			}

			b32 first_token = true;
			b32 inside_memory_operand = false;
			for (isize j = operand_count - 1; j >= 0; j--) {
				if (first_token) {
					stream_print(out, " ");
					first_token = false;
				} else if (!inside_memory_operand) {
					stream_print(out, ", ");
				}

				x86_operand_kind kind = kinds[operand_count - 1 - j];
				if (kind == X86_INDEX || kind == X86_DISP_IMM
					|| kind == X86_DISP_SYM || kind == X86_BASE)
				{
					if (!inside_memory_operand) {
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
							stream_print(out, "qword");
							break;
						}

						stream_print(out, "[");
						inside_memory_operand = true;
					} else {
						stream_print(out, " + ");
					}
				} else {
					if (inside_memory_operand) {
						stream_print(out, "], ");
					}
				}

				u32 value = tokens[i + 1 + j].value;
				switch (kind) {
				case X86_REG:
				case X86_BASE:
				case X86_INDEX:
					{
						// TODO: Handle spilled registers. We need to reserve at least
						// three registers, since one instruction can contain three
						// registers, which can all be spilled at the same time.
						if (value >= X86_REGISTER_COUNT) {
							value = reg_table[value];
						}

						stream_print(out, x86_get_register_name(value, token.size));
					} break;
				case X86_IMM:
				case X86_DISP_IMM:
					{
						stream_printu(out, value);
					} break;
				case X86_SYM:
				case X86_DISP_SYM:
					{
						ASSERT(value < symtab.symbol_count);
						symbol *sym = &symtab.symbols[value];
						if (sym->name.length > 0) {
							stream_prints(out, sym->name);
						} else {
							stream_print(out, "L#");
							stream_printu(out, value);
						}
					} break;
				default:
					ASSERT(!"Invalid operand");
				}
			}

			if (inside_memory_operand) {
				stream_print(out, "]");
			}

			stream_print(out, "\n");
		}

		// Print function epilogue
		stream_print(out, "\n.exit:\n");
		if (stack_size > 0) {
			stream_print(out, "\tadd rsp, ");
			stream_printu(out, stack_size);
			stream_print(out, "\n");
		}

#if 0
		for (isize j = 0; j < LENGTH(x86_saved_regs); j++) {
			u32 mreg = x86_saved_regs[j];
			if (info.used[mreg]) {
				stream_print(out, "\tpop ");
				stream_print(out, x86_get_register_name(mreg, 8));
				stream_print(out, "\n");
			}
		}
#endif

		stream_print(out, "\tret\n\n");
next:
		sym_id = sym->next;
	}

	// Print the remaining sections
	for (isize j = 0; j < SECTION_COUNT; j++) {
		symbol_id sym_id = symtab.section[j];
		if (sym_id.value == 0) {
			continue;
		}

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
