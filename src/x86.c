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

	i32 inst_format = opcode;
	inst_format |= (size & 0xf) << 16;
	for (isize i = 0; i < arg_count; i++) {
		inst_format |= (kind[i] & 0x7) << (20 + 3 * i);
	}

	mach_token inst = inst_token(opcode, inst_format);
	if (opcode == X86_CALL) {
		inst.flags |= MACH_CALL;
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

static void x86_select_inst(x86_context *ctx, isize i, mach_token dst, isize dst_size);

static mach_token
x86_register_token(x86_context *ctx, x86_register hint, i32 size)
{
	i32 value = ctx->vreg_count++;
	mach_token result = make_mach_token(value, size);
	result.hint = hint;
	return result;
}

static mach_token
x86_select_const(x86_context *ctx, isize inst_index)
{
	ir_inst *inst = ctx->inst;
	mach_token result;
	b32 is_float = is_float_opcode(inst[inst_index].opcode);
	i32 size = inst[inst_index].size;
	if (inst[inst_index].opcode == IR_CONST) {
		result = make_const(inst[inst_index].args[0], size);
	} else {
		result = register_token(inst_index, is_float);
		x86_select_inst(ctx, inst_index, result, size);
	}

	return result;
}

// IMPORTANT: Destination must be a register!
static void
x86_select_inst(x86_context *ctx, isize i, mach_token dst, isize size)
{
	isize addr_size = 8;
	ir_inst *inst = ctx->inst;
	ir_opcode opcode = inst[i].opcode;
	i32 arg0 = inst[i].args[0];
	i32 arg1 = inst[i].args[1];
	b32 is_float = is_float_opcode(opcode);

	ASSERT(inst[i].size == size);
	switch (opcode) {
	case IR_NOP:
		break;
	case IR_LABEL:
		{
			isize src_size = inst[arg0].size;
			mach_token src = make_const(arg0, src_size);
			x86_emit1(ctx, X86_LABEL, X86_DWORD, X86_IMM, src);
		} break;
	case IR_GLOBAL:
		{
			mach_token src = make_global(arg0);
			x86_emit2(ctx, X86_MOV, size, X86_REG, dst, X86_SYM, src);
		} break;
	case IR_PARAM:
		{
			// TODO: Set the correct size of the parameters
			x86_register mreg = 0;
			switch (arg0) {
			case 0: mreg = X86_RDI; break;
			case 1: mreg = X86_RSI; break;
			case 2: mreg = X86_RDX; break;
			case 3: mreg = X86_RCX; break;
			case 4: mreg = X86_R8;  break;
			case 5: mreg = X86_R9;  break;
			default:
				// TODO: The function should store the parameter offsets of
				// each argument. Then, we can calculate the offset for this
				// parameter.
				ASSERT(!"Too many parameters");
			}

			mach_token src = make_mach_token(mreg, size);
			x86_emit2(ctx, X86_MOV, size, X86_REG, dst, X86_REG, src);
		} break;
	case IR_I2F:
	case IR_F2I:
		{
#if 0
			mach_token src = register_token(arg0, is_float);

			x86_select_inst(ctx, arg0, src, size);
			if (type == IR_F32) {
				x86_emit2(ctx, X86_CVTSI2SS, size, dst, src);
			} else if (type == IR_F64) {
				x86_emit2(ctx, X86_CVTSI2SD, size, dst, src);
			} else if (arg0_type == IR_F32) {
				x86_emit2(ctx, X86_CVTTSS2SI, size, dst, src);
			} else if (arg0_type == IR_F64) {
				x86_emit2(ctx, X86_CVTTSD2SI, size, dst, src);
			}

			if (type == IR_I8) {
			}
#endif
		} break;
	case IR_CONST:
		{
			mach_token src = make_const(arg0, size);
			x86_emit2(ctx, X86_MOV, size, X86_REG, dst, X86_IMM, src);
		} break;
	case IR_BUILTIN:
		{
			ASSERT(!"Builtins should be handled in their respective instruction");
		} break;
	case IR_ALLOC:
		{
			mach_token src = make_const(arg1, size);
			mach_token rsp = x86_register_token(ctx, X86_RSP, size);

			x86_emit3(ctx, X86_LEA, size, X86_REG, dst, X86_BASE, rsp, X86_DISP_IMM, src);
		} break;
	case IR_COPY:
	case IR_FCOPY:
		{
			x86_select_inst(ctx, arg0, dst, size);
		} break;
	case IR_LOAD:
	case IR_FLOAD:
		{
			x86_opcode mov = X86_MOV;
			if (opcode == IR_FLOAD) {
				mov = X86_MOVSS;
			}

			if (inst[arg0].opcode == IR_GLOBAL) {
				mach_token src = make_global(inst[arg0].args[0]);

				x86_emit2(ctx, mov, size, X86_REG, dst, X86_DISP_SYM, src);
			} else if (inst[arg0].opcode == IR_ALLOC) {
				mach_token src = make_const(arg1, size);
				mach_token rsp = x86_register_token(ctx, X86_RSP, size);

				x86_emit3(ctx, X86_MOV, size, X86_REG, dst, X86_BASE, rsp, X86_DISP_IMM, src);
			} else {
				mach_token src = register_token(arg0, is_float);

				x86_select_inst(ctx, arg0, src, addr_size);
				x86_emit2(ctx, mov, size, X86_REG, dst, X86_BASE, src);
			}
		} break;
	case IR_STORE:
	case IR_FSTORE:
		{
			mach_token src = register_token(arg1, is_float);
			x86_opcode mov = (opcode == IR_FSTORE ? X86_MOVSS : X86_MOV);

			ASSERT(!equals_token(src, dst));
			if (inst[arg1].opcode != IR_CONST) {
				x86_select_inst(ctx, arg1, src, size);
			} else {
				src = make_const(inst[arg1].args[0], size);
			}

			x86_select_inst(ctx, arg0, dst, addr_size);
			x86_emit2(ctx, mov, size, X86_BASE, dst, X86_REG, src);
		} break;
	case IR_ADD:
		{
			if (inst[arg1].opcode == IR_CONST && inst[arg1].args[0] == 1) {
				x86_select_inst(ctx, arg0, dst, size);
				x86_emit1(ctx, X86_INC, size, X86_REG, dst);
			} else if (inst[arg1].opcode == IR_CONST) {
				x86_select_inst(ctx, arg0, dst, size);
				arg1 = inst[arg1].args[0];
				mach_token src = make_const(arg1, size);
				x86_emit2(ctx, X86_ADD, size, X86_REG, dst, X86_REG, src);
			} else if (inst[arg0].opcode == IR_CONST) {
				x86_select_inst(ctx, arg1, dst, size);
				arg0 = inst[arg0].args[0];
				mach_token src = make_const(arg0, size);
				x86_emit2(ctx, X86_ADD, size, X86_REG, dst, X86_REG, src);
			} else {
				mach_token src = register_token(arg1, is_float);
				x86_select_inst(ctx, arg0, dst, size);
				x86_select_inst(ctx, arg1, src, size);
				x86_emit2(ctx, X86_ADD, size, X86_REG, dst, X86_REG, src);
			}
		} break;
	case IR_SUB:
		{
			if (inst[arg1].opcode == IR_CONST && inst[arg1].args[0] == 1) {
				x86_select_inst(ctx, arg0, dst, size);
				x86_emit1(ctx, X86_DEC, size, X86_REG, dst);
			} else if (inst[arg0].opcode == IR_CONST && inst[arg0].args[0] == 0) {
				x86_select_inst(ctx, arg1, dst, size);
				x86_emit1(ctx, X86_NEG, size, X86_REG, dst);
			} else if (inst[arg1].opcode == IR_CONST) {
				isize src_size = inst[arg1].size;
				arg1 = inst[arg1].args[0];
				x86_select_inst(ctx, arg0, dst, size);
				mach_token src = make_const(arg1, src_size);
				x86_emit2(ctx, X86_SUB, size, X86_REG, dst, X86_IMM, src);
			} else {
				mach_token src = register_token(arg1, is_float);
				x86_select_inst(ctx, arg0, dst, size);
				x86_select_inst(ctx, arg1, src, size);
				x86_emit2(ctx, X86_SUB, size, X86_REG, dst, X86_REG, src);
			}
		} break;
	case IR_MUL:
		{
			if (inst[arg1].opcode == IR_CONST && inst[arg1].args[0] == 1) {
				x86_select_inst(ctx, arg0, dst, size);
			} else if (inst[arg1].opcode == IR_CONST && inst[arg1].args[0] == 2) {
				x86_select_inst(ctx, arg0, dst, size);
				x86_emit2(ctx, X86_ADD, size, X86_REG, dst, X86_REG, dst);
			} else {
				mach_token rax = make_mach_token(X86_RAX, inst[arg0].size);
				mach_token reg0 = register_token(arg0, is_float);
				mach_token reg1 = register_token(arg1, is_float);

				x86_select_inst(ctx, arg0, reg0, size);
				x86_select_inst(ctx, arg1, reg1, size);
				x86_emit2(ctx, X86_MOV, size, X86_REG, rax, X86_REG, reg0);
				x86_emit1(ctx, X86_IMUL, size, X86_REG, reg1);
				x86_emit2(ctx, X86_MOV, size, X86_REG, dst, X86_REG, rax);
			}
		} break;
	case IR_DIV:
	case IR_MOD:
		{
			mach_token reg0 = x86_register_token(ctx, X86_RAX, inst[arg0].size);
			mach_token reg1 = x86_register_token(ctx, X86_RAX, inst[arg1].size);

			x86_select_inst(ctx, arg0, reg0, size);
			x86_select_inst(ctx, arg1, reg1, size);

			mach_token rax = make_mach_token(X86_RAX, inst[arg0].size);
			mach_token rcx = make_mach_token(X86_RCX, inst[arg1].size);
			mach_token rdx = make_mach_token(X86_RDX, size);
			mach_token zero = make_const(0, size);
			mach_token src = opcode == IR_DIV ? rax : rdx;

			x86_emit2(ctx, X86_MOV, size, X86_REG, rdx, X86_IMM, zero);
			x86_emit2(ctx, X86_MOV, size, X86_REG, rax, X86_REG, reg0);
			x86_emit2(ctx, X86_MOV, size, X86_REG, rcx, X86_REG, reg1);
			x86_emit1(ctx, X86_IDIV, size, X86_REG, rcx);
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

			mach_token lhs = register_token(arg0, is_float);
			mach_token rhs = register_token(arg1, is_float);

			x86_select_inst(ctx, arg0, lhs, size);
			x86_select_inst(ctx, arg1, rhs, size);
			x86_emit2(ctx, cmp, size, X86_REG, lhs, X86_REG, rhs);
			x86_emit1(ctx, setcc, X86_BYTE, X86_REG, dst_byte);
			x86_emit2(ctx, X86_MOVZX, size, X86_REG, dst, X86_REG, dst_byte);
		} break;
	case IR_SHL:
	case IR_SHR:
		{
			mach_token rcx = x86_register_token(ctx, X86_RCX, 1);
			x86_opcode shift = opcode == IR_SHL ? X86_SHL : X86_SHR;

			x86_select_inst(ctx, arg0, dst, size);
			x86_select_inst(ctx, arg1, rcx, size);
			x86_emit2(ctx, shift, size, X86_REG, dst, X86_REG, rcx);
		} break;
	case IR_AND:
	case IR_OR:
	case IR_XOR:
		{
			mach_token src = register_token(arg1, is_float);
			x86_opcode bitop =
				opcode == IR_AND ? X86_AND :
				opcode == IR_OR ? X86_OR : X86_XOR;

			if (inst[arg1].opcode == IR_CONST) {
				src = make_const(inst[arg1].args[0], size);
				x86_select_inst(ctx, arg0, dst, size);
			} else if (inst[arg0].opcode == IR_CONST) {
				src = make_const(inst[arg0].args[0], size);
				x86_select_inst(ctx, arg1, dst, size);
			} else {
				x86_select_inst(ctx, arg0, dst, size);
				x86_select_inst(ctx, arg1, src, size);
			}

			x86_emit2(ctx, bitop, size, X86_REG, dst, X86_REG, src);
		} break;
	case IR_NOT:
		{
			x86_select_inst(ctx, arg0, dst, size);
			x86_emit1(ctx, X86_NOT, size, X86_REG, dst);
		} break;
	case IR_JMP:
		x86_emit1(ctx, X86_JMP, X86_DWORD, X86_IMM, make_label(arg0));
		break;
	case IR_JIZ:
	case IR_JNZ:
		{
			b32 is_jiz = opcode == IR_JIZ;
			x86_opcode jcc = is_jiz ? X86_JZ : X86_JNZ;
			if (is_comparison_opcode(inst[arg0].opcode)) {
				x86_opcode cmp = (is_float ? X86_COMISS : X86_CMP);
				jcc = x86_get_jcc_opcode(inst[arg0].opcode, is_jiz);

				dst = register_token(inst[arg0].args[0], is_float);
				mach_token src = x86_select_const(ctx, inst[arg0].args[1]);

				x86_select_inst(ctx, inst[arg0].args[0], dst, size);
				x86_emit2(ctx, cmp, size, X86_REG, dst, X86_REG, src);
			} else {
				mach_token src = register_token(arg0, is_float);
				x86_select_inst(ctx, arg0, src, size);
				x86_emit2(ctx, X86_TEST, size, X86_REG, src, X86_REG, src);
			}

			x86_emit1(ctx, jcc, X86_DWORD, X86_IMM, make_label(arg1));
		} break;
	case IR_RET:
	case IR_FRET:
		{
			mach_token return_reg;
			if (opcode == IR_FRET) {
				return_reg = x86_register_token(ctx, X86_XMM0, size);
			} else {
				return_reg = x86_register_token(ctx, X86_RAX, size);
			}

			if (arg0 == 0) {
				x86_emit0(ctx, X86_RET);
			} else {
				x86_select_inst(ctx, arg0, return_reg, size);
				x86_emit1(ctx, X86_RET, 0, 0, return_reg);
			}
		} break;
	case IR_SEXT:
		{
			mach_token src = register_token(arg0, is_float);

			x86_select_inst(ctx, arg0, src, size);
			x86_emit2(ctx, X86_MOVSX, size, X86_REG, dst, X86_REG, src);
		} break;
	case IR_ZEXT:
		{
			mach_token src = register_token(arg0, is_float);

			x86_select_inst(ctx, arg0, src, size);
			x86_emit2(ctx, X86_MOVZX, size, X86_REG, dst, X86_REG, src);
		} break;
	case IR_TRUNC:
		{
			x86_select_inst(ctx, arg0, dst, size);
		} break;
	case IR_CALL:
		{
			mach_token called = register_token(arg0, is_float);
			if (inst[arg0].opcode == IR_BUILTIN) {
				mach_token src = {0};
				ir_builtin builtin = inst[arg0].args[0];
				switch (builtin) {
				case BUILTIN_POPCOUNT:
					src = register_token(inst[i - 1].args[0], is_float);
					x86_emit2(ctx, X86_POPCNT, size, X86_REG, dst, X86_REG, src);
					break;
				default:
					ASSERT(!"Builtin is not supported");
				}
			} else {
				x86_operand_kind called_kind = X86_REG;
				if (inst[arg0].opcode == IR_GLOBAL) {
					called = make_global(inst[arg0].args[0]);
					called_kind = X86_SYM;
					//ASSERT(called.value < ctx->symtab->global_count);
				} else {
					x86_select_inst(ctx, arg0, called, size);
				}

				isize param_count = 0;
				for (isize param = arg1; param; param = inst[param].args[1]) {
					param_count++;
				}

				isize param_index = param_count;
				for (isize param = arg1; param; param = inst[param].args[1]) {
					x86_register mreg = 0;
					switch (--param_index) {
					case 0: mreg = X86_RDI; break;
					case 1: mreg = X86_RSI; break;
					case 2: mreg = X86_RDX; break;
					case 3: mreg = X86_RCX; break;
					case 4: mreg = X86_R8; break;
					case 5: mreg = X86_R9; break;
					default:
						ASSERT(!"Too many arguments");
						break;
					}

					ir_inst param_inst = inst[param];
					ASSERT(param_inst.opcode == IR_CALL);
					isize param_size = param_inst.size;

					mach_token src = x86_register_token(ctx, mreg, param_size);
					x86_select_inst(ctx, param_inst.args[0], src, param_size);

					mach_token dst = make_mach_token(mreg, param_size);
					x86_emit2(ctx, X86_MOV, param_size, X86_REG, dst, X86_REG, src);
				}

				mach_token rax = x86_register_token(ctx, X86_RAX, size);
				x86_emit1(ctx, X86_CALL, X86_QWORD, called_kind, called);
				x86_emit2(ctx, X86_MOV, size, X86_REG, dst, X86_REG, rax);
			}
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

			mach_token src = register_token(arg1, is_float);
			dst.flags |= MACH_FLOAT;

			x86_select_inst(ctx, arg0, dst, size);
			x86_select_inst(ctx, arg1, src, size);
			x86_emit2(ctx, x86_opcode, size, X86_REG, dst, X86_REG, src);
		} break;
		break;
	}
}

static void
x86_generate(writer *out, ir_program p, arena *arena)
{
	isize max_token_count = 1024 * 1024;
	mach_token *tokens = ALLOC(arena, max_token_count, mach_token);

	print_cstr(out, "section .text\n");
	for (isize func_id = 0; func_id < p.func_count; func_id++) {
		ir_function *ir_func = &p.funcs[func_id];

		//
		// 1. Instruction selection
		//

		x86_context ctx = {0};
		ctx.inst = ir_func->insts;
		ctx.tokens = tokens;
		ctx.max_token_count = max_token_count;
		ctx.vreg_count = X86_REGISTER_COUNT;
		ctx.globals = p.globals;

		i32 curr_block = 0;
		basic_block *blocks = ALLOC(arena, ir_func->label_count, basic_block);

		ir_inst *inst = ir_func->insts;
		i32 *ref_count = get_ref_count(inst, ir_func->inst_count, arena);
		for (isize j = 0; j < ir_func->inst_count; j++) {
			ir_opcode opcode = inst[j].opcode;
			if (ref_count[j] == 1) {
				continue;
			}

			isize size = inst[j].size;
			b32 is_float = is_float_opcode(inst[j].opcode);
			mach_token dst = register_token(j, is_float);
			if (opcode == IR_LABEL) {
				i32 prev_block = curr_block;
				curr_block = inst[j].args[0];
				// If the previous block was a conditional jump
				if (prev_block > 0 && blocks[prev_block].succ[0] == 0) {
					blocks[prev_block].succ[0] = curr_block;
				}

				blocks[curr_block].offset = j;
			} else {
				x86_select_inst(&ctx, j, dst, size);

				if (opcode == IR_JIZ) {
					i32 next_block = inst[j].args[1];
					isize start = blocks[curr_block].offset;
					blocks[curr_block].succ[0] = 0;
					blocks[curr_block].succ[1] = next_block;
					blocks[curr_block].size = ctx.token_count - start;
				} else if (opcode == IR_JMP) {
					i32 next_block = inst[j].args[0];
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
			continue;
		}

		//
		// 2. Register allocation
		//

		mach_info mach = {0};
		mach.tmp_mreg_count = LENGTH(x86_temp_regs);
		mach.int_mreg_count = LENGTH(x86_int_regs);
		mach.float_mreg_count = LENGTH(x86_float_regs);
		mach.vreg_count = ctx.vreg_count;
		mach.tmp_mregs = x86_temp_regs;
		mach.int_mregs = x86_int_regs;
		mach.float_mregs = x86_float_regs;
		mach.mreg_count = X86_REGISTER_COUNT;

		mach_location *reg_table = regalloc(tokens, token_count, blocks,
			ir_func->label_count, mach, arena);

		//
		// 3. Generate the code
		//

		print_str(out, ir_func->name);
		print_cstr(out, ":\n");

		// Print function prologue
#if 0
		isize used_volatile_register_count = 0;
		for (isize j = 0; j < LENGTH(x86_saved_regs); j++) {
			i32 mreg = x86_saved_regs[j];
			if (reg_table[mreg]) {
				print_cstr(out, "\tpush ");
				print_cstr(out, x86_get_register_name(mreg, 8));
				print_cstr(out, "\n");
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
			if (!is_inst_token(token)) {
				continue;
			}

			isize operand_count = 0;
			x86_operand_kind kinds[4] = {0};
			x86_opcode opcode = (token.value & X86_OPCODE_MASK);
			if (opcode == X86_LABEL) {
				print_cstr(out, ".L");
				print_u32(out, tokens[i + 1].value);
				print_cstr(out, ":");
				operand_count = 0;
			} else if (opcode == X86_RET) {
				print_cstr(out, "    jmp .exit");
			} else {
				print_cstr(out, "    ");
				print_cstr(out, x86_get_opcode_name(opcode));

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
					print_cstr(out, " ");
					first_token = false;
				} else if (!inside_memory_operand) {
					print_cstr(out, ", ");
				}

				x86_operand_kind kind = kinds[operand_count - 1 - j];
				if (kind == X86_INDEX || kind == X86_DISP_IMM
					|| kind == X86_DISP_SYM || kind == X86_BASE)
				{
					if (!inside_memory_operand) {
						switch (token.hint) {
						case 1:
							print_cstr(out, "byte");
							break;
						case 2:
							print_cstr(out, "word");
							break;
						case 4:
							print_cstr(out, "dword");
							break;
						case 8:
							print_cstr(out, "qword");
							break;
						}

						print_cstr(out, "[");
						inside_memory_operand = true;
					} else {
						print_cstr(out, " + ");
					}
				} else {
					if (inside_memory_operand) {
						print_cstr(out, "], ");
					}
				}

				i32 value = tokens[i + 1 + j].value;
				switch (kind) {
				case X86_REG:
				case X86_BASE:
				case X86_INDEX:
					{
						// TODO: Handle spilled registers. We need to reserve at least
						// three registers, since one instruction can contain three
						// registers, which can all be spilled at the same time.
						u64 reg = 0;
						if (value >= X86_REGISTER_COUNT) {
							reg = reg_table[value].value;
						} else {
							reg = value;
						}

						// TODO: Set the correct size during register allocation
						i32 reg_size = 0;
						print_cstr(out, x86_get_register_name(reg, reg_size));
					} break;
				case X86_IMM:
				case X86_DISP_IMM:
					{
						print_u32(out, value);
					} break;
				case X86_SYM:
				case X86_DISP_SYM:
					{
						//ASSERT(value < symtab.global_count);
						global *global = &ctx.globals[value];
						if (global->name.length > 0) {
							print_str(out, global->name);
						} else {
							print_cstr(out, "L#");
							print_u32(out, value);
						}
					} break;
				default:
					ASSERT(!"Invalid operand");
				}
			}

			if (inside_memory_operand) {
				print_cstr(out, "]");
			}

			print_cstr(out, "\n");
		}

		// Print function epilogue
		print_cstr(out, "\n.exit:\n");
		if (stack_size > 0) {
			print_cstr(out, "\tadd rsp, ");
			print_u32(out, stack_size);
			print_cstr(out, "\n");
		}

#if 0
		for (isize j = 0; j < LENGTH(x86_saved_regs); j++) {
			u32 mreg = x86_saved_regs[j];
			if (info.used[mreg]) {
				print_cstr(out, "\tpop ");
				print_cstr(out, x86_get_register_name(mreg, 8));
				print_cstr(out, "\n");
			}
		}
#endif

		print_cstr(out, "\tret\n\n");
	}

	// Print the remaining sections
	section prev_section = SECTION_TEXT;
	for (global_id global_id = {0}; global_id.value < p.global_count; global_id.value++) {
		global *global = &p.globals[global_id.value];

		section section = global->section;
		if (section != prev_section) {
			prev_section = section;
			switch (section) {
			case SECTION_TEXT:
				print_cstr(out, "section .text\n");
				break;
			case SECTION_DATA:
				print_cstr(out, "section .data\n");
				break;
			case SECTION_RODATA:
				print_cstr(out, "section .rodata\n");
				break;
			case SECTION_BSS:
				print_cstr(out, "section .bss\n");
				break;
			default:
				continue;
			}
		}

		linkage linkage = global->linkage;
		switch (linkage) {
		case LINK_STATIC:
			print_cstr(out, "static ");
			print_str(out, global->name);
			print_cstr(out, "\n");
			break;
		case LINK_EXTERN:
			print_cstr(out, "extern ");
			print_str(out, global->name);
			print_cstr(out, "\n");
			break;
		default:
			if (global->name.length > 0) {
				print_cstr(out, "global ");
				print_str(out, global->name);
				print_cstr(out, "\n");
			}
		}

		if (global->size > 0) {
			if (global->name.length > 0) {
				print_str(out, global->name);
			} else {
				print_cstr(out, "L#");
				print_u32(out, global_id.value);
			}

			if (section == SECTION_DATA || section == SECTION_RODATA) {
				// NOTE: Inside data or rodata section, globals contain byte data
				if (global->data) {
					print_cstr(out, ": db ");
					u8 *byte = global->data;
					for (isize i = 0; i < global->size; i++) {
						if (i != 0) {
							print_cstr(out, ", ");
						}

						print_hex(out, byte[i]);
					}

					print_cstr(out, "\n");
				} else {
					print_cstr(out, ": times ");
					print_u32(out, global->size);
					print_cstr(out, " db 0\n");
				}
			} else if (section == SECTION_BSS) {
				// NOTE; Inside bss section, globals have no data
				print_cstr(out, " resb ");
				print_hex(out, global->size);
				print_cstr(out, "\n");
			}
		}
	}
}
