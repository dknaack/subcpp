#define x86_emit2(ctx, opcode, arg0, arg1) x86_emit(ctx, 0, opcode, arg0, arg1)
#define x86_emit1(ctx, opcode, arg0) x86_emit(ctx, 0, opcode, arg0, 0)
#define x86_emit0(ctx, opcode) x86_emit(ctx, 0, opcode, 0, 0)
#define x86_emit2x(ctx, hint, opcode, arg0, arg1) x86_emit(ctx, hint, opcode, arg0, arg1)
#define x86_emit1x(ctx, hint, opcode, arg0) x86_emit(ctx, hint, opcode, arg0, 0)
#define x86_emit0x(ctx, hint, opcode) x86_emit(ctx, hint, opcode, 0, 0)

static x86_opcode_info
x86_get_opcode_info(x86_opcode opcode)
{
	x86_opcode_info result = {0};

	switch (opcode) {
 	case X86_NOP:
 	case X86_RET:
		break;
	case X86_MOVrg:
		result.dest = X86_OPERAND_REGISTER;
		result.args[0] = X86_OPERAND_GLOBAL;
		break;
	case X86_MOVri:
		result.dest = X86_OPERAND_REGISTER;
		result.args[0] = X86_OPERAND_IMMEDIATE;
		break;
	case X86_MOVrm:
		result.dest = X86_OPERAND_REGISTER;
		result.args[0] = X86_OPERAND_MEMORY;
		break;
	case X86_MOVmr:
		result.args[0] = X86_OPERAND_MEMORY;
		result.args[1] = X86_OPERAND_REGISTER;
		break;
	case X86_MOVrr:
		result.dest = X86_OPERAND_REGISTER;
		result.args[0] = X86_OPERAND_REGISTER;
		break;
	case X86_MOVr:
	case X86_SETA:
	case X86_SETAE:
	case X86_SETB:
	case X86_SETBE:
	case X86_SETG:
	case X86_SETGE:
	case X86_SETL:
	case X86_SETLE:
	case X86_SETNZ:
	case X86_SETZ:
 	case X86_PHI:
		result.dest = X86_OPERAND_REGISTER;
		break;
	case X86_CALLf:
		result.args[0] = X86_OPERAND_FUNCTION;
		break;
	case X86_JA:
	case X86_JAE:
	case X86_JB:
	case X86_JBE:
	case X86_JG:
	case X86_JGE:
	case X86_JL:
	case X86_JLE:
	case X86_JNZ:
	case X86_JZ:
	case X86_JMP:
		result.args[0] = X86_OPERAND_LABEL;
		break;
	case X86_ADDrr:
		result.dest = X86_OPERAND_REGISTER;
		result.args[0] = X86_OPERAND_REGISTER;
		result.args[1] = X86_OPERAND_REGISTER;
		break;
	case X86_CMP:
	case X86_TEST:
		result.args[0] = X86_OPERAND_REGISTER;
		result.args[1] = X86_OPERAND_REGISTER;
		break;
	}

	return result;
}

static i32
x86_emit(x86_context *ctx, x86_register hint, x86_opcode opcode, i32 arg0, i32 arg1)
{
	i32 flags = 0;
	x86_opcode_info info = x86_get_opcode_info(opcode);
	if (info.args[0] == X86_OPERAND_REGISTER) {
		flags |= INST_USE0;
	}

	if (info.args[1] == X86_OPERAND_REGISTER) {
		flags |= INST_USE1;
	}

	i32 result = emit(&ctx->output, opcode, hint, 0, flags, arg0, arg1);
	return result;
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
		case IR_JIZ:
			return X86_JZ;
		case IR_JNZ:
			return X86_JNZ;
		default:
			ASSERT(!"Not a comparison operator");
			return X86_JZ;
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
		case IR_JIZ:
			return X86_JZ;
		case IR_JNZ:
			return X86_JNZ;
		default:
			ASSERT(!"Not a comparison operator");
			return X86_JNZ;
		}
	}
}

static i32
x86_select(x86_context *ctx, i32 inst_id)
{
	i32 result = 0;

	inst *input = ctx->input;
	i32 arg0 = input[inst_id].args[0];
	i32 arg1 = input[inst_id].args[1];
	ir_opcode opcode = input[inst_id].opcode;
	switch (opcode) {
	case IR_NOP:
		break;
	case IR_GLOBAL:
	case IR_FUNC:
		{
			result = x86_emit1(ctx, X86_MOVrg, arg0);
		} break;
	case IR_CONST:
		{
			result = x86_emit1(ctx, X86_MOVri, arg1);
		} break;
	case IR_ALLOC:
		{
			ASSERT(!"Instruction selection for stack slots");
		} break;
	case IR_COPY:
	case IR_FCOPY:
		{
			result = x86_select(ctx, arg0);
		} break;
	case IR_LOAD:
		{
			arg0 = x86_select(ctx, arg0);
			result = x86_emit1(ctx, X86_MOVrm, arg0);
		} break;
	case IR_STORE:
		{
			arg0 = x86_select(ctx, arg0);
			arg1 = x86_select(ctx, arg1);
			x86_emit2(ctx, X86_MOVmr, arg0, arg1);
		} break;
	case IR_PARAM:
		{
			x86_register hint = 0;
			switch (arg0) {
			case 0: hint = X86_RDI; break;
			case 1: hint = X86_RSI; break;
			case 2: hint = X86_RDX; break;
			case 3: hint = X86_RCX; break;
			case 4: hint = X86_R8;  break;
			case 5: hint = X86_R9;  break;
			}

			if (hint != 0) {
				result = x86_emit0x(ctx, hint, X86_MOVr);
			} else {
				ASSERT(!"Implement more than 6 parameters");
			}
		} break;
	case IR_ADD:
		{
			arg0 = x86_select(ctx, arg0);
			arg1 = x86_select(ctx, arg1);
			result = x86_emit2(ctx, X86_ADDrr, arg0, arg1);
		} break;
	case IR_CALL:
		{
			i32 params[5];
			i32 param_count = 0;

			for (isize param = arg1; param; param = input[param].args[1]) {
				ASSERT(param_count < 5);
				params[param_count++] = x86_select(ctx, input[param].args[0]);
			}

			for (isize i = param_count - 1; i >= 0; i--) {
				i32 hint = 0;
				switch (i) {
				case 0: hint = X86_RDI; break;
				case 1: hint = X86_RSI; break;
				case 2: hint = X86_RDX; break;
				case 3: hint = X86_RCX; break;
				case 4: hint = X86_R8;  break;
				case 5: hint = X86_R9;  break;
				default:
					ASSERT(!"Implement more than 6 parameters");
				}

				if (hint != 0) {
					x86_emit1x(ctx, hint, X86_MOVrr, params[i]);
				}
			}

			if (input[arg0].opcode == IR_FUNC) {
				i32 func_id = input[arg0].args[0];
				x86_emit1(ctx, X86_CALLf, func_id);
				result = x86_emit0x(ctx, X86_RAX, X86_MOVr);
			} else {
				ASSERT(!"Not implemented");
			}
		} break;
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

			arg0 = x86_select(ctx, arg0);
			arg1 = x86_select(ctx, arg1);
			x86_emit2(ctx, X86_CMP, arg0, arg1);
			result = x86_emit0(ctx, setcc);
		} break;
	case IR_RET:
		{
			arg0 = x86_select(ctx, arg0);
			x86_emit1x(ctx, X86_RAX, X86_MOVrr, arg0);
			x86_emit0(ctx, X86_RET);
		} break;
	case IR_JMP:
		{
			x86_emit1(ctx, X86_JMP, arg0);
		} break;
	case IR_JIZ:
	case IR_JNZ:
		{
			b32 is_jiz = (opcode == IR_JIZ);
			x86_opcode jcc = is_jiz ? X86_JZ : X86_JNZ;

			inst comparison = input[arg0];
			if (is_comparison_opcode(comparison.opcode)) {
				jcc = x86_get_jcc_opcode(comparison.opcode, is_jiz);
				i32 lhs = x86_select(ctx, comparison.args[0]);
				i32 rhs = x86_select(ctx, comparison.args[1]);
				x86_emit2(ctx, X86_CMP, lhs, rhs);
			} else {
				arg0 = x86_select(ctx, arg0);
				x86_emit2(ctx, X86_TEST, arg0, arg0);
			}

			i32 jump_target = arg1;
			x86_emit1(ctx, jcc, jump_target);
		} break;
	default:
		{
			ASSERT(!"Not implemented yet");
		} break;
	}

	return result;
}

static void
x86_generate_symbol(writer *out, str name, linkage linkage)
{
	switch (linkage) {
	case LINK_STATIC:
		print_cstr(out, "static ");
		print_str(out, name);
		print_cstr(out, "\n");
		break;
	case LINK_EXTERN:
		print_cstr(out, "extern ");
		print_str(out, name);
		print_cstr(out, "\n");
		break;
	default:
		if (name.length > 0) {
			print_cstr(out, "global ");
			print_str(out, name);
			print_cstr(out, "\n");
		}
	}
}


static void
x86_generate(writer *out, program prog, arena *arena)
{
	print_cstr(out, "section .text\n");

	for (isize func_id = 1; func_id < prog.func_count; func_id++) {
		function *func = &prog.funcs[func_id];
		isize inst_count = func->inst_count;
		inst *insts = func->insts;

		x86_context ctx = {0};
		ctx.input = insts;

		//
		// Instruction Selection
		//

		isize block_count = func->block_count;
		block *blocks = ALLOC(arena, block_count, block);
		isize block_id = 0;
		i32 *use_count = get_use_count(insts, inst_count, arena);
		for (isize i = 0; i < inst_count; i++) {
			if (insts[i].opcode == IR_LABEL) {
				blocks[block_id].end = ctx.output.inst_count;
				block_id = insts[i].args[0];
				blocks[block_id].begin = ctx.output.inst_count;
			} else if (use_count[i] != 1) {
				x86_select(&ctx, i);
			}
		}

		blocks[block_id].end = ctx.output.inst_count;

		//
		// Register Allocation
		//

		machine_info info = {0};
		info.int_registers = x86_int_registers;
		info.float_registers = x86_float_registers;
		info.volatile_registers = x86_volatile_registers;
		info.int_register_count = LENGTH(x86_int_registers);
		info.float_register_count = LENGTH(x86_float_registers);
		info.volatile_register_count = LENGTH(x86_volatile_registers);
		info.machine_register_count = X86_REGISTER_COUNT;

		inst *output = ctx.output.insts;
		machine_location *locations = allocate_registers(
			output, ctx.output.inst_count,
			blocks, block_count, info, arena);

		//
		// Code Generation
		//

		x86_generate_symbol(out, func->name, func->linkage);
		if (ctx.output.inst_count > 0) {
			print_cstr(out, "\n");
			print_str(out, func->name);
			print_cstr(out, ":\n");
		}

		for (isize b = 0; b < block_count; b++) {
			print_cstr(out, "L");
			print_u32(out, b);
			print_cstr(out, ":\n");

			for (isize i = blocks[b].begin; i < blocks[b].end; i++) {
				inst inst = output[i];
				if (inst.opcode == X86_NOP || inst.opcode == X86_MOVr) {
					continue;
				}

				if (inst.opcode == X86_JMP && inst.args[0] == b + 1) {
					break;
				}

				x86_opcode_info info = x86_get_opcode_info(inst.opcode);

				struct {
					i32 value;
					x86_operand_kind kind;
				} operands[3] = {
					{ i, info.dest },
					{ inst.args[0], info.args[0] },
					{ inst.args[1], info.args[1] },
				};

				print_cstr(out, x86_get_opcode_name(inst.opcode));

				b32 is_first = true;
				for (isize i = 0; i < 3; i++) {
					x86_operand_kind operand_kind = operands[i].kind;
					if (operand_kind == X86_OPERAND_NONE) {
						continue;
					}

					print_cstr(out, is_first ? " " : ", ");
					is_first = false;

					i32 operand = operands[i].value;
					switch (operand_kind) {
					case X86_OPERAND_FUNCTION:
						{
							str function = prog.funcs[operand].name;
							print_str(out, function);
						} break;
					case X86_OPERAND_GLOBAL:
						{
							str global = prog.globals[operand].name;
							if (global.length > 0) {
								print_str(out, global);
							} else {
								print_cstr(out, "G#");
								print_u32(out, operand);
							}
						} break;
					case X86_OPERAND_IMMEDIATE:
						{
							print_hex(out, operand);
						} break;
					case X86_OPERAND_REGISTER:
						{
							machine_location location = locations[operand];
							if (location.is_spilled) {
								print_cstr(out, "(spilled ");
								print_u32(out, operand);
								print_cstr(out, ")");
							} else {
								print_cstr(out, x86_get_register_name(location.value, 8));
							}
						} break;
					case X86_OPERAND_LABEL:
						{
							print_cstr(out, "L");
							print_u32(out, operand);
						} break;
					default:
						{
							print_cstr(out, "(unknown)");
						} break;
					}
				}

				print_cstr(out, "\n");
				if (inst.opcode == X86_RET) {
					break;
				}
			}
		}

		print_cstr(out, "nop\n");
	}

	section prev_section = SECTION_TEXT;

	for (isize global_id = 0; global_id < prog.global_count; global_id++) {
		global *global = &prog.globals[global_id];

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

		x86_generate_symbol(out, global->name, global->linkage);

		if (global->size > 0) {
			if (global->name.length > 0) {
				print_str(out, global->name);
			} else {
				print_cstr(out, "G#");
				print_u32(out, global_id);
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
