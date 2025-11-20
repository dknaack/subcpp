#define x86_emit2(ctx, opcode, arg0, arg1) x86_emit(ctx, 0, opcode, arg0, arg1)
#define x86_emit2x(ctx, opcode, hint, arg0, arg1) x86_emit(ctx, hint, opcode, arg0, arg1)
#define x86_emit1(ctx, opcode, arg0) x86_emit(ctx, 0, opcode, arg0, 0)
#define x86_emit1x(ctx, hint, opcode, arg0) x86_emit(ctx, hint, opcode, arg0, 0)
#define x86_emit0(ctx, opcode) x86_emit(ctx, 0, opcode, 0, 0)
#define x86_emit0x(ctx, hint, opcode) x86_emit(ctx, hint, opcode, 0, 0)

static i32 x86_emit(x86_context *ctx,
	x86_register hint, x86_opcode opcode, i32 arg0, i32 arg1)
{
	i32 result = emit(&ctx->output, opcode, hint, 0, 0, arg0, arg1);
	return result;
}

static i32 x86_select(x86_context *ctx, i32 inst_id)
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
				// We abuse NOP to generate a machine register
				result = x86_emit0x(ctx, hint, X86_NOP);
			} else {
				ASSERT(!"Implement more than 6 parameters");
			}
		} break;
	default:
		{
			ASSERT(!"Not implemented yet");
		} break;
	}

	return result;
}

static void x86_generate(writer *out, program prog, arena *arena)
{
	for (isize func_id = 0; func_id < prog.func_count; func_id++) {
		function *func = &prog.funcs[func_id];
		isize inst_count = func->inst_count;
		inst *insts = func->insts;

		x86_context ctx = {0};
		ctx.input = insts;

		//
		// Instruction Selection
		//

		block *blocks = ALLOC(arena, func->block_count, block);
		isize block_id = 0;
		i32 *use_count = get_use_count(insts, inst_count, arena);
		for (isize i = 0; i < inst_count; i++) {
			if (insts[i].opcode == IR_LABEL) {
				blocks[block_id].end = i;
				block_id = insts[i].args[0];
				blocks[block_id].begin = i;
			} else if (use_count[i] != 1) {
				x86_select(&ctx, i);
			}
		}

		//
		// Register Allocation
		//

		//
		// Code Generation
		//
	}
}
