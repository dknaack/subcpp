static char *
get_builtin_str(ir_builtin builtin)
{
	switch (builtin) {
	case BUILTIN_POPCOUNT:
		return "popcount";
	}

	return "(invalid builtin)";
}

static char *
get_ir_type_str(ir_type type)
{
	switch (type) {
	case IR_VOID: return "void";
	case IR_I8:   return "i8";
	case IR_I16:  return "i16";
	case IR_I32:  return "i32";
	case IR_I64:  return "i64";
	case IR_F32:  return "f32";
	case IR_F64:  return "f64";
	}

	return "(invalid)";
}

static void
print_ir_instr(ir_instr instr, u32 i)
{
	u32 dst = i;
	u32 op0 = instr.op0;
	u32 op1 = instr.op1;
	char *type = get_ir_type_str(instr.type);
	switch (instr.opcode) {
	case IR_NOP:
		printf("\tnop\n");
		break;
	case IR_CAST:
		printf("\t%%%d =%s cast %%%d\n", dst, type, op0);
		break;
	case IR_CASTU:
		printf("\t%%%d =%s castu %%%d\n", dst, type, op0);
		break;
	case IR_GLOBAL:
		printf("\t%%%d =%s global %d\n", dst, type, op0);
		break;
	case IR_VAR:
		printf("\t%%%d =%s new vreg\n", dst, type);
		break;
	case IR_CONST:
		printf("\t%%%d =%s %d\n", dst, type, op0);
		break;
	case IR_COPY:
		printf("\t%%%d =%s %%%d (copy)\n", dst, type, op0);
		break;
	case IR_MOV:
		printf("\t%%%d =%s %%%d (mov)\n", op0, type, op1);
		break;
	case IR_LOAD:
		printf("\t%%%d =%s load %%%d\n", dst, type, op0);
		break;
	case IR_STORE:
		printf("\tstore %s %%%d, %%%d\n", type, op0, op1);
		break;
	case IR_ADD:
		printf("\t%%%d =%s %%%d + %%%d\n",  dst, type, op0, op1);
		break;
	case IR_AND:
		printf("\t%%%d =%s %%%d & %%%d\n",  dst, type, op0, op1);
		break;
	case IR_SUB:
		printf("\t%%%d =%s %%%d - %%%d\n",  dst, type, op0, op1);
		break;
	case IR_MUL:
		printf("\t%%%d =%s %%%d * %%%d\n",  dst, type, op0, op1);
		break;
	case IR_DIV:
		printf("\t%%%d =%s %%%d / %%%d\n",  dst, type, op0, op1);
		break;
	case IR_MOD:
		printf("\t%%%d =%s %%%d %% %%%d\n", dst, type, op0, op1);
		break;
	case IR_EQL:
		printf("\t%%%d =%s %%%d == %%%d\n", dst, type, op0, op1);
		break;
	case IR_LEQ:
		printf("\t%%%d =%s %%%d <= %%%d\n", dst, type, op0, op1);
		break;
	case IR_GEQ:
		printf("\t%%%d =%s %%%d >= %%%d\n", dst, type, op0, op1);
		break;
	case IR_LT:
		printf("\t%%%d =%s %%%d < %%%d\n",  dst, type, op0, op1);
		break;
	case IR_GT:
		printf("\t%%%d =%s %%%d > %%%d\n",  dst, type, op0, op1);
		break;
	case IR_LEQU:
		printf("\t%%%d =%s %%%d <= %%%d (unsigned)\n", dst, type, op0, op1);
		break;
	case IR_GEQU:
		printf("\t%%%d =%s %%%d >= %%%d (unsigned)\n", dst, type, op0, op1);
		break;
	case IR_LTU:
		printf("\t%%%d =%s %%%d < %%%d (unsigned)\n",  dst, type, op0, op1);
		break;
	case IR_GTU:
		printf("\t%%%d =%s %%%d > %%%d (unsigned)\n",  dst, type, op0, op1);
		break;
	case IR_OR:
		printf("\t%%%d =%s %%%d | %%%d\n",  dst, type, op0, op1);
		break;
	case IR_SHL:
		printf("\t%%%d =%s %%%d << %%%d\n",  dst, type, op0, op1);
		break;
	case IR_SHR:
		printf("\t%%%d =%s %%%d >> %%%d\n",  dst, type, op0, op1);
		break;
	case IR_XOR:
		printf("\t%%%d =%s %%%d ^ %%%d\n",  dst, type, op0, op1);
		break;
	case IR_JMP:
		printf("\tgoto L%d\n", op0);
		break;
	case IR_JIZ:
		printf("\tjiz %%%d, L%d\n", op0, op1);
		break;
	case IR_JNZ:
		printf("\tjnz %%%d, L%d\n", op0, op1);
		break;
	case IR_RET:
		printf("\tret %%%d\n", op0);
		break;
	case IR_TRUNC:
		printf("\t%%%d =%s trunc %%%d\n", dst, type, op0);
		break;
	case IR_SEXT:
		printf("\t%%%d =%s sext %%%d\n", dst, type, op0);
		break;
	case IR_ZEXT:
		printf("\t%%%d =%s zext %%%d\n", dst, type, op0);
		break;
	case IR_CALL_BUILTIN:
		printf("\t%%%d =%s call_builtin %s\n", dst, type, get_builtin_str(op0));
		break;
	case IR_CALL:
		printf("\t%%%d =%s call %%%d, %d\n", dst, type, op0, op1);
		break;
	case IR_PRINT:
		printf("\tprint %s %%%d\n", type, op0);
		break;
	case IR_PARAM:
		printf("\tparam %s %%%d\n", type, op0);
		break;
	case IR_ALLOC:
		printf("\t%%%d =%s alloc %d, %d\n", dst, type, op0, op1);
		break;
	case IR_LABEL:
		printf("L%d:\n", op0);
		break;
	}
}

static void
print_ir_program(ir_program program)
{
	u32 i = 0;
	for (ir_function *func = program.function_list; func; func = func->next) {
		printf("function[%d]:\n", i++);
		printf("  name: %.*s\n", (int)func->name.length, func->name.at);
		printf("  parameter_count: %d\n", func->parameter_count);
		printf("  instr_index: %d\n", func->instr_index);
		printf("  stack_size: %d\n", func->stack_size);

		for (u32 i = 0; i < func->instr_count; i++) {
			printf("%2d| ", i);
			ir_instr instr = program.instrs[func->instr_index + i];
			print_ir_instr(instr, i);
		}
	}

	printf("\n");
}

static void
print_x86_program(machine_program program)
{
	char *code = program.code;
	char *end = code + program.size;
	u32 i = 0;
	while (code < end) {
		printf("%2d|", i++);

		machine_instr *instr = (machine_instr *)code;
		if (instr->opcode != X86_LABEL) {
			printf("\tX86.%s ", x86_get_opcode_name(instr->opcode));
		}

		machine_operand *operands = (machine_operand *)(instr + 1);
		for (u32 j = 0; j < instr->operand_count; j++) {
			u32 value = operands[j].value;
			switch (operands[j].kind) {
			case MOP_VREG:
				printf("%%%d", value);
				break;
			case MOP_MREG:
				printf("%s", x86_get_register_name(value, 8));
				break;
			case MOP_LABEL:
				printf("L%d:", value);
				break;
			case MOP_IMMEDIATE:
				printf("%d", value);
				break;
			case MOP_GLOBAL:
				printf("global_%d", value);
				break;
			case MOP_SPILL:
				printf("spill");
				break;
			default:
				printf("?");
				break;
			}

			if (j + 1 < instr->operand_count) {
				printf(", ");
			}
		}

		putchar('\n');
	}
}
