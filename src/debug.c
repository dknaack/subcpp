static void
print_node(ast_node *node, int indent)
{
	for (int i = 0; i < indent; i++) {
		printf("    ");
	}

	switch (node->kind) {
	case AST_EXPR_BINARY:
		printf("(");
		print_node(node->u.bin_expr.lhs, 0);
		switch (node->u.bin_expr.op) {
		case TOKEN_ADD:    printf(" + ");  break;
		case TOKEN_SUB:    printf(" - ");  break;
		case TOKEN_MUL:    printf(" * ");  break;
		case TOKEN_DIV:    printf(" / ");  break;
		case TOKEN_MOD:    printf(" %% "); break;
		case TOKEN_ASSIGN: printf(" = ");  break;
		default:
			printf(" (invalid operation) ");
		}

		print_node(node->u.bin_expr.rhs, 0);
		printf(")");
		break;
	case AST_EXPR_IDENT:
		printf("%.*s", (int)node->u.ident.length, node->u.ident.at);
		break;
	case AST_EXPR_INT:
		printf("%jd", node->u.ival);
		break;
	case AST_STMT_BREAK:
		printf("break;\n");
		break;
	case AST_STMT_CONTINUE:
		printf("continue;\n");
		break;
	case AST_DECL:
		// TODO: print declarations and declarators
		break;
	case AST_STMT_EMPTY:
		printf(";\n");
		break;
	case AST_STMT_FOR:
		printf("for (");
		node = node->children;
		print_node(node, 0);
		printf("; ");
		node = node->next;
		print_node(node, 0);
		printf("; ");
		node = node->next;
		print_node(node, 0);
		printf(")\n");
		break;
	case AST_STMT_COMPOUND:
		printf("{\n");
		for (node = node->children; node != AST_NIL; node = node->next) {
			print_node(node, indent + 1);
		}
		printf("}\n");
		break;
	case AST_STMT_IF:
		printf("if (");
		node = node->children;
		print_node(node, 0);
		printf(")\n");
		node = node->next;
		print_node(node, indent);
		if (node->next != AST_NIL) {
			printf("else\n");
			node = node->next;
			print_node(node, indent);
		}

		break;
	case AST_STMT_WHILE:
		printf("while (");
		node = node->children;
		print_node(node, 0);
		printf(")\n");
		node = node->next;
		print_node(node, indent);
		break;
	case AST_STMT_RETURN:
		printf("return ");
		print_node(node->children, 0);
		printf(";\n");
		break;
	case AST_STMT_PRINT:
		printf("print ");
		print_node(node->children, 0);
		printf(";\n");
		break;
	default:
		printf("(invalid)");
	}
}

static void
print_ir_instr(ir_instr instr, u32 i)
{
	u32 dst = i;
	u32 op0 = instr.op0;
	u32 op1 = instr.op1;
	switch (instr.opcode) {
	case IR_NOP:   printf("\tnop\n"); break;
	case IR_VAR: printf("\t%%%d = new vreg\n", dst); break;
	case IR_CONST: printf("\t%%%d = %d\n", dst, op0); break;
	case IR_COPY:  printf("\t%%%d = %%%d (copy)\n", dst, op0); break;
	case IR_MOV:   printf("\t%%%d = %%%d (mov)\n", op0, op1); break;
	case IR_LOAD:  printf("\t%%%d = load %%%d\n", dst, op0); break;
	case IR_STORE: printf("\tstore %%%d, %%%d\n", op0, op1); break;
	case IR_ADD:   printf("\t%%%d = %%%d + %%%d\n",  dst, op0, op1); break;
	case IR_SUB:   printf("\t%%%d = %%%d - %%%d\n",  dst, op0, op1); break;
	case IR_MUL:   printf("\t%%%d = %%%d * %%%d\n",  dst, op0, op1); break;
	case IR_DIV:   printf("\t%%%d = %%%d / %%%d\n",  dst, op0, op1); break;
	case IR_MOD:   printf("\t%%%d = %%%d %% %%%d\n", dst, op0, op1); break;
	case IR_EQL:   printf("\t%%%d = %%%d == %%%d\n", dst, op0, op1); break;
	case IR_LEQ:   printf("\t%%%d = %%%d <= %%%d\n", dst, op0, op1); break;
	case IR_GEQ:   printf("\t%%%d = %%%d >= %%%d\n", dst, op0, op1); break;
	case IR_LT:    printf("\t%%%d = %%%d < %%%d\n",  dst, op0, op1); break;
	case IR_GT:    printf("\t%%%d = %%%d > %%%d\n",  dst, op0, op1); break;
	case IR_JMP:   printf("\tgoto L%d\n", op0); break;
	case IR_JIZ:   printf("\tjiz %%%d, L%d\n", op0, op1); break;
	case IR_RET:   printf("\tret %%%d\n", op0); break;
	case IR_CALL:  printf("\t%%%d = call L%d, %d\n", dst, op0, op1); break;
	case IR_PRINT: printf("\tprint %%%d\n", op0); break;
	case IR_PARAM: printf("\tparam %%%d\n", op0); break;
	case IR_ALLOC: printf("\t%%%d = alloc %d, %d\n", dst, op0, op1); break;
	case IR_LABEL: printf("L%d:\n", op0); break;
	}
}

static void
print_ir_program(ir_program program)
{
	printf("function_count: %d\n", program.function_count);
	for (u32 i = 0; i < program.function_count; i++) {
		ir_function *func = &program.functions[i];
		printf("function[%d]:\n", i);
		printf("  name: %.*s\n", (int)func->name.length, func->name.at);
		printf("  parameter_count: %d\n", func->parameter_count);
		printf("  block_count: %d\n", func->block_count);
		printf("  block_index: %d\n", func->block_index);
		printf("  instr_index: %d\n", func->instr_index);
		printf("  stack_size: %d\n", func->stack_size);
	}

	for (u32 i = 0; i < program.instr_count; i++) {
		printf("%2d| ", i);
		ir_instr instr = program.instrs[i];
		print_ir_instr(instr, i);
	}

	printf("\n");
}

static void
print_x86_program(machine_program program)
{
	char *code = program.code;
	for (uint32_t i = 0; i < program.instr_count; i++) {
		u32 offset = program.instr_offsets[i];
		machine_instr *instr = (machine_instr *)(code + offset);
		if (instr->opcode != X86_LABEL) {
			printf("\tX86.%s ", x86_get_opcode_name(instr->opcode));
		}

		machine_operand *operands = (machine_operand *)(instr + 1);
		for (u32 j = 0; j < instr->operand_count; j++) {
			u32 value = operands[j].value;
			switch (operands[j].kind) {
			case MOP_VREG:
				if ((instr->opcode == X86_STORE && j == 0)
					|| (instr->opcode == X86_LOAD && j == 1)) {
					printf("[%%%d]", value);
				} else {
					printf("%%%d", value);
				}
				break;
			case MOP_MREG:
				if ((instr->opcode == X86_STORE && j == 0)
					|| (instr->opcode == X86_LOAD && j == 1)) {
					printf("[%s]", x86_get_register_name(value, 8));
				} else {
					printf("%s", x86_get_register_name(value, 8));
				}
				break;
			case MOP_LABEL:
				printf("L%d:", value);
				break;
			case MOP_IMMEDIATE:
				printf("%d", value);
				break;
			case MOP_FUNC:
				printf("func_%d", value);
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
