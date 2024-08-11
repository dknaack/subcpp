static char *
get_builtin_str(ir_builtin builtin)
{
	switch (builtin) {
	case BUILTIN_POPCOUNT:
		return "popcount";
	case BUILTIN_VA_LIST:
		return "va_list";
	case BUILTIN_VA_START:
		return "va_start";
	case BUILTIN_VA_ARG:
		return "va_arg";
	case BUILTIN_VA_END:
		return "va_end";
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
print_ast_node(ast_pool *pool, ast_id node_id, int indent)
{
	ast_id tmp;

	if (node_id.value == 0) {
		return;
	}

	ast_node *node = get_node(pool, node_id);
	switch (node->kind) {
	case AST_INVALID:
	case AST_STMT_FOR2:
	case AST_STMT_FOR3:
		printf("(invalid)");
		break;
	case AST_EXTERN_DEF:
	case AST_DECL:
		printf("%.*s: ", (int)node->token.value.length, node->token.value.at);
		print_ast_node(pool, node->child[0], indent);
		if (node->child[1].value != 0) {
			printf(" = ");
			print_ast_node(pool, node->child[1], indent);
		}

		break;
	case AST_EXPR_BINARY:
		printf("(");
		print_ast_node(pool, node->child[0], indent);
		printf(") <*> (");
		print_ast_node(pool, node->child[1], indent);
		printf(")");
		break;
	case AST_EXPR_CALL:
		print_ast_node(pool, node->child[0], indent);

		printf("(");
		for (node_id = node->child[1]; node_id.value != 0; node_id = node->child[1]) {
			node = get_node(pool, node_id);
			print_ast_node(pool, node->child[0], indent);
			if (node->child[1].value != 0) {
				printf(", ");
			}
		}

		printf(")");
		break;
	case AST_EXPR_CAST:
		printf("(");
		print_ast_node(pool, node->child[0], indent);
		printf(")");
		print_ast_node(pool, node->child[1], indent);
		break;
	case AST_EXPR_LITERAL:
		printf("%.*s", (int)node->token.value.length, node->token.value.at);
		break;
	case AST_EXPR_MEMBER:
	case AST_EXPR_MEMBER_PTR:
		print_ast_node(pool, node->child[0], indent);
		printf(".%.*s", (int)node->token.value.length, node->token.value.at);
		break;
	case AST_EXPR_POSTFIX:
		print_ast_node(pool, node->child[0], indent);
		switch (node->token.kind) {
		case TOKEN_PLUS_PLUS:
			printf("++");
			break;
		case TOKEN_MINUS_MINUS:
			printf("--");
			break;
		default:
			ASSERT(!"Invalid postfix expression");
		}

		break;
	case AST_EXPR_TERNARY1:
		print_ast_node(pool, node->child[0], indent);
		printf(" ? ");
		print_ast_node(pool, node->child[1], indent);
		break;
	case AST_EXPR_TERNARY2:
		print_ast_node(pool, node->child[0], indent);
		printf(" : ");
		print_ast_node(pool, node->child[1], indent);
		break;
	case AST_EXPR_UNARY:
		switch (node->token.kind) {
		case TOKEN_PLUS_PLUS:
			printf("++");
			break;
		case TOKEN_MINUS_MINUS:
			printf("--");
			break;
		case TOKEN_PLUS:
			printf("+");
			break;
		case TOKEN_MINUS:
			printf("-");
			break;
		case TOKEN_STAR:
			printf("*");
			break;
		case TOKEN_BANG:
			printf("!");
			break;
		case TOKEN_AMP:
			printf("&");
			break;
		default:
			ASSERT(!"Invalid unary expression");
		}

		print_ast_node(pool, node->child[0], indent);
		break;
	case AST_STMT_BREAK:
		printf("break");
		break;
	case AST_STMT_CASE:
		printf("case ");
		print_ast_node(pool, node->child[0], indent + 1);
		printf(":\n");
		print_ast_node(pool, node->child[1], indent + 1);
		break;
	case AST_STMT_CONTINUE:
		printf("continue");
		break;
	case AST_STMT_DEFAULT:
		printf("default:\n");
		print_ast_node(pool, node->child[0], indent + 1);
		break;
	case AST_STMT_DO_WHILE:
		printf("do");
		print_ast_node(pool, node->child[1], indent);
		printf("while (");
		print_ast_node(pool, node->child[0], indent);
		printf(");");
		break;
	case AST_STMT_EMPTY:
		break;
	case AST_STMT_GOTO:
		printf("goto %.*s", (int)node->token.value.length, node->token.value.at);
		break;
	case AST_STMT_FOR1:
		printf("for (");
		print_ast_node(pool, node->child[0], indent);
		printf("; ");
		node = get_node(pool, node->child[1]);
		print_ast_node(pool, node->child[0], indent);
		printf("; ");
		node = get_node(pool, node->child[1]);
		print_ast_node(pool, node->child[0], indent);
		printf(") ");
		node = get_node(pool, node->child[1]);
		print_ast_node(pool, node->child[0], indent);
		break;
	case AST_STMT_IF1:
		printf("if (");
		print_ast_node(pool, node->child[0], indent);
		printf(") ");
		print_ast_node(pool, node->child[1], indent);
		break;
	case AST_STMT_IF2:
		print_ast_node(pool, node->child[0], indent);
		if (node->child[1].value != 0) {
			printf("else ");
			print_ast_node(pool, node->child[1], indent);
		}
		break;
	case AST_STMT_LABEL:
		printf("%.*s:\n", (int)node->token.value.length, node->token.value.at);
		print_ast_node(pool, node->child[0], indent);
		break;
	case AST_LIST:
		printf("{\n");
		while (node_id.value != 0) {
			for (int i = 0; i < indent + 1; i++) printf("    ");
			node = get_node(pool, node_id);
			print_ast_node(pool, node->child[0], indent+1);
			node_id = node->child[1];
			printf(";\n");
		}
		for (int i = 0; i < indent; i++) printf("    ");
		printf("}\n");
		break;
	case AST_STMT_SWITCH:
		printf("switch (");
		print_ast_node(pool, node->child[0], indent);
		printf(")");
		print_ast_node(pool, node->child[0], indent);
		break;
	case AST_STMT_RETURN:
		printf("return ");
		print_ast_node(pool, node->child[0], indent);
		break;
	case AST_STMT_WHILE:
		printf("while (");
		print_ast_node(pool, node->child[0], indent);
		printf(")");
		print_ast_node(pool, node->child[1], indent);
		break;
	case AST_TYPE_ARRAY:
		printf("[");
		print_ast_node(pool, node->child[0], indent);
		printf("]");
		print_ast_node(pool, node->child[1], indent);
		break;
	case AST_TYPE_BITFIELD:
		print_ast_node(pool, node->child[1], indent);
		printf(":");
		print_ast_node(pool, node->child[0], indent);
		break;
	case AST_TYPE_FUNC:
		tmp = node->child[1];
		printf("(");
		for (node_id = node->child[0]; node_id.value != 0; node_id = node->child[1]) {
			node = get_node(pool, node_id);
			print_ast_node(pool, node->child[0], indent);
			if (node->child[1].value != 0) {
				printf(", ");
			}
		}

		printf(") -> ");
		print_ast_node(pool, tmp, indent);
		break;
	case AST_TYPE_IDENT:
	case AST_EXPR_IDENT:
		printf("%.*s", (int)node->token.value.length, node->token.value.at);
		break;
	case AST_TYPE_POINTER:
		printf("*");
		print_ast_node(pool, node->child[1], indent);
		break;
	case AST_TYPE_STRUCT:
		printf("(struct)");
		break;
	case AST_TYPE_UNION:
		printf("(union)");
		break;
	case AST_TYPE_ENUM:
		printf("(enum)");
		break;
	case AST_TYPE_BASIC:
		switch (node->token.kind) {
		case TOKEN_CHAR:
			printf("char");
			break;
		case TOKEN_FLOAT:
			printf("float");
			break;
		case TOKEN_INT:
			printf("int");
			break;
		case TOKEN_VOID:
			printf("void");
			break;
		default:
			ASSERT(!"Invalid basic type");
		}

		break;
	default:
		printf("(TODO)");
		break;
	}
}

static void
print_ast(ast_pool *pool)
{
	print_ast_node(pool, pool->root, 0);
}

static void
print_ir_inst(ir_inst inst, u32 i)
{
	u32 dst = i;
	u32 op0 = inst.op0;
	u32 op1 = inst.op1;
	char *type = get_ir_type_str(inst.type);
	switch (inst.opcode) {
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
	case IR_BUILTIN:
		printf("\t%%%d =%s %s\n", dst, type, get_builtin_str(op0));
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
	case IR_NOT:
		printf("\t%%%d =%s ~%%%d\n",  dst, type, op0);
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
	case IR_CALL:
		printf("\t%%%d =%s call %%%d, %d\n", dst, type, op0, op1);
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
	for (isize i = 0; i < program.function_count; i++) {
		ir_function *func = &program.functions[i];
		printf("function[%ld]:\n", i);
		printf("  name: %.*s\n", (int)func->name.length, func->name.at);
		printf("  parameter_count: %d\n", func->parameter_count);
		printf("  inst_index: %d\n", func->inst_index);
		printf("  stack_size: %d\n", func->stack_size);

		for (u32 i = 0; i < func->inst_count; i++) {
			printf("%2d| ", i);
			ir_inst inst = program.insts[func->inst_index + i];
			print_ir_inst(inst, i);
		}
	}

	printf("\n");
}

static void
print_x86_operand(mach_operand operand)
{
	u32 value = operand.value;
	switch (operand.kind) {
	case MOP_VREG:
		printf("%%%d%s", value, operand.size == 4 ? "d" : "");
		break;
	case MOP_MREG:
		printf("%s", x86_get_register_name(value, operand.size));
		break;
	case MOP_LABEL:
		printf("L%d:", value);
		break;
	case MOP_CONST:
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

}

static void
print_x86_program(mach_program program)
{
	char *code = program.code;
	char *end = code + program.size;
	isize i = 0;
	while (code < end) {
		printf("%2ld|", i++);

		mach_inst *inst = (mach_inst *)code;
		code += sizeof(*inst) + inst->operand_count * sizeof(mach_operand);
		if (inst->opcode != X86_LABEL) {
			printf("\tX86.%s ", x86_get_opcode_name(inst->opcode));
		} else {
			printf("L");
		}

		mach_operand *operands = (mach_operand *)(inst + 1);
		for (u32 j = 0; j < inst->operand_count; j++) {
			print_x86_operand(operands[j]);
			if (j + 1 < inst->operand_count) {
				printf(", ");
			}
		}

		putchar('\n');
	}

	printf("\n");
}

static void
print_row(bit_matrix matrix, u32 y)
{
	printf("{");
	b32 first = true;
	for (u32 x = 0; x < matrix.width; x++) {
		if (matrix.bits[y * matrix.width + x]) {
			if (first) {
				first = false;
			} else {
				printf(", ");
			}

			u32 mreg = matrix.width - 1 - x;
			if (mreg < X86_REGISTER_COUNT) {
				printf("%s", x86_get_register_name(mreg, 8));
			} else {
				printf("r%d", x);
			}
		}
	}

	printf("}\n");
}

static void
print_matrix(bit_matrix matrix)
{
	for (u32 y = 0; y < matrix.height; y++) {
		printf("live[%d] = ", y);
		print_row(matrix, y);
	}
}

static void
print_tokens(parse_context *ctx)
{
	i32 indent = 0;
	b32 newline = true;
	b32 requires_space = false;
	for (;;) {
		token token = get_token(ctx);
		if (token.kind == TOKEN_EOF) break;
		if (token.kind == TOKEN_RBRACE) {
			newline = true;
			indent--;
		}

		if (newline) {
			for (i32 i = 0; i < indent; i++) {
				printf("    ");
			}

			newline = false;
		}

		switch (token.kind) {
		case TOKEN_LPAREN:
		case TOKEN_SEMICOLON:
		case TOKEN_MINUS_MINUS:
		case TOKEN_PLUS_PLUS:
		case TOKEN_RBRACE:
			requires_space = false;
			break;
		case TOKEN_LBRACE:
			printf(" ");
			requires_space = false;
			break;
		case TOKEN_PLUS:
		case TOKEN_MINUS:
		case TOKEN_AMP:
		case TOKEN_STAR:
			if (requires_space) {
				printf(" ");
			}

			break;
		default:
			if (requires_space) {
				printf(" ");
			}

			/* fallthrough */
		case TOKEN_RPAREN:
		case TOKEN_COMMA:
			requires_space = true;
		}

		printf("%.*s", (int)token.value.length, token.value.at);
		if (token.kind == TOKEN_SEMICOLON) {
			printf("\n");
			newline = true;
		} else if (token.kind == TOKEN_LBRACE) {
			indent++;
			printf("\n");
			newline = true;
		} else if (token.kind == TOKEN_RBRACE) {
			printf("\n\n");
			newline = true;
		} else if (token.kind == TOKEN_EOF) {
			break;
		}
	}
}
