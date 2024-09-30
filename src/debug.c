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
	ast_id children[4] = {0};
	ast_id tmp;

	if (node_id.value == 0) {
		return;
	}

	ast_node *node = get_node(pool, node_id);
	get_children(pool, node_id, children, LENGTH(children));

	switch (node->kind) {
	case AST_INVALID:
		printf("(invalid)");
		break;
	case AST_NONE:
		break;
	case AST_EXTERN_DEF:
	case AST_DECL:
		printf("%.*s: ", (int)node->token.value.length, node->token.value.at);
		print_ast_node(pool, node->children, indent);
		if (children[0].value != 0) {
			printf(" = ");
			print_ast_node(pool, children[0], indent);
		}

		break;
	case AST_EXPR_BINARY:

		printf("(");
		print_ast_node(pool, children[0], indent);
		printf(") <*> (");
		print_ast_node(pool, children[1], indent);
		printf(")");
		break;
	case AST_EXPR_CALL:
		print_ast_node(pool, children[0], indent);

		printf("(");
		node_id = children[1];
		while (node_id.value != 0) {
			print_ast_node(pool, node_id, indent);

			node_id = get_node(pool, node_id)->next;
			if (node_id.value != 0) {
				printf(", ");
			}
		}

		printf(")");
		break;
	case AST_EXPR_CAST:

		printf("(");
		print_ast_node(pool, children[0], indent);
		printf(")");
		print_ast_node(pool, children[1], indent);
		break;
	case AST_EXPR_LITERAL:
		printf("%.*s", (int)node->token.value.length, node->token.value.at);
		break;
	case AST_EXPR_MEMBER:
	case AST_EXPR_MEMBER_PTR:
		print_ast_node(pool, children[0], indent);
		printf(".%.*s", (int)node->token.value.length, node->token.value.at);
		break;
	case AST_EXPR_POSTFIX:
		print_ast_node(pool, children[0], indent);
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
	case AST_EXPR_TERNARY:

		print_ast_node(pool, children[0], indent);
		printf(" ? ");
		print_ast_node(pool, children[1], indent);
		printf(" : ");
		print_ast_node(pool, children[2], indent);
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

		print_ast_node(pool, children[0], indent);
		break;
	case AST_STMT_BREAK:
		printf("break");
		break;
	case AST_STMT_CASE:
		printf("case ");
		print_ast_node(pool, children[0], indent + 1);
		printf(":\n");
		print_ast_node(pool, children[1], indent + 1);
		break;
	case AST_STMT_CONTINUE:
		printf("continue");
		break;
	case AST_STMT_DEFAULT:
		printf("default:\n");
		print_ast_node(pool, children[0], indent + 1);
		break;
	case AST_STMT_DO_WHILE:
		printf("do");
		print_ast_node(pool, children[1], indent);
		printf("while (");
		print_ast_node(pool, children[0], indent);
		printf(");");
		break;
	case AST_STMT_GOTO:
		printf("goto %.*s", (int)node->token.value.length, node->token.value.at);
		break;
	case AST_STMT_FOR:
		printf("for (");
		print_ast_node(pool, children[0], indent);
		printf("; ");
		print_ast_node(pool, children[1], indent);
		printf("; ");
		print_ast_node(pool, children[2], indent);
		printf(") ");
		print_ast_node(pool, children[3], indent);
		break;
	case AST_STMT_IF:
		printf("if (");
		print_ast_node(pool, children[0], indent);
		printf(") ");
		print_ast_node(pool, children[1], indent);

		if (children[2].value != 0) {
			printf("else ");
			print_ast_node(pool, children[2], indent);
		}
		break;
	case AST_STMT_LABEL:
		printf("%.*s:\n", (int)node->token.value.length, node->token.value.at);
		print_ast_node(pool, children[0], indent);
		break;
	case AST_STMT_COMPOUND:
		printf("{\n");
		while (node_id.value != 0) {
			for (int i = 0; i < indent + 1; i++) printf("    ");
			node = get_node(pool, node_id);
			print_ast_node(pool, children[0], indent+1);
			node_id = children[1];
			printf(";\n");
		}
		for (int i = 0; i < indent; i++) printf("    ");
		printf("}\n");
		break;
	case AST_STMT_SWITCH:
		printf("switch (");
		print_ast_node(pool, children[0], indent);
		printf(")");
		print_ast_node(pool, children[0], indent);
		break;
	case AST_STMT_RETURN:
		printf("return ");
		print_ast_node(pool, children[0], indent);
		break;
	case AST_STMT_WHILE:
		printf("while (");
		print_ast_node(pool, children[0], indent);
		printf(")");
		print_ast_node(pool, children[1], indent);
		break;
	case AST_TYPE_ARRAY:
		printf("[");
		print_ast_node(pool, children[0], indent);
		printf("]");
		print_ast_node(pool, children[1], indent);
		break;
	case AST_TYPE_BITFIELD:
		print_ast_node(pool, children[1], indent);
		printf(":");
		print_ast_node(pool, children[0], indent);
		break;
	case AST_TYPE_FUNC:
		tmp = children[1];
		printf("(");
		for (node_id = children[0]; node_id.value != 0; node_id = children[1]) {
			node = get_node(pool, node_id);
			print_ast_node(pool, children[0], indent);
			if (children[1].value != 0) {
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
		print_ast_node(pool, children[0], indent);
		break;
	case AST_TYPE_COMPOUND:
		printf("(compound type)");
		break;
	case AST_TYPE_TAG:
		printf("(compound type with tag)");
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
print_ir_inst(ir_inst *inst, u32 i)
{
	u32 dst = i;
	u32 op0 = inst[i].op0;
	u32 op1 = inst[i].op1;
	char *type = get_ir_type_str(inst[i].type);
	switch (inst[i].opcode) {
	case IR_NOP:
		printf("nop");
		break;
	case IR_GLOBAL:
		printf("(global.%s %d)", type, op0);
		break;
	case IR_VAR:
		printf("(var.%s %%%d)", type, dst);
		break;
	case IR_CONST:
		printf("(const.%s %d)", type, op0);
		break;
	case IR_BUILTIN:
		printf("(builtin.%s %s)", type, get_builtin_str(op0));
		break;
	case IR_COPY:
		printf("(copy.%s %%%d)", type, op0);
		break;
	case IR_ALLOC:
		printf("(%s.%s %d %d)", get_ir_opcode_str(inst[i].opcode), type, op0, op1);
		break;
	case IR_MOV:
	case IR_ADD:
	case IR_AND:
	case IR_DIV:
	case IR_EQL:
	case IR_GEQ:
	case IR_GEQU:
	case IR_GT:
	case IR_GTU:
	case IR_LEQ:
	case IR_LEQU:
	case IR_LT:
	case IR_LTU:
	case IR_MOD:
	case IR_MUL:
	case IR_OR:
	case IR_SHL:
	case IR_SHR:
	case IR_STORE:
	case IR_SUB:
	case IR_XOR:
		printf("(%s.%s ", get_ir_opcode_str(inst[i].opcode), type);
		print_ir_inst(inst, op0);
		printf(" ");
		print_ir_inst(inst, op1);
		printf(")");
		break;
	case IR_JMP:
		printf("(jmp.%s L%d)", type, op0);
		break;
	case IR_JIZ:
	case IR_JNZ:
		printf("(%s.%s ", get_ir_opcode_str(inst[i].opcode), type);
		print_ir_inst(inst, op0);
		printf(" L%d)", op1);
		break;
	case IR_CAST:
	case IR_CASTU:
	case IR_LOAD:
	case IR_NOT:
	case IR_RET:
	case IR_TRUNC:
	case IR_SEXT:
	case IR_ZEXT:
		printf("(%s.%s ", get_ir_opcode_str(inst[i].opcode), type);
		print_ir_inst(inst, op0);
		printf(")");
		break;
	case IR_CALL:
		printf("(call.%s ", type);
		print_ir_inst(inst, op0);

		for (isize j = op1; j; j = inst[j].op1) {
			printf(" ");
			print_ir_inst(inst, inst[j].op0);
		}

		printf(")");
		break;
	case IR_PARAM:
		printf("(param.%s %d %d)", type, op0, op1);
		break;
	case IR_LABEL:
		printf("L%d: ", op0);
		break;
	}
}

static void
print_ir_program(ir_program program)
{
	for (isize i = 0; i < program.function_count; i++) {
		arena *temp = new_arena(4096);

		ir_function *func = &program.functions[i];
		printf("function[%ld]:\n", i);
		printf("  name: %.*s\n", (int)func->name.length, func->name.at);
		printf("  param_count: %d\n", func->param_count);
		printf("  inst_index: %d\n", func->inst_index);
		printf("  stack_size: %d\n", func->stack_size);

		ir_inst *inst = program.insts + func->inst_index;
		i32 *ref_count = get_ref_count(inst, func->inst_count, temp);
		for (isize j = 0; j < func->inst_count; j++) {
			if (ref_count[j] == 0) {
				printf("\t");
				print_ir_inst(inst, j);
				printf("\n");
			}
		}

		free(temp);
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
		for (isize j = 0; j < inst->operand_count; j++) {
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
