static char *
get_builtin_str(ir_builtin builtin)
{
	switch (builtin) {
	case BUILTIN_MEMCPY:
		return "memcpy";
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
get_ir_opcode_str(ir_opcode opcode)
{
	switch (opcode) {
	case IR_NOP:
		return "nop";
	case IR_PHI:
		return "phi";

	// declarations
	case IR_ALLOC:
		return "alloc";
	case IR_BUILTIN:
		return "builtin";
	case IR_CONST:
		return "const";
	case IR_GLOBAL:
		return "global";
	case IR_FUNC:
		return "func";
	case IR_LABEL:
		return "label";
	case IR_PARAM:
		return "param";

	// data movement operators
	case IR_COPY:
		return "copy";
	case IR_LOAD:
		return "load";
	case IR_STORE:
		return "store";

	// control-flow operators
	case IR_CALL:
		return "call";
	case IR_JIZ:
		return "jiz";
	case IR_JMP:
		return "jmp";
	case IR_JNZ:
		return "jnz";
	case IR_RET:
		return "ret";

	// integer operators
	case IR_ADD:
		return "add";
	case IR_DIV:
		return "div";
	case IR_MOD:
		return "mod";
	case IR_MUL:
		return "mul";
	case IR_SUB:
		return "sub";

	// comparison operators
	case IR_EQ:
		return "eq";
	case IR_GT:
		return "gt";
	case IR_GE:
		return "ge";
	case IR_LT:
		return "lt";
	case IR_LE:
		return "le";
	case IR_GTU:
		return "gtu";
	case IR_GEU:
		return "geu";
	case IR_LTU:
		return "ltu";
	case IR_LEU:
		return "leu";

	// bitwise operators
	case IR_AND:
		return "and";
	case IR_NOT:
		return "not";
	case IR_OR:
		return "or";
	case IR_SHL:
		return "shl";
	case IR_SHR:
		return "shr";
	case IR_XOR:
		return "xor";

	// conversion operators
	case IR_I2F:
		return "i2f";
	case IR_SEXT:
		return "sext";
	case IR_TRUNC:
		return "trunc";
	case IR_ZEXT:
		return "zext";

	// float operations
	case IR_FSTORE:
		return "fstore";
	case IR_FLOAD:
		return "fload";
	case IR_FCOPY:
		return "fcopy";

	case IR_FADD:
		return "fadd";
	case IR_FSUB:
		return "fsub";
	case IR_FMUL:
		return "fmul";
	case IR_FDIV:
		return "fdiv";

	case IR_FEQ:
		return "feq";
	case IR_FGT:
		return "fgt";
	case IR_FGE:
		return "fge";
	case IR_FLT:
		return "flt";
	case IR_FLE:
		return "fle";
	case IR_FRET:
		return "fret";
	case IR_F2I:
		return "f2i";
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

	ast_node node = get_node(pool, node_id);
	get_children(pool, node_id, children, LENGTH(children));

	switch (node.kind) {
	case AST_INVALID:
		printf("(invalid)");
		break;
	case AST_NONE:
		break;
	case AST_DECL:
		printf("%.*s: ", (int)node.token.value.length, node.token.value.at);
		print_ast_node(pool, node.children, indent);
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

			node_id = get_node(pool, node_id).next;
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
		printf("%.*s", (int)node.token.value.length, node.token.value.at);
		break;
	case AST_EXPR_MEMBER:
	case AST_EXPR_MEMBER_PTR:
		print_ast_node(pool, children[0], indent);
		printf(".%.*s", (int)node.token.value.length, node.token.value.at);
		break;
	case AST_EXPR_POSTFIX:
		print_ast_node(pool, children[0], indent);
		switch (node.token.kind) {
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
		switch (node.token.kind) {
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
		printf("goto %.*s", (int)node.token.value.length, node.token.value.at);
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
		printf("%.*s:\n", (int)node.token.value.length, node.token.value.at);
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
		printf("%.*s", (int)node.token.value.length, node.token.value.at);
		break;
	case AST_TYPE_POINTER:
		printf("*");
		print_ast_node(pool, children[0], indent);
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
		switch (node.token.kind) {
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
print_ir_inst(ir_inst *inst, u32 i, i32 *ref_count)
{
	if (ref_count[i] > 1) {
		printf("%%%d", i);
		return;
	}

	u32 dst = i;
	i32 *args = inst[i].args;
	u32 size = inst[i].size;
	switch (inst[i].opcode) {
	case IR_NOP:
		printf("nop");
		break;
	case IR_PHI:
		printf("(phi ");
		for (i32 j = i; j; j = inst[j].args[1]) {
			printf("%d, ", inst[j].args[0]);
		}
		printf(")");
		break;
	case IR_GLOBAL:
		printf("(global.%d %d)", size, args[0]);
		break;
	case IR_FUNC:
		printf("(func.%d %d)", size, args[0]);
		break;
	case IR_CONST:
		printf("(const.%d %d)", size, args[0]);
		break;
	case IR_BUILTIN:
		printf("(builtin.%d %s)", size, get_builtin_str(args[0]));
		break;
	case IR_COPY:
		print_ir_inst(inst, args[0], ref_count);
		break;
	case IR_ALLOC:
		printf("(%%%d = alloc.%d %d %d)", dst, size, args[0], args[1]);
		break;
	case IR_ADD:
	case IR_AND:
	case IR_DIV:
	case IR_EQ:
	case IR_GE:
	case IR_GEU:
	case IR_GT:
	case IR_GTU:
	case IR_LE:
	case IR_LEU:
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
	case IR_FSTORE:
	case IR_FADD:
	case IR_FSUB:
	case IR_FMUL:
	case IR_FDIV:
	case IR_FEQ:
	case IR_FGT:
	case IR_FGE:
	case IR_FLT:
	case IR_FLE:
		printf("(%s.%d ", get_ir_opcode_str(inst[i].opcode), size);
		print_ir_inst(inst, args[0], ref_count);
		printf(" ");
		print_ir_inst(inst, args[1], ref_count);
		printf(")");
		break;
	case IR_JMP:
		printf("(jmp.%d L%d)", size, args[0]);
		break;
	case IR_JIZ:
	case IR_JNZ:
		printf("(%s.%d ", get_ir_opcode_str(inst[i].opcode), size);
		print_ir_inst(inst, args[0], ref_count);
		printf(" L%d)", args[1]);
		break;
	case IR_LOAD:
	case IR_NOT:
	case IR_RET:
	case IR_TRUNC:
	case IR_SEXT:
	case IR_ZEXT:
	case IR_I2F:
	case IR_F2I:
	case IR_FLOAD:
	case IR_FRET:
	case IR_FCOPY:
		printf("(%s.%d ", get_ir_opcode_str(inst[i].opcode), size);
		print_ir_inst(inst, args[0], ref_count);
		printf(")");
		break;
	case IR_CALL:
		printf("(call.%d ", size);
		print_ir_inst(inst, args[0], ref_count);

		for (isize j = args[1]; j; j = inst[j].args[1]) {
			printf(" %d", inst[j].size);
			print_ir_inst(inst, inst[j].args[0], ref_count);
		}

		printf(")");
		break;
	case IR_PARAM:
		printf("(param.%d %d %d)", size, args[0], args[1]);
		break;
	case IR_LABEL:
		printf("L%d: ", args[0]);
		break;
	}
}

static i32 *get_ref_count(ir_inst *inst, isize inst_count, arena *perm);

static void
print_ir_program(ir_program program)
{
	for (isize func_id = 1; func_id < program.func_count; func_id++) {
		arena *temp = new_arena(4096);

		ir_function *func = &program.funcs[func_id];
		printf("func[%ld]:\n", func_id);

		i32 *ref_count = get_ref_count(func->insts, func->inst_count, temp);
		for (isize i = 0; i < func->inst_count; i++) {
			if (ref_count[i] == 0 && func->insts[i].opcode != IR_NOP) {
				printf("\t%%%zd = ", i);
				print_ir_inst(func->insts, i, ref_count);
				printf("\n");
			}
		}

		free(temp);
	}

	printf("\n");
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
