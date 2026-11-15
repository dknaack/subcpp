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
print_syntax_node(syntax_tree *tree, syntax_node_id node_id, char *prefix)
{
	syntax_node node = get_node(tree, node_id);

	const char *name = "(invalid)";
	switch (node.kind) {
	case SYNTAX_ERROR:
		name = "Error";
		break;
	case SYNTAX_NONE:
		name = "None";
		break;
	case SYNTAX_BUILTIN:
		name = "Builtin";
		break;
	case SYNTAX_DECL:
		name = "Decl";
		break;
	case SYNTAX_DECLARATION:
		name = "Declaration";
		break;
	case SYNTAX_ENUMERATOR:
		name = "Enumerator";
		break;
	case SYNTAX_INIT:
		name = "Init";
		break;
	case SYNTAX_EXPR_BINARY:
		name = "ExprBinary";
		break;
	case SYNTAX_EXPR_CALL:
		name = "ExprCall";
		break;
	case SYNTAX_EXPR_CAST:
		name = "ExprCast";
		break;
	case SYNTAX_EXPR_COMPOUND:
		name = "ExprCompound";
		break;
	case SYNTAX_EXPR_IDENT:
		name = "ExprIdent";
		break;
	case SYNTAX_EXPR_LITERAL:
		name = "ExprLiteral";
		break;
	case SYNTAX_EXPR_MEMBER:
		name = "ExprMember";
		break;
	case SYNTAX_EXPR_MEMBER_PTR:
		name = "ExprMemberPtr";
		break;
	case SYNTAX_EXPR_POSTFIX:
		name = "ExprPostfix";
		break;
	case SYNTAX_EXPR_SIZEOF:
		name = "ExprSizeof";
		break;
	case SYNTAX_EXPR_TERNARY:
		name = "ExprTernary";
		break;
	case SYNTAX_EXPR_UNARY:
		name = "ExprUnary";
		break;
	case SYNTAX_STMT_ASM:
		name = "StmtAsm";
		break;
	case SYNTAX_STMT_BREAK:
		name = "StmtBreak";
		break;
	case SYNTAX_STMT_CASE:
		name = "StmtCase";
		break;
	case SYNTAX_STMT_COMPOUND:
		name = "StmtCompound";
		break;
	case SYNTAX_STMT_CONTINUE:
		name = "StmtContinue";
		break;
	case SYNTAX_STMT_DEFAULT:
		name = "StmtDefault";
		break;
	case SYNTAX_STMT_DO_WHILE:
		name = "StmtDo_while";
		break;
	case SYNTAX_STMT_FOR:
		name = "StmtFor";
		break;
	case SYNTAX_STMT_GOTO:
		name = "StmtGoto";
		break;
	case SYNTAX_STMT_IF:
		name = "StmtIf";
		break;
	case SYNTAX_STMT_LABEL:
		name = "StmtLabel";
		break;
	case SYNTAX_STMT_RETURN:
		name = "StmtReturn";
		break;
	case SYNTAX_STMT_SWITCH:
		name = "StmtSwitch";
		break;
	case SYNTAX_STMT_WHILE:
		name = "StmtWhile";
		break;
	case SYNTAX_TYPE_BASIC:
		name = "TypeBasic";
		break;
	case SYNTAX_TYPE_ARRAY:
		name = "TypeArray";
		break;
	case SYNTAX_TYPE_BITFIELD:
		name = "TypeBitfield";
		break;
	case SYNTAX_TYPE_ENUM:
		name = "TypeEnum";
		break;
	case SYNTAX_TYPE_FUNC:
		name = "TypeFunc";
		break;
	case SYNTAX_TYPE_IDENT:
		name = "TypeIdent";
		break;
	case SYNTAX_TYPE_POINTER:
		name = "TypePointer";
		break;
	case SYNTAX_TYPE_STRUCT:
		name = "TypeStruct";
		break;
	case SYNTAX_TYPE_UNION:
		name = "TypeUnion";
		break;
	}


	b32 is_last = (node.next.value == 0);
    printf("%s%s %s '%.*s'", prefix, is_last ? "└──" : "├──", name,
		(int)node.token.value.length, node.token.value.at);

	const char *flags[] = {
		"AUTO",
		"CONST",
		"EXTERN",
		"LONG",
		"LLONG",
		"REGISTER",
		"RESTRICT",
		"SHORT",
		"SIGNED",
		"STATIC",
		"THREAD_LOCAL",
		"TYPEDEF",
		"UNSIGNED",
		"VARIADIC",
		"VOLATILE",
		"OPAQUE",
	};

	b32 is_first_flag = true;
	for (int i = 0; i < LENGTH(flags); i++) {
		if (node.flags & (1 << i)) {
			if (!is_first_flag) {
				printf(" | ");
			} else {
				printf(", ");
				is_first_flag = false;
			}

			printf("%s", flags[i]);
		}
	}

	printf("\n");

    char child_prefix[256] = {0};
    snprintf(child_prefix, sizeof(child_prefix), "%s%s", prefix, is_last ? "    " : "│   ");

	syntax_node_id child_id = node.children;
	while (child_id.value != 0) {
		print_syntax_node(tree, child_id, child_prefix);
		syntax_node child = get_node(tree, child_id);
		child_id = child.next;
	}
}

static void
print_ast(syntax_tree *tree)
{
	printf("Root\n");

	char prefix[256] = {0};
	syntax_node_id node_id = tree->root;
	while (node_id.value != 0) {
		print_syntax_node(tree, node_id, prefix);
		syntax_node node = get_node(tree, node_id);
		node_id = node.next;
	}
}

static void
print_ir_inst(inst *inst, u32 i, i32 *ref_count)
{
	i32 *args = inst[i].args;
	i32 size = inst[i].size;
	switch (inst[i].opcode) {
	case IR_NOP:
		printf("nop");
		break;
	case IR_PHI:
		printf("phi ");
		for (i32 j = i; j; j = inst[j].args[1]) {
			printf("%%%d", inst[j].args[0]);
			if (inst[j].args[1]) {
				printf(", ");
			}
		}
		break;
	case IR_GLOBAL:
		printf("global.%d %d", size, args[0]);
		break;
	case IR_FUNC:
		printf("func.%d %d", size, args[0]);
		break;
	case IR_CONST:
		printf("const.%d %d", size, args[0]);
		break;
	case IR_BUILTIN:
		printf("builtin.%d %s", size, get_builtin_str(args[0]));
		break;
	case IR_COPY:
		printf("copy %%%d", args[0]);
		break;
	case IR_ALLOC:
		printf("alloc.%d %d %d", size, args[0], args[1]);
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
		printf("%s.%d ", get_ir_opcode_str(inst[i].opcode), size);
		printf("%%%d", args[0]);
		printf(" ");
		printf("%%%d", args[1]);
		break;
	case IR_JMP:
		printf("jmp.%d L%d", size, args[0]);
		break;
	case IR_JIZ:
	case IR_JNZ:
		printf("%s.%d ", get_ir_opcode_str(inst[i].opcode), size);
		printf("%%%d", args[0]);
		printf(" L%d", args[1]);
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
		printf("%s.%d ", get_ir_opcode_str(inst[i].opcode), size);
		printf("%%%d", args[0]);
		break;
	case IR_CALL:
		printf("call.%d ", size);
		printf("%%%d", args[0]);

		for (isize j = args[1]; j; j = inst[j].args[1]) {
			printf(" %d", inst[j].size);
			printf("%%%d", inst[j].args[0]);
		}

		break;
	case IR_PARAM:
		printf("param.%d %d %d", size, args[0], args[1]);
		break;
	case IR_LABEL:
		printf("label L%d", args[0]);
		break;
	}
}

static i32 *get_use_count(inst *inst, isize inst_count, arena *perm);

static void
print_ir_program(program program)
{
	for (isize func_id = 1; func_id < program.func_count; func_id++) {
		arena *temp = new_arena(4096);

		function *func = &program.funcs[func_id];
		printf("func[%ld]:\n", func_id);

		i32 *use_count = get_use_count(func->insts, func->inst_count, temp);
		for (isize i = 0; i < func->inst_count; i++) {
			printf("\t%%%zd = ", i);
			print_ir_inst(func->insts, i, use_count);
			printf("\n");
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
