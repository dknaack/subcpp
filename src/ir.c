static i32
new_label(ir_context *ctx)
{
	i32 result = ctx->label_count++;
	return result;
}

static b32
is_func(ast_pool *pool, ast_id node_id)
{
	b32 result = false;

	ast_node node = get_node(pool, node_id);
	if (node.kind == AST_DECL && (node.flags & (AST_EXTERN | AST_STATIC))) {
		ast_node type = get_node(pool, node.children);
		if (type.kind == AST_TYPE_FUNC) {
			result = true;
		}
	}

	return result;
}

static b32
is_global(ast_pool *pool, ast_id node_id)
{
	b32 result = false;

	ast_node node = get_node(pool, node_id);
	switch (node.kind) {
	case AST_DECL:
		if (node.flags & (AST_EXTERN | AST_STATIC)) {
			result = !is_func(pool, node_id);
		}
		break;
	case AST_EXPR_LITERAL:
		switch (node.token.kind) {
		case TOKEN_LITERAL_STRING:
		case TOKEN_LITERAL_FLOAT:
			result = true;
		default:
			break;
		}
	default:
		break;
	}

	return result;
}

static u32
ir_emit(ir_context *ctx, i32 size, ir_opcode opcode, i32 arg0, i32 arg1)
{
	i32 result = ctx->inst_count++;
	i8 flags = 0;

	switch (opcode) {
	case IR_NOP:
	case IR_JMP:
	case IR_LABEL:
		break;
	case IR_ALLOC:
	case IR_BUILTIN:
	case IR_CONST:
	case IR_GLOBAL:
	case IR_FUNC:
	case IR_PARAM:
		flags |= IR_DEF;
		break;
	case IR_RET:
	case IR_LOAD:
	case IR_COPY:
	case IR_TRUNC:
	case IR_SEXT:
	case IR_ZEXT:
	case IR_NOT:
	case IR_FCOPY:
	case IR_FLOAD:
	case IR_FRET:
	case IR_JIZ:
	case IR_JNZ:
	case IR_I2F:
	case IR_F2I:
		flags |= IR_USE0;
		flags |= IR_DEF;
		break;
	case IR_STORE:
	case IR_FSTORE:
		flags |= IR_USE0;
		flags |= IR_USE1;
		break;
	default:
		flags |= IR_USE0;
		flags |= IR_USE1;
		flags |= IR_DEF;
		break;
	}

	ir_inst *inst = &ctx->insts[result];
	inst->opcode = opcode;
	inst->size = size;
	inst->flags = flags;
	inst->args[0] = arg0;
	inst->args[1] = arg1;

	// Sanity check: Ensure that arguments come before this instruction
	ASSERT(!(flags & IR_USE0) || arg0 < result);
	ASSERT(!(flags & IR_USE1) || arg1 < result);
	ASSERT(size <= 8);
	ASSERT(ctx->inst_count <= ctx->max_inst_count);
	ASSERT(opcode != IR_STORE || arg1 != 0);
	return result;
}

static i32
ir_emit2(ir_context *ctx, i32 size, ir_opcode opcode, i32 arg0, i32 arg1)
{
	ASSERT(arg0 != 0);
	ASSERT(arg1 != 0);
	i32 result = ir_emit(ctx, size, opcode, arg0, arg1);
	return result;
}

static i32
ir_emit1(ir_context *ctx, i32 size, ir_opcode opcode, i32 arg0)
{
	i32 result = ir_emit(ctx, size, opcode, arg0, 0);

	// Ensure that the provided argument for globals is in bounds
	if (opcode == IR_GLOBAL) {
		ASSERT(0 <= arg0 && arg0 < ctx->program->global_count);
	}

	// Only constants can have zero as their argument and
	// the first instruction is a label with zero
	if (opcode != IR_CONST && ctx->inst_count > 1) {
		ASSERT(arg0 != 0);
	}

	return result;
}

static i32
ir_emit0(ir_context *ctx, i32 size, ir_opcode opcode)
{
	i32 result = ir_emit(ctx, size, opcode, 0, 0);
	return result;
}

static i32
ir_emit_alloca(ir_context *ctx, u32 size)
{
	// TODO: alignment
	i32 result = ir_emit(ctx, 8, IR_ALLOC, size, ctx->stack_size);
	ctx->stack_size += size;
	return result;
}

static i32
ir_call(ir_context *ctx, i32 size, i32 proc, i32 *params, isize param_count)
{
	i32 prev_call = 0;
	while (param_count-- > 0) {
		i32 curr_param = params[param_count];
		i32 curr_call = ir_emit2(ctx, size, IR_CALL, curr_param, prev_call);
		ctx->insts[curr_call].flags |= IR_CONT;
		prev_call = curr_call;
	}

	i32 result = ir_emit2(ctx, size, IR_CALL, proc, prev_call);
	return result;
}

static i32
ir_memcpy(ir_context *ctx, u32 dst, u32 src, u32 size)
{
	i32 args[] = { dst, src, size };
	i32 memcpy = ir_emit1(ctx, 8, IR_BUILTIN, BUILTIN_MEMCPY);
	i32 result = ir_call(ctx, 0, memcpy, args, LENGTH(args));
	return result;
}

static i32
ir_load(ir_context *ctx, u32 addr, type_id type_id)
{
	i32 result = 0;
	ast_node type = get_type(ctx->ast, type_id);
	isize size = get_node_size(ctx->ast, type_id);
	if (is_compound_type(type.kind)) {
		result = ir_emit_alloca(ctx, size);
		ir_memcpy(ctx, result, addr, size);
	} else {
		result = ir_emit1(ctx, size, IR_LOAD, addr);
	}

	return result;
}

static void
ir_store(ir_context *ctx, u32 dst, u32 src, type_id type_id)
{
	ASSERT(src != 0);
	ast_node type = get_type(ctx->ast, type_id);
	isize size = get_node_size(ctx->ast, type_id);
	ASSERT(size > 0);

	if (is_compound_type(type.kind)) {
		ir_memcpy(ctx, dst, src, size);
	} else {
		ir_emit2(ctx, size, IR_STORE, dst, src);
	}
}

static void
ir_mov(ir_context *ctx, i32 dst, i32 src, type_id type_id)
{
	ASSERT(!"TODO: Implement this function using stack variables");
#if 0
	isize size = get_node_size(ctx->ast, type_id);
	ast_node type = get_type(ctx->ast, type_id);
	if (is_compound_type(type.kind)) {
		ir_memcpy(ctx, dst, src, size);
	} else {
		ir_emit2(ctx, size, IR_MOV, dst, src);
	}
#endif
}

static f64
parse_f64(str input)
{
	f64 result = 0;
	(void)input;
	return result;
}

static char
parse_char(str input)
{
	if (input.length > 0) {
		input.at++;
	}

	if (input.length > 0 && input.at[input.length - 1] == '\'') {
		input.length--;
	}

	if (input.length > 1 && input.at[0] == '\\') {
		switch (input.at[1]) {
		case 'f':
			return 0x0C;
		case 'n':
			return 0x0A;
		case 'r':
			return 0x0D;
		case 't':
			return 0x09;
		case 'v':
			return 0x0B;
		case '\'':
			return 0x27;
		case '\\':
			return 0x5C;
		case '0':
			return 0;
		case 'x':
			ASSERT(!"TODO");
			return '\0';
		default:
			ASSERT(!"Invalid escape sequence");
		}
	} else if (input.length > 0) {
		return input.at[0];
	}

	return 0;
}

static linkage
get_linkage(ast_node_flags flags)
{
	linkage result = LINK_DEFAULT;
	if (flags & AST_EXTERN) {
		result = LINK_EXTERN;
	} else if (flags & AST_STATIC) {
		result = LINK_STATIC;
	}

	return result;
}

typedef union {
	i64 i;
	f64 f;
} ir_value;

static ir_value
ir_eval(ir_inst *inst, isize inst_count, ir_value *values, arena stack)
{
	for (isize i = 0; i < inst_count; i++) {
		ir_value result = {0};

		ir_value arg0 = {0};
		isize arg0_size = 0;
		if ((inst[i].flags & IR_USE0) && inst[i].args[0] != 0) {
			arg0 = values[inst[i].args[0]];
			arg0_size = inst[inst[i].args[0]].size;
		} else {
			arg0.i = inst[i].args[0];
		}

		ir_value arg1 = {0};
		isize arg1_size = 0;
		if ((inst[i].flags & IR_USE1) && inst[i].args[1] != 0) {
			arg1 = values[inst[i].args[1]];
			arg1_size = inst[inst[i].args[1]].size;
		} else {
			arg1.i = inst[i].args[1];
		}

		result = values[i];
		switch (inst[i].opcode) {
		case IR_NOP:
		case IR_LABEL:
		case IR_GLOBAL:
			break;
		case IR_ALLOC:
			if (!result.i) {
				result.i = stack.pos;
				alloc(&stack, arg0.i, 1);
			}

			break;
		case IR_CONST:
			result.i = inst[i].args[0];
			break;
		case IR_COPY:
			result = arg0;
			break;
		case IR_LOAD:
		case IR_FLOAD:
			if (0 <= arg0.i && arg0.i < (isize)stack.size) {
				memcpy(&result.i, stack.data + arg0.i, arg0_size);
			}

			break;
		case IR_STORE:
		case IR_FSTORE:
			if (0 <= arg0.i && arg0.i < (isize)stack.size) {
				memcpy(stack.data + arg0.i, &arg1.i, arg1_size);
			}

			break;
		case IR_RET:
		case IR_FRET:
			return arg0;
		case IR_ADD:
			result.i = arg0.i + arg1.i;
			break;
		case IR_SUB:
			result.i = arg0.i - arg1.i;
			break;
		case IR_MUL:
			result.i = arg0.i * arg1.i;
			break;
		case IR_DIV:
			result.i = arg0.i / arg1.i;
			break;
		case IR_MOD:
			result.i = arg0.i % arg1.i;
			break;
		case IR_EQ:
			result.i = arg0.i == arg1.i;
			break;
		case IR_GT:
			result.i = arg0.i > arg1.i;
			break;
		case IR_GE:
			result.i = arg0.i >= arg1.i;
			break;
		case IR_LT:
			result.i = arg0.i < arg1.i;
			break;
		case IR_LE:
			result.i = arg0.i <= arg1.i;
			break;
		case IR_GTU:
			result.i = arg0.i > arg1.i;
			break;
		case IR_GEU:
			result.i = arg0.i >= arg1.i;
			break;
		case IR_LTU:
			result.i = arg0.i < arg1.i;
			break;
		case IR_LEU:
			result.i = arg0.i <= arg1.i;
			break;
		case IR_AND:
			result.i = arg0.i & arg1.i;
			break;
		case IR_NOT:
			result.i = ~arg0.i;
			break;
		case IR_OR:
			result.i = arg0.i | arg1.i;
			break;
		case IR_SHL:
			result.i = arg0.i << arg1.i;
			break;
		case IR_SHR:
			result.i = arg0.i >> arg1.i;
			break;
		case IR_XOR:
			result.i = arg0.i ^ arg1.i;
			break;
		case IR_F2I:
			result.i = arg0.f;
			break;
		case IR_FADD:
			result.f = arg0.f + arg1.f;
			break;
		case IR_FSUB:
			result.f = arg0.f - arg1.f;
			break;
		case IR_FMUL:
			result.f = arg0.f * arg1.f;
			break;
		case IR_FDIV:
			result.f = arg0.f * arg1.f;
			break;
		case IR_FEQ:
			result.i = arg0.f == arg1.f;
			break;
		case IR_FGT:
			result.i = arg0.f > arg1.f;
			break;
		case IR_FGE:
			result.i = arg0.f >= arg1.f;
			break;
		case IR_FLT:
			result.i = arg0.f < arg1.f;
			break;
		case IR_FLE:
			result.i = arg0.f <= arg1.f;
			break;
		case IR_I2F:
			result.f = arg0.i;
			break;
		default:
			ASSERT(!"TODO");
			break;
		}

		values[i] = result;
	}

	ir_value zero = {0};
	return zero;
}

static u32 translate_node(ir_context *ctx, ast_id node_id, b32 is_lvalue);

// TODO: This only works for initializers with a correct set of braces,
// this does not work if there are no braces in the initializer, for example.
static void
translate_initializer(ir_context *ctx, ast_id node_id, u32 result)
{
	ast_pool *pool = ctx->ast;

	ast_node node = get_node(pool, node_id);
	switch (node.kind) {
	case AST_INIT:
		{
			usize offset = 0;

			node_id = node.children;
			while (node_id.value != 0) {
				type_id child_type = get_type_id(pool, node_id);
				isize child_align = get_node_alignment(pool, child_type);
				offset = (offset + child_align - 1) & ~(child_align - 1);

				u32 offset_reg = ir_emit1(ctx, 8, IR_CONST, offset);
				u32 addr = ir_emit2(ctx, 8, IR_ADD, result, offset_reg);
				translate_initializer(ctx, node_id, addr);

				isize child_size = get_node_size(pool, child_type);
				offset += child_size;

				ast_node node = get_node(pool, node_id);
				node_id = node.next;
			}
		} break;
	default:
		{
			type_id node_type = get_type_id(pool, node_id);
			u32 expr = translate_node(ctx, node_id, false);
			ir_store(ctx, result, expr, node_type);
		} break;
	}
}

static u32
translate_node(ir_context *ctx, ast_id node_id, b32 is_lvalue)
{
	ast_pool *pool = ctx->ast;
	arena *perm = ctx->arena;
	u32 result = 0;

	ast_id children[4] = {0};
	get_children(pool, node_id, children, LENGTH(children));

	ast_node node = get_node(pool, node_id);
	switch (node.kind) {
	case AST_INVALID:
		ASSERT(!"Invalid node");
		break;
	case AST_NONE:
		break;
	case AST_BUILTIN:
		{
			ASSERT(!"TODO");
		} break;
	case AST_DECL:
		{
			i32 *symbol_ids = &ctx->symbol_ids[node_id.value];
			ast_node type = get_node(pool, children[0]);
			if (node.flags & AST_TYPEDEF) {
				// NOTE: typedefs are ignored during code generation.
			} else if (is_func(pool, node_id)) {
				ASSERT(*symbol_ids < ctx->program->func_count);
				ir_function *func = &ctx->program->funcs[*symbol_ids];
				func->name = node.token.value;
				func->linkage = get_linkage(node.flags);
				func->insts = NULL;
				func->inst_count = 0;

				if (children[1].value != 0) {
					ctx->insts = calloc(ctx->max_inst_count, sizeof(*ctx->insts));
					ctx->inst_count = 0;
					ctx->label_count = 0;

					ir_emit1(ctx, 0, IR_LABEL, new_label(ctx));

					// NOTE: Emit parameter registers
					isize param_index = 0;
					ast_id return_id = type.children;
					ast_node return_type = get_node(pool, return_id);
					ast_id param_id = return_type.next;
					while (param_id.value != 0) {
						type_id param_type = get_type_id(pool, param_id);
						isize param_size = get_node_size(pool, param_type);

						u32 param_reg = ir_emit(ctx, param_size, IR_PARAM, param_index, param_size);
						u32 param_local = ir_emit_alloca(ctx, param_size);
						ir_store(ctx, param_local, param_reg, param_type);
						ctx->symbol_ids[param_id.value] = param_local;

						ast_node param = get_node(pool, param_id);
						param_id = param.next;
						param_index++;
					}

					// translate the function body
					translate_node(ctx, children[1], false);

					isize insts_size = ctx->inst_count * sizeof(*func->insts);
					func->insts = realloc(ctx->insts, insts_size);
					func->inst_count = ctx->inst_count;
					func->label_count = ctx->label_count;
					ASSERT(func->insts);

					// Reset the context
					ctx->insts = NULL;
					ctx->inst_count = 0;
					ctx->max_inst_count = 0;
				}
			} else if (is_global(pool, node_id)) {
				// global variable (initialized or uninitialized)
				b32 is_initialized = (children[1].value != 0);
				section section = is_initialized ? SECTION_DATA : SECTION_BSS;
				type_id node_type = find_type_id(pool, node_id);

				isize global_id = ctx->symbol_ids[node_id.value];
				global *global = &ctx->program->globals[global_id];
				global->name = node.token.value;
				global->size = get_node_size(pool, node_type);
				global->linkage = get_linkage(node.flags);
				global->section = section;

				if (is_initialized) {
					ctx->insts = calloc(ctx->max_inst_count, sizeof(*ctx->insts));
					ctx->inst_count = 1;
					ctx->label_count = 1;

					i32 result = ir_emit1(ctx, global->size, IR_ALLOC, global->size);
					translate_initializer(ctx, children[1], result);

					arena stack = subarena(perm, global->size);
					ir_value *values = ALLOC(perm, ctx->inst_count, ir_value);
					ir_eval(ctx->insts, ctx->inst_count, values, stack);

					global->data = stack.data;

					free(ctx->insts);
					ctx->insts = NULL;
					ctx->inst_count = 0;
					ctx->max_inst_count = 0;
				}
			} else {
				ast_id decl_id = find_decl(pool, node_id);
				ASSERT(ctx->symbol_ids[decl_id.value] == 0);
				type_id type = find_type_id(pool, decl_id);
				isize size = get_node_size(pool, type);
				result = ir_emit_alloca(ctx, size);

				if (children[1].value != 0) {
					ast_node init_expr = get_node(pool, children[1]);
					if (init_expr.kind == AST_INIT) {
						translate_initializer(ctx, children[1], result);
					} else {
						u32 value = translate_node(ctx, children[1], false);
						ir_store(ctx, result, value, type);
					}
				}

				ctx->symbol_ids[decl_id.value] = result;
			}
		} break;
	case AST_INIT:
		ASSERT(!"Should have been handled by DECL");
		break;
	case AST_STMT_COMPOUND:
		{
			ast_id child = children[0];
			while (child.value != 0) {
				ast_node child_node = get_node(pool, child);
				translate_node(ctx, child, false);
				child = child_node.next;
			}
		} break;
	case AST_EXPR_BINARY:
		{
			type_id type_id = get_type_id(pool, node_id);
			ast_node node_type = get_type(pool, type_id);

			b32 is_assign = false;
			b32 is_float = (node_type.token.kind == TOKEN_FLOAT || node_type.token.kind == TOKEN_DOUBLE);
			b32 is_unsigned = (node_type.flags & AST_UNSIGNED);

			ir_opcode opcode = IR_NOP;
			switch (node.token.kind) {
			case TOKEN_PLUS:
				opcode = is_float ? IR_FADD : IR_ADD;
				break;
			case TOKEN_MINUS:
				opcode = is_float ? IR_FSUB : IR_SUB;
				break;
			case TOKEN_STAR:
				opcode = is_float ? IR_FMUL : IR_MUL;
				break;
			case TOKEN_SLASH:
				opcode = is_float ? IR_FDIV : IR_DIV;
				break;
			case TOKEN_PERCENT:
				opcode = IR_MOD;
				break;
			case TOKEN_PLUS_EQUAL:
				opcode = is_float ? IR_FADD : IR_ADD;
				is_assign = true;
				break;
			case TOKEN_MINUS_EQUAL:
				opcode = is_float ? IR_FSUB : IR_SUB;
				is_assign = true;
				break;
			case TOKEN_STAR_EQUAL:
				opcode = is_float ? IR_FMUL : IR_MUL;
				is_assign = true;
				break;
			case TOKEN_SLASH_EQUAL:
				opcode = is_float ? IR_FDIV : IR_DIV;
				is_assign = true;
				break;
			case TOKEN_PERCENT_EQUAL:
				opcode = IR_MOD;
				is_assign = true;
				break;
			case TOKEN_AMP_EQUAL:
				opcode = IR_AND;
				is_assign = true;
				break;
			case TOKEN_BAR_EQUAL:
				opcode = IR_OR;
				is_assign = true;
				break;
			case TOKEN_CARET_EQUAL:
				opcode = IR_XOR;
				is_assign = true;
				break;
			case TOKEN_EQUAL:
				opcode = is_float ? IR_FCOPY : IR_COPY;
				is_assign = true;
				break;
			case TOKEN_EQUAL_EQUAL:
				opcode = is_float ? IR_FEQ : IR_EQ;
				break;
			case TOKEN_BANG_EQUAL:
				opcode = is_float ? IR_FEQ : IR_EQ;
				break;
			case TOKEN_LESS:
				opcode = is_float ? IR_FLT : is_unsigned ? IR_LTU : IR_LT;
				break;
			case TOKEN_GREATER:
				opcode = is_float ? IR_FGT : is_unsigned ? IR_GTU : IR_GT;
				break;
			case TOKEN_LESS_EQUAL:
				opcode = is_float ? IR_FLE : is_unsigned ? IR_LEU : IR_LE;
				break;
			case TOKEN_GREATER_EQUAL:
				opcode = is_float ? IR_FGE : is_unsigned ? IR_GEU : IR_GE;
				break;
			case TOKEN_RSHIFT:
				opcode = IR_SHR;
				break;
			case TOKEN_LSHIFT:
				opcode = IR_SHL;
				break;
			case TOKEN_AMP:
				opcode = IR_AND;
				break;
			case TOKEN_BAR:
				opcode = IR_OR;
				break;
			case TOKEN_CARET:
				opcode = IR_XOR;
				break;
			case TOKEN_LBRACKET:
				{
					// NOTE: Array access
					u32 lhs_reg = translate_node(ctx, children[0], false);
					u32 rhs_reg = translate_node(ctx, children[1], false);
					isize size = get_node_size(pool, type_id);
					u32 size_reg = ir_emit1(ctx, 8, IR_CONST, size);
					u32 offset = ir_emit2(ctx, 8, IR_MUL, rhs_reg, size_reg);
					result = ir_emit2(ctx, 8, IR_ADD, lhs_reg, offset);
					if (!is_lvalue) {
						result = ir_load(ctx, result, type_id);
					}
				} break;
			case TOKEN_AMP_AMP:
				{
					// NOTE: Logical and operation
					u32 end_label = new_label(ctx);
					u32 zero_label = new_label(ctx);

					u32 addr = ir_emit_alloca(ctx, 4);
					u32 lhs_reg = translate_node(ctx, children[0], false);
					ir_emit2(ctx, 0, IR_JIZ, lhs_reg, zero_label);
					ir_emit1(ctx, 0, IR_LABEL, new_label(ctx));

					u32 rhs_reg = translate_node(ctx, children[1], false);
					ir_emit2(ctx, 0, IR_JIZ, rhs_reg, zero_label);
					ir_emit1(ctx, 0, IR_LABEL, new_label(ctx));

					u32 one = ir_emit1(ctx, 4, IR_CONST, 1);
					ir_emit2(ctx, 0, IR_STORE, addr, one);
					ir_emit1(ctx, 0, IR_JMP, end_label);

					ir_emit1(ctx, 0, IR_LABEL, zero_label);
					u32 zero = ir_emit1(ctx, 4, IR_CONST, 0);
					ir_emit2(ctx, 0, IR_STORE, addr, zero);
					ir_emit1(ctx, 0, IR_LABEL, end_label);
					result = ir_emit1(ctx, 4, IR_LOAD, addr);
				} break;
			case TOKEN_BAR_BAR:
				{
					// NOTE: Logical or operation
					i32 end_label = new_label(ctx);
					i32 one_label = new_label(ctx);

					i32 addr = ir_emit_alloca(ctx, 4);
					i32 lhs_reg = translate_node(ctx, children[0], false);
					ir_emit2(ctx, 0, IR_JNZ, lhs_reg, one_label);
					ir_emit1(ctx, 0, IR_LABEL, new_label(ctx));

					i32 rhs_reg = translate_node(ctx, children[1], false);
					ir_emit2(ctx, 0, IR_JNZ, rhs_reg, one_label);
					ir_emit1(ctx, 0, IR_LABEL, new_label(ctx));

					i32 zero = ir_emit1(ctx, 4, IR_CONST, 0);
					ir_emit2(ctx, 0, IR_STORE, addr, zero);
					ir_emit1(ctx, 0, IR_JMP, end_label);

					ir_emit1(ctx, 0, IR_LABEL, one_label);
					i32 one = ir_emit1(ctx, 4, IR_CONST, 1);
					ir_emit2(ctx, 0, IR_STORE, addr, one);
					ir_emit1(ctx, 0, IR_LABEL, end_label);
					result = ir_emit1(ctx, 4, IR_LOAD, addr);
				} break;
			default:
				ASSERT(!"Invalid operator");
			}

			if (opcode != IR_NOP) {
				i32 lhs_reg = translate_node(ctx, children[0], is_assign);
				i32 rhs_reg = translate_node(ctx, children[1], false);
				isize size = get_node_size(pool, type_id);
				i32 lhs_addr = lhs_reg;

				if (node.token.kind != TOKEN_EQUAL) {
					if (is_assign) {
						lhs_reg = ir_emit1(ctx, size, IR_LOAD, lhs_addr);
					}

					result = ir_emit2(ctx, size, opcode, lhs_reg, rhs_reg);
				} else {
					result = rhs_reg;
				}

				if (is_assign) {
					ir_store(ctx, lhs_addr, result, type_id);
					if (is_lvalue) {
						result = lhs_addr;
					}
				}

				if (node.token.kind == TOKEN_BANG_EQUAL) {
					i32 one = ir_emit1(ctx, size, IR_CONST, 1);
					lhs_reg = ir_emit2(ctx, size, IR_SUB, one, lhs_reg);
				}
			}

			ASSERT(result != 0);
		} break;
	case AST_EXPR_CALL:
		{
			type_id called_type_id = find_type_id(pool, children[0]);
			ast_node called_type = get_type(pool, called_type_id);
			ASSERT(called_type.kind == AST_TYPE_FUNC);

			i32 called_reg = translate_node(ctx, children[0], true);

			// Make struct return value first argument
			isize result_size;
			i32 prev_param = 0;

			type_id return_type_id = {called_type.children.value};
			ast_node return_type = get_type(pool, return_type_id);
			if (is_compound_type(return_type.kind)) {
				isize size = get_node_size(pool, return_type_id);
				i32 param = ir_emit_alloca(ctx, size);
				prev_param = ir_emit2(ctx, 0, IR_CALL, param, 0);
				result_size = 8;
			} else {
				result_size = get_node_size(pool, return_type_id);
			}

			// Load parameters
			ast_id param_id = children[1];
			while (param_id.value != 0) {
				i32 param_value = translate_node(ctx, param_id, false);

				type_id param_type_id = get_type_id(pool, param_id);
				ast_node param_type = get_type(pool, param_type_id);
				isize size = get_node_size(pool, param_type_id);
				if (is_compound_type(param_type.kind)) {
					i32 tmp = ir_emit_alloca(ctx, size);
					ir_memcpy(ctx, tmp, param_value, size);
					param_value = tmp;
				}

				ASSERT(size > 0);
				i32 param_reg = ir_emit(ctx, size, IR_CALL, param_value, prev_param);
				prev_param = param_reg;

				ast_node param_node = get_node(pool, param_id);
				param_id = param_node.next;
			}

			result = ir_emit(ctx, result_size, IR_CALL, called_reg, prev_param);
		} break;
	case AST_EXPR_CAST:
		{
			ASSERT(!"TODO");
#if 0
			type_id cast_type_id = get_type_id(pool, children[0]);
			type_id expr_type_id = get_type_id(pool, children[0]);
			ast_node cast_type = get_type(pool, cast_type_id);
			ast_node expr_type = get_type(pool, expr_type_id);
			if (cast_type->kind != TYPE_VOID) {
				isize cast_size = get_node_size(pool, cast_type_id);
				isize expr_size = get_node_size(pool, expr_type_id);
				result = translate_node(ctx, children[1], false);

				if (expr_type_id.value != cast_type_id.value) {
					if (cast_ir_type == IR_F32 || cast_ir_type == IR_F64
						|| expr_ir_type == IR_F32 || expr_ir_type == IR_F64)
					{
						// TODO: Unsigned conversion
						result = ir_emit1(ctx, cast_ir_type, IR_CAST, result);
					} else if (cast_size < expr_size) {
						result = ir_emit1(ctx, cast_ir_type, IR_TRUNC, result);
					} else if (cast_size > expr_size) {
						if (expr_type->kind & TYPE_UNSIGNED) {
							result = ir_emit1(ctx, cast_ir_type, IR_ZEXT, result);
						} else {
							result = ir_emit1(ctx, cast_ir_type, IR_SEXT, result);
						}
					}
				}

				ASSERT(result != 0);
			}
#endif
		} break;
	case AST_EXPR_COMPOUND:
		{
			type_id node_type = get_type_id(pool, node_id);
			result = ir_emit_alloca(ctx, get_node_size(pool, node_type));
			translate_initializer(ctx, children[1], result);
		} break;
	case AST_EXPR_IDENT:
		{
			ast_id decl_id = find_decl(pool, node_id);
			result = ctx->symbol_ids[decl_id.value];

			type_id type_id = find_type_id(pool, node_id);
			ast_node node_type = get_type(pool, type_id);

			// Global variables must be loaded as globals first
			if (is_func(pool, decl_id)) {
				result = ir_emit1(ctx, 8, IR_FUNC, result);
			} else if (is_global(pool, decl_id)) {
				result = ir_emit1(ctx, 8, IR_GLOBAL, result);
			}

			if (!is_lvalue && !is_compound_type(node_type.kind)) {
				isize size = get_node_size(pool, type_id);
				result = ir_emit1(ctx, size, IR_LOAD, result);
			}

			ASSERT(result != 0);
		} break;
	case AST_EXPR_LITERAL:
		{
			switch (node.token.kind) {
			case TOKEN_LITERAL_INT:
				{
					i64 value = parse_i64(node.token.value);
					result = ir_emit1(ctx, 4, IR_CONST, value);
				} break;
			case TOKEN_LITERAL_CHAR:
				{
					char value = parse_char(node.token.value);
					result = ir_emit1(ctx, 1, IR_CONST, value);
				} break;

			case TOKEN_LITERAL_FLOAT:
				{
					// Convert the string into a floating-point number
					double *value = ALLOC(perm, 1, double);
					*value = strtod(node.token.value.at, NULL);

					// Allocate a new global variable and store the value
					isize global_id = ctx->symbol_ids[node_id.value];
					global *global = &ctx->program->globals[global_id];
					global->linkage = get_linkage(node.flags);
					global->data = value;
					global->size = sizeof(double);
					global->section = SECTION_RODATA;

					// Load the global variable
					result = ir_emit1(ctx, 8, IR_GLOBAL, global_id);
					result = ir_emit1(ctx, 4, IR_FLOAD, result);
				} break;
			case TOKEN_LITERAL_STRING:
				{
					str escaped = node.token.value;
					str unescaped = {0};
					unescaped.at = ALLOC(perm, escaped.length + 1, char);
					for (isize i = 1; i < escaped.length - 1; i++) {
						char c = escaped.at[i];
						if (c == '\\') {
							c = escaped.at[++i];
							switch (c) {
							case '"':
								unescaped.at[unescaped.length++] = c;
								break;
							case 'n':
								unescaped.at[unescaped.length++] = '\n';
								break;
							case 't':
								unescaped.at[unescaped.length++] = '\t';
								break;
							case 'v':
								unescaped.at[unescaped.length++] = '\v';
								break;
							case 'r':
								unescaped.at[unescaped.length++] = '\r';
								break;
							case 'f':
								unescaped.at[unescaped.length++] = '\f';
								break;
							case '\\':
								unescaped.at[unescaped.length++] = '\\';
								break;
							default:
								ASSERT(!"Invalid escape sequence");
							}
						} else {
							unescaped.at[unescaped.length++] = c;
						}
					}

					unescaped.at[unescaped.length++] = '\0';

					// Allocate a new global and store the string
					isize global_id = ctx->symbol_ids[node_id.value];
					global *global = &ctx->program->globals[global_id];
					global->linkage = get_linkage(node.flags);
					global->data = unescaped.at;
					global->size = unescaped.length;
					global->section = SECTION_RODATA;

					// Load the string
					result = ir_emit1(ctx, 8, IR_GLOBAL, global_id);
				} break;
			default:
				ASSERT(!"Invalid literal");
				break;
			}
		} break;
	case AST_EXPR_MEMBER:
	case AST_EXPR_MEMBER_PTR:
		{
			type_id operand_type = get_type_id(pool, children[0]);
			if (node.kind == AST_EXPR_MEMBER_PTR) {
				ast_id base_type = get_type(pool, operand_type).children;
				operand_type.value = base_type.value;
			}

			// TODO: Combine these two functions or add info to this expression.
			ast_id member_id = get_member(pool, operand_type, node.token.value);
			i32 offset = get_member_offset(pool, operand_type, member_id);

			i32 offset_reg = ir_emit1(ctx, 8, IR_CONST, offset);
			b32 base_is_lvalue = (node.kind == AST_EXPR_MEMBER_PTR);
			i32 base_reg = translate_node(ctx, children[0], base_is_lvalue);
			result = ir_emit2(ctx, 8, IR_ADD, base_reg, offset_reg);
			if (!is_lvalue) {
				type_id type_id = get_type_id(pool, node_id);
				ast_node node_type = get_type(pool, type_id);
				if (!is_compound_type(node_type.kind)) {
					isize size = get_node_size(pool, type_id);
					result = ir_emit1(ctx, size, IR_LOAD, result);
				} else {
					isize size = get_node_size(pool, type_id);
					i32 tmp = ir_emit_alloca(ctx, size);
					ir_memcpy(ctx, tmp, result, size);
					result = tmp;
				}
			}

			ASSERT(result != 0);
		} break;
	case AST_EXPR_POSTFIX:
		{
			token_kind operator = node.token.kind;
			switch (operator) {
			case TOKEN_PLUS_PLUS:
			case TOKEN_MINUS_MINUS:
				{
					ir_opcode opcode = IR_ADD;
					if (operator == TOKEN_MINUS_MINUS) {
						opcode = IR_SUB;
					}

					type_id type_id = get_type_id(pool, node_id);
					isize size = get_node_size(pool, type_id);
					i32 addr = translate_node(ctx, children[0], true);
					result = ir_emit1(ctx, size, IR_LOAD, addr);
					i32 one = ir_emit1(ctx, size, IR_CONST, 1);
					i32 value = ir_emit2(ctx, size, opcode, result, one);
					ir_emit2(ctx, size, IR_STORE, addr, value);
					if (is_lvalue) {
						result = addr;
					}
				} break;
			default:
				ASSERT(!"Invalid postfix operator");
			}
		} break;
	case AST_EXPR_SIZEOF:
		{
			isize size = eval_ast(pool, node_id);
			result = ir_emit1(ctx, 8, IR_CONST, size);
		} break;
	case AST_EXPR_TERNARY:
		{
			ASSERT(!"TODO: Implement ternary expressions");
#if 0
			i32 endif_label = new_label(ctx);
			i32 else_label = new_label(ctx);

			ast_id cond = children[0];
			i32 cond_reg = translate_node(ctx, cond, false);

			type_id type_id = get_type_id(pool, node_id);
			ast_node node_type = get_type(pool, type_id);
			if (is_compound_type(node_type.kind)) {
				result = ir_emit_alloca(ctx, get_node_size(pool, type_id));
			} else {
				isize size = get_node_size(pool, type_id);
				result = ir_emit0(ctx, size, IR_VAR);
			}

			ir_emit2(ctx, 0, IR_JIZ, cond_reg, else_label);
			ir_emit1(ctx, 0, IR_LABEL, new_label(ctx));

			ast_id if_branch = children[1];
			i32 if_reg = translate_node(ctx, if_branch, false);
			ir_mov(ctx, result, if_reg, type_id);
			ir_emit1(ctx, 0, IR_JMP, endif_label);

			ir_emit1(ctx, 0, IR_LABEL, else_label);
			ast_id else_branch = children[2];
			i32 else_reg = translate_node(ctx, else_branch, false);
			ir_mov(ctx, result, else_reg, type_id);

			ir_emit1(ctx, 0, IR_LABEL, endif_label);
#endif
		} break;
	case AST_EXPR_UNARY:
		{
			type_id type_id = get_type_id(pool, node_id);
			ast_node node_type = get_type(pool, type_id);
			token_kind operator = node.token.kind;
			switch (operator) {
			case TOKEN_AMP:
				{
					result = translate_node(ctx, children[0], true);
				} break;
			case TOKEN_TILDE:
				{
					result = translate_node(ctx, children[0], false);
					result = ir_emit1(ctx, 0, IR_NOT, result);
				} break;
			case TOKEN_PLUS:
				{
					result = translate_node(ctx, children[0], false);
				} break;
			case TOKEN_MINUS:
				{
					isize size = get_node_size(pool, type_id);
					i32 zero = ir_emit1(ctx, size, IR_CONST, 0);
					result = translate_node(ctx, children[0], false);
					result = ir_emit2(ctx, size, IR_SUB, zero, result);
				} break;
			case TOKEN_BANG:
				{
					isize size = get_node_size(pool, type_id);
					i32 zero = ir_emit1(ctx, size, IR_CONST, 0);
					result = translate_node(ctx, children[0], false);
					result = ir_emit2(ctx, size, IR_EQ, result, zero);
				} break;
			case TOKEN_STAR:
				{
					result = translate_node(ctx, children[0], false);
					if (!is_lvalue && node_type.kind != AST_TYPE_FUNC) {
						result = ir_load(ctx, result, type_id);
					}
				} break;
			case TOKEN_PLUS_PLUS:
			case TOKEN_MINUS_MINUS:
				{
					ir_opcode opcode = IR_ADD;
					if (operator == TOKEN_MINUS_MINUS) {
						opcode = IR_SUB;
					}

					isize size = get_node_size(pool, type_id);
					i32 addr = translate_node(ctx, children[0], true);
					i32 value = ir_emit1(ctx, size, IR_LOAD, addr);
					i32 one = ir_emit1(ctx, size, IR_CONST, 1);
					result = ir_emit2(ctx, size, opcode, value, one);
					ir_emit2(ctx, size, IR_STORE, addr, result);
					if (is_lvalue) {
						result = addr;
					}
				} break;
			default:
				ASSERT(!"Invalid operator");
			}
		} break;
	case AST_STMT_ASM:
		{
			ASSERT(!"TODO");
		} break;
	case AST_STMT_BREAK:
		{
			ir_emit1(ctx, 0, IR_JMP, ctx->break_label);
			ir_emit1(ctx, 0, IR_LABEL, new_label(ctx));
		} break;
	case AST_STMT_CASE:
		{
			i32 label = ctx->symbol_ids[node_id.value];
			ir_emit1(ctx, 0, IR_LABEL, label);
			translate_node(ctx, children[1], false);
		} break;
	case AST_STMT_CONTINUE:
		{
			ir_emit1(ctx, 0, IR_JMP, ctx->continue_label);
			ir_emit1(ctx, 0, IR_LABEL, new_label(ctx));
		} break;
	case AST_STMT_DECL:
		{
			ast_id child_id = node.children;
			while (child_id.value != 0) {
				translate_node(ctx, child_id, false);
				ast_node child = get_node(pool, child_id);
				child_id = child.next;
			}
		} break;
	case AST_STMT_DEFAULT:
		ASSERT(!"Should have been handled by SWITCH");
		break;
	case AST_STMT_DO_WHILE:
		{
			ir_context new_ctx = *ctx;

			new_ctx.break_label = new_label(&new_ctx);
			new_ctx.continue_label = new_label(&new_ctx);
			ast_id cond = children[0];
			ast_id body = children[1];

			ir_emit1(&new_ctx, 0, IR_LABEL, new_ctx.continue_label);
			translate_node(&new_ctx, body, false);

			i32 cond_reg = translate_node(&new_ctx, cond, false);
			ir_emit2(&new_ctx, 0, IR_JNZ, cond_reg, new_ctx.continue_label);
			ir_emit1(&new_ctx, 0, IR_LABEL, new_ctx.break_label);

			ctx->inst_count = new_ctx.inst_count;
			ctx->label_count = new_ctx.label_count;
		} break;
	case AST_STMT_FOR:
		{
			ir_context new_ctx = *ctx;

			new_ctx.break_label = new_label(&new_ctx);
			new_ctx.continue_label = new_label(&new_ctx);
			i32 cond_label = new_label(&new_ctx);

			translate_node(&new_ctx, children[0], false);
			ir_emit1(&new_ctx, 0, IR_LABEL, cond_label);

			ast_node cond = get_node(pool, children[1]);
			if (cond.kind == AST_NONE) {
				ir_emit0(&new_ctx, 0, IR_NOP);
			} else {
				i32 cond_reg = translate_node(&new_ctx, children[1], false);
				ir_emit2(&new_ctx, 0, IR_JIZ, cond_reg, new_ctx.break_label);
				ir_emit1(&new_ctx, 0, IR_LABEL, new_label(&new_ctx));
			}

			translate_node(&new_ctx, children[3], false);
			ir_emit1(&new_ctx, 0, IR_LABEL, new_ctx.continue_label);

			translate_node(&new_ctx, children[2], false);
			ir_emit1(&new_ctx, 0, IR_JMP, cond_label);
			ir_emit1(&new_ctx, 0, IR_LABEL, new_ctx.break_label);

			ctx->inst_count = new_ctx.inst_count;
			ctx->label_count = new_ctx.label_count;
		} break;
	case AST_STMT_GOTO:
	case AST_STMT_LABEL:
		{
			ast_id label_id = node_id;
			ir_opcode opcode = IR_LABEL;

			if (node.kind == AST_STMT_GOTO) {
				label_id = pool->nodes[node_id.value].info.ref;
				opcode = IR_JMP;
			}

			i32 *label = &ctx->symbol_ids[label_id.value];
			if (*label == 0) {
				*label = new_label(ctx);
			}

			ir_emit1(ctx, 0, opcode, *label);
			if (opcode == IR_JMP) {
				ir_emit1(ctx, 0, IR_LABEL, new_label(ctx));
			}

			if (children[0].value != 0) {
				translate_node(ctx, children[0], false);
			}
		} break;
	case AST_STMT_IF:
		{
			i32 endif_label = new_label(ctx);
			i32 else_label = new_label(ctx);

			i32 cond_reg = translate_node(ctx, children[0], false);
			ir_emit2(ctx, 0, IR_JIZ, cond_reg, else_label);
			ir_emit1(ctx, 0, IR_LABEL, new_label(ctx));

			translate_node(ctx, children[1], false);
			ir_emit1(ctx, 0, IR_JMP, endif_label);

			ir_emit1(ctx, 0, IR_LABEL, else_label);
			if (children[2].value != 0) {
				translate_node(ctx, children[2], false);
			}

			ir_emit1(ctx, 0, IR_LABEL, endif_label);
		} break;
	case AST_STMT_RETURN:
		{
			i32 value = 0;
			b32 returns_struct = false;
			isize struct_size = 0;
			isize size = 0;
			if (children[0].value != 0) {
				value = translate_node(ctx, children[0], false);
				type_id value_type_id = find_type_id(pool, children[0]);
				ast_node value_type = get_type(pool, value_type_id);
				returns_struct = is_compound_type(value_type.kind);
				struct_size = get_node_size(pool, value_type_id);
				size = get_node_size(pool, value_type_id);
			}

			if (returns_struct) {
				ir_memcpy(ctx, 1, value, struct_size);
			}

			ir_emit1(ctx, size, IR_RET, value);
		} break;
	case AST_STMT_SWITCH:
		{
			ir_context new_ctx = *ctx;

			i32 switch_reg = translate_node(&new_ctx, children[0], false);
			new_ctx.break_label = new_label(&new_ctx);

			ast_id case_id = pool->nodes[node_id.value].info.ref;
			ast_id default_id = {0};
			while (case_id.value != 0) {
				i32 label = new_label(&new_ctx);
				new_ctx.symbol_ids[case_id.value] = label;

				ast_node case_node = get_node(pool, case_id);
				if (case_node.kind == AST_STMT_CASE) {
					i32 case_reg = translate_node(&new_ctx, case_node.children, false);
					i32 cond_reg = ir_emit2(&new_ctx, 8, IR_EQ, switch_reg, case_reg);
					ir_emit2(&new_ctx, 0, IR_JNZ, cond_reg, label);
					ir_emit1(&new_ctx, 0, IR_LABEL, new_label(&new_ctx));
				} else {
					default_id = case_id;
				}

				case_id = pool->nodes[case_id.value].info.ref;
			}

			if (default_id.value != 0) {
				ast_node default_node = get_node(pool, default_id);
				translate_node(&new_ctx, default_node.children, false);
			}

			ir_emit1(&new_ctx, 0, IR_JMP, new_ctx.break_label);
			translate_node(&new_ctx, children[1], false);
			ir_emit1(&new_ctx, 0, IR_LABEL, new_ctx.break_label);

			ctx->inst_count = new_ctx.inst_count;
			ctx->label_count = new_ctx.label_count;
		} break;
	case AST_STMT_WHILE:
		{
			ir_context new_ctx = *ctx;
			new_ctx.break_label = new_label(&new_ctx);
			new_ctx.continue_label = new_label(&new_ctx);

			ir_emit1(&new_ctx, 0, IR_LABEL, new_ctx.continue_label);
			ast_id cond = children[0];
			i32 cond_reg = translate_node(&new_ctx, cond, false);
			ir_emit2(&new_ctx, 0, IR_JIZ, cond_reg, new_ctx.break_label);
			ir_emit1(&new_ctx, 0, IR_LABEL, new_label(&new_ctx));

			ast_id body = children[1];
			translate_node(&new_ctx, body, false);
			ir_emit1(&new_ctx, 0, IR_JMP, new_ctx.continue_label);
			ir_emit1(&new_ctx, 0, IR_LABEL, new_ctx.break_label);

			ctx->inst_count = new_ctx.inst_count;
			ctx->label_count = new_ctx.label_count;
		} break;
	case AST_ENUMERATOR:
	case AST_TYPE_BASIC:
	case AST_TYPE_ENUM:
	case AST_TYPE_STRUCT:
	case AST_TYPE_UNION:
	case AST_TYPE_FUNC:
	case AST_TYPE_IDENT:
	case AST_TYPE_ARRAY:
	case AST_TYPE_POINTER:
	case AST_TYPE_BITFIELD:
		ASSERT(!"Invalid node");
		break;
	}

	return result;
}

static i32 *
get_ref_count(ir_inst *inst, isize inst_count, arena *perm)
{
	i32 *ref_count = ALLOC(perm, inst_count, i32);

	for (isize i = 0; i < inst_count; i++) {
		ref_count[inst[i].args[0]] += (inst[i].flags & IR_USE0);
		ref_count[inst[i].args[1]] += (inst[i].flags & IR_USE1);
	}

	return ref_count;
}

static ir_program
translate(ast_pool *pool, arena *arena)
{
	// Initialize the program and context
	isize func_count = 1;
	isize global_count = 1;
	i32 *symbol_ids = ALLOC(arena, pool->size, i32);
	for (isize i = 1; i < pool->size; i++) {
		ast_id node_id = {i};
		if (is_func(pool, node_id)) {
			symbol_ids[node_id.value] = func_count++;
		} else if (is_global(pool, node_id)) {
			symbol_ids[node_id.value] = global_count++;
		}
	}

	ir_program program = {0};
	isize max_inst_count = 1024 * 1024;
	program.funcs = ALLOC(arena, func_count, ir_function);
	program.globals = ALLOC(arena, global_count, global);
	program.func_count = func_count;
	program.global_count = global_count;

	ir_context ctx = {0};
	ctx.ast = pool;
	ctx.program = &program;
	ctx.insts = ALLOC(arena, max_inst_count, ir_inst);
	ctx.max_inst_count = max_inst_count;
	ctx.arena = arena;
	ctx.symbol_ids = symbol_ids;

	// Translate all nodes into IR
	ast_id node_id = pool->root;
	while (node_id.value != 0) {
		translate_node(&ctx, node_id, false);
		node_id = get_node(pool, node_id).next;
	}

	// Sort the globals by section and linkage
	isize bucket_end[LINK_COUNT * SECTION_COUNT] = {0};
	global *globals = program.globals;
	for (isize i = 0; i < global_count; i++) {
		linkage linkage = globals[i].linkage;
		section section = globals[i].section;
		isize bucket = section * LINK_COUNT + linkage;
		bucket_end[bucket]++;
	}

	isize bucket_start[LINK_COUNT * SECTION_COUNT] = {0};
	for (isize i = 1; i < LENGTH(bucket_start); i++) {
		bucket_end[i] += bucket_end[i-1];
		bucket_start[i] = bucket_end[i-1];
	}

	for (isize i = 0; i < LENGTH(bucket_start); i++) {
		isize j = bucket_start[i];
		while (j < bucket_end[i]) {
			linkage linkage = globals[j].linkage;
			section section = globals[j].section;
			isize bucket = section * LINK_COUNT + linkage;
			if (i != bucket) {
				isize dest = bucket_start[bucket]++;
				global tmp = globals[dest];
				globals[dest] = globals[j];
				globals[j] = tmp;
			} else {
				j++;
			}
		}
	}

	return program;
}
