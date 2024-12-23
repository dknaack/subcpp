static u32
new_label(ir_context *ctx)
{
	u32 result = ctx->label_count++;
	if (ctx->program->max_label_count < ctx->label_count) {
		ctx->program->max_label_count = ctx->label_count;
	}

	return result;
}

static u32
ir_emit(ir_context *ctx, i32 size, ir_opcode opcode, u32 op0, u32 op1)
{
	ASSERT(size <= 8);
	ASSERT(ctx->program->inst_count + ctx->func_inst_count <= ctx->max_inst_count);
	if (opcode == IR_STORE) ASSERT(op1 != 0);

	u32 result = ctx->func_inst_count++;

	// Ensure that operands are valid
	ir_opcode_info info = get_opcode_info(opcode);
	ASSERT(!is_register_operand(info.op0) || op0 < result);
	ASSERT(!is_register_operand(info.op1) || op1 < result);

	ir_inst *inst = &ctx->func_insts[result];
	inst->opcode = opcode;
	inst->size = size;
	inst->op0 = op0;
	inst->op1 = op1;

	return result;
}

static u32
ir_emit2(ir_context *ctx, i32 size, ir_opcode opcode, u32 op0, u32 op1)
{
	ASSERT(op0 != 0);
	ASSERT(op1 != 0);
	u32 result = ir_emit(ctx, size, opcode, op0, op1);
	return result;
}

static u32
ir_emit1(ir_context *ctx, i32 size, ir_opcode opcode, u32 op0)
{
	ASSERT(opcode == IR_CONST || op0 != 0);
	u32 result = ir_emit(ctx, size, opcode, op0, 0);
	ASSERT(opcode != IR_GLOBAL || op0 < ctx->program->symtab.symbol_count);
	return result;
}

static u32
ir_emit0(ir_context *ctx, i32 size, ir_opcode opcode)
{
	u32 result = ir_emit(ctx, size, opcode, 0, 0);
	if (opcode == IR_VAR) ASSERT(size != 0);
	return result;
}

static u32
ir_emit_alloca(ir_context *ctx, u32 size)
{
	// TODO: alignment
	u32 result = ir_emit(ctx, 8, IR_ALLOC, size, ctx->stack_size);
	ctx->stack_size += size;
	return result;
}

static void
ir_memcpy(ir_context *ctx, u32 dst, u32 src, isize size)
{
	u32 continue_label = new_label(ctx);
	u32 break_label = new_label(ctx);

	u32 counter_reg = ir_emit0(ctx, 8, IR_VAR);
	u32 zero = ir_emit1(ctx, 8, IR_CONST, 0);
	u32 one = ir_emit1(ctx, 8, IR_CONST, 1);
	ir_emit2(ctx, 0, IR_MOV, counter_reg, zero);

	ir_emit1(ctx, 0, IR_LABEL, continue_label);
	u32 size_reg = ir_emit1(ctx, 8, IR_CONST, size);
	u32 comparison = ir_emit2(ctx, 4, IR_LT, counter_reg, size_reg);
	ir_emit2(ctx, 0, IR_JIZ, comparison, break_label);

	u32 dst_ptr = ir_emit2(ctx, 8, IR_ADD, dst, counter_reg);
	u32 src_ptr = ir_emit2(ctx, 8, IR_ADD, src, counter_reg);
	u32 src_byte = ir_emit1(ctx, 1, IR_LOAD, src_ptr);
	ir_emit2(ctx, 1, IR_STORE, dst_ptr, src_byte);

	u32 next = ir_emit2(ctx, 8, IR_ADD, counter_reg, one);
	ir_emit2(ctx, 0, IR_MOV, counter_reg, next);
	ir_emit1(ctx, 0, IR_JMP, continue_label);
	ir_emit1(ctx, 0, IR_LABEL, break_label);
}

static u32
ir_load(ir_context *ctx, u32 addr, type_id type_id)
{
	u32 result = 0;

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
ir_mov(ir_context *ctx, u32 dst, u32 src, type_id type_id)
{
	isize size = get_node_size(ctx->ast, type_id);
	ast_node type = get_type(ctx->ast, type_id);
	if (is_compound_type(type.kind)) {
		ir_memcpy(ctx, dst, src, size);
	} else {
		ir_emit2(ctx, size, IR_MOV, dst, src);
	}
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
	char result = 0;
	(void)input;
	return result;
}

static u32 translate_node(ir_context *ctx, ast_id node_id, b32 is_lvalue);

// TODO: This only works for initializers with a correct set of braces,
// this does not work if there are no braces in the initializer, for example.
static void
translate_initializer(ir_context *ctx, ast_id node_id, u32 result)
{
	semantic_info *info = ctx->info;
	ast_pool *pool = ctx->ast;

	ast_node node = get_node(pool, node_id);
	switch (node.kind) {
	case AST_INIT:
		{
			usize offset = 0;

			node_id = node.children;
			while (node_id.value != 0) {
				type_id child_type = get_type_id(info, node_id);
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
			type_id node_type = get_type_id(info, node_id);
			u32 expr = translate_node(ctx, node_id, false);
			ir_store(ctx, result, expr, node_type);
		} break;
	}
}

static u32
translate_node(ir_context *ctx, ast_id node_id, b32 is_lvalue)
{
	ast_pool *pool = ctx->ast;
	semantic_info *info = ctx->info;
	symbol_table *symtab = &ctx->program->symtab;
	arena *arena = ctx->arena;
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
			if (ctx->node_addr[node_id.value] == 0) {
				type_id type = get_type_id(info, node_id);
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

				ctx->node_addr[node_id.value] = result;
			} else {
				result = ctx->node_addr[node_id.value];
			}
		} break;
	case AST_EXTERN_DEF:
		{
			u32 *node_addr = &ctx->node_addr[node_id.value];
			ast_node type = get_node(pool, children[0]);
			if (*node_addr != 0) {
				result = ir_emit1(ctx, 8, IR_GLOBAL, *node_addr);
				ASSERT(result != 0);
			} else if (node.flags & AST_TYPEDEF) {
				// NOTE: typedefs are ignored during code generation.
			} else if (type.kind == AST_TYPE_FUNC) {
				// Create a new symbol for the function
				symbol *sym = new_symbol(ctx, SECTION_TEXT);
				symbol_id sym_id = get_symbol_id(symtab, sym);
				*node_addr = sym_id.value;
				sym->linkage = get_linkage(node.flags);
				sym->name = node.token.value;

				ir_function *func = &ctx->program->funcs[*node_addr];
				ASSERT(*node_addr < ctx->program->func_count);
				func->sym_id = sym_id;

				if (children[1].value != 0) {
					// Create a new function
					func->inst_index = ctx->program->inst_count;

					// Reset the instructions, registers and labels
					ctx->func_insts = ctx->program->insts + func->inst_index;
					ctx->func_inst_count = 1;
					ctx->label_count = 1;

					// NOTE: Emit parameter registers
					ast_id return_id = type.children;
					ast_node return_type = get_node(pool, return_id);
					ast_id param_id = return_type.next;
					while (param_id.value != 0) {
						type_id param_type = get_type_id(info, param_id);
						isize param_size = get_node_size(pool, param_type);
						isize param_index = func->param_count++;

						u32 param_reg = ir_emit(ctx, param_size, IR_PARAM, param_index, param_size);
						u32 param_local = ir_emit_alloca(ctx, param_size);
						ir_store(ctx, param_local, param_reg, param_type);
						ctx->node_addr[param_id.value] = param_local;

						ast_node param = get_node(pool, param_id);
						param_id = param.next;
					}

					// translate the function body
					translate_node(ctx, children[1], false);

					// record the maximum number of instructions/registers
					if (ctx->program->max_reg_count < ctx->func_inst_count) {
						ctx->program->max_reg_count = ctx->func_inst_count;
					}

					func->inst_count = ctx->func_inst_count;
					ctx->program->inst_count += ctx->func_inst_count;
					sym->size = func->inst_count * sizeof(ir_inst);
					sym->data = ctx->program->insts + func->inst_index;
				}
			} else {
				// global variable (initialized or uninitialized)
				b32 is_initialized = (children[1].value != 0);
				section section = is_initialized ? SECTION_DATA : SECTION_BSS;

				symbol *sym = new_symbol(ctx, section);
				sym->linkage = get_linkage(node.flags);
				sym->name = node.token.value;
				type_id node_type = get_type_id(info, node_id);
				sym->size = get_node_size(pool, node_type);
				*node_addr = get_symbol_id(symtab, sym).value;

				if (is_initialized) {
					ASSERT(!"TODO: Translate the global variable into bytes");
				}
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
			token_kind operator = node.token.kind;
			ir_opcode opcode = IR_NOP;

			type_id type_id = get_type_id(info, node_id);
			ast_node node_type = get_type(pool, type_id);

			b32 is_float = (node.token.kind == TOKEN_FLOAT || node_type.token.kind == TOKEN_DOUBLE);
			if (is_float) {
				switch (operator) {
				case TOKEN_PLUS:          opcode = IR_FADD;   break;
				case TOKEN_MINUS:         opcode = IR_FSUB;   break;
				case TOKEN_STAR:          opcode = IR_FMUL;   break;
				case TOKEN_SLASH:         opcode = IR_FDIV;   break;
				case TOKEN_PLUS_EQUAL:    opcode = IR_FSTORE; break;
				case TOKEN_MINUS_EQUAL:   opcode = IR_FSTORE; break;
				case TOKEN_STAR_EQUAL:    opcode = IR_FSTORE; break;
				case TOKEN_SLASH_EQUAL:   opcode = IR_FSTORE; break;
				case TOKEN_EQUAL:         opcode = IR_FSTORE; break;
				case TOKEN_EQUAL_EQUAL:   opcode = IR_FEQ;   break;
				case TOKEN_BANG_EQUAL:    opcode = IR_FEQ;   break;
				case TOKEN_LESS:          opcode = IR_FLT;    break;
				case TOKEN_GREATER:       opcode = IR_FGT;    break;
				case TOKEN_LESS_EQUAL:    opcode = IR_FLE;   break;
				case TOKEN_GREATER_EQUAL: opcode = IR_FGE;   break;
				default:                  ASSERT(!"Invalid operator");
				}
			} else {
				switch (operator) {
				case TOKEN_PLUS:          opcode = IR_ADD;   break;
				case TOKEN_MINUS:         opcode = IR_SUB;   break;
				case TOKEN_STAR:          opcode = IR_MUL;   break;
				case TOKEN_SLASH:         opcode = IR_DIV;   break;
				case TOKEN_PERCENT:       opcode = IR_MOD;   break;
				case TOKEN_PLUS_EQUAL:    opcode = IR_STORE; break;
				case TOKEN_MINUS_EQUAL:   opcode = IR_STORE; break;
				case TOKEN_STAR_EQUAL:    opcode = IR_STORE; break;
				case TOKEN_SLASH_EQUAL:   opcode = IR_STORE; break;
				case TOKEN_PERCENT_EQUAL: opcode = IR_STORE; break;
				case TOKEN_AMP_EQUAL:     opcode = IR_STORE; break;
				case TOKEN_BAR_EQUAL:     opcode = IR_STORE; break;
				case TOKEN_CARET_EQUAL:   opcode = IR_STORE; break;
				case TOKEN_EQUAL:         opcode = IR_STORE; break;
				case TOKEN_EQUAL_EQUAL:   opcode = IR_EQ;   break;
				case TOKEN_BANG_EQUAL:    opcode = IR_EQ;   break;
				case TOKEN_LESS:          opcode = IR_LT;    break;
				case TOKEN_GREATER:       opcode = IR_GT;    break;
				case TOKEN_RSHIFT:        opcode = IR_SHR;   break;
				case TOKEN_LSHIFT:        opcode = IR_SHL;   break;
				case TOKEN_LESS_EQUAL:    opcode = IR_LE;   break;
				case TOKEN_GREATER_EQUAL: opcode = IR_GE;   break;
				case TOKEN_LBRACKET:      opcode = IR_ADD;   break;
				case TOKEN_AMP:           opcode = IR_AND;   break;
				case TOKEN_BAR:           opcode = IR_OR;    break;
				case TOKEN_CARET:         opcode = IR_XOR;   break;
				case TOKEN_AMP_AMP:       opcode = IR_JIZ;   break;
				case TOKEN_BAR_BAR:       opcode = IR_JIZ;   break;
				default:                  ASSERT(!"Invalid operator");
				}
			}

			if (is_comparison_opcode(opcode) && (node_type.flags & AST_UNSIGNED)) {
				// TODO: Use switch instead of addition?
				opcode += IR_LTU - IR_LT;
			}

			if (operator == TOKEN_AMP_AMP) {
				// NOTE: Logical and operation
				u32 end_label = new_label(ctx);
				u32 zero_label = new_label(ctx);

				result = ir_emit0(ctx, 4, IR_VAR);
				u32 lhs_reg = translate_node(ctx, children[0], false);
				ir_emit2(ctx, 0, IR_JIZ, lhs_reg, zero_label);

				u32 rhs_reg = translate_node(ctx, children[1], false);
				ir_emit2(ctx, 0, IR_JIZ, rhs_reg, zero_label);

				u32 one = ir_emit1(ctx, 4, IR_CONST, 1);
				ir_emit2(ctx, 0, IR_MOV, result, one);
				ir_emit1(ctx, 0, IR_JMP, end_label);

				ir_emit1(ctx, 0, IR_LABEL, zero_label);
				u32 zero = ir_emit1(ctx, 4, IR_CONST, 0);
				ir_emit2(ctx, 0, IR_MOV, result, zero);
				ir_emit1(ctx, 0, IR_LABEL, end_label);
			} else if (operator == TOKEN_BAR_BAR) {
				// NOTE: Logical or operation
				u32 end_label = new_label(ctx);
				u32 one_label = new_label(ctx);

				result = ir_emit0(ctx, 4, IR_VAR);
				u32 lhs_reg = translate_node(ctx, children[0], false);
				ir_emit2(ctx, 0, IR_JNZ, lhs_reg, one_label);

				u32 rhs_reg = translate_node(ctx, children[1], false);
				ir_emit2(ctx, 0, IR_JNZ, rhs_reg, one_label);

				u32 zero = ir_emit1(ctx, 4, IR_CONST, 0);
				ir_emit2(ctx, 0, IR_MOV, result, zero);
				ir_emit1(ctx, 0, IR_JMP, end_label);

				ir_emit1(ctx, 0, IR_LABEL, one_label);
				u32 one = ir_emit1(ctx, 4, IR_CONST, 1);
				ir_emit2(ctx, 0, IR_MOV, result, one);
				ir_emit1(ctx, 0, IR_LABEL, end_label);
			} else if (opcode == IR_STORE) {
				// NOTE: Assignment operation
				switch (operator) {
				case TOKEN_PLUS_EQUAL:    opcode = IR_ADD; break;
				case TOKEN_MINUS_EQUAL:   opcode = IR_SUB; break;
				case TOKEN_STAR_EQUAL:    opcode = IR_MUL; break;
				case TOKEN_SLASH_EQUAL:   opcode = IR_DIV; break;
				case TOKEN_PERCENT_EQUAL: opcode = IR_MOD; break;
				case TOKEN_AMP_EQUAL:     opcode = IR_AND; break;
				case TOKEN_BAR_EQUAL:     opcode = IR_OR;  break;
				case TOKEN_CARET_EQUAL:   opcode = IR_XOR; break;
				case TOKEN_EQUAL:         opcode = IR_NOP; break;
				default:                  ASSERT(!"Invalid operator");
				}

				u32 lhs_reg = translate_node(ctx, children[0], true);
				u32 rhs_reg = translate_node(ctx, children[1], false);
				if (operator != TOKEN_EQUAL) {
					isize size = get_node_size(pool, type_id);
					u32 value = ir_emit1(ctx, size, IR_LOAD, lhs_reg);
					result = ir_emit2(ctx, size, opcode, value, rhs_reg);
					ir_emit2(ctx, size, IR_STORE, lhs_reg, result);
				} else {
					ir_store(ctx, lhs_reg, rhs_reg, type_id);
					if (!is_lvalue) {
						result = ir_load(ctx, lhs_reg, type_id);
					} else {
						result = lhs_reg;
					}
				}

				ASSERT(result != 0);
			} else if (operator == TOKEN_LBRACKET) {
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
			} else {
				u32 lhs_reg = translate_node(ctx, children[0], false);
				u32 rhs_reg = translate_node(ctx, children[1], false);
				isize size = get_node_size(pool, type_id);

				result = ir_emit2(ctx, size, opcode, lhs_reg, rhs_reg);
				if (operator == TOKEN_BANG_EQUAL) {
					u32 one = ir_emit1(ctx, size, IR_CONST, 1);
					lhs_reg = ir_emit2(ctx, size, IR_SUB, one, lhs_reg);
				}
			}

			ASSERT(result != 0);
		} break;
	case AST_EXPR_CALL:
		{
			type_id called_type_id = get_type_id(info, children[0]);
			ast_node called_type = get_type(pool, called_type_id);
			ASSERT(called_type.kind == AST_TYPE_FUNC);

			u32 called_reg = translate_node(ctx, children[0], true);

			// Make struct return value first argument
			isize result_size;
			u32 prev_param = 0;

			type_id return_type_id = {called_type.children.value};
			ast_node return_type = get_type(pool, return_type_id);
			if (is_compound_type(return_type.kind)) {
				isize size = get_node_size(pool, return_type_id);
				u32 param = ir_emit_alloca(ctx, size);
				prev_param = ir_emit2(ctx, 0, IR_CALL, param, 0);
				result_size = 8;
			} else {
				result_size = get_node_size(pool, return_type_id);
			}

			// Load parameters
			ast_id param_id = children[1];
			while (param_id.value != 0) {
				u32 param_value = translate_node(ctx, param_id, false);

				type_id param_type_id = get_type_id(info, param_id);
				ast_node param_type = get_type(pool, param_type_id);
				isize size = get_node_size(pool, param_type_id);
				if (is_compound_type(param_type.kind)) {
					u32 tmp = ir_emit_alloca(ctx, size);
					ir_memcpy(ctx, tmp, param_value, size);
					param_value = tmp;
				}

				ASSERT(size > 0);
				u32 param_reg = ir_emit(ctx, size, IR_CALL, param_value, prev_param);
				prev_param = param_reg;

				ast_node param_node = get_node(pool, param_id);
				param_id = param_node.next;
			}

			type_id type_id = get_type_id(info, node_id);
			ast_node node_type = get_type(pool, type_id);

			b32 is_void = (node_type.kind == AST_TYPE_BASIC && node_type.token.kind == TOKEN_VOID);
			if (is_void) {
				// Call the function
				result = ir_emit(ctx, result_size, IR_CALL, called_reg, prev_param);
			} else {
				ASSERT(!"TODO");
			}
		} break;
	case AST_EXPR_CAST:
		{
			ASSERT(!"TODO");
#if 0
			type_id cast_type_id = get_type_id(info, children[0]);
			type_id expr_type_id = get_type_id(info, children[0]);
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
			type_id node_type = get_type_id(info, node_id);
			result = ir_emit_alloca(ctx, get_node_size(pool, node_type));
			translate_initializer(ctx, children[1], result);
		} break;
	case AST_EXPR_IDENT:
		{
			// Global variables must be loaded as globals first
			ast_id decl_id = ctx->info->at[node_id.value].ref;
			result = translate_node(ctx, decl_id, true);

			type_id type_id = get_type_id(info, node_id);
			ast_node node_type = get_type(pool, type_id);
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
					double *value = ALLOC(arena, 1, double);
					*value = strtod(node.token.value.at, NULL);

					// Allocate a new global variable and store the value
					symbol *sym = new_symbol(ctx, SECTION_RODATA);
					sym->linkage = get_linkage(node.flags);
					sym->data = value;
					sym->size = sizeof(double);

					// Load the global variable
					symbol_id sym_id = get_symbol_id(symtab, sym);
					result = ir_emit1(ctx, 8, IR_GLOBAL, sym_id.value);
					result = ir_emit1(ctx, 4, IR_FLOAD, result);
				} break;
			case TOKEN_LITERAL_STRING:
				{
					str escaped = node.token.value;
					str unescaped = {0};
					unescaped.at = ALLOC(arena, escaped.length + 1, char);
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

					// Allocate a new symbol and store the string
					symbol *sym = new_symbol(ctx, SECTION_RODATA);
					sym->linkage = get_linkage(node.flags);
					sym->data = unescaped.at;
					sym->size = unescaped.length;

					// Load the string
					symbol_id sym_id = get_symbol_id(symtab, sym);
					result = ir_emit1(ctx, 8, IR_GLOBAL, sym_id.value);
				} break;
			default:
				ASSERT(!"Invalid literal");
				break;
			}
		} break;
	case AST_EXPR_MEMBER:
	case AST_EXPR_MEMBER_PTR:
		{
			type_id operand_type = get_type_id(info, children[0]);
			if (node.kind == AST_EXPR_MEMBER_PTR) {
				ast_id base_type = get_type(pool, operand_type).children;
				operand_type.value = base_type.value;
			}

			// TODO: Combine these two functions or add info to this expression.
			ast_id member_id = get_member(pool, operand_type, node.token.value);
			u32 offset = get_member_offset(pool, operand_type, member_id);

			u32 offset_reg = ir_emit1(ctx, 8, IR_CONST, offset);
			b32 base_is_lvalue = (node.kind == AST_EXPR_MEMBER_PTR);
			u32 base_reg = translate_node(ctx, children[0], base_is_lvalue);
			result = ir_emit2(ctx, 8, IR_ADD, base_reg, offset_reg);
			if (!is_lvalue) {
				type_id type_id = get_type_id(info, node_id);
				ast_node node_type = get_type(pool, type_id);
				if (!is_compound_type(node_type.kind)) {
					isize size = get_node_size(pool, type_id);
					result = ir_emit1(ctx, size, IR_LOAD, result);
				} else {
					isize size = get_node_size(pool, type_id);
					u32 tmp = ir_emit_alloca(ctx, size);
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

					type_id type_id = get_type_id(info, node_id);
					isize size = get_node_size(pool, type_id);
					u32 addr = translate_node(ctx, children[0], true);
					result = ir_emit1(ctx, size, IR_LOAD, addr);
					u32 one = ir_emit1(ctx, size, IR_CONST, 1);
					u32 value = ir_emit2(ctx, size, opcode, result, one);
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
			semantic_context sem_ctx = {0};
			sem_ctx.ast = pool;

			isize size = eval_ast(sem_ctx, node_id);
			result = ir_emit1(ctx, 8, IR_CONST, size);
		} break;
	case AST_EXPR_TERNARY:
		{
			u32 endif_label = new_label(ctx);
			u32 else_label = new_label(ctx);

			ast_id cond = children[0];
			u32 cond_reg = translate_node(ctx, cond, false);

			type_id type_id = get_type_id(info, node_id);
			ast_node node_type = get_type(pool, type_id);
			if (is_compound_type(node_type.kind)) {
				result = ir_emit_alloca(ctx, get_node_size(pool, type_id));
			} else {
				isize size = get_node_size(pool, type_id);
				result = ir_emit0(ctx, size, IR_VAR);
			}

			ir_emit2(ctx, 0, IR_JIZ, cond_reg, else_label);

			ast_id if_branch = children[1];
			u32 if_reg = translate_node(ctx, if_branch, false);
			ir_mov(ctx, result, if_reg, type_id);
			ir_emit1(ctx, 0, IR_JMP, endif_label);

			ir_emit1(ctx, 0, IR_LABEL, else_label);
			ast_id else_branch = children[2];
			u32 else_reg = translate_node(ctx, else_branch, false);
			ir_mov(ctx, result, else_reg, type_id);

			ir_emit1(ctx, 0, IR_LABEL, endif_label);
		} break;
	case AST_EXPR_UNARY:
		{
			type_id type_id = get_type_id(info, node_id);
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
					u32 zero = ir_emit1(ctx, size, IR_CONST, 0);
					result = translate_node(ctx, children[0], false);
					result = ir_emit2(ctx, size, IR_SUB, zero, result);
				} break;
			case TOKEN_BANG:
				{
					isize size = get_node_size(pool, type_id);
					u32 zero = ir_emit1(ctx, size, IR_CONST, 0);
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
					u32 addr = translate_node(ctx, children[0], true);
					u32 value = ir_emit1(ctx, size, IR_LOAD, addr);
					u32 one = ir_emit1(ctx, size, IR_CONST, 1);
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
		} break;
	case AST_STMT_CASE:
		{
			u32 label = ctx->node_addr[node_id.value];
			ir_emit1(ctx, 0, IR_LABEL, label);
			translate_node(ctx, children[1], false);
		} break;
	case AST_STMT_CONTINUE:
		{
			ir_emit1(ctx, 0, IR_JMP, ctx->continue_label);
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

			u32 cond_reg = translate_node(&new_ctx, cond, false);
			ir_emit2(&new_ctx, 0, IR_JNZ, cond_reg, new_ctx.continue_label);
			ir_emit1(&new_ctx, 0, IR_LABEL, new_ctx.break_label);

			ctx->func_inst_count = new_ctx.func_inst_count;
			ctx->label_count = new_ctx.label_count;
			ctx->reg_count = new_ctx.reg_count;
		} break;
	case AST_STMT_FOR:
		{
			ir_context new_ctx = *ctx;

			new_ctx.break_label = new_label(&new_ctx);
			new_ctx.continue_label = new_label(&new_ctx);
			u32 cond_label = new_label(&new_ctx);

			translate_node(&new_ctx, children[0], false);
			ir_emit1(&new_ctx, 0, IR_LABEL, cond_label);

			ast_node cond = get_node(pool, children[1]);
			if (cond.kind == AST_NONE) {
				ir_emit0(&new_ctx, 0, IR_NOP);
			} else {
				u32 cond_reg = translate_node(&new_ctx, children[1], false);
				ir_emit2(&new_ctx, 0, IR_JIZ, cond_reg, new_ctx.break_label);
			}

			translate_node(&new_ctx, children[3], false);
			ir_emit1(&new_ctx, 0, IR_LABEL, new_ctx.continue_label);

			translate_node(&new_ctx, children[2], false);
			ir_emit1(&new_ctx, 0, IR_JMP, cond_label);
			ir_emit1(&new_ctx, 0, IR_LABEL, new_ctx.break_label);

			ctx->func_inst_count = new_ctx.func_inst_count;
			ctx->label_count = new_ctx.label_count;
			ctx->reg_count = new_ctx.reg_count;
		} break;
	case AST_STMT_GOTO:
	case AST_STMT_LABEL:
		{
			ast_id label_id = node_id;
			ir_opcode opcode = IR_LABEL;

			if (node.kind == AST_STMT_GOTO) {
				label_id = ctx->info->at[node_id.value].ref;
				opcode = IR_JMP;
			}

			u32 *label = &ctx->node_addr[label_id.value];
			if (*label == 0) {
				*label = new_label(ctx);
			}

			ir_emit1(ctx, 0, opcode, *label);
			if (children[0].value != 0) {
				translate_node(ctx, children[0], false);
			}
		} break;
	case AST_STMT_IF:
		{
			u32 endif_label = new_label(ctx);
			u32 else_label = new_label(ctx);

			u32 cond_reg = translate_node(ctx, children[0], false);
			ir_emit2(ctx, 0, IR_JIZ, cond_reg, else_label);

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
			u32 value = 0;
			b32 returns_struct = false;
			isize struct_size = 0;
			isize size = 0;
			if (children[0].value != 0) {
				value = translate_node(ctx, children[0], false);
				type_id value_type_id = get_type_id(info, children[0]);
				ast_node value_type = get_type(pool, value_type_id);
				returns_struct = is_compound_type(value_type.kind);
				struct_size = get_node_size(pool, value_type_id);
				size = get_node_size(pool, value_type_id);
			}

			if (returns_struct) {
				ir_memcpy(ctx, 1, value, struct_size);
			}

			ir_emit1(ctx, size, IR_RET, value);
			// NOTE: For dead code elimination
			ir_emit1(ctx, 0, IR_LABEL, new_label(ctx));
		} break;
	case AST_STMT_SWITCH:
		{
			ir_context new_ctx = *ctx;

			u32 switch_reg = translate_node(&new_ctx, children[0], false);
			new_ctx.break_label = new_label(&new_ctx);

			ast_id case_id = ctx->info->at[node_id.value].ref;
			ast_id default_id = {0};
			while (case_id.value != 0) {
				u32 label = new_label(&new_ctx);
				new_ctx.node_addr[case_id.value] = label;

				ast_node case_node = get_node(pool, case_id);
				if (case_node.kind == AST_STMT_CASE) {
					u32 case_reg = translate_node(&new_ctx, case_node.children, false);
					u32 cond_reg = ir_emit2(&new_ctx, 8, IR_EQ, switch_reg, case_reg);
					ir_emit2(&new_ctx, 0, IR_JNZ, cond_reg, label);
				} else {
					default_id = case_id;
				}

				case_id = new_ctx.info->at[case_id.value].ref;
			}

			if (default_id.value != 0) {
				ast_node default_node = get_node(pool, default_id);
				translate_node(&new_ctx, default_node.children, false);
			}

			ir_emit1(&new_ctx, 0, IR_JMP, new_ctx.break_label);
			translate_node(&new_ctx, children[1], false);
			ir_emit1(&new_ctx, 0, IR_LABEL, new_ctx.break_label);

			ctx->func_inst_count = new_ctx.func_inst_count;
			ctx->label_count = new_ctx.label_count;
			ctx->reg_count = new_ctx.reg_count;
		} break;
	case AST_STMT_WHILE:
		{
			ir_context new_ctx = *ctx;
			new_ctx.break_label = new_label(&new_ctx);
			new_ctx.continue_label = new_label(&new_ctx);

			ir_emit1(&new_ctx, 0, IR_LABEL, new_ctx.continue_label);
			ast_id cond = children[0];
			u32 cond_reg = translate_node(&new_ctx, cond, false);
			ir_emit2(&new_ctx, 0, IR_JIZ, cond_reg, new_ctx.break_label);

			ast_id body = children[1];
			translate_node(&new_ctx, body, false);
			ir_emit1(&new_ctx, 0, IR_JMP, new_ctx.continue_label);
			ir_emit1(&new_ctx, 0, IR_LABEL, new_ctx.break_label);

			ctx->func_inst_count = new_ctx.func_inst_count;
			ctx->label_count = new_ctx.label_count;
			ctx->reg_count = new_ctx.reg_count;
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
		ir_opcode_info info = get_opcode_info(inst[i].opcode);
		if (is_register_operand(info.op0)) {
			ref_count[inst[i].op0]++;
		}

		if (is_register_operand(info.op1)) {
			ref_count[inst[i].op1]++;
		}
	}

	return ref_count;
}

static ir_program
translate(ast_pool *pool, semantic_info *info, arena *arena)
{
	// Count the total number of symbols
	isize symbol_count = 1;
	for (isize i = 1; i < pool->size; i++) {
		ast_node node = pool->nodes[i];
		token_kind token = node.token.kind;
		if (node.kind == AST_EXTERN_DEF || (node.kind == AST_EXPR_LITERAL
			&& (token == TOKEN_LITERAL_FLOAT || token == TOKEN_LITERAL_STRING)))
		{
			symbol_count++;
		}
	}

	// initialize the program
	ir_program program = {0};
	isize max_inst_count = 1024 * 1024;
	program.insts = ALLOC(arena, max_inst_count, ir_inst);
	program.funcs = ALLOC(arena, symbol_count, ir_function);
	program.func_count = symbol_count;

	// initialize the symbol table
	program.symtab.symbols = ALLOC(arena, symbol_count, symbol);
	program.symtab.max_symbol_count = symbol_count;
	program.symtab.symbol_count = 1; // Reserve the first symbol as NULL symbol.

	// initialize the context
	ir_context ctx = {0};
	ctx.ast = pool;
	ctx.program = &program;
	ctx.max_inst_count = max_inst_count;
	ctx.arena = arena;
	ctx.info = info;
	ctx.node_addr = ALLOC(arena, pool->size, u32);
	for (i32 i = 0; i < SECTION_COUNT; i++) {
		ctx.section_tail[i] = &program.symtab.section[i];
	}

	// Translate all nodes into IR
	ast_id node_id = pool->root;
	while (node_id.value != 0) {
		translate_node(&ctx, node_id, false);
		node_id = get_node(pool, node_id).next;
	}

	// TODO: Remove this
	// NOTE: Propagate sizes through the instructions
#if 0
	for (isize i = 0; i < program.func_count; i++) {
		ir_function *func = &program.funcs[i];
		ir_inst *inst = program.insts + func->inst_index;
		for (isize j = 0; j < func->inst_count; j++) {
			if (inst[j].type != 0 || inst[j].opcode == IR_CAST
				|| inst[j].opcode == IR_CASTU)
			{
				continue;
			}

			u32 op0 = inst[j].op0;
			u32 op1 = inst[j].op1;

			ir_opcode_info info = get_opcode_info(inst[j].opcode);
			// If a store instruction has the type from the first operand, then
			// it will always be a pointer type, even if we're dealing with a
			// float. Therefore, we propagate the second operand first.
			if (is_register_operand(info.op1)) {
				inst[j].size = inst[op1].size;
			} else if (is_register_operand(info.op0)) {
				inst[j].size = inst[op0].size;
			}
		}
	}
#endif

	return program;
}
