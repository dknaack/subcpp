static void
x86_emit_token(stream *out, mach_token token, symbol_table *symtab)
{
	x86_register reg;

	b32 print_size = (token.kind == MACH_SPILL
		|| (token.flags & MACH_INDIRECT));
	if (print_size) {
		switch (token.size) {
		case 1:
			stream_print(out, "byte");
			break;
		case 2:
			stream_print(out, "word");
			break;
		case 4:
			stream_print(out, "dword");
			break;
		case 8:
		case 0:
			stream_print(out, "qword");
			break;
		default:
			ASSERT(!"Invalid size");
		}
	}

	switch (token.kind) {
	case MACH_GLOBAL:
		{
			ASSERT(token.value < symtab->symbol_count);
			symbol *sym = &symtab->symbols[token.value];
			if (sym->name.length > 0) {
				stream_prints(out, sym->name);
			} else {
				stream_print(out, "L#");
				stream_printu(out, token.value);
			}
		} break;
	case MACH_SPILL:
		stream_print(out, "[rsp+");
		stream_printu(out, token.value);
		stream_print(out, "]");
		break;
	case MACH_LABEL:
		stream_print(out, ".L");
		stream_printu(out, token.value);
		break;
	case MACH_MREG:
		if (token.flags & MACH_INDIRECT) {
			stream_print(out, "[");
			token.size = 8;
		}

		reg = (x86_register)token.value;
		stream_print(out, x86_get_register_name(reg, token.size));

		if (token.flags & MACH_INDIRECT) {
			stream_print(out, "]");
		}
		break;
	case MACH_CONST:
		stream_printu(out, token.value);
		break;
	case MACH_FLOAT:
		stream_print(out, "[float#");
		stream_printu(out, token.value);
		stream_print(out, "]");
		break;
	case MACH_FUNC:
		{
			ASSERT(!"TODO");
		} break;
	case MACH_VREG:
		stream_print(out, "v");
		stream_printu(out, token.value);
		//ASSERT(!"Cannot use virtual register during code generation");
		break;
	default:
		ASSERT(false);
		stream_print(out, "(invalid token)");
	}
}

static void
x86_generate(stream *out, mach_program p, symbol_table *symtab, regalloc_info *info)
{
	for (isize j = 0; j < SECTION_COUNT; j++) {
		if (j == SECTION_TEXT) {
			stream_print(out, "section .text\n");
		} else if (j == SECTION_DATA) {
			stream_print(out, "section .data\n");
		} else if (j == SECTION_RODATA) {
			stream_print(out, "section .rodata\n");
		} else if (j == SECTION_BSS) {
			stream_print(out, "section .bss\n");
		} else {
			// Unsupported section
			continue;
		}

		symbol_id sym_id = symtab->section[j];
		while (sym_id.value != 0) {
			symbol *sym = &symtab->symbols[sym_id.value];
			if (sym->linkage == LINK_STATIC) {
				stream_print(out, "static ");
				stream_prints(out, sym->name);
				stream_print(out, "\n");
			} else if (sym->linkage == LINK_EXTERN) {
				stream_print(out, "extern ");
				stream_prints(out, sym->name);
				stream_print(out, "\n");
			} else if (sym->name.length > 0) {
				stream_print(out, "global ");
				stream_prints(out, sym->name);
				stream_print(out, "\n");
			}

			// NOTE: text section was already printed in the loop above
			if (sym->size > 0) {
				if (sym->name.length > 0) {
					stream_prints(out, sym->name);
				} else {
					stream_print(out, "L#");
					stream_printu(out, sym_id.value);
				}

				if (j == SECTION_TEXT) {
					stream_print(out, ":\n");
					mach_function *func = &p.funcs[sym_id.value];
					if (func->inst_count == 0) {
						// NOTE: Do not print empty functions
						goto next;
					}

					// Print function prologue
					isize func_index = sym_id.value;
					isize used_volatile_register_count = 0;
					for (isize j = 0; j < LENGTH(x86_preserved_regs); j++) {
						u32 mreg = x86_preserved_regs[j];
						if (info[func_index].used[mreg]) {
							stream_print(out, "\tpush ");
							x86_emit_token(out, make_mach_token(MACH_MREG, mreg, 8), symtab);
							stream_print(out, "\n");
							used_volatile_register_count++;
						}
					}

					// TODO: Set function stack size
					isize stack_size = 0;
#if 0
					char *code = (char *)p.tokens + func->inst_offset;
					mach_inst *first_inst = (mach_inst *)code;
					if (first_inst->opcode == X86_SUB) {
						mach_token *tokens = (mach_token *)(first_inst + 1);
						if (tokens[0].kind == MACH_MREG && tokens[0].value == X86_RSP) {
							ASSERT(tokens[1].kind == MACH_CONST);
							stack_size = tokens[1].value;

							stack_size += 8 * info[func_index].spill_count;
							stack_size += used_volatile_register_count;
							b32 is_stack_aligned = ((stack_size & 15) == 8);
							if (!is_stack_aligned) {
								stack_size += 24 - (stack_size & 15);
							}

							tokens[1].value = stack_size;
						}
					}
#endif
					b32 first_inst = true;
					b32 first_token = true;

					// TODO: We need to ensure that instructions do not
					// contain two address tokens, e.g. mov [rax], [rax]
					for (isize i = 0; i < p.token_count; i++) {
						mach_token token = p.tokens[i];
						if (token.flags & MACH_IMPLICIT) {
							continue;
						}

						switch (token.kind) {
						case MACH_INST:
							if (first_inst) {
								first_inst = false;
							} else {
								stream_print(out, "\n");
							}

							if (token.value == X86_LABEL) {
								if (i + 1 < p.token_count) {
									stream_print(out, ".L");
									stream_printu(out, p.tokens[i + 1].value);
									stream_print(out, ":");
									i++;
								}
							} else if (token.value == X86_RET) {
								stream_print(out, "\tjmp .exit\n");
							} else {
								stream_print(out, "\t");
								stream_print(out, x86_get_opcode_name(token.value));
								stream_print(out, " ");
							}

							first_token = true;
							break;
						default:
							if (first_token) {
								first_token = false;
							} else {
								stream_print(out, ", ");
							}

							x86_emit_token(out, token, symtab);
							break;
						}
					}

					// Print function epilogue
					stream_print(out, "\n.exit:\n");
					if (stack_size > 0) {
						stream_print(out, "\tadd rsp, ");
						stream_printu(out, stack_size);
						stream_print(out, "\n");
					}

					isize j = LENGTH(x86_preserved_regs);
					while (j-- > 0) {
						u32 mreg = x86_preserved_regs[j];
						if (info[func_index].used[mreg]) {
							stream_print(out, "\tpop ");
							x86_emit_token(out, make_mach_token(MACH_MREG, mreg, 8), symtab);
							stream_print(out, "\n");
						}
					}

					stream_print(out, "\tret\n\n");
				} else if (j == SECTION_DATA || j == SECTION_RODATA) {
					// NOTE: Inside data or rodata section, symbols contain byte data
					if (sym->data) {
						stream_print(out, ": db ");
						char *byte = sym->data;
						for (isize i = 0; i < sym->size; i++) {
							if (i != 0) {
								stream_print(out, ", ");
							}

							stream_print_hex(out, byte[i]);
						}

						stream_print(out, "\n");
					} else {
						stream_print(out, ": times ");
						stream_printu(out, sym->size);
						stream_print(out, " db 0\n");
					}
				} else if (j == SECTION_BSS) {
					// NOTE; Inside bss section, symbols have no data
					stream_print(out, " resb ");
					stream_print_hex(out, sym->size);
					stream_print(out, "\n");
				}
			}

next:
			sym_id = sym->next;
		}
	}
}
