static void
x86_emit_operand(stream *out, mach_operand operand, symbol_table *symtab)
{
	x86_register reg;

	b32 print_size = (operand.kind == MOP_SPILL
		|| (operand.flags & MOP_INDIRECT));
	if (print_size) {
		switch (operand.size) {
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

	switch (operand.kind) {
	case MOP_GLOBAL:
		{
			ASSERT(operand.value < symtab->symbol_count);
			symbol *sym = &symtab->symbols[operand.value];
			if (sym->name.length > 0) {
				stream_prints(out, sym->name);
			} else {
				stream_print(out, "L#");
				stream_printu(out, operand.value);
			}
		} break;
	case MOP_SPILL:
		stream_print(out, "[rsp+");
		stream_printu(out, operand.value);
		stream_print(out, "]");
		break;
	case MOP_LABEL:
		stream_print(out, ".L");
		stream_printu(out, operand.value);
		break;
	case MOP_MREG:
		if (operand.flags & MOP_INDIRECT) {
			stream_print(out, "[");
			operand.size = 8;
		}

		reg = (x86_register)operand.value;
		stream_print(out, x86_get_register_name(reg, operand.size));

		if (operand.flags & MOP_INDIRECT) {
			stream_print(out, "]");
		}
		break;
	case MOP_CONST:
		stream_printu(out, operand.value);
		break;
	case MOP_FLOAT:
		stream_print(out, "[float#");
		stream_printu(out, operand.value);
		stream_print(out, "]");
		break;
	case MOP_FUNC:
		{
			ASSERT(!"TODO");
		} break;
	case MOP_VREG:
		stream_print(out, "v");
		stream_printu(out, operand.value);
		//ASSERT(!"Cannot use virtual register during code generation");
		break;
	default:
		ASSERT(false);
		stream_print(out, "(invalid operand)");
	}
}

static void
x86_generate(stream *out, mach_program program, symbol_table *symtab, regalloc_info *info)
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
					mach_function *func = &program.funcs[sym_id.value];
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
							x86_emit_operand(out, make_operand(MOP_MREG, mreg, 8), symtab);
							stream_print(out, "\n");
							used_volatile_register_count++;
						}
					}

					// TODO: Set function stack size
					isize stack_size = 0;
#if 0
					char *code = (char *)program.code + func->inst_offset;
					mach_inst *first_inst = (mach_inst *)code;
					if (first_inst->opcode == X86_SUB) {
						mach_operand *operands = (mach_operand *)(first_inst + 1);
						if (operands[0].kind == MOP_MREG && operands[0].value == X86_RSP) {
							ASSERT(operands[1].kind == MOP_CONST);
							stack_size = operands[1].value;

							stack_size += 8 * info[func_index].spill_count;
							stack_size += used_volatile_register_count;
							b32 is_stack_aligned = ((stack_size & 15) == 8);
							if (!is_stack_aligned) {
								stack_size += 24 - (stack_size & 15);
							}

							operands[1].value = stack_size;
						}
					}
#endif
					b32 first_inst = true;
					b32 first_operand = true;

					// TODO: We need to ensure that instructions do not
					// contain two address operands, e.g. mov [rax], [rax]
					for (isize i = 0; i < program.inst_count; i++) {
						mach_operand operand = program.code[i];
						if (first_operand) {
							first_operand = false;
						} else {
							stream_print(out, ", ");
						}

						switch (operand.kind) {
						case MOP_INST:
							if (!first_inst) {
								putchar('\n');
							} else {
								first_inst = false;
							}

							if (operand.value == X86_LABEL) {
								stream_print(out, ".L");
							} else if (operand.value == X86_RET) {
								stream_print(out, "\tjmp .exit\n");
							} else {
								stream_print(out, "\t");
								stream_print(out, x86_get_opcode_name(operand.value));
								stream_print(out, " ");
							}

							first_operand = true;
							break;
						default:
							x86_emit_operand(out, operand, symtab);
							break;
						}
					}

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
							x86_emit_operand(out, make_operand(MOP_MREG, mreg, 8), symtab);
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
