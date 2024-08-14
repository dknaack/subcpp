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
	stream_print(out, "section .text\n");
	ASSERT(program.function_count > 0);
	for (isize i = 0; i < program.function_count; i++) {
		mach_function *func = &program.functions[i];
		if (func->inst_count == 0) {
			// NOTE: Do not print empty functions
			continue;
		}

		// NOTE: Inside text section, symbols contain x86 instructions
		stream_prints(out, func->name);
		stream_print(out, ":\n");

		isize function_index = i - symtab->text_offset;
		isize used_volatile_register_count = 0;
		for (isize j = 0; j < LENGTH(x86_preserved_regs); j++) {
			u32 mreg = x86_preserved_regs[j];
			if (info[function_index].used[mreg]) {
				stream_print(out, "\tpush ");
				x86_emit_operand(out, make_operand(MOP_MREG, mreg, 8), symtab);
				stream_print(out, "\n");
				used_volatile_register_count++;
			}
		}

		// TODO: Set function stack size
		mach_inst *first_inst = (mach_inst *)((char *)program.code + func->inst_offsets[0]);
		isize stack_size = 0;
		if (first_inst->opcode == X86_SUB) {
			mach_operand *operands = (mach_operand *)(first_inst + 1);
			if (operands[0].kind == MOP_MREG && operands[0].value == X86_RSP) {
				ASSERT(operands[1].kind == MOP_CONST);
				stack_size = operands[1].value;

				stack_size += 8 * info[function_index].spill_count;
				stack_size += used_volatile_register_count;
				b32 is_stack_aligned = ((stack_size & 15) == 8);
				if (!is_stack_aligned) {
					stack_size += 24 - (stack_size & 15);
				}

				operands[1].value = stack_size;
			}
		}

		char *code = (char *)program.code + func->inst_offsets[0];
		isize size = func->inst_offsets[func->inst_count - 1] - func->inst_offsets[0];
		while (size > 0) {
			mach_inst *inst = (mach_inst *)code;
			mach_operand *operands = (mach_operand *)(inst + 1);
			isize operand_count = inst->operand_count;
			isize inst_size = sizeof(*inst) + operand_count * sizeof(*operands);

			x86_opcode opcode = (x86_opcode)inst->opcode;
			if (opcode == X86_LABEL) {
				stream_print(out, ".L");
				x86_emit_operand(out, operands[0], symtab);
				stream_print(out, ":\n");
			} else if (opcode == X86_RET) {
				stream_print(out, "\tjmp .exit\n");
			} else {
				if (opcode == X86_MOV || opcode == X86_MOVSS) {
					if  (equals_operand(operands[0], operands[1])) {
						continue;
					}
				}

				b32 both_spill = (operands[0].kind == MOP_SPILL
					&& operands[1].kind == MOP_SPILL);
				if (both_spill) {
					mach_operand rax = make_operand(MOP_MREG, X86_RAX, operands[0].size);
					stream_print(out, "\tmov ");
					x86_emit_operand(out, rax, symtab);
					stream_print(out, ", ");
					x86_emit_operand(out, operands[1], symtab);
					operands[1] = rax;
					stream_print(out, "\n");
				}

				stream_print(out, "\t");
				stream_print(out, x86_get_opcode_name(opcode));
				stream_print(out, " ");
				for (isize j = 0; j < operand_count; j++) {
					if (operands[j].flags & MOP_IMPLICIT) {
						continue;
					}

					if (j != 0) {
						stream_print(out, ", ");
					}

					x86_emit_operand(out, operands[j], symtab);
				}

				stream_print(out, "\n");
			}

			size -= inst_size;
			code += inst_size;
		}

		stream_print(out, ".exit:\n");
		if (stack_size > 0) {
			stream_print(out, "\tadd rsp, ");
			stream_printu(out, stack_size);
			stream_print(out, "\n");
		}

		u32 j = LENGTH(x86_preserved_regs);
		while (j-- > 0) {
			u32 mreg = x86_preserved_regs[j];
			if (info[function_index].used[mreg]) {
				stream_print(out, "\tpop ");
				x86_emit_operand(out, make_operand(MOP_MREG, mreg, 8), symtab);
				stream_print(out, "\n");
			}
		}

		stream_print(out, "\tret\n\n");
	}

	for (isize i = 0; i < symtab->symbol_count; i++) {
		symbol *sym = &symtab->symbols[i];

		if (i == symtab->text_offset) {
			stream_print(out, "section .text\n");
		} else if (i == symtab->data_offset) {
			stream_print(out, "section .data\n");
		} else if (i == symtab->rodata_offset) {
			stream_print(out, "section .rodata\n");
		} else if (i == symtab->bss_offset) {
			stream_print(out, "section .bss\n");
		}

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
		if (i >= symtab->data_offset) {
			if (sym->name.length > 0) {
				stream_prints(out, sym->name);
			} else {
				stream_print(out, "L#");
				stream_printu(out, i);
			}
		}

		if (symtab->data_offset <= i && i < symtab->bss_offset) {
			// NOTE: Inside data or rodata section, symbols contain byte data
			stream_print(out, ": db ");

			char *byte = sym->data;
			for (isize i = 0; i < sym->size; i++) {
				if (i != 0) {
					stream_print(out, ", ");
				}

				stream_print_hex(out, byte[i]);
			}

			stream_print(out, "\n");
		} else if (i >= symtab->bss_offset) {
			// NOTE; Inside bss section, symbols have no data
			stream_print(out, " resb ");
			stream_print_hex(out, sym->size);
			stream_print(out, "\n");
		}
	}
}
