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
	case MOP_IMMEDIATE:
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
x86_generate(stream *out, mach_program program, regalloc_info *info)
{
	symbol_table *symtab = program.symtab;
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
		} else if (sym->linkage == LINK_EXTERN) {
			stream_print(out, "extern ");
			stream_prints(out, sym->name);
		}

		if (symtab->text_offset <= i && i < symtab->data_offset) {
			// NOTE: Inside text section, symbols contain x86 instructions
			stream_prints(out, sym->name);
			stream_print(out, ":\n");

			isize function_index = i - symtab->text_offset;
			isize used_volatile_register_count = 0;
			for (isize j = 0; j < LENGTH(x86_preserved_regs); j++) {
				u32 mreg = x86_preserved_regs[j];
				if (info[function_index].used[mreg]) {
					stream_print(out, "\tpush ");
					x86_emit_operand(out, make_operand(MOP_MREG, mreg, 8), program.symtab);
					stream_print(out, "\n");
					used_volatile_register_count++;
				}
			}

			// TODO: Set function stack size
			ASSERT(!"TODO");
			mach_inst *first_inst = (mach_inst *)sym->data;
			isize stack_size = 0;
			if (first_inst->opcode == X86_SUB) {
				mach_operand *operands = (mach_operand *)(first_inst + 1);
				if (operands[0].kind == MOP_MREG && operands[0].value == X86_RSP) {
					ASSERT(operands[1].kind == MOP_IMMEDIATE);
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

			char *code = (char *)sym->data;
			isize size = sym->size;
			while (size > 0) {
				mach_inst *inst = (mach_inst *)code;
				mach_operand *operands = (mach_operand *)(inst + 1);
				isize operand_count = inst->operand_count;
				isize inst_size = sizeof(*inst) + operand_count * sizeof(*operands);

				x86_opcode opcode = (x86_opcode)inst->opcode;
				if (opcode == X86_LABEL) {
					stream_print(out, ".L");
					x86_emit_operand(out, operands[0], program.symtab);
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
						x86_emit_operand(out, rax, program.symtab);
						stream_print(out, ", ");
						x86_emit_operand(out, operands[1], program.symtab);
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

						x86_emit_operand(out, operands[j], program.symtab);
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
					x86_emit_operand(out, make_operand(MOP_MREG, mreg, 8), program.symtab);
					stream_print(out, "\n");
				}
			}

			stream_print(out, "\tret\n\n");
		} else if (i < symtab->bss_offset) {
			// NOTE: Inside data or rodata section, symbols contain byte data
			stream_prints(out, sym->name);
			stream_print(out, ": db ");

			char *byte = sym->data;
			for (isize i = 0; i < sym->size; i++) {
				if (i != 0) {
					stream_print(out, ", ");
				}

				stream_print_hex(out, byte[i]);
			}
		} else {
			// NOTE; Inside bss section, symbols have no data
			stream_prints(out, sym->name);
			stream_print(out, " resb ");
			stream_print_hex(out, sym->size);
		}
	}
}
