typedef enum {
	X86_RAX,
	X86_RBX,
	X86_RCX,
	X86_RDX,
	X86_RSI,
	X86_RDI,
	X86_R8,
	X86_R9,
	X86_R10,
	X86_R11,
	X86_R12,
	X86_R13,
	X86_R14,
	X86_R15,
	X86_RBP,
	X86_REGISTER_COUNT,
	X86_RSP,
} x86_register;

typedef enum {
	X86_ADD,
	X86_AND,
	X86_CALL,
	X86_CMP,
	X86_DEC,
	X86_IDIV,
	X86_IMUL,
	X86_INC,
	X86_JMP,
	X86_JZ,
	X86_JNZ,
	X86_JL,
	X86_JG,
	X86_JLE,
	X86_JGE,
	X86_LEA,
	X86_MOV,
	X86_MOVZX,
	X86_OR,
	X86_RET,
	X86_SETZ,
	X86_SETL,
	X86_SETG,
	X86_SETLE,
	X86_SETGE,
	X86_SHL,
	X86_SHR,
	X86_SUB,
	X86_TEST,
	X86_XOR,
	/* NOTE: pseudo opcodes; they don't actually exist. */
	X86_LOAD,
	X86_STORE,
	X86_LABEL,
	X86_PRINT
} x86_opcode;

static u32 x86_temp_regs[] = {
	X86_RAX,
	X86_RCX,
	X86_RDX,
	X86_RSI,
	X86_RDI,
	X86_R8,
	X86_R9,
	X86_R10,
	X86_R11,
};

static u32 x86_preserved_regs[] = {
	X86_RBX,
	X86_RBP,
	X86_R12,
	X86_R13,
	X86_R14,
	X86_R15,
};

static char *
x86_get_opcode_name(x86_opcode opcode)
{
	switch (opcode) {
	case X86_ADD:   return "add";
	case X86_AND:   return "and";
	case X86_CALL:  return "call";
	case X86_CMP:   return "cmp";
	case X86_DEC:   return "dec";
	case X86_IDIV:  return "idiv";
	case X86_IMUL:  return "imul";
	case X86_INC:   return "inc";
	case X86_JMP:   return "jmp";
	case X86_JZ:    return "jz";
	case X86_JNZ:   return "jnz";
	case X86_JL:    return "jl";
	case X86_JG:    return "jg";
	case X86_JLE:   return "jle";
	case X86_JGE:   return "jge";
	case X86_LEA:   return "lea";
	case X86_MOV:   return "mov";
	case X86_MOVZX: return "movzx";
	case X86_OR:    return "or";
	case X86_RET:   return "ret";
	case X86_SETZ:  return "setz";
	case X86_SETL:  return "setl";
	case X86_SETG:  return "setg";
	case X86_SETLE: return "setle";
	case X86_SETGE: return "setge";
	case X86_SHL:   return "shl";
	case X86_SHR:   return "shr";
	case X86_SUB:   return "sub";
	case X86_TEST:  return "test";
	case X86_XOR:   return "xor";
	case X86_LOAD:  return "mov";
	case X86_STORE: return "mov";
	case X86_LABEL: return "label";
	case X86_PRINT: return "print";
	}

	return "(invalid)";
}

static char *
x86_get_byte_register_name(x86_register reg)
{
	switch (reg) {
	case X86_R8:  return "r8b";
	case X86_R9:  return "r9b";
	case X86_R10: return "r10b";
	case X86_R11: return "r11b";
	case X86_R12: return "r12b";
	case X86_R13: return "r13b";
	case X86_R14: return "r14b";
	case X86_R15: return "r15b";
	case X86_RAX: return "al";
	case X86_RBX: return "bl";
	case X86_RCX: return "cl";
	case X86_RDX: return "dl";
	case X86_RSI: return "sil";
	case X86_RDI: return "dil";
	default:      return "(invalid)";
	}
}

static char *
x86_get_register_name(x86_register reg, u32 size)
{
	switch (size) {
	case 1:
		switch (reg) {
		case X86_R8:  return "r8b";
		case X86_R9:  return "r9b";
		case X86_R10: return "r10b";
		case X86_R11: return "r11b";
		case X86_R12: return "r12b";
		case X86_R13: return "r13b";
		case X86_R14: return "r14b";
		case X86_R15: return "r15b";
		case X86_RAX: return "al";
		case X86_RBX: return "bl";
		case X86_RCX: return "cl";
		case X86_RDX: return "dl";
		case X86_RSI: return "sil";
		case X86_RDI: return "dil";
		case X86_RSP: return "spl";
		case X86_RBP: return "bpl";
		default:      return "(invalid)";
		}
	case 2:
		switch (reg) {
		case X86_R8:  return "r8w";
		case X86_R9:  return "r9w";
		case X86_R10: return "r10w";
		case X86_R11: return "r11w";
		case X86_R12: return "r12w";
		case X86_R13: return "r13w";
		case X86_R14: return "r14w";
		case X86_R15: return "r15w";
		case X86_RAX: return "ax";
		case X86_RBX: return "bx";
		case X86_RCX: return "cx";
		case X86_RDX: return "dx";
		case X86_RSI: return "si";
		case X86_RDI: return "di";
		case X86_RSP: return "sp";
		case X86_RBP: return "bp";
		default:      return "(invalid)";
		}
	case 4:
		switch (reg) {
		case X86_R8:  return "r8d";
		case X86_R9:  return "r9d";
		case X86_R10: return "r10d";
		case X86_R11: return "r11d";
		case X86_R12: return "r12d";
		case X86_R13: return "r13d";
		case X86_R14: return "r14d";
		case X86_R15: return "r15d";
		case X86_RAX: return "eax";
		case X86_RBX: return "ebx";
		case X86_RCX: return "ecx";
		case X86_RDX: return "edx";
		case X86_RSI: return "esi";
		case X86_RDI: return "edi";
		case X86_RSP: return "esp";
		case X86_RBP: return "ebp";
		default:      return "(invalid)";
		}
	case 8:
	case 0:
		switch (reg) {
		case X86_R8:  return "r8";
		case X86_R9:  return "r9";
		case X86_R10: return "r10";
		case X86_R11: return "r11";
		case X86_R12: return "r12";
		case X86_R13: return "r13";
		case X86_R14: return "r14";
		case X86_R15: return "r15";
		case X86_RAX: return "rax";
		case X86_RBX: return "rbx";
		case X86_RCX: return "rcx";
		case X86_RDX: return "rdx";
		case X86_RSI: return "rsi";
		case X86_RDI: return "rdi";
		case X86_RSP: return "rsp";
		case X86_RBP: return "rbp";
		default:      return "(invalid)";
		}
	}

	return "(invalid size)";
}
