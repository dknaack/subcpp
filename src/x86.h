enum x86_register {
	X86_R11,
	X86_R12,
	X86_R13,
	X86_R14,
	X86_R15,
	X86_REGISTER_COUNT,
	X86_RAX,
	X86_RBX,
	X86_RCX,
	X86_RDX,
	X86_RSI,
	X86_RDI,
	X86_RSP,
	X86_RBP,
	X86_R8,
	X86_R9,
	X86_R10,
};

enum x86_opcode {
	X86_ADD,
	X86_CALL,
	X86_DEC,
	X86_IDIV,
	X86_IMUL,
	X86_INC,
	X86_JMP,
	X86_JZ,
	X86_MOV,
	X86_RET,
	X86_SUB,
	X86_TEST,
	X86_XOR,
	/* NOTE: pseudo opcodes; they don't actually exist. */
	X86_LABEL,
	X86_PRINT
};
