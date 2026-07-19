typedef enum {
	X86_RAX = 1,
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
	X86_RSP,
	X86_XMM0,
	X86_XMM1,
	X86_XMM2,
	X86_XMM3,
	X86_XMM4,
	X86_XMM5,
	X86_XMM6,
	X86_XMM7,
	X86_XMM8,
	X86_XMM9,
	X86_XMM10,
	X86_XMM11,
	X86_XMM12,
	X86_XMM13,
	X86_XMM14,
	X86_XMM15,
	X86_REGISTER_COUNT
} x86_register;

typedef enum {
	X86_NOP,
	X86_PHI,
	X86_MOVrg,
	X86_MOVri,
	X86_MOVrm,
	X86_MOVmr,
	X86_MOVrr,
	X86_MOVr,
	X86_ADDrr,
	X86_CALLf,
	X86_SETA,
	X86_SETAE,
	X86_SETB,
	X86_SETBE,
	X86_SETG,
	X86_SETGE,
	X86_SETL,
	X86_SETLE,
	X86_SETNZ,
	X86_SETZ,
	X86_JMP,
	X86_JA,
	X86_JAE,
	X86_JB,
	X86_JBE,
	X86_JG,
	X86_JGE,
	X86_JL,
	X86_JLE,
	X86_JNZ,
	X86_JZ,
	X86_CMP,
	X86_RET,
} x86_opcode;

typedef enum {
	X86_OPERAND_NONE,
	X86_OPERAND_FUNCTION,
	X86_OPERAND_GLOBAL,
	X86_OPERAND_IMMEDIATE,
	X86_OPERAND_LABEL,
	X86_OPERAND_MEMORY,
	X86_OPERAND_REGISTER,
} x86_operand_kind;

typedef struct {
	x86_operand_kind dest;
	x86_operand_kind args[2];
} x86_opcode_info;

#define X86_OPCODE_MASK 0xffff

typedef struct {
	inst_buffer output;
	inst *input;
} x86_context;

static i32 x86_int_registers[] = {
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
};

static i32 x86_float_registers[] = {
	X86_XMM0,
	X86_XMM1,
	X86_XMM2,
	X86_XMM3,
	X86_XMM4,
	X86_XMM5,
	X86_XMM6,
	X86_XMM7,
	X86_XMM8,
	X86_XMM9,
	X86_XMM10,
	X86_XMM11,
	X86_XMM12,
	X86_XMM13,
	X86_XMM14,
	X86_XMM15,
};

static i32 x86_volatile_registers[] = {
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

static i32 x86_saved_regs[] = {
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
 	case X86_NOP:
 	case X86_PHI:
		return "nop";
	case X86_MOVrg:
	case X86_MOVri:
	case X86_MOVrm:
	case X86_MOVmr:
	case X86_MOVrr:
	case X86_MOVr:
		return "mov";
	case X86_ADDrr:
		return "add";
	case X86_CALLf:
		return "call";
	case X86_SETA:
		return "seta";
	case X86_SETAE:
		return "setae";
	case X86_SETB:
		return "setb";
	case X86_SETBE:
		return "setbe";
	case X86_SETG:
		return "setg";
	case X86_SETGE:
		return "setge";
	case X86_SETL:
		return "setl";
	case X86_SETLE:
		return "setle";
	case X86_SETNZ:
		return "setnz";
	case X86_SETZ:
		return "setz";
	case X86_JMP:
		return "jmp";
	case X86_JA:
		return "ja";
	case X86_JAE:
		return "jae";
	case X86_JB:
		return "jb";
	case X86_JBE:
		return "jbe";
	case X86_JG:
		return "jg";
	case X86_JGE:
		return "jge";
	case X86_JL:
		return "jl";
	case X86_JLE:
		return "jle";
	case X86_JNZ:
		return "jnz";
	case X86_JZ:
		return "jz";
	case X86_CMP:
		return "cmp";
	case X86_RET:
		return "ret";
	}

	return "(invalid x86 opcode)";
}

static char *
x86_get_register_name(x86_register reg, u32 size)
{
	switch (size) {
	case 1:
		switch (reg) {
		case X86_R8:
			return "r8b";
		case X86_R9:
			return "r9b";
		case X86_R10:
			return "r10b";
		case X86_R11:
			return "r11b";
		case X86_R12:
			return "r12b";
		case X86_R13:
			return "r13b";
		case X86_R14:
			return "r14b";
		case X86_R15:
			return "r15b";
		case X86_RAX:
			return "al";
		case X86_RBX:
			return "bl";
		case X86_RCX:
			return "cl";
		case X86_RDX:
			return "dl";
		case X86_RSI:
			return "sil";
		case X86_RDI:
			return "dil";
		case X86_RSP:
			return "spl";
		case X86_RBP:
			return "bpl";
		default:
			ASSERT(!"Invalid register");
			return "(invalid)";
		}
	case 2:
		switch (reg) {
		case X86_R8:
			return "r8w";
		case X86_R9:
			return "r9w";
		case X86_R10:
			return "r10w";
		case X86_R11:
			return "r11w";
		case X86_R12:
			return "r12w";
		case X86_R13:
			return "r13w";
		case X86_R14:
			return "r14w";
		case X86_R15:
			return "r15w";
		case X86_RAX:
			return "ax";
		case X86_RBX:
			return "bx";
		case X86_RCX:
			return "cx";
		case X86_RDX:
			return "dx";
		case X86_RSI:
			return "si";
		case X86_RDI:
			return "di";
		case X86_RSP:
			return "sp";
		case X86_RBP:
			return "bp";
		default:
			ASSERT(!"Invalid register");
			return "(invalid)";
		}
	case 4:
		switch (reg) {
		case X86_R8:
			return "r8d";
		case X86_R9:
			return "r9d";
		case X86_R10:
			return "r10d";
		case X86_R11:
			return "r11d";
		case X86_R12:
			return "r12d";
		case X86_R13:
			return "r13d";
		case X86_R14:
			return "r14d";
		case X86_R15:
			return "r15d";
		case X86_RAX:
			return "eax";
		case X86_RBX:
			return "ebx";
		case X86_RCX:
			return "ecx";
		case X86_RDX:
			return "edx";
		case X86_RSI:
			return "esi";
		case X86_RDI:
			return "edi";
		case X86_RSP:
			return "esp";
		case X86_RBP:
			return "ebp";
		case X86_XMM0:
			return "xmm0";
		case X86_XMM1:
			return "xmm1";
		case X86_XMM2:
			return "xmm2";
		case X86_XMM3:
			return "xmm3";
		case X86_XMM4:
			return "xmm4";
		case X86_XMM5:
			return "xmm5";
		case X86_XMM6:
			return "xmm6";
		case X86_XMM7:
			return "xmm7";
		case X86_XMM8:
			return "xmm8";
		case X86_XMM9:
			return "xmm9";
		case X86_XMM10:
			return "xmm10";
		case X86_XMM11:
			return "xmm11";
		case X86_XMM12:
			return "xmm12";
		case X86_XMM13:
			return "xmm13";
		case X86_XMM14:
			return "xmm14";
		case X86_XMM15:
			return "xmm15";
		default:
			ASSERT(!"Invalid register");
			return "(invalid)";
		}
	case 8:
	case 0:
		switch (reg) {
		case X86_R8:
			return "r8";
		case X86_R9:
			return "r9";
		case X86_R10:
			return "r10";
		case X86_R11:
			return "r11";
		case X86_R12:
			return "r12";
		case X86_R13:
			return "r13";
		case X86_R14:
			return "r14";
		case X86_R15:
			return "r15";
		case X86_RAX:
			return "rax";
		case X86_RBX:
			return "rbx";
		case X86_RCX:
			return "rcx";
		case X86_RDX:
			return "rdx";
		case X86_RSI:
			return "rsi";
		case X86_RDI:
			return "rdi";
		case X86_RSP:
			return "rsp";
		case X86_RBP:
			return "rbp";
		case X86_XMM0:
			return "xmm0";
		case X86_XMM1:
			return "xmm1";
		case X86_XMM2:
			return "xmm2";
		case X86_XMM3:
			return "xmm3";
		case X86_XMM4:
			return "xmm4";
		case X86_XMM5:
			return "xmm5";
		case X86_XMM6:
			return "xmm6";
		case X86_XMM7:
			return "xmm7";
		case X86_XMM8:
			return "xmm8";
		case X86_XMM9:
			return "xmm9";
		case X86_XMM10:
			return "xmm10";
		case X86_XMM11:
			return "xmm11";
		case X86_XMM12:
			return "xmm12";
		case X86_XMM13:
			return "xmm13";
		case X86_XMM14:
			return "xmm14";
		case X86_XMM15:
			return "xmm15";
		default:
			ASSERT(!"Invalid register");
			return "(invalid)";
		}
	}

	ASSERT(false);
	return "(invalid)";
}
