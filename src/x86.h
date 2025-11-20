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
	X86_MOVrg,
	X86_MOVri,
	X86_MOVrm,
	X86_MOVmr,
} x86_opcode;

#define X86_OPCODE_MASK 0xffff

typedef enum {
	X86_NIL,
	X86_IMM,
	X86_SYM,
	X86_REG,
	X86_BASE,
	X86_INDEX, // TODO: Add scale factor to this type
	X86_DISP_IMM,
	X86_DISP_SYM,
	X86_OPERAND_COUNT
} x86_operand_kind;

typedef enum {
	X86_BYTE = 1,
	X86_WORD,
	X86_DWORD,
	X86_QWORD,
} x86_operand_size;

typedef struct {
	inst_buffer output;
	inst *input;
} x86_context;

static i32 x86_int_regs[] = {
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

static i32 x86_float_regs[] = {
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

static i32 x86_temp_regs[] = {
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
	ASSERT(!"TODO");
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
