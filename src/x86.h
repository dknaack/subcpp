typedef enum {
	X86_RAX,
	X86_RBX,
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
	X86_XMM0,
	X86_XMM1,
	X86_XMM2,
	X86_XMM3,
	X86_XMM4,
	X86_XMM5,
	X86_XMM6,
	X86_XMM7,
	X86_RSP,
	X86_RCX,

	X86_REGISTER_COUNT = X86_RSP,
	X86_INT_REGISTER_COUNT = X86_XMM0,
} x86_register;

typedef enum {
	X86_NOP,
	X86_ADD,
	X86_ADDSS,
	X86_AND,
	X86_CALL,
	X86_CMP,
	X86_COMISS,
	X86_CVTSI2SS,
	X86_CVTSI2SD,
	X86_CVTTSS2SI,
	X86_CVTTSD2SI,
	X86_DEC,
	X86_DIVSS,
	X86_IDIV,
	X86_IMUL,
	X86_INC,
	X86_JA,
	X86_JAE,
	X86_JB,
	X86_JBE,
	X86_JG,
	X86_JGE,
	X86_JL,
	X86_JLE,
	X86_JMP,
	X86_JNZ,
	X86_JZ,
	X86_LEA,
	X86_MOV,
	X86_MOVSS,
	X86_MOVSX,
	X86_MOVZX,
	X86_MULSS,
	X86_NEG,
	X86_NOT,
	X86_POPCNT,
	X86_OR,
	X86_RET,
	X86_SETA,
	X86_SETAE,
	X86_SETB,
	X86_SETBE,
	X86_SETG,
	X86_SETGE,
	X86_SETL,
	X86_SETLE,
	X86_SETZ,
	X86_SHL,
	X86_SHR,
	X86_SUB,
	X86_SUBSS,
	X86_TEST,
	X86_XOR,
	/* NOTE: pseudo opcodes; they don't actually exist. */
	X86_LABEL,
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
	ir_inst *inst;
	mach_token *tokens;
	b32 *is_float;
	i32 token_count;
	i32 max_token_count;
	i32 vreg_count;
	symbol_table *symtab;
} x86_context;

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

static u32 x86_saved_regs[] = {
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
	case X86_ADD:
		return "add";
	case X86_ADDSS:
		return "addss";
	case X86_AND:
		return "and";
	case X86_CALL:
		return "call";
	case X86_CMP:
		return "cmp";
	case X86_COMISS:
		return "comiss";
	case X86_CVTSI2SD:
		return "cvtsi2sd";
	case X86_CVTSI2SS:
		return "cvtsi2ss";
	case X86_CVTTSD2SI:
		return "cvttsd2si";
	case X86_CVTTSS2SI:
		return "cvttss2si";
	case X86_DEC:
		return "dec";
	case X86_DIVSS:
		return "divss";
	case X86_IDIV:
		return "idiv";
	case X86_IMUL:
		return "imul";
	case X86_INC:
		return "inc";
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
	case X86_JMP:
		return "jmp";
	case X86_JNZ:
		return "jnz";
	case X86_JZ:
		return "jz";
	case X86_LABEL:
		return "label";
	case X86_LEA:
		return "lea";
	case X86_MOV:
		return "mov";
	case X86_MOVSS:
		return "movss";
	case X86_MOVSX:
		return "movsx";
	case X86_MOVZX:
		return "movzx";
	case X86_MULSS:
		return "mulss";
	case X86_NEG:
		return "neg";
	case X86_NOT:
		return "not";
	case X86_NOP:
		return "nop";
	case X86_POPCNT:
		return "popcnt";
	case X86_OR:
		return "or";
	case X86_RET:
		return "ret";
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
	case X86_SETZ:
		return "setz";
	case X86_SHL:
		return "shl";
	case X86_SHR:
		return "shr";
	case X86_SUB:
		return "sub";
	case X86_SUBSS:
		return "subss";
	case X86_TEST:
		return "test";
	case X86_XOR:
		return "xor";
	default:
		ASSERT(!"Invalid instruction");
		return "(invalid)";
	}
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
			return "(invalid register with size 1)";
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
			return "(invalid register with size 2)";
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
		default:
			return "(invalid register with size 4)";
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
		default:
			return "(invalid register with size 8)";
		}
	}

	ASSERT(false);
	return "(invalid size)";
}
