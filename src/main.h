#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#define TRAP() __builtin_trap()
#define ASSERT(x) do { if (!(x)) TRAP(); } while (0)
#define LENGTH(x) (sizeof(x)/sizeof((x)[0]))
#define S(x) (struct string){(x), sizeof(x) - 1}

struct string {
	char *at;
	size_t length;
};

#include "memory.h"
#include "tokenizer.h"
#include "ast.h"
#include "ir.h"
#include "codegen.h"
#include "x86.h"
