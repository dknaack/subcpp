#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#define TRAP() __builtin_trap()
#define ASSERT(x) do { if (!(x)) TRAP(); } while (0)
#define LENGTH(x) (sizeof(x)/sizeof((x)[0]))
#define MIN(a, b) ((a) < (b)? (a) : (b))
#define S(x) {(x), sizeof(x) - 1}

typedef struct string {
	char *at;
	size_t length;
} string;

#include "memory.h"
#include "tokenizer.h"
#include "ast.h"
#include "type.h"
#include "ir.h"
#include "codegen.h"
#include "x86.h"
