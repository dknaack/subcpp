#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#define TRAP() __builtin_trap()
#define ASSERT(x) do { if (!(x)) TRAP(); } while (0)
#define LENGTH(x) (sizeof(x)/sizeof((x)[0]))
#define MIN(a, b) ((a) < (b)? (a) : (b))
#define S(x) (str){(x), sizeof(x) - 1}

typedef uintptr_t usize;
typedef uint64_t  u64;
typedef uint32_t  u32;
typedef uint16_t  u16;
typedef uint8_t   u8;

typedef intptr_t isize;
typedef int64_t  i64;
typedef int32_t  i32;
typedef int16_t  i16;
typedef int8_t   i8;

typedef double f64;
typedef float  f32;

typedef uint32_t b32;
typedef uint8_t  b8;

typedef struct str {
	char *at;
	usize length;
} str;

#include "memory.h"
#include "util.h"
#include "tokenizer.h"
#include "ast.h"
#include "type.h"
#include "ir.h"
#include "codegen.h"
#include "x86.h"
