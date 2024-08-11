#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#ifndef TRAP
#ifdef DEBUG
#define TRAP() __builtin_trap()
#else
#define TRAP() (void)0
#endif
#endif
#define BREAK() asm("int3; nop")
#define BREAK_IF(x) do { if ((x)) BREAK(); } while (0)
#define ASSERT(x) do { if (!(x)) TRAP(); } while (0)
#define LENGTH(x) ((isize)(sizeof(x)/sizeof((x)[0])))
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define MAX(a, b) ((a) > (b) ? (a) : (b))
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
	isize length;
} str;

typedef struct {
	i32 value;
} ast_id;

#include "memory.h"
#include "util.h"
#include "lexer.h"
#include "check.h"
#include "ast.h"
#include "ir.h"
#include "codegen.h"
#include "regalloc.h"
#include "x86.h"
