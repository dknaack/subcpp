#include <stdbool.h>
#include <stdint.h>

#define TRAP() __builtin_trap()
#define ASSERT(x) do { if (!(x)) TRAP(); } while (0)

struct string {
	char *at;
	size_t length;
};
