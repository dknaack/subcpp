#define TRAP() do {} while (0)
#define main main_
#include "../src/main.c"
#undef main

static void
test_parse_initializer(void)
{
	static struct {
		str input;
		b32 result;
	} test_cases[] = {
		{S("{}"), false},
		{S("{{}}"), false},
		{S("{0,,}"), false},
		{S("{0}"), true},
		{S("{0, {0}}"), true},
		{S("{0, {0,}, {2},}"), true},
		{S("{1.2, {0,}, {2},}"), true},
	};

	u32 error_count = 0;
	for (isize i = 0; i < LENGTH(test_cases); i++) {
		arena *perm = new_arena(1000 * 1000);
		lexer t = tokenize_str(test_cases[i].input, perm);
		ast_pool pool = {0};
		parse_initializer(&t, &pool);
		if (t.error == test_cases[i].result) {
			printf("%s: FAIL [%ld/%ld] \"%s\"\n", __func__, i+1, LENGTH(test_cases),
				test_cases[i].input.at);
			error_count++;
		}
	}

	if (error_count == 0) {
		printf("%s: PASS\n", __func__);
		fflush(stdout);
	}
}

static void
test_parse_expr(void)
{
	static struct {
		str input;
		b32 result;
	} test_cases[] = {
		{S("1 + 2"), true},
		{S("++-2"), true},
		{S("-+0"), true},
		{S("0[]"), false},
		{S("0()"), true},
		{S("0[0]()"), true},
	};

	u32 error_count = 0;
	for (isize i = 0; i < LENGTH(test_cases); i++) {
		arena *perm = new_arena(1000 * 1000);
		ast_pool pool = {0};
		lexer t = tokenize_str(test_cases[i].input, perm);
		parse_assign_expr(&t, &pool);
		if (t.error == test_cases[i].result) {
			printf("%s: FAIL [%ld/%ld] \"%s\"\n", __func__, i+1, LENGTH(test_cases),
				test_cases[i].input.at);
			error_count++;
		}

		free(perm);
	}

	if (error_count == 0) {
		printf("%s: PASS\n", __func__);
		fflush(stdout);
	}
}

int
main(void)
{
	test_parse_expr();
	test_parse_initializer();
	return 0;
}
