struct foo {
	int i;
};

struct bar {
	int i;
};

int
main(void)
{
	struct foo f = {1};
	struct bar b = {0};

	b = f;
	return 0;
}
