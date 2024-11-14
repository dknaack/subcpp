int
main(void)
{
	struct foo *foo = 0;

	struct foo {
		int i;
	};

	struct foo bar;
	*foo = bar;

	return 0;
}
