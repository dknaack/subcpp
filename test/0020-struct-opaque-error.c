int
main(void)
{
	struct foo *f;

	struct foo {
		int i;
	};

	return f->i;
}
