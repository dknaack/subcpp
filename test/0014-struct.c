struct foo {
	int i;
};

int
main(void)
{
	struct foo f1, f2;
	f1.i = 42;
	f2 = f1;

	return f1.i - f2.i;
}
