int
foo(int n)
{
	return n + 60;
}

int
main(void)
{
	int a = 420;

	for (int i = 0; i - 10; i = i + 1) {
		a = foo(a);
	}

	print a;
	return 0;
}
