int
foo(int n, int m, int k)
{
	return n + m + k;
}

int
main(void)
{
	int a = 420;

	for (int i = 0; i < 10; i = i + 1) {
		a = foo(a, a, a);
	}

	print a;
	return 0;
}
