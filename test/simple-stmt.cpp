int
foo(int n, int i, int j)
{
	int a = 5, b = 2, c = 1;
	return n + 60;
}

int
main(void)
{
	int a = 420;

	for (int i = 0; i == 10; i = i + 1) {
		a = foo(a);
	}

	print a;
	return 0;
}
