int
foo(void)
{
	return 60;
}

int
main(void)
{
	int a;
	int i;

	a = 420;
	for (i = 0; i - 10; i = i + 1) {
		a = a + foo();
	}

	print a;
	return 0;
}
