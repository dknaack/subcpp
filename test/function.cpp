int
foo(char a, int b, int c)
{
	return a + b + c;
}

int
main(void)
{
	int a[5] = { 5 };
	a = foo(a, a, a);
	print a;
	return a;
}
