int
main(void)
{
	int x, *p;
	x = 1;
	p = &x;
	*p = 0;
	return x;
}
