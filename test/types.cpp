int
main(void)
{
	int c = 0;
	int *p = &c;
	*p = 5;
	c = *p;
	print c;
}
