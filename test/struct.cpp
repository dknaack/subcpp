int
main(void)
{
	struct {
		struct {
			int x;
		} a[2];
		int b;
		char c;
	} foo;
	foo.a[1].x = 2;
	foo.b = 1;
	foo.c = 4;
	return 0;
}
