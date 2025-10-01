int
main(void)
{
	int i, j;

	j = 0;
	for (i = 0; i < 10; i++) {
		j += 2;
	}

	return j == 20;
}
