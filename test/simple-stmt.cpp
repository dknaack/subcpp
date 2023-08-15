{
	a = 420;
	b = 69;
	while (b) {
		r = a % b;
		a = b;
		b = r;
	}

	return a;
}
