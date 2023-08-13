{
	a = 420;
	b = 69;
	while (a) {
		t = b;
		b = a % b;
		a = t;
	}
	a = a;
}
