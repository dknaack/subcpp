#if 0
#if 1
X
#endif
#elif 0
X
#elif 1
#if 0
#else
int i = 0;
#endif

#else
X
#endif

#if 0
X
#else
int main() { return i; }
#endif
