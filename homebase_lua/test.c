#include <stdio.h>

void main(int argc);

void test(int bork)
{
	printf("%s: in test (bork=%d)\n", __func__, bork);
	if (bork == 1) {
		printf("%s: Calling main with argc=0\n", __func__);
		main(0);
	}

	printf("%s: test exiting (bork=%d)\n", __func__, bork);
}

void main(int argc)
{
	printf("%s: in main (argc=%d)\n", __func__, argc);
	printf("%s: calling test (bork=%d)\n", __func__, argc);
	test(argc);
	printf("%s: Exiting main (argc=%d)\n", __func__, argc);
}

