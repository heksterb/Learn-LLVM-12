#include <stdio.h>
#include <stdlib.h>


extern int _t3Gcd3GCD(int a, int b);


int main(
	int		argc,
	char		*argv[]
	)
{
int
	a = atoi(argv[1]),
	b = atoi(argv[2]);

int c = _t3Gcd3GCD(a, b);

fprintf(stdout, "the greatest common denominator of %d and %d is %d\n", a, b, c);

return 0;
}

