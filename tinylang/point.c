#include <stdio.h>
#include <stdlib.h>


extern void _t5Point7AssignX(int a);

struct Point { long x, y; };
extern struct Point _t5Point1p;


int main(
	int		argc,
	char		*argv[]
	)
{
int a = atoi(argv[1]);

_t5Point7AssignX(a);

fprintf(stdout, "the value read was %ld\n", _t5Point1p.x);

return 0;
}

