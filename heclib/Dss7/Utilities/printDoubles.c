#include <stdio.h>


//  Data types are mixed, so we avoid header declaration to avoid
//  a compiler error


//  Debug routine to print doubles (from ints)
void printDoubles(double *values, int number)
{
	int i;
	int numb;

	if (number <1) {
		numb = -number;
		printf(" %d,  %8.2f\n", numb, values[0]);
	}
	else {
		for (i=0; i<number; i++) {
			printf(" %d,  %8.2f\n", i, values[i]);
		}
	}
}

