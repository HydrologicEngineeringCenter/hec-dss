#include "heclib.h"
#include "hecdssInternal.h"


void zdebugout7(long long *ifltab, long long *iarray, long long address, int len)
{
	long long iadd;

	iadd = address;
	zdebugout7_(ifltab, iarray, &iadd, &len);
}

