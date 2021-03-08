#include "heclib.h"
#include "hecdssInternal.h"

//  int zget(long long *ifltab, long long iaddress,  int *iarray, int numberInts);
int zget_(long long *ifltab, long long iaddress,  int *iarray, int *numberwords, int *wordSize)
{
	return zget(ifltab, iaddress, iarray, *numberwords, *wordSize);
}

