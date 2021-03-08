#include "hecdssInternal.h"

/**
*  Function:	numberIntsInLongs
*
*  Use:			Private (Internal)
*
*  Description:	 A small utility function that determines the number of ints
*					for a number of long words. (3 longs = 6 ints, 4 = 8)
*
*  Declaration: int numberIntsInLongs (long long numberLongs);
*
*  Parameters:	long long numberLongs - Number of longs
*
*  Returns:		numberInts - number of ints in the provided number of longs
*

*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int numberIntsInLongs (long long numberLongs)
{
	int numberInts;
	if (numberLongs > 0) {
		numberInts = (int)(numberLongs * 2);
	}
	else {
		numberInts = 0;
	}
	return numberInts;
}

