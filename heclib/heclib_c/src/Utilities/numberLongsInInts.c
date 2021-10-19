#include "hecdssInternal.h"

/**
*  Function:	numberLongsInInts
*
*  Use:			Private (Internal)
*
*  Description:	 A small utility function that determines the number of longs
*					for a number of ints words. (3 ints = 2 longs, 4 = 2)
*
*  Declaration: int numberLongsInInts (int numberInts);
*
*  Parameters:	int numberInts - Number of ints
*
*  Returns:		numberLongs - number of longs in the provided number of ints
*

*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int numberLongsInInts (int numberInts)
{
	//  compute the number of longs a given number of ints occupy
	//  Cannot have fractions....
	int numberLongs;
	if (numberInts > 0) {
		numberLongs = ((numberInts - 1) / 2) + 1;
	}
	else {
		numberLongs = 0;
	}
	return numberLongs;
}

