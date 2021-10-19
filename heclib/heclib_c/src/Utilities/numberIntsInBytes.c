#include "hecdssInternal.h"

/**
*  Function:	numberIntsInBytes
*
*  Use:			Private (Internal)
*
*  Description:	 A small utility function that determines the number of ints
*					for a number of bytes. (3 bytes = 1 int, 4 = 1, 5 = 2)
*
*  Declaration: int numberIntsInBytes(int numberBytes);
*
*  Parameters:	int numberBytes - Number of bytes
*
*  Returns:		numberInts - number of ints in the provided number of bytes
*

*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int numberIntsInBytes(int numberBytes)
{
	//  compute the number of ints a given number of bytes
	//  Cannot have fractions....
	int numberInts;
	if (numberBytes > 0) {
		numberInts = ((numberBytes - 1) / 4) + 1;
	}
	else {
		numberInts = 0;
	}
	return numberInts;
}

