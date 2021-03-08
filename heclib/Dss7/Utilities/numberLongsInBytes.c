#include "hecdssInternal.h"

/**
*  Function:	numberLongsInBytes
*
*  Use:			Private (Internal)
*
*  Description:	 A small utility function that determines the number of longs (long long)
*					for a number of bytes. (7 bytes = 1 long, 8 = 1, 9 = 2)
*
*  Declaration: int numberLongsInBytes(int numberBytes);
*
*  Parameters:	int numberBytes - Number of bytes
*
*  Returns:		numberLongs - number of longs in the provided number of bytes
*

*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int numberLongsInBytes(int numberBytes)
{
	//  compute the number of longs a given number of bytes occupy
	//  Cannot have fractions....
	int numberLongs;
	if (numberBytes > 0) {
		numberLongs = ((numberBytes - 1) / 8) + 1;
	}
	else {
		numberLongs = 0;
	}
	return numberLongs;
}

