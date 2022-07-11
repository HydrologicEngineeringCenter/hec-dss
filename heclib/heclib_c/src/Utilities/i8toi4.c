#include "hecdssInternal.h"

/**
*  Function:	i8toi4
*
*  Use:			Semi-private
*
*  Description:	 A small utility function that converts a long (integer 8) into two ints (integer 4)
*
*  Declaration: void i8toi4(unsigned long long integer8, int *int4a, int *int4b);
*
*  Parameters:
*				long long integer8 (input) - the long word to break into two ints
*				int int4a (output) - The integer from the first part of the long word
*				int int4b (output) - The integer from the second part of the long word
*
*  Returns:		None
*
*  See Also:	i4toi8()
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

void i8toi4(unsigned long long integer8, int *int4a, int *int4b)
{
	int *i4;

	i4 = (int *)&integer8;

	if (bigEndian()) {
		//  Big Endian
		*int4a = i4[1];
		*int4b = i4[0];
	}
	else {
		//  Little Endian
		*int4a = i4[0];
		*int4b = i4[1];
	}


	/*
	unsigned long long i8;

	i8 = integer8 >> 32;
	*int4b = (int)i8;
	i8 = integer8 << 32;
	i8 = i8 >> 32;
	*int4a = (int)i8;
	*/
}

