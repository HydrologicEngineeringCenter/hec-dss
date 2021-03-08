#include "hecdssInternal.h"

/**
*  Function:	i4toi8
*
*  Use:			Semi-private
*
*  Description:	 A small utility function that converts two ints (integer 4) into a long (integer 8)
*
*  Declaration: long long i4toi8(int int4a, int int4b);
*
*  Parameters:
*				int int4a - The integer to occupy the first part of the long word
*				int int4b - The integer to occupy the second part of the long word
*
*  Returns:		long long i8 - an int 8 with the first int 4 in the first part of the word, and
*					the second in the second part.
*
*  See Also:	i8toi4()
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

long long f4toi8(float f4a, float f4b)
{
	float f4[2];
	long long *i8;
	i8 = (long long*)&f4;
	f4[0] = f4a;
	f4[1] = f4b;
	return *i8;
}

