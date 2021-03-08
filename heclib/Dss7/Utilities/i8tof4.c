#include "hecdssInternal.h"

/**
*  Function:	i8tof4
*
*  Use:			Semi-private
*
*  Description:	 A small utility function that converts a long (integer 8) into two floats
*
*  Declaration: void i8toi4(long long integer8, int *int4a, int *int4b);
*
*  Parameters:
*				long long integer8 (input) - the long word to break into two ints
*				float f4a (output) - The float from the first part of the long word
*				float f4b (output) - The float from the second part of the long word
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

void i8tof4(long long integer8, float *f4a, float *f4b)
{
	float *f4;
	f4 = (float *)&integer8;
	*f4a = f4[0];
	*f4b = f4[1];

}

