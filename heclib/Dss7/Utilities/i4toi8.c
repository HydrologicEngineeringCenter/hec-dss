#include "hecdssInternal.h"

/**
*  Function:	i4toi8
*
*  Use:			Semi-private
*
*  Description:	 A small utility function that converts two ints (integer 4) into a long (integer 8)
*
*  unsigned long long i4toi8(int int4a, int int4b);
*
*  Parameters:
*				int int4a - The integer to occupy the first part of the long word
*				int int4b - The integer to occupy the second part of the long word
*
*  Returns:		unsigned long long - an int 8 with the first int 4 in the first part of the word, and
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

unsigned long long i4toi8(int int4a, int int4b)
{
	int i4[2];
	long long *i8;
	i8 = (long long*)&i4;

	if (getEndian()) {
		//  Big Endian
		i4[1] = int4a;
		i4[0] = int4b;
	}
	else {
		//  Little Endian
		i4[0] = int4a;
		i4[1] = int4b;
	}

	return *i8;
	/*
	unsigned long long i8 = 0LL;
	unsigned long long b8;
	unsigned int a4;
	b8 = (unsigned long long)int4b;
	a4 = int4a;
	i8 = (unsigned long long)(b8 << 32);
	i8 += a4;
	return i8;
	*/
}

