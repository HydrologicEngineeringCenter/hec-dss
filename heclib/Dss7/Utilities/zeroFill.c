#include "hecdssInternal.h"

/**
*  Function:	zeroFill
*
*  Description:	A short utility function to fill a character string with zeros
*
*  Declaration: void zeroFill (char *string, size_t len);
*
*  Parameters:	char *string
*					The character string to zero.
*
*				size_t len
*					The number of characters to zero.
*
*
*	Returns:	None
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


void zeroFill (char *string, size_t len)
{
	int i;
	for(i=0; i<(int)len; i++)
		string[i] = 0;
}

