#include "hecdssInternal.h"

/**
*  Function:	stringFill
*
*  Description:	A simple utility function to fill a character string with a single character
*
*  Declaration: void stringFill (char *string, char cval, size_t len);
*
*  Parameters:	char *string
*					The character string to zero.
*
*				char cval
*					The character to fill the string with.
*
*				size_t len
*					The number of characters to fill.
*
*
*	Returns:	None
*
*
*	Author:			Bill Charley
*	Date:			2015
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


void stringFill (char *string, char cval, size_t len)
{
	int i;
	for(i=0; i<(int)len; i++)
		string[i] = cval;
}

