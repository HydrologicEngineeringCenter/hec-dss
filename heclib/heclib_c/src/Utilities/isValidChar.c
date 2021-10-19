#include <ctype.h>

#include "hecdssInternal.h"

/**
*  Function:		isValidChar
*
*  Use:				Public
*
*	Declariation	int isValidChar(char ich);
*
*  Description:	 A small utility function that returns 1 if the character
*					passed in is a valid character (usually pathname character)
*					or 0, if not.
*					Based on Extended ASCII, so negatives are okay
*
*
*  Parameters:	char ich - a single character
*
*  Returns:		1, if valid, 0 if not.
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int isValidChar(char ich)
{
	if (((int)ich >= 0) && ((int)ich < 32)) {
		return 0;
	}
	if ((int)ich == 127) {
		return 0;
	}
	if ((int)ich > 254) {
		return 0;
	}
	if ((int)ich < -128) {
		return 0;
	}
	return 1;
}

