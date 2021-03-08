#include <ctype.h>

#include "hecdss7.h"

/**
*  Function:	zfindString
*
*  Use:			Public
*
*  Description:	finds the position of one string within another, case insensitive
*
*  Declaration: zfindString(const char *fullString, int fullStringLength, const char *stringToFind, int stringToFindLength);
*
*  Parameters:	const char *fullString:  The string to search
*
*				int fullStringLength:	The length of fullString
*
*				const char *stringToFind:  The string to look for in fullString
*
*				int stringToFindLength:  The length of stringToFind.
*
*  Returns:		int position:  The position in fullString where stringToFind starts (0 to fullStringLength)
*					-1 if fullString does not contain stringToFind
*
*  Remarks:		Case insensitive, may be either upper or lower, results will be the same
*
*
*	Author:			Bill Charley
*	Date:			2015
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zfindString(const char *fullString, int fullStringLength, const char *stringToFind, int stringToFindLength)
{
	int i;
	int j;
	int i1;
	int i2;
	int position;

	if (stringToFindLength > fullStringLength) {
		return -1;
	}

	for(i=0; i<fullStringLength; i++) {
		position = 0;
		for (j=0; j<stringToFindLength; j++) {
			i1 = toupper(fullString[i+j]);
			i2 = toupper(stringToFind[j]);
			if (i1 != i2) {
				break;
			}
			position++;
		}
		if (position == stringToFindLength) {
			return i;
		}
	}
	//  No match
	return -1;
}

