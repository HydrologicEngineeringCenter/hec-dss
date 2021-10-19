#include <string.h>

#include "hecdssInternal.h"


//  Returns the position of the first non-blank character (usually 0)
//  and the position of the last non-blank character (excluding null)
//  if all blank or zero length string, returns 0, 0

//  Note:  returns positions, not lengths

/**
*  Function:	trimPositions
*
*  Use:			Public
*
*  Description:	Determines the first and last non-blank character positions of a string
*
*  Declaration: int trimPositions(const char *string, int *start, int *end);
*
*  Parameters:	const char *string
*					The character string to find the first and last non-blank character in (exclusive of '\0')
*
*				int *start
*					Returns the position of the first non-blank character. "ABC" returns 0, " ABC" returns 1.
*
*				int *end
*					Returns the position of the last non-blank character (excluding null). "ABC" returns 2, " ABC " returns 3.
*
*
*	Returns:	int len
*					The length of the string without leading or trailing blanks
*					Returns 0 if all blanks.
*
*	Example:
*					stringCopy(mystr, sizeof(mystr), "  My String  ", _TRUNCATE);
*					i = trimPositions(mystr, &start, &end);
*					//  i == 9,  start == 2,  y == 10
*					if (i) {
*						end++;
*						mystr[end] = '\0';
*						str = mallocAndCopy(&mystr[start]);
*					}
*					else {
*						str = mallocAndCopy("\0");
*					}
*					//  str = "My String"
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int trimPositions(const char *string, int *start, int *end)
{
	int len;
	int i;

	len = (int)strlen(string);
	if (len == 0) {
		*start = 0;
		*end = 0;
		return 0;
	}

	*start = -1;
	for (i=0; i<len; i++) {
		if (string[i] != ' ') {
			*start = i;
			break;
		}
	}

	if (*start == -1) {
		*start = 0;
		*end = 0;
		return 0;
	}

	for (i=len-1; i>=0; i--) {
		if (string[i] != ' ') {
			*end = i;
			break;
		}
	}
	return *end - *start + 1;
}

