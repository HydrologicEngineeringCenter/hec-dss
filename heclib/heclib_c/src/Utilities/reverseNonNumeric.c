#include <ctype.h>
#include <string.h>

#include "heclib7.h"

/**
*  Function:	reverseNonNumeric
*
*  Use:			public
*
*  Description:	 Finds the last non-numeric character of a string.
*
*  Declaration:	 char *reverseNonNumeric(const char *string);
*
*  Parameters:	const char *string
*					A null-terminated string that contains may contain a mix of alpha and numeric characters.
*
*  Returns:		char*  - the last non-numeric character
*					0 - if all numeric
*
*
*  Example:		"23Mar1994"
*					returns "r"
*				"1234"
*					returns 0  (null)
*
*  Remarks:		An ASCII dependent function
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

char *reverseNonNumeric(const char *string)
{
	int i;
	int len;

	len = (int)strlen(string) - 1;
	for (i=len; i>=0; i--) {
		if ((string[i] < 48) || (string[i] > 57)) {
			return (char *)(string + i);
		}
	}
	return 0;
}

