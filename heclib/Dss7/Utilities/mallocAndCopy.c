#include <stdlib.h>
#include <string.h>
#include <stdio.h>


#include "hecdssInternal.h"

//  Allocate memory for a character array and copy characters into it.
//  Return null if malloc fails.
/**
*  Function:	mallocAndCopy
*
*  Use:			Public
*
*  Description:	Allocates space and then copies the provided string into it, removing any trailing blanks
*
*  Declaration: char *mallocAndCopy(const char *copyFrom);
*
*  Parameters:	const char* copyFrom:  Character string to copy from
*
*
*  Returns:		Copy of the character string with trailing blanks removed.
*					null if malloc fails
*
*  Remarks:		copyFrom must be null terminated.  Copy will be null terminated.
*
*
*	Author:			Bill Charley
*	Date:			2013
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

char *mallocAndCopy(const char *copyFrom)
{
	char *copyTo;
	int len;
	int i;

	len = (int)strlen(copyFrom);
	//  Null length string?
	if (len == 0) {
		copyTo = (char *)calloc((size_t)1, CHAR_SIZE);
		return copyTo;
	}
	//  Look for illegal characters
	for (i=0; i<len-1; i++) {
		if ((int)copyFrom[i] <= 0) {
			len = i + 1;
			break;
		}
	}
	//  Remove trailing blanks
	if (copyFrom[len-1] == ' ') {
		len--;
		for (i=len; i>0; i--) {
			if (copyFrom[i] != ' ') {
				len = i+1;
				break;
			}
		}
	}
	len++;
	copyTo = (char *)malloc((size_t)len);

	if (!copyTo) {
		//fprintf(stderr, "\n\nExhausted Memory - mallocAndCopy, size: %d\n\n", len);
		return copyTo;
	}

	for (i=0; i<len-1; i++) {
		copyTo[i] = copyFrom[i];
	}
	copyTo[len-1] = '\0';
	return copyTo;
}

