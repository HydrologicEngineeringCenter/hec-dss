#include <stdlib.h>
#include <string.h>
#include <stdio.h>


#include "hecdssInternal.h"

//  Allocate memory for a character array and copy characters into it.
//  Return null if malloc fails.
/**
*  Function:	mallocAndCopyTrim
*
*  Use:			Public
*
*  Description:	Same as mallocAndCopy, but removes any leading or trailing blanks
*
*  Declaration: char *mallocAndCopyTrim(const char *copyFromString);
*
*  Parameters:	const char* copyFromString:  Character string to copy from
*
*
*  Returns:		Copy of the character string with  any leading or trailing blanks removed.
*					null if malloc fails
*
*  Remarks:		copyFromString must be null terminated.  Copy will be null terminated.
*
*
*	Author:			Bill Charley
*	Date:			2018
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

char *mallocAndCopyTrim(const char *copyFromString)
{
	char *copyTo;
	int len;
	int i;
	int istart;
	int end;
	int size;
	int ipos;


	if (!copyFromString) {
		copyTo = (char *)calloc((size_t)1, CHAR_SIZE);
		return copyTo;
	}

	len = (int)strlen(copyFromString);

	//  Null length string?
	if (len == 0) {
		copyTo = (char *)calloc((size_t)1, CHAR_SIZE);
		return copyTo;
	}
	int trimLen = trimPositions(copyFromString, &istart, &end);


	//  All blanks?
	if (trimLen==0) {
		copyTo = (char *)calloc((size_t)1, CHAR_SIZE);
		return copyTo;
	}


	//   Allocate memory
	size = end - istart + 2;
	copyTo = (char *)calloc((size_t)size, CHAR_SIZE);

	if (!copyTo) {
		fprintf(stderr, "\n\nExhausted Memory - mallocAndCopy, size: %d\n\n", len);
		return copyTo;
	}

	ipos = 0;
	for (i= istart; i<=end; i++) {
		copyTo[ipos++] = copyFromString[i];
	}

	copyTo[ipos] = '\0';
	return copyTo;
}

