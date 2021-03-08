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

	//  Remove leading blanks
	istart = 0;
	for (i = 0; i < len; i++) {
		if (copyFromString[i] == ' ') {
			istart++;
		}
		else {
			break;
		}
	}

	//  All blanks?
	if (istart == len) {
		copyTo = (char *)calloc((size_t)1, CHAR_SIZE);
		return copyTo;
	}

	//  Remove trailing blanks
	if (copyFromString[len-1] == ' ') {
		len--;
		for (i=len; i>0; i--) {
			if (copyFromString[i] != ' ') {
				len = i+1;
				break;
			}
		}
	}


	//   Allocate memory
	size = len - istart + 1;
	copyTo = (char *)malloc((size_t)size);

	if (!copyTo) {
		fprintf(stderr, "\n\nExhausted Memory - mallocAndCopy, size: %d\n\n", len);
		return copyTo;
	}

	ipos = 0;
	for (i= istart; i<len; i++) {
		copyTo[ipos++] = copyFromString[i];
	}

	copyTo[ipos] = '\0';
	return copyTo;
}

