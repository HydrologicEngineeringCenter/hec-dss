#include <stdlib.h>
#include <string.h>
#include <stdio.h>


#include "hecdssInternal.h"

//  Allocate memory for a character array and copy characters into it.
//  Return null if malloc fails.
/**
*  Function:	mallocAndCopyPath
*
*  Use:			Public
*
*  Description:	Same as mallocAndCopy, but ensures that there are (only) seven slashes
*
*  Declaration: char *mallocAndCopyPath(const char *copyFromPathname);
*
*  Parameters:	const char* copyFromPath:  Character string to copy from
*
*
*  Returns:		Copy of the character string with trailing blanks removed.
*					null if malloc fails
*
*  Remarks:		copyFromPath must be null terminated.  Copy will be null terminated.
*
*
*	Author:			Bill Charley
*	Date:			2013
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

char *mallocAndCopyPath(const char *copyFromPath)
{
	char *copyTo;
	int len;
	int i;
	int numberSlashes;
	int number;

	len = (int)strlen(copyFromPath);
	//  Null length string?
	if (len == 0) {
		copyTo = (char *)calloc((size_t)1, CHAR_SIZE);
		return copyTo;
	}
	//  Remove trailing blanks
	if (copyFromPath[len-1] == ' ') {
		len--;
		for (i=len; i>0; i--) {
			if (copyFromPath[i] != ' ') {
				len = i+1;
				break;
			}
		}
	}

	//  Now count the number of slashes
	numberSlashes = 0;
	for (i=0; i<len; i++) {
		if (copyFromPath[i] == '/') {
			numberSlashes++;
			if (numberSlashes == 7) {
				len = i + 1;
				break;
			}
		}
	}
	if (numberSlashes < 7) {
		len += (7 - numberSlashes);
	}

	//   Allocate memory
	len++;
	copyTo = (char *)malloc((size_t)len);

	if (!copyTo) {
		fprintf(stderr, "\n\nExhausted Memory - mallocAndCopy, size: %d\n\n", len);
		return copyTo;
	}

	if (numberSlashes < 7) {
		number = len - (7 - numberSlashes);
		for (i=0; i<number-1; i++) {
			copyTo[i] = copyFromPath[i];
		}
		for (i=number-1; i<len-1; i++) {
			copyTo[i] = '/';
		}
	}
	else {
		for (i=0; i<len-1; i++) {
			copyTo[i] = copyFromPath[i];
		}
	}
	copyTo[len-1] = '\0';
	return copyTo;
}

