#include <stdlib.h>
#include <string.h>
#include <stdio.h>


#include "hecdssInternal.h"

//  Allocate memory for a character array and copy characters into it.
//  Return null if malloc fails.
/**
*  Function:	stringFortToC
*
*  Use:			Public
*
*  Description:	A function to convert a Fortran string into C.
*					Allocates space and then copies the provided string len character long into it,
*					removing any trailing blanks
*
*  Declaration: char *stringFortToC(const char *fortString, size_t fortStringLen);
*
*  Parameters:	const char* fortString:  Character string from Fortran to copy from
*
*				size_t fortStringLen: The size or length of the fortString.
*
*
*  Returns:		Copy of the null terminated character string with trailing blanks removed.
*					null if malloc fails
*
*  Remarks:		fortString does not need to be null terminated.  The return string will be
*					null terminated and one character longer to hold the null
*
*
*	Author:			Bill Charley
*	Date:			2013
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

char *stringFortToC(const char *fortString, size_t fortStringLen)
{
	char *copyTo;
	int len;
	int i;

	len = (int)fortStringLen;
	//  Null length string?
	if (len == 0) {
		copyTo = (char *)calloc((size_t)1, CHAR_SIZE);
		return copyTo;
	}

	//  Remove trailing blanks
	if (fortString[len-1] == ' ') {
		len--;
		for (i=len; i>=0; i--) {
			if (fortString[i] != ' ') {
				len = i+1;
				break;
			}
		}
	}
	len++;
	copyTo = (char *)malloc((size_t)len);

	if (!copyTo) {
		//fprintf(stderr, "\n\nExhausted Memory - stringFortToC, size: %d\n\n", len);
		return copyTo;
	}

	for (i=0; i<len-1; i++) {
		copyTo[i] = fortString[i];
	}
	copyTo[len-1] = '\0';
	return copyTo;
}

