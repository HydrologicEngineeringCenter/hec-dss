#include <string.h>
#include <stdio.h>

#include "hecdssInternal.h"


/**
*  Function:	copyLines
*
*  Use:			Semi-Public
*
*  Description:	Copies a specified number of lines (character strings) from source to destination and returns
*					the number of characters copied.  Also used to just count the number of characters for the number of lines.
*
*  Declaration: int copyLines(char *destination, size_t sizeOfDestination, const char *source, size_t sourceMax, int numberLines);
*
*  Parameters:	char *destination:  Character string to copy into.
*
*				size_t sizeOfDestination:  The size of the destination string.
*					To just count the number of characters for numberLines, set sizeOfDestination to zero (0).
*
*				const char* source:  Character string to copy from.
*
*				size_t sourceMax:  The maximum number of characters in source.
*
*				int numberLines:  The number of lines to copy.  A line is null terminated and may be zero length.
*
*  Returns:		The number of characters for the specified number of lines.
*
*  Remarks:		Lines are always be null terminated.  If destination is too small to hold
*					the number of characters to copy, the number that will be copied will be
*					(sizeOfDestination-1) and the last character in destination will be '\0'.
*				Often used just to count characters (sizeOfDestination = 0)
*
*
*
*	Author:			Bill Charley
*	Date:			2013
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int copyLines(char *destination, size_t sizeOfDestination, const char *source, size_t sourceMax, int numberLines)
{
	int remain;
	int ipos;
	int len;
	int i;
	int numberChars;



	if ((destination == 0) || ((int)sizeOfDestination <= 0)) {
		//  Check for error
		if ((source == 0) || ((int)sourceMax <= 0)) {
			return 0;
		}
	}

	//  Count number chars in source, for numberLines
	ipos = 0;
	if ((source != 0) && ((int)sourceMax > 0)) {
		for (i=0; i<numberLines; i++) {
			remain = (int)sourceMax - ipos;
			assert(ipos >= 0);
			if (ipos >= (int)sourceMax) {
				printf("assert fails with ipos = %d, sourceMax = %d, i = %d\n",ipos,(int)sourceMax, i);
			}
			assert(ipos < (int)sourceMax);
			assert(remain > 0);
			assert(remain <= (int)sourceMax);
			len = strnlen_hec(&source[ipos], remain);
			ipos += len;
			ipos++;
			if (ipos >= (int)sourceMax) {
				break;
			}
		}
	}
	numberChars = ipos;

	if ((destination == 0) || ((int)sizeOfDestination <= 0)) {
		//  Just counting?
		return numberChars;
	}

	if ((int)sourceMax == 0) {
		//  Fill with blank lines ('\0')?
		for (i=0; i<numberLines; i++) {
			if (i >= (int)sizeOfDestination) {
				return i;
			}
			destination[i] = '\0';
		}
		return numberLines;
	}

	//  Copy the number of characters in numberLines
	for (i=0; i<numberChars; i++) {
		if (i >= (int)sizeOfDestination) {
			return i;
		}
		destination[i] = source[i];
	}
	return numberChars;
}

