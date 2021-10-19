#include <string.h>
#include <stdio.h>

#include "hecdssInternal.h"


/**
*  Function:	stringCopy
*
*  Use:			Public
*
*  Description:	Replaces strncpy_s to be cross platform usable and more controlable
*
*  Declaration: int stringCopy (char *destination, size_t sizeOfDestination, const char* source, size_t lenSource)
*
*  Parameters:	char *destination:  Character string to copy into
*
*				size_t sizeOfDestination:  The size of the destination string.  This should be at least one
*						more than the of character that will be copied into destination to hold null pointer
*
*				const char* source:  Character string to copy from
*
*				size_t lenSource:  Number of characters to copy, or if source is null terminated, can be
*						_TRUNCATE or sizeof.
*
*  Returns:		Length (not size) of the destination string.
*
*  Remarks:		destination will always be null terminated.  If destination is too small to hold
*						the number of characters to copy, the number that will be copied will be
*						(sizeOfDestination-1) and the last character in destination will be '\0'.
*
*				An example of an issue with strncpy_s is that when we tried to copy 10
*				characters from a 20 character array and the destination size was only 5,
*				strncpy_s barfed and halted the program.  stringCopy will only copy 4 + null.
*				No barfing.
*
*  See Also:	strncpy_s
*
*
*	Author:			Bill Charley
*	Date:			2013
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int stringCopy (char *destination, size_t sizeOfDestination, const char* source, size_t lenSource)
{
	size_t len;
	int ilen;
	int i;


	if ((lenSource == 0) || (!source)) {
		//  Nothing to copy
		if (sizeOfDestination > 0) {
			destination[0] = '\0';
		}
		return 0;
	}
	if ((sizeOfDestination == 0) || (!destination)) {
		//  No place to copy to
		return 0;
	}

	if (lenSource == _TRUNCATE) lenSource = sizeOfDestination;

	len = sizeOfDestination - 1;
	if (lenSource > 0) {
		if (lenSource < len) {
			len = lenSource;
		}
	}
	else {
		//  Let strncpy take care of truncation and other special cases
#ifdef _MSC_VER
		return strncpy_s(destination, sizeOfDestination, source, lenSource);
#else
		strncpy(destination, source, sizeOfDestination);
		return strlen(destination);
#endif
	}
	lenSource = (int)strlen(source);
	if (lenSource < len) {
		len = lenSource;
	}
	ilen = (int)len;
	for (i=0; i<ilen; i++) {
		destination[i] = source[i];
	}
	destination[ilen] = '\0';

	return ilen;
}


