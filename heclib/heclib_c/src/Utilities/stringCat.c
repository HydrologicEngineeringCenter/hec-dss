#include <string.h>
#include <stdio.h>

#include "hecdssInternal.h"


/**
*  Function:	stringCat
*
*  Use:			Public
*
*  Description:	Replaces strncat_s for compatibility between MS and Unix C
*
*  Declaration: int stringCat (char *destination, size_t sizeOfDestination, const char* source, size_t lenSource)
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
*  Returns:		0 if source was copied without truncation.
*
*
*	Author:			Bill Charley
*	Date:			2017
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int stringCat (char *destination, size_t sizeOfDestination, const char* source, size_t lenSource)
{
	if( source == NULL)
	 return -1;
#ifdef _MSC_VER
	//  Avoid an overflow error
	if (lenSource != _TRUNCATE) {
		if (lenSource > sizeOfDestination) {
			lenSource = _TRUNCATE;
		}
	}
	return strncat_s(destination, sizeOfDestination, source, lenSource);
#else
	size_t remainingSpace = sizeOfDestination- strlen(destination)-1;
		if( remainingSpace >0)
		{
          	strncat(destination, source, remainingSpace);
			  return 0;
		}
		else{
	     return -1;
		}
#endif
}


