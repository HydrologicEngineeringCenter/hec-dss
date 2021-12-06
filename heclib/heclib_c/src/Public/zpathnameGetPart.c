#include <string.h>

#include "heclib7.h"
#include "hecdssInternal.h"


/**
*  Function:	zpathnameGetPart
*
*  Use:			Public
*
*  Description:	Gets a part from a pathname
*
*  Declaration: int zpathnameGetPart (const char *pathname, int partPosition, char *part, int sizeofPart);
*
*  Parameters:
*				const char *pathname:  The pathname to get the part from.
*
*				int partPosition:  The part number to get, where A part = 1, B part = 2, etc.
*
*				char *part:  A string to hold the part that is returned.  This should be dimensioned to MAX_PART_SIZE or MAX_F_PART_SIZE chars.
*
*				int sizeofPart:  The size of part.
*
*
*  Returns:		int length:
*					The length of part
*					STATUS_NOT_OKAY if an error (invalid part number or pathname)
*
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zpathnameGetPart (const char *pathname, int partPosition, char *part, size_t sizeofPart)
{
	int positions[7];

	int start;
	int end;
	int partLen;

	if ((partPosition < 1) || (partPosition > 6)) {
		part[0] = '\0';
		return STATUS_NOT_OKAY;
	}

	zpathnamePartPositions(pathname, strlen(pathname), positions, 7);
	
	end = positions[partPosition];	
	start = positions[partPosition-1];
	partLen = end - start -1;

	if (partLen <1) {
		part[0] = '\0';
		return 0;
	}
	
	if (partPosition == 6)  // F part
	{
		if (partLen >= MAX_F_PART_SIZE) {
			partLen = MAX_F_PART_SIZE - 1;
		}
	}
	else if (partLen >= MAX_PART_SIZE) {
		partLen = MAX_PART_SIZE - 1;   //  MAX_PART_SIZE includes space for '\0'
	}
	stringCopy(part, sizeofPart, &pathname[start], (size_t)partLen);
	
	return partLen;
}

