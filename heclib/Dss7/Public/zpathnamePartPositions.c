#include <string.h>

#include "hecdssInternal.h"
#include "hecdss7.h"


/**
*  Function:	zpathnamePartPositions
*
*  Use:			Public
*
*  Description:	Determines the starting location of each part from a pathname (first char after slash)
*
*  Declaration: int zpathnamePartPositions (const char *pathname, size_t pathnameLen, int positions[7], int dimOfPositions);
*
*  Parameters:
*				const char *pathname:  The pathname to get the part positions from.
*
*				size_t pathnameLen: The number of characters in the pathname (excluding any terminating \0)
*
*				int positions[7]:  An int array dimensioned to 7 to hold the starting position of each part.
*					This should be dimensioned to 7 to get the end of the F part
*
*				int dimOfPositions:  The dimension of positions.  Should be 7.
*
*
*  Returns:		int count:
*					The number of positions (should be 7)
*					STATUS_NOT_OKAY if an error
*
*
*	Remarks:	The 7th position contains the character location one char after the last slash (usually '\0' position)
*				For the B part:
*					Start of B part = positions[1];
*					End of B part = positions[2] - 2;
*					Length of B part = positions[2] - positions[1] - 1;
*					B part = pathname(positions[1], positions[2] - 2)
*				For the F part:
*					Start of F part = positions[6];
*					End of F part = positions[7] - 2;
*					Length of F part = positions[7] - positions[6] - 1;
*					F part = pathname(positions[7], positions[7] - 2)
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int zpathnamePartPositions (const char *pathname, size_t pathnameLen, int positions[], int dimOfPositions)
{
	int i;
	int size;
	int number;

	size = dimOfPositions - 1;
	if (size < 2) {
		return STATUS_NOT_OKAY;
	}

	number = zpathnamePartLengths(pathname, pathnameLen, &positions[1], size);
	if (number < 1) {
		return number;
	}

	if (size > 7) {
		size = 7;
	}
	if (number+1 > size) {
		size = number + 1;
	}
	number++;
	positions[0] = 1;
	for (i=1; i<size; i++) {
		positions[i] = positions[i-1] + positions[i] + 1;
	}
	return number;
}

