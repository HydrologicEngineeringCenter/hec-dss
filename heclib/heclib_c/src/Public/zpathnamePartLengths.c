#include <string.h>

#include "heclib7.h"
#include "hecdssInternal.h"


/**
*  Function:	zpathnamePartLengths
*
*  Use:			Semi-Public
*
*  Description:	Determines the length of each part from a pathname
*
*  Declaration: int zpathnamePartLengths (const char *pathname, size_t pathnameLen, int lengths[], int dimOfLengths);
*
*  Parameters:
*				const char *pathname:  The pathname to get the part lengths from.
*
*				size_t pathnameLen: The number of characters in the pathname (excluding any terminating \0)
*
*				int lengths[6]:  An int array dimensioned to 6 to hold the length of each part
*
*				int dimOfLengths:  The dimension of lengths.  Should be 6
*
*
*  Returns:		int count:
*					The number of parts (should be 6)
*					STATUS_NOT_OKAY if an error
*
*
*	Remarks:	Use zpathnamePartPositions() to determine the pathname part locations
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zpathnamePartLengths(const char *pathname, size_t pathnameLen, int lengths[], int dimOfLengths)
{
	int i;
	int istart;
	int icount;
	int partLen;
	int size;

	size = dimOfLengths;
	if (size < 2) {
		return STATUS_NOT_OKAY;
	}
	if (size > 6) {
		size = 6;
	}

	istart = 0;
	icount = 0;
	for (i=1; i<(int)pathnameLen; i++) {
		if (pathname[i] == '/') {
			partLen = i - istart - 1;
			if (icount < size) {
				lengths[icount++] = partLen;
			}
			istart = i;
		}
	}
	if (icount < size) {
		for (i=icount; i<size; i++) {
			lengths[i] = 0;
		}
	}
	return icount;
}

