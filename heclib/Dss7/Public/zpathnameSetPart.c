#include <string.h>

#include "hecdss7.h"
#include "hecdssInternal.h"



/**
*  Function:	zpathnameSetPart
*
*  Use:			Public
*
*  Description:	Set (Replaces) a part of a pathname
*
*  Declaration: int zpathnameSetPart (char *pathname, size_t sizeofPathname, const char *part, int partPosition);
*
*  Parameters:
*				const char *pathname:  The pathname to set the part into.  This must be a valid pathname, as this
*					function only replaces a part.
*
*				size_t sizeofPathname:  The size (dimension) of the pathname.  This should not be the length, but the size.
*
*				const char *part:  The part that will be set in pathname.
*
*				int partPosition:  The part number to set, where A part = 1, B part = 2, etc.
*
*
*
*  Returns:		int status:
*					STATUS_OKAY
*					STATUS_NOT_OKAY for error (sizeofPathname too small or invalid position)
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

int zpathnameSetPart (char *pathname, size_t sizeofPathname, const char *part, int partPosition)
{
	int positions[7];

	int start;
	int end;
	int endPath;
	int oldLength;
	int newLength;
	int i;
	int icount;
	int diff;

	if ((partPosition < 1) || (partPosition > 6)) {
		return STATUS_NOT_OKAY;
	}

	//  Determine the position of the part and
	//  the lengths of the old and new parts
	zpathnamePartPositions(pathname, strlen(pathname), positions, 7);
	end = positions[partPosition];
	start = positions[partPosition-1];
	oldLength = end - start -1;
	newLength = (int)strlen(part);

	if (newLength == oldLength) {
		//  Simply copy the new part into the old part's position
		icount = start;
		for (i=0; i<newLength; i++) {
			pathname[icount++] = part[i];
		}
	}
	else if (newLength < oldLength) {
		//  First, copy the new part into the old part's position
		diff = oldLength - newLength;
		endPath = positions[6] - diff;
		icount = start;
		for (i=0; i<newLength; i++) {
			pathname[icount++] = part[i];
		}
		//  Now move the remainder of the pathname forward
		//  the difference between the part lengths
		for (i=icount; i<endPath; i++) {
			pathname[i] = pathname[i+diff];
		}
		//  Null terminate
		pathname[endPath] = '\0';
	}
	else if (newLength > oldLength) {
		//  This is essentially the reverse of above
		//  First we move the end of the pathname back
		//  the difference.  (where end is the pathname after the part)
		//  To do this, we must increment negatively.
		diff = newLength - oldLength;
		endPath = positions[6] + diff;
		if (endPath > (int)sizeofPathname) {
			//  We don't have enough room to expand the pathname!
			return STATUS_NOT_OKAY;
		}
		//  Null terminate
		pathname[endPath] = '\0';
		endPath--;
		//  Move the end backwards
		for (i=endPath; i>=end; i--) {
			pathname[i] = pathname[i-diff];
		}
		//  Now copy the new part into the old part's position
		icount = start;
		for (i=0; i<newLength; i++) {
			pathname[icount++] = part[i];
		}
	}
	return STATUS_OKAY;
}

