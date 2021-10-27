#include "hecdssInternal.h"


/**
*  Function:	zpathnameAddPart
*
*  Use:			Private
*
*  Description:	Trims and appends a part for building a pathname and adds trailing slash.
*
*  Declaration: int zpathnameAddPart(const char *part, int count, char *pathname, size_t sizeofPathname);
*
*  Parameters:
*				const char *part:  The pathname part to append (input).  This should NOT contain slashes (/)
*
*				int count:  The current length of the pathname being built (input).
*
*				char *pathname:	The pathname being built (input-output).
*
*				size_t sizeofPathname:  The size of pathname (input).
*
*
*
*  Returns:		int count:	The length or count of the resulting (partial) pathname
*
*
*  Called By:	zpathnameForm()
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int zpathnameAddPart(const char *part, int count, char *pathname, size_t sizeofPathname)
{
	int len;
	int start;
	int end;
	int i;

	len = trimPositions(part, &start, &end);

	if (len > 0) {
		for (i=start; i<=end; i++) {
			if (count < (int)sizeofPathname) {
				pathname[count++] = part[i];
			}
			else {
				return count;
			}
		}
	}

	if (count < (int)sizeofPathname) {
		pathname[count++] = '/';
	}

	return count;
}

