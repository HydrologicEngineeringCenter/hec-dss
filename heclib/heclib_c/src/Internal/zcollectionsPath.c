#include <ctype.h>

#include "hecdssInternal.h"

/**
*  Function:	zcollectionsPath
*
*  Use:			Private (Internal)
*
*  Description:	Determines if a pathname is a collections path
*
*  Declaration: int zcollectionsPath(char* pathname, size_t pathnameLength);
*
*  Parameters:
*				char *pathname (input and output!)
*					The pathname to determine if a collections path, and change sequence to "XXXXXX"
*
*				size_t pathnameLength
*					The length of the pathname
*
*
*	Returns:	int booleanIsCollection
*					1 if is a collections path, otherwise 0.
*
*	Note:		Collections are identified by an F part of /C:000000|REST OF FPART/
*				Where 00000 is generally a sequence number, for example
*				/YUBA/SMARTSVILLE/FLOW/01JAN1997/1HOUR/C:000042|OPERATION A/
*
*
*
*
*  Called By:	zhash only
*
*	Author:			Bill Charley, 2010
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zcollectionsPath(char* pathname, size_t pathnameLength)
{
	int position;

	if ((int)pathnameLength < 12)
		return 0;

	position = zisaCollection(pathname, pathnameLength);
	if (!position) {
		return 0;
	}
	return 1;
}

