#include <string.h>

#include "heclib.h"

/**
*  Function:	zdelete
*
*  Use:			Public
*
*  Description:	Marks a single record as deleted in a DSS file.  Record is not actually removed until
*					file is squeezed or space is reclaimed.  Version 6 and 7.
*
*  Declaration: int zdelete(long long *ifltab, const char *pathname);
*
*
*	See:	zdeleteInteral() for more
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zdelete(long long *ifltab, const char* pathname)
{
	int len;
	int boolfound;


	if (!pathname) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zdelete_ID, zdssErrorCodes.NULL_PATHNAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "pathname is null");
	}

	if (zgetVersion(ifltab) == 6) {
		len = (int)strlen(pathname);
		zdelet6_(ifltab, pathname, &len, &boolfound, strlen(pathname));
		if (boolfound) {
			return STATUS_RECORD_FOUND;
		}
		else {
			return STATUS_RECORD_NOT_FOUND;
		}
	}
	else {
		return zdeleteInternal(ifltab, pathname, 0);
	}
}

void zdelet7_(long long *ifltab, const char* pathname, int *numberPathname,
	int *found, int lenPathname)
{
	char *cpath;

	cpath = stringFortToC(pathname, lenPathname);
	*found = zdelete(ifltab, cpath);
	free(cpath);
}



