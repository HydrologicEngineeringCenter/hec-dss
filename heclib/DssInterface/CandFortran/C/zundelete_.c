#include <stdlib.h>

#include "heclib.h"

/**
*  Function:	zundelete - Fortran Interface
*
*  Use:			Public
*
*  Description:	Marks a single record as deleted in a DSS file.  Record is not actually removed until
*				file is squeezed or space is reclaimed.  Version 6 and 7.
*				use zundelete() for calling from C.
*
*  Fortran Interfaces:
*				CALL ZUNDELETE(IFLTAB, PATHNAME, STATUS)
*					// C translation
*					void zundelete_(long long *ifltab, const char *pathname, int *status, size_t lenPathname);
*				CALL ZUNDEL(IFLTAB, CPATH, NPATH, ISTATUS)  //  Version 6 arguments
*					void zundel_(long long *ifltab, const char *pathname, int *numberPathname,
*								 int *status, int lenPathname);
*
*
*	See:	zundelete() for description
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

void zundelete_(long long *ifltab, const char *pathname, int *status, size_t lenPathname)
{
	char *path;

	if (zgetVersion(ifltab) == 6) {
		zundel6_(ifltab, pathname, (int *)&lenPathname, status, lenPathname);
	}
	else {
		path = stringFortToC(pathname, lenPathname);
		*status = zdeleteInternal(ifltab, path, 1);
		free(path);
	}
}

//  Depreciated version
void zundel_(long long *ifltab, const char *pathname, int *numberPathname,
			 int *status, size_t lenPathname)
{
	char *path;

	if (zgetVersion(ifltab) == 6) {
		zundel6_(ifltab, pathname, numberPathname, status, lenPathname);
	}
	else {
		path = stringFortToC(pathname, lenPathname);
		*status = zdeleteInternal(ifltab, path, 1);
		free(path);
	}
}




