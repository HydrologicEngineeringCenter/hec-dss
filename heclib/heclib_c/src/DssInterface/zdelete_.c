#include <stdlib.h>

#include "heclib.h"


/**
*  Function:	zdelete - Fortran Interface
*
*  Use:			Public
*
*  Description:	Marks a single record as deleted in a DSS file.  Record is not actually removed until
*					file is squeezed or space is reclaimed.  Version 6 and 7.
*				use zdelete() for calling from C.
*
*  Fortran Interfaces:
*				CALL ZDELETE(IFLTAB, PATHNAME, STATUS)
*					// C translation
*					void zdelete_(long long *ifltab, const char *pathname, int *status, (size_t) lenPathname);
*				CALL ZDELET(IFLTAB, CPATH, NPATH, LFOUND)  //  Version 6 arguments
*					void zdelet_(long long *ifltab, const char *pathname, int *numberPathname,
*								 int *boolfound, (size_t) lenPathname);
*
*
*	See:	zdeleteInternal() for description
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

void zdelete_(long long *ifltab, const char *pathname, int *status, size_t lenPathname)
{
	int boolfound;
	char *path;
	int len;

	if (zgetVersion(ifltab) == 6) {
		len = (int)lenPathname;
		zdelet6_(ifltab, pathname, &len, &boolfound, lenPathname);
		if (boolfound) {
			*status = STATUS_RECORD_FOUND;
		}
		else {
			*status = STATUS_RECORD_NOT_FOUND;
		}
	}
	else {
		path = stringFortToC(pathname, lenPathname);
		*status = zdeleteInternal(ifltab, path, 0);
		free(path);
	}
}

//  Version 6 fully compatible
void zdelet_(long long *ifltab, const char *pathname, int *numberPathname,
			 int *boolfound, size_t lenPathname)
{
	int status;
	char *path;

	if (zgetVersion(ifltab) == 6) {
		zdelet6_(ifltab, pathname, numberPathname, boolfound, lenPathname);
	}
	else {
		path = stringFortToC(pathname, lenPathname);
		status = zdeleteInternal(ifltab, path, 0);
		free(path);
		if (status == STATUS_RECORD_FOUND) {
			*boolfound = 1;
		}
		else {
			*boolfound = 0;
		}
	}
}

