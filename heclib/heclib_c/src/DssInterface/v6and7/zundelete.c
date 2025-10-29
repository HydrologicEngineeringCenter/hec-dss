#include <string.h>

#include "heclib.h"

/**
*  Function:	zundelete
*
*  Use:			Public
*
*  Description:	Marks a single deleted as good in a DSS file.   Version 6 and 7.
*
*  Declaration: int zundelete(long long *ifltab, const char *pathname);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				const char *pathname
*					The pathname of the record to be undeleted.
*
*	Note:		A deleted record cannot be undeleted in version 7 file that uses space reclamation.
*					If you really need the data, you can try turning reclamation off, then undelete.
*					If space has been reclaimed since deletion, then the data set may be bad.
*					A file squeeze will remove all deleted records (and cannot be undeleted after that).
*
*	Remarks:	A delete only marks a record as deleted.  It physically deleted on a squeeze or the
*					area is reused by reclamation.
*					You can use the catalog function to determine deleted records in a DSS file.  You can
*					call zundeleteAll to undelete all records that have been deleted.
*
*	See:	zdeleteInteral() and zundeleteAll()  for more
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zundelete(long long *ifltab, const char* pathname)
{

	if (!pathname) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zundelete_ID, zdssErrorCodes.NULL_PATHNAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "pathname is null");
	}

	//  zdeleteInternal also does undeletes
	int status =  zdeleteInternal(ifltab, pathname, 1);
	return status;
}



