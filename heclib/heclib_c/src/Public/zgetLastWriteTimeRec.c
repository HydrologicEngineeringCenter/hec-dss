#include <string.h>

#include "zdssKeys.h"
#include "hecdss7.h"
#include "hecdssInternal.h"
#include "hecdssFort.h"


/**
*  Function:	zgetLastWriteTimeRec
*
*  Use:			Public
*
*  Description:	Returns the last write time of a record in milliseconds since 01 Jan 1970
*
*  Declaration: long long zgetLastWriteTimeRec(long long *ifltab, const char *pathname);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				const char *pathname
*					The pathname of the individual record to get the last time for.
*					This must be a valid pathname, not a deviate or group.
*
*
*	Retruns:	long long timeMillis
*					The system time of the last write for that record, in milliseconds
*					No conversion of time (e.g., timezone) is done.
*					This time is also updated in the file header.
*
*	See Also:	long long zgetLastWriteTimeFile()
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*
*/
long long zgetLastWriteTimeRec (long long *ifltab, const char *pathname)
{
	int status;
	long long *info;

	if (zgetVersion(ifltab) != 7) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zreadInfo_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
								zgetVersion(ifltab), 0, zdssErrorSeverity.WARNING, pathname, "");
	}

	//  Force the read the info block
	ifltab[zdssKeys.kpathnameHash] = 0;
	status = zreadInfo(ifltab, pathname, 0);
	if (zisError(status)) {
		//  An error code
		status = zerrorUpdate(ifltab, status, DSS_FUNCTION_zreadInfo_ID);
		return status;
	}
	if (status != STATUS_RECORD_FOUND) {
		return (long long)0;
	}
	info = (long long *)ifltab[zdssKeys.kinfo];

	return info[zdssInfoKeys.kinfoLastWriteTime];
}


