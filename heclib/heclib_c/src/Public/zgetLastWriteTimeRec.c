#include <string.h>

#include "heclib6.h"
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
long long zgetLastWriteTime6(long long *ifltab, const char *pathname);

long long zgetLastWriteTimeRec (long long *ifltab, const char *pathname)
{
	int status;
	long long *info;

	//  Check for correct DSS Version
	if (zgetVersion(ifltab) == 6) {
		return zgetLastWriteTime6(ifltab,pathname);
	}

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

//  Interface to version 6 (Fortran code)
long long zgetLastWriteTime6(long long *ifltab, const char *pathname)
{
	char cpath[MAX_PATHNAME_SIZE];
	int ibuff[20];
	int istat;
	long long millis;


	copyAndTrim(cpath, MAX_PATHNAME_LENGTH, pathname, strlen(pathname));
	zgetinfo6_(ifltab, cpath, ibuff, &istat, strlen(pathname));

	if (istat != 0) {
		return 0;
	}
	if (ibuff[6] == 0) {
		return 0;
	}

	//  Seconds since Jan 01, 1970
	millis = (long long)(ibuff[6] - JUL_01JAN1970) * SECS_IN_1_DAY;

	//  Add seconds after midnight
	millis += (long long)ibuff[7];

	//  Make into milliseconds
	millis *= 1000L;
	return millis;
}

