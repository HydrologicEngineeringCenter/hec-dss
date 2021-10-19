#include <string.h>

#include "heclib6.h"
#include "zdssKeys.h"
#include "hecdss7.h"
#include "hecdssInternal.h"
#include "hecdssFort.h"


/**
*  Function:	zgetLastWriteTimeFile
*
*  Use:			Public
*
*  Description:	Returns the last write time of the file in milliseconds since 01 Jan 1970
*
*  Declaration: long long zgetLastWriteTimeFile(long long *ifltab);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*	Retruns:	long long timeMillis
*					The system time of the last write for the file, in milliseconds
*					No conversion of time (e.g., timezone) is done.
*					This time is identical of the last record written (unless other writes occurred.)
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
long long zgetLastWriteTimeFile6(long long *ifltab);

long long zgetLastWriteTimeFile(long long *ifltab)
{
	int status;
	long long *fileHeader;

	//  Check for correct DSS Version
	if (zgetVersion(ifltab) == 6) {
		return zgetLastWriteTimeFile6(ifltab);
	}

	if (zgetVersion(ifltab) != 7) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zreadInfo_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
								zgetVersion(ifltab), 0, zdssErrorSeverity.WARNING, "", "");
	}

	if (!ifltab[zdssKeys.kwritingNow]) {
		status =  zpermRead(ifltab);
		if (zisError(status)) {
			//  An error code
			status = zerrorUpdate(ifltab, status, DSS_FUNCTION_zreadInfo_ID);
			return status;
		}
	}

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	return fileHeader[zdssFileKeys.klastWriteTime];
}


long long zgetLastWriteTimeFile6(long long *ifltab)
{
	char cstring[20];
	char crequest[20];
	char canswer[20];
	int number;
	int istat;
	int jul;
	int secs;
	long long millis;


	stringFill (cstring, ' ', sizeof(cstring));

	istat = 0;
	zrdprm6_(ifltab, &istat);

	stringCToFort(crequest, sizeof(crequest), "LWDA");
	zinqir6_ (ifltab, crequest, cstring, &number, strlen(crequest), sizeof(cstring));
	copyAndTrim(canswer, sizeof(canswer), cstring, sizeof(cstring));

	jul = dateToJulian(canswer);
	if (jul < 1) {
		return 0;
	}

	stringCToFort(crequest, sizeof(crequest), "LWTI");
	zinqir6_ (ifltab, crequest, cstring, &number, strlen(crequest), sizeof(cstring));
	copyAndTrim(canswer, sizeof(canswer), cstring, sizeof(cstring));
	//  CTIME(1:8) = '12:34:56'

	secs = timeStringToSeconds(canswer);
	if (secs < 0) {
		return 0;
	}

	//  Seconds since Jan 01, 1970
	millis = (long long)(jul - 25568) * (long long)86400;

	//  Add seconds after midnight
	millis += (long long)secs;

	//  Make into milliseconds
	millis *= 1000L;

	return millis;
}

