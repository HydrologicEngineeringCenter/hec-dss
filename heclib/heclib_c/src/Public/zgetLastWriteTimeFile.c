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

long long zgetLastWriteTimeFile(long long *ifltab)
{
	int status;
	long long *fileHeader;

	//  Check for correct DSS Version

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
