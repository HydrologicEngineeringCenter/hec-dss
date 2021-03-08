#include <string.h>

#include "heclib6.h"
#include "zdssKeys.h"
#include "hecdss7.h"
#include "hecdssInternal.h"
#include "hecdssFort.h"


/**
*  Function:	zgetMyLastWriteTime
*
*  Use:			Public
*
*  Description:	Returns the last write time for this process in milliseconds since 01 Jan 1970
*					(for use in muti-user access)
*
*  Declaration: long long zgetMyLastWriteTime(long long *ifltab);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*	Retruns:	long long timeMillis
*					The system time of the last write for this process, in milliseconds
*
*	See Also:	long long zgetLastWriteTimeFile()
*				long long zgetLastWriteTimeRec()
*
*
*	Author:			Bill Charley
*	Date:			2017
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*
*/

long long zgetMyLastWriteTime(long long *ifltab)
{

	//  Check for correct DSS Version
	if (zgetVersion(ifltab) == 6) {
		return 0L;
	}

	if (zgetVersion(ifltab) != 7) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zreadInfo_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
								zgetVersion(ifltab), 0, zdssErrorSeverity.WARNING, "", "");
	}

	return ifltab[zdssKeys.kmyLastWriteTime];
}

