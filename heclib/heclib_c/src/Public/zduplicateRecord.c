#include <stdlib.h>
#include <string.h>

#include "heclib.h"
#include "hecdssInternal.h"
#include "zdssKeys.h"

/**
*  Function:	zduplicateRecord
*
*  Use:			Public
*
*  Description:	Duplicate a record within a DSS file
*
*  Declaration: int zduplicateRecord(long long *ifltab, const char *pathnameFrom, const char *pathnameTo);
*
*  Parameters:
*				long long ifltab
*					The ifltab of the DSS file.  Maybe either version 6 o r7.
*
*				const char *pathnameFrom
*					The pathname to copy.  Must be a full valid pathname; a single record, not a dataset
*
*				const char *pathnameTo
*					The pathname to copy to. If time series, you cannot change the D or E parts
*					The dataset needs to be converted to do that.
*
*
*	Returns:	int status
*					STATUS_OKAY for successful operation.
*					errorCode for error
*					errorCode may be decoded by function zerrorDecode()
*
*
*	Remarks:	A convienence function, as zcopyRecord does the real work.
*
*
*	Author:			Bill Charley
*	Date:			2010
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zduplicateRecord(long long *ifltab, const char *pathnameFrom, const char *pathnameTo)
{
	int vers;
	int istatus[1];
	int status;
	int ldup;
	int *IBUFF1, KBUFF1, *IBUFF2, KBUFF2;

	vers = zgetVersion(ifltab);
	if (vers == 6) {
		ldup = 1;
		KBUFF1 = 10000;
		KBUFF2 = 3000;
		IBUFF1 = calloc(KBUFF1, 4);
		IBUFF2 = calloc(KBUFF2, 4);
		zcorec6_(ifltab, ifltab, pathnameFrom, pathnameTo,
			IBUFF1, &KBUFF1, IBUFF2, &KBUFF2, &ldup, istatus,
			strlen(pathnameFrom), strlen(pathnameTo));
		free(IBUFF1);
		free(IBUFF2);
		return istatus[0];
	}
	else {
		status = zreadInfo(ifltab, pathnameFrom, 0);
		if (status == 0) {
			status = zcopyRecordInternal(ifltab, ifltab, pathnameTo, 1);
		}
		return status;
	}
}

