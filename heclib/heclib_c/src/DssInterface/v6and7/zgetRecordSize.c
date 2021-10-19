#include <string.h>

#include "zdssKeys.h"
#include "zStructRecordAddresses.h"
#include "heclib.h"


/**
*  Function:	zgetRecordSize
*
*  Use:			Semi-Public
*
*  Description:	Function to get size information about a single record, including type specific (e.g., Time Series)
*
*  Declaration: int zgetRecordSize(long long *ifltab, zStructRecordSize *recordSize);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				zStructRecordSize *recordSize
*					A struct will contain size and information for a single record.
*					See zStructRecordSize.h for description of zStructRecordSize contents
*					This struct is created by the following method:
*						zStructRecordSize* zstructRecordSizeNew(const char* pathname);
*					When you are finished with the struct, the struct must be freed by a call to
*						void zstructFree(zStructRecordSize *recordSize)
*					NEVER REUSE A zStructRecordSize, always free and create a new on.
*
*
*	Remarks:	Generally, one does not need to get size information, as a read allocates
*					the appropriate space for a record automatically.
*					Gets basic size information for all records, and specific sizes for
*					time series and paired data records.
*
*	See Also:	int ztsGetSizes(long long *ifltab, zStructTimeSeries *tss,
*								zStructRecordSize *timeSeriesRecordSizes)
*					Which gives the combined sizes for a group of time series records
*					spanning the time window defined in zStructTimeSeries
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

int zgetRecordSize(long long *ifltab, zStructRecordSize *recordSize)
{
	int version;

	version = zgetVersion(ifltab);
	if (version == 7) {
		return zgetRecordSize7(ifltab, recordSize);
	}
	else if (version == 6) {
		return zgetRecordSize6(ifltab, recordSize);
	}
	else  if (version > 0) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zreadInfo_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
			version, 0, zdssErrorSeverity.WARNING, recordSize->pathname, "");
	}
	else {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zreadInfo_ID, zdssErrorCodes.UNABLE_TO_ACCESS_FILE,
								version, 0, zdssErrorSeverity.WARNING, recordSize->pathname, "");
	}
}


