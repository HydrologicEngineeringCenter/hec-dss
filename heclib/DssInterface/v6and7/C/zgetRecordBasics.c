#include <string.h>

#include "zdssKeys.h"
#include "zStructRecordBasics.h"
#include "heclib.h"


/**
*  Function:	zgetRecordSize
*
*  Use:			Semi-Public
*
*  Description:Function to get basic information about a single record, such as type, version, last written, etc
*
*  Declaration: int zgetRecordBasics(long long *ifltab, zStructRecordBasics *recordBasics);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				zStructRecordBasics *recordbasics
*					A struct will contain information for a single record.
*					See zStructRecordBasics.h for description of zStructRecordBasics contents
*					This struct is created by the following method:
*						zStructRecordBasics* zstructRecordBasicsNew(const char* pathname);
*					When you are finished with the struct, the struct must be freed by a call to
*						void zstructFree(zStructRecordBasics *recordBasics)
*					NEVER REUSE A zStructRecordBasics, always free and create a new on.
*
*
*	Remarks:	Generally, one does not need to get size information, as a read allocates
*					the appropriate space for a record automatically.
*					Gets basic size information for all records, and specific sizes for
*					time series and paired data records.
*
*	See Also:	int zgetRecordSize (long long *ifltab, zStructRecordSize *recordSize);
*					int ztsGetSizes(long long *ifltab, zStructTimeSeries *tss,
*						zStructRecordSize *timeSeriesRecordSizes)
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

int zgetRecordBasics(long long *ifltab, zStructRecordBasics *recordBasics)
{
	int version;

	version = zgetVersion(ifltab);
	if (version == 7) {
		return zgetRecordBasics7(ifltab, recordBasics);
	}
	else if (version == 6) {
		return zgetRecordBasics6(ifltab, recordBasics);
	}
	else  if (version > 0) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zreadInfo_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
			version, 0, zdssErrorSeverity.WARNING, recordBasics->pathname, "");
	}
	else {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zreadInfo_ID, zdssErrorCodes.UNABLE_TO_ACCESS_FILE,
								version, 0, zdssErrorSeverity.WARNING, recordBasics->pathname, "");
	}
}


