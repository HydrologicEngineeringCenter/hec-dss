#include <string.h>

#include "zStructRecordAddresses.h"
#include "heclib.h"

/**
*  Function:	zgetRecordSize6
*
*  Use:			Private;  use zgetRecordBasics instead
*
*  Description:	Function to get basic information about a single record, such as type, version, last written, etc
*
*  Declaration: int zgetRecordBasics6(long long *ifltab, zStructRecordBasics *recordBasics);
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
*	Remarks:	zgetRecordBasics() is the public function.

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
*	Date:			2017
*
*
*/

// int zgetRecordBasics6(long long *ifltab, zStructRecordBasics *recordBasics)
// {
// 	int status;
// 	int lastWriteSecs;
// 	int dummy[1];
// 	char *pathname;

// 	pathname = recordBasics->pathname;

// 	//  Check for correct DSS Version
// 	if (zgetVersion(ifltab) != 6) {
// 		return zerrorProcessing(ifltab, DSS_FUNCTION_zreadInfo_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
// 								zgetVersion(ifltab), 0, zdssErrorSeverity.WARNING, recordBasics->pathname, "");
// 	}

// 	zrecordinfo6_(ifltab, pathname, &recordBasics->recordType, &recordBasics->version,
// 		&recordBasics->numberValues, &recordBasics->logicalNumberValues,
// 		&recordBasics->values1Number, &recordBasics->internalHeaderNumber, &recordBasics->userHeaderNumber,
// 		&recordBasics->allocatedSize, &lastWriteSecs, dummy,
// 		dummy, dummy, dummy,
// 		dummy, dummy, dummy,
// 		dummy, dummy, dummy, &status,
// 		strlen(pathname));

// 	recordBasics->recLastWriteTimeMillis = (long long)lastWriteSecs * 1000L;
// 	recordBasics->recCreationTimeMillis = 0L;

// 	recordBasics->fileLastWriteTimeMillis = zgetLastWriteTimeFile(ifltab);
// 	recordBasics->fileCreationTimeMillis = 0L;

// 	return status;
// }
