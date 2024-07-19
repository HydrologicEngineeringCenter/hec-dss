#include <string.h>

#include "zStructRecordAddresses.h"
#include "heclib.h"

/**
*  Function:	zgetRecordSize6
*
*  Use:			Private;  use zgetRecordSize instead
*
*  Description:	Function to get size information about a single record, including type specific (e.g., Time Series)
*
*  Declaration: int zgetRecordSize6(long long *ifltab, zStructRecordSize *recordSize);
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
*	Remarks:	Calls a Fortran subroutine, passing all params as arguments
*
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

// int zgetRecordSize6(long long *ifltab, zStructRecordSize *recordSize)
// {
// 	int status;
// 	int lastWriteSecs;
// 	char *pathname;

// 	pathname = recordSize->pathname;

// 	//  Check for correct DSS Version
// 	if (zgetVersion(ifltab) != 6) {
// 		return zerrorProcessing(ifltab, DSS_FUNCTION_zreadInfo_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
// 								zgetVersion(ifltab), 0, zdssErrorSeverity.WARNING, recordSize->pathname, "");
// 	}

// 	zrecordinfo6_(ifltab, pathname, &recordSize->dataType, &recordSize->version,
// 		&recordSize->numberValues, &recordSize->logicalNumberValues,
// 		&recordSize->values1Number, &recordSize->internalHeaderNumber, &recordSize->userHeaderNumber,
// 		&recordSize->allocatedSize, &lastWriteSecs, &recordSize->tsPrecision,
// 		&recordSize->tsTimeOffset, &recordSize->tsValueElementSize, &recordSize->tsQualityElementSize,
// 		&recordSize->pdNumberCurves, &recordSize->pdNumberOrdinates, &recordSize->ipdValueSize,
// 		&recordSize->pdBoolIndependentIsXaxis, &recordSize->pdLabelsLength, &recordSize->pdPrecision, &status,
// 		strlen(pathname));

// 	recordSize->lastWriteTimeMillis = (long long)lastWriteSecs * (long long)1000;

// 	return status;
// }
