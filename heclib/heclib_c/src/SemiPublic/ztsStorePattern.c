#include <string.h>

#include "hecdssInternal.h"
#include "zdssKeys.h"
#include "heclib.h"
#include "zStructTsTimeWindow.h"
#include "zStructTimeSeries.h"

/**
*  Function:	ztsStorePattern
*
*  Use:			Semi-Public
*
*  Description:	Store time series pattern data (e.g., unit hydrographs, daily average temperatures, etc.)
*					Use ztsStore instead (it will call this function.)
*					Note:  Pattern data has a D part of "TS-Pattern", instead of a date
*
*  Declaration: int ztsStorePattern(long long *ifltab, zStructTimeSeries *tss);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*					See zopen() for more information.
*
*				zStructTimeSeries *tss
*					A struct that needs to contain all data and information related to this data set to store.
*					This struct must be created by one of the TS Struct new methods, such as:
*						zStructTimeSeries* zstructTsNew(const char* pathname);
*						zStructTimeSeries* zstructTsNewRegFloats(const char* pathname, float *floatValues, int numberValues,
*												const char *startDate, const char *startTime, const char *units, const char *type);
*						What the struct needs to contain is defined below.
*					When the store is complete, the struct must be freed by a call to
*						void zstructFree(zStructTimeSeries *zstruct)
*					NEVER REUSE A zStructTimeSeries, always free and create a new on.

*
*	Returns:	int status
*					STATUS_OKAY
*					error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*
*
*	Required:
*
*				const char* pathname
*					The pathname of the record to store.  The pathname must be exact, but is case insensitive
*					(i.e., "Apart" is the same as "APART"), and have a correct E (interval) part.
*
*				const char *units
*					The units of the data, such as "CFS".
*					The number of characters in units is not limited, but generally should not be greater than 25.
*
*				const char *type
*					The type of data.  Valid values include "PER-AVER", "PER-CUM", "INST-VAL", and "INST-CUM".
*					The number of characters in units is not limited, but generally should not be greater than 25.
*
*				int numberValues
*					The number of values to store (and the number of values in the data array and other optional arrays (e.g., quality))
*
*				float *floatValues
*					The float array containing the data to store if storing floats.  If storing doubles, this must be zero.
*			or
*				double *doubleValues
*					The double array containing the data to store if storing double.  If storing floats, this must be zero.
*

	In DSS Version 6, irregular data is stored as: time1 from base, value1, time2 from base, value2...
	In DSS Version 7, irregular data is stored as: time1 from base, time2 from base, ... value1, value2...
*
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int ztsStorePattern(long long *ifltab, zStructTimeSeries *tss)
{

	int status;
	int version;
	int i;
	zStructTransfer* ztransfer;

	int internalHeader[INT_HEAD_SIZE];

	//  Messages and debug
	if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztsStore_ID, "Enter ztsStorePattern, Pathname: ", tss->pathname);
	}

	for (i = 0; i < INT_HEAD_SIZE; i++) {
		internalHeader[i] = 0;
	}

	//  Determine if regular interval or irregular interval
	if (tss->timeWindow == 0) {
		ztsProcessTimes(ifltab, tss, 0);
	}
	else if (tss->timeWindow->intervalSeconds == 0) {
		ztsProcessTimes(ifltab, tss, 0);
	}
	if ((tss->timeWindow == 0) || ((tss->timeWindow->intervalSeconds == 0) && (tss->timeWindow->blockSize == 0))) {
		// error out
		return -1;
	}
	if (tss->pathnameInternal == 0) {
		//  This shouldn't happen - supposed to be done in ztsProcessTimes
		tss->pathnameInternal = mallocAndCopy(tss->pathname);
		tss->allocated[zSTRUCT_pathnameInternal] = 1;
	}

	//  We don't have to mess with times (much) for pattern data, like
	//  we do with standard time series.  We just return the array if irregular
	ztransfer = zstructTransferNew(tss->pathnameInternal, 1);
	if (!ztransfer) {
		//  error out
		return -1;
	}
	ztransfer->internalHeader = internalHeader;

	version = zgetVersion(ifltab);
	ztransfer->numberValues = tss->numberValues;

	if (version == 6) {
	//	tss->numberValues = ztransfer->values1Number;
		ztransfer->internalHeaderNumber = 5;
		ztransfer->internalHeader[0] = tss->timeOffsetSeconds / SECS_IN_1_MINUTE;
		stringCToFort((char *)&ztransfer->internalHeader[1], 8, tss->units);
		stringCToFort((char *)&ztransfer->internalHeader[3], 8, tss->type);
	}
	else {
		internalHeader[INT_HEAD_timeGranularity] = tss->timeGranularitySeconds;
		internalHeader[INT_HEAD_precision] = tss->precision;
		internalHeader[INT_HEAD_timeOffset] = tss->timeOffsetSeconds;
		ztransfer->internalHeaderNumber = ztsInternalHeaderPack(tss, ztransfer->internalHeader);
	}

	if (tss->timeWindow->intervalSeconds > 0) {
		if (tss->floatValues) {
			ztransfer->dataType = DATA_TYPE_RTS_PATTERN;
			ztransfer->values1 = (int*)tss->floatValues;
			ztransfer->values1Number = ztransfer->numberValues;
		}
		else {
			ztransfer->dataType = DATA_TYPE_RTD_PATTERN;
			ztransfer->values1 = (int*)tss->doubleValues;
			ztransfer->values1Number = ztransfer->numberValues * 2;
		}
	}
	else {
		ztransfer->values2 = tss->times;
		tss->allocated[zSTRUCT_TS_times] = 0;
		if (tss->floatValues) {
			ztransfer->dataType = DATA_TYPE_ITS_PATTERN;
			ztransfer->values1 = (int*)calloc(ztransfer->numberValues, 8);
			ztransfer->values1Number = ztransfer->numberValues * 2;
			ztransfer->allocated[zSTRUCT_TRANS_values1] = 1;
			convertDataArray((int *)tss->times, ztransfer->values1, tss->numberValues, 1, 1);
			convertDataArray((int *)tss->floatValues, &ztransfer->values1[tss->numberValues], tss->numberValues, 1, 1);
		}
		else {
			ztransfer->dataType = DATA_TYPE_ITD_PATTERN;
			ztransfer->values1 = (int*)calloc(ztransfer->numberValues, 12);
			ztransfer->values1Number = ztransfer->numberValues * 3;
			ztransfer->allocated[zSTRUCT_TRANS_values1] = 1;
			convertDataArray((int *)tss->times, ztransfer->values1, tss->numberValues, 1, 1);
			convertDataArray((int *)tss->doubleValues, &ztransfer->values1[tss->numberValues], tss->numberValues, 2, 2);
		}
	}
	tss->dataType = ztransfer->dataType;

	status = zwrite(ifltab, ztransfer);
	zstructFree(ztransfer);

	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztsStore_ID,  "Exit ztsStorePattern; Pathname: ", tss->pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsStore_ID, "status: ", status);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsStore_ID, "numberValues: ", tss->numberValues);
	}

	return status;
}


