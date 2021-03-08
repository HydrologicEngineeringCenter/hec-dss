#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssVals.h"
#include "zdssMessages.h"
#include "hecdss7.h"


/**
*  Function:	ztsGetSizesInternal
*
*  Use:			Private
*
*  Description:	Gets size information about a time series record or data set (series of records) blocks
*
*  Declaration: int ztsGetSizesInternal(long long *ifltab, ztsTimeWindow *timeWindow, zStructRecordSize *recordSize);
*
*  Parameters:	long long *ifltab
*					The file table array, similar to a handle number
*
*				const char* pathname
*					The pathname of the record or data set to get sizes from.
*
*				ztsTimeWindow *timeWindow
*					A pointer to a time window struct that identifies the start and end dates/times
*					of the blocks.  To retrieve the sizes for a single record only, set this to zero (void *)0.
*					Note:  This reports sizes for blocks defined by the time window, not just the time window.
*
*				zStructRecordSize *recordSize
*					The time series record sizes struct that will be returned with the size of either the
*					single record (ztsTimeWindow set to 0), or the accumulation of records using the dates/times
*					identified in ztsTimeWindow.
*
*
*	Returns:	int status
*					STATUS_OKAY
*					error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*	Remarks:	Time Series may be either regular or irregular.
*					Reports sizes for blocks, not within blocks.
*					Set time window to null for single record and read info before use!
*					Assumes correct data types, pathname, etc.  Does minimal error checking -
*					it is assumed that error checking is done prior to this function being called.
*					If an error occurs in a lower function, just pass that error code back
*					(don't update at this level, as the calling function will.)
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int ztsGetSizesInternal(long long *ifltab, ztsTimeWindow *timeWindow, zStructRecordSize *recordSize)
{


	int status;
	int returnStatus;
	int boolMultipleRecs;
	int number;
	int dataType;
	int version;
	int lengths[6];
	ztsTimeWindow timeWind;
	ztsTimeWindow *ztimeWindow;

	int julianBlockDate;
	int internalHeader[INT_HEAD_SIZE];
	int internalHeaderArraySize  = INT_HEAD_SIZE;
	long long *info;
	char path[MAX_PATHNAME_LENGTH];
	char blockDate[20];

	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zreadInfo_ID, "Enter ztsGetSizesInternal, Pathname: ", recordSize->pathname);
	}

	//  Initialize all to zero, so we can add for multiple records
	//  (or just return if pathname not found)
	recordSize->numberRecordsFound = 0;
	recordSize->dataType = 0;
	recordSize->version = 0;
	recordSize->numberValues = 0;
	recordSize->logicalNumberValues = 0;
	recordSize->values1Number = 0;
	recordSize->values2Number = 0;
	recordSize->values3Number = 0;
	recordSize->internalHeaderNumber = 0;
	recordSize->header2Number = 0;
	recordSize->userHeaderNumber = 0;
	recordSize->allocatedSize = 0;
	recordSize->lastWriteTimeMillis = 0;
	recordSize->tsPrecision = 0;
	recordSize->tsTimeOffset = 0;
	recordSize->itsTimePrecisionStored = 0;
	recordSize->tsProfileDepthsNumber = 0;
	recordSize->tsBlockStartPosition = 0;
	recordSize->tsBlockEndPosition = 0;
	recordSize->tsValueSize = 0;
	recordSize->tsValueElementSize = 0;
	recordSize->tsValuesCompressionFlag = 0;
	recordSize->tsQualityElementSize = 0;
	recordSize->tsQualityCompressionFlag = 0;
	recordSize->tsInotesElementSize = 0;
	recordSize->tsInotesCompressionFlag = 0;
	recordSize->tsCnotesLength = 0;

	returnStatus = STATUS_RECORD_NOT_FOUND;
	boolMultipleRecs = 0;
	if (recordSize->pathname) {
		stringCopy (path, sizeof(path), recordSize->pathname, strlen(recordSize->pathname));
	}

	//  No time window passed in; just return values for single record
	if (!timeWindow) {
		zpathnamePartLengths(path, strlen(path), lengths, 6);
		if (lengths[3] > 15) {
			//  Probably a time range in the D part.  See if we can figure it out...
			boolMultipleRecs = ztsGetPathTimeWindow(zgetVersion(ifltab), path, strlen(path), &timeWind);
			if (boolMultipleRecs) {
				ztimeWindow = &timeWind;
			}
		}
	}
	else {
		boolMultipleRecs = 1;
		ztimeWindow = timeWindow;
	}

	recordSize->numberRecordsFound = 0;
	if (!boolMultipleRecs) {
		status = zgetRecordSize (ifltab, recordSize);
		if (zisError(status)) {
			//  Just pass any error code back (don't update at this level, as the calling function will.)
			return status;
		}
		if (status == STATUS_OKAY) {
			recordSize->numberRecordsFound = 1;
		}
	}
	else {

		//  Time window given; multiple records
		julianBlockDate = ztimeWindow->startBlockJulian;
		while (julianBlockDate <= ztimeWindow->endBlockJulian) {
			recordSize->numberRecordsFound++;
			//  Get the path for this block and compute next block date
			julianToDate(julianBlockDate, 4, blockDate, sizeof(blockDate));
			zpathnameSetPart (path, sizeof(path), blockDate, 4);
			julianBlockDate = ztsIncrementBlock(julianBlockDate, ztimeWindow->blockSize);
			//  Read info and header
			status = zreadInfo(ifltab, path, 0);
			if (zisError(status)) {
				return status;
			}
			if (status == STATUS_RECORD_FOUND) {
				info = (long long *)ifltab[zdssKeys.kinfo];
				number = (int)info[zdssInfoKeys.kinfoInternalHeadNumber];
				if (number > INT_HEAD_SIZE) number = INT_HEAD_SIZE;
				status = zget(ifltab, info[zdssInfoKeys.kinfoInternalHeadAddress], internalHeader, number, 1);
				if (zisError(status)) {
					return status;
				}
				if (getEndian()) {
					zswitchInts(internalHeader, INT_HEAD_cnotesLength + 1);
				}
				i8toi4(info[zdssInfoKeys.kinfoTypeVersion], &dataType, &version);
				recordSize->dataType = dataType;
				recordSize->version = version;
				recordSize->numberValues +=			(int)info[zdssInfoKeys.kinfoNumberData];
				recordSize->logicalNumberValues +=	(int)info[zdssInfoKeys.kinfoLogicalNumber];
				recordSize->values1Number +=		(int)info[zdssInfoKeys.kinfoValues1Number];
				recordSize->values2Number +=		(int)info[zdssInfoKeys.kinfoValues2Number];
				recordSize->values3Number +=		(int)info[zdssInfoKeys.kinfoValues3Number];
				recordSize->internalHeaderNumber =	(int)info[zdssInfoKeys.kinfoInternalHeadNumber];
				recordSize->header2Number =			(int)info[zdssInfoKeys.kinfoHeader2Number];
				recordSize->userHeaderNumber =		(int)info[zdssInfoKeys.kinfoUserHeadNumber];
				recordSize->allocatedSize  =		(int)info[zdssInfoKeys.kinfoAllocatedSize];
				recordSize->lastWriteTimeMillis =	 info[zdssInfoKeys.kinfoLastWriteTime];
				recordSize->tsTimeOffset =				internalHeader[INT_HEAD_timeOffset];
				recordSize->tsProfileDepthsNumber =		internalHeader[INT_HEAD_profileDepthsNumber];
				recordSize->tsBlockStartPosition =		internalHeader[INT_HEAD_blockStartPosition];
				recordSize->tsBlockEndPosition =		internalHeader[INT_HEAD_blockEndPosition];
				recordSize->tsValuesCompressionFlag =	internalHeader[INT_HEAD_valuesCompressionFlag];
				recordSize->tsQualityCompressionFlag =	internalHeader[INT_HEAD_qualityCompressionFlag];
				recordSize->tsInotesCompressionFlag =	internalHeader[INT_HEAD_inotesCompressionFlag];
				recordSize->tsCnotesLength +=			internalHeader[INT_HEAD_cnotesLength];

				if (internalHeader[INT_HEAD_valueSize] > recordSize->tsValueSize) {
					recordSize->tsValueSize = internalHeader[INT_HEAD_valueSize];
				}
				if (internalHeader[INT_HEAD_valueElementSize] > recordSize->tsValueElementSize) {
					recordSize->tsValueElementSize = internalHeader[INT_HEAD_valueElementSize];
				}
				if (internalHeader[INT_HEAD_qualityElementSize] > recordSize->tsQualityElementSize) {
					recordSize->tsQualityElementSize = internalHeader[INT_HEAD_qualityElementSize];
				}
				if (internalHeader[INT_HEAD_inotesElementSize] > recordSize->tsInotesElementSize) {
					recordSize->tsInotesElementSize = internalHeader[INT_HEAD_inotesElementSize];
				}

				if (internalHeader[INT_HEAD_precision] > recordSize->tsPrecision) {
					recordSize->tsPrecision = internalHeader[INT_HEAD_precision];
				}
				recordSize->itsTimePrecisionStored = internalHeader[INT_HEAD_timeGranularity];
			}
			else {
				//  Not found.
			}
		}
	}

	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zreadInfo_ID, "Exit ztsGetSizesInternal, Status: ", status);
	}

	if (recordSize->numberRecordsFound == 0) {
		return status;
	}
	else {
		return recordSize->numberRecordsFound;
	}
}


