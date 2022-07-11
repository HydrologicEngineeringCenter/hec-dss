#include <string.h>
#include <stdio.h>

#include "heclib.h"
#include "zdssKeys.h"
#include "zStructRecordAddresses.h"
#include "hecdssInternal.h"


/**
*  Function:	zgetRecordSize7
*
*  Use:			Private;  use zgetRecordSize instead
*
*  Description:	Function to get size information about a single record, including type specific (e.g., Time Series)
*
*  Declaration: int zgetRecordSize7 (long long *ifltab, zStructRecordSize *recordSize);
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
*	Remarks:	zgetRecordSize() is the public function.
*					Gets basic size information for all records, and specific sizes for
*					time series and paired data records.
*
*	See Also:	int ztsGetSizes(long long *ifltab, zStructTimeSeries *tss,
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

int zgetRecordSize7 (long long *ifltab, zStructRecordSize *recordSize)
{
	int status;
	int number;
	int i;
	int internalHeader[INT_HEAD_SIZE];
	int internalHeaderArraySize  = INT_HEAD_SIZE;
	long long *info;
	char messageString[100];
	char *pathname;


	//  Check for correct DSS Version
	if (zgetVersion(ifltab) != 7) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zreadInfo_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
								zgetVersion(ifltab), 0, zdssErrorSeverity.WARNING, recordSize->pathname, "");
	}

	if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zreadInfo_ID, "Enter zgetRecordSize7, Pathname: ", recordSize->pathname);
	}

	pathname = recordSize->pathname;
	//  In case we just accessed this record, check to see if we have it in memory
	info = (long long *)ifltab[zdssKeys.kinfo];
	//  Double check that this is the correct path
	if (!zpathnameCompare(pathname, &ifltab[zdssKeys.kpathAddressInBin], strlen(pathname))) {
		//  No.  Read the info block
		ifltab[zdssKeys.kpathnameHash] = 0;
		status = zreadInfo(ifltab, pathname, 0);
		if (zisError(status)) {
			//  An error code
			status = zerrorUpdate(ifltab, status, DSS_FUNCTION_zreadInfo_ID);
			return status;
		}
		if (status != STATUS_RECORD_FOUND) {
			if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, ZREAD_NOT_EXIST, zhandle(ifltab));
				zmessage2(ifltab, messageString, recordSize->pathname);
			}
			return status;
		}
		info = (long long *)ifltab[zdssKeys.kinfo];
	}

	//  Fill in the struct
	i8toi4(info[zdssInfoKeys.kinfoTypeVersion], &recordSize->dataType, &recordSize->version);
	recordSize->numberValues = (int)info[zdssInfoKeys.kinfoNumberData];
	recordSize->logicalNumberValues = (int)info[zdssInfoKeys.kinfoLogicalNumber];
	recordSize->values1Number = (int)info[zdssInfoKeys.kinfoValues1Number];
	recordSize->values2Number = (int)info[zdssInfoKeys.kinfoValues2Number];
	recordSize->values3Number = (int)info[zdssInfoKeys.kinfoValues3Number];

	recordSize->internalHeaderNumber = (int)info[zdssInfoKeys.kinfoInternalHeadNumber];
	recordSize->header2Number = (int)info[zdssInfoKeys.kinfoHeader2Number];
	recordSize->values3Number = (int)info[zdssInfoKeys.kinfoValues3Number];
	recordSize->userHeaderNumber = (int)info[zdssInfoKeys.kinfoUserHeadNumber];
	recordSize->allocatedSize = (int)info[zdssInfoKeys.kinfoAllocatedSize];
	recordSize->lastWriteTimeMillis = info[zdssInfoKeys.kinfoLastWriteTime];

	charInt(&info[zdssInfoKeys.kinfoProgram], recordSize->programLastWrite, zdssVals.numberProgram, sizeof(recordSize->programLastWrite), 0, 1, 0);

	//  Get internal header for data type specific parameters
	for (i=0; i<INT_HEAD_SIZE; i++) {
		internalHeader[i] = 0;
	}
	number = (int)info[zdssInfoKeys.kinfoInternalHeadNumber];
	if (number > INT_HEAD_SIZE) number = INT_HEAD_SIZE;
	status = zget(ifltab, info[zdssInfoKeys.kinfoInternalHeadAddress], internalHeader, number, 1);
	if (zisError(status)) {
		//  Just pass any error code back (don't update at this level, as the calling function will.)
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zreadInfo_ID);
	}

	if ((recordSize->dataType >= DATA_TYPE_RTS) && (recordSize->dataType < DATA_TYPE_PD)) {
		//  Time series
		if (bigEndian()) {
			zswitchInts(internalHeader, INT_HEAD_cnotesLength + 1);
		}
		recordSize->tsPrecision = internalHeader[INT_HEAD_precision];
		recordSize->tsTimeOffset = internalHeader[INT_HEAD_timeOffset];
		recordSize->tsProfileDepthsNumber = internalHeader[INT_HEAD_profileDepthsNumber];
		recordSize->tsBlockStartPosition = internalHeader[INT_HEAD_blockStartPosition];
		recordSize->tsBlockEndPosition = internalHeader[INT_HEAD_blockEndPosition];
		recordSize->tsValueSize = internalHeader[INT_HEAD_valueSize];
		recordSize->tsValueElementSize = internalHeader[INT_HEAD_valueElementSize];
		recordSize->tsValuesCompressionFlag = internalHeader[INT_HEAD_valuesCompressionFlag];
		recordSize->tsQualityElementSize = internalHeader[INT_HEAD_qualityElementSize];
		recordSize->tsQualityCompressionFlag = internalHeader[INT_HEAD_qualityCompressionFlag];
		recordSize->tsInotesElementSize = internalHeader[INT_HEAD_inotesElementSize];
		recordSize->tsInotesCompressionFlag = internalHeader[INT_HEAD_inotesCompressionFlag];
		recordSize->tsCnotesLength = internalHeader[INT_HEAD_cnotesLength];
	}
	else if ((recordSize->dataType >= DATA_TYPE_PD) && (recordSize->dataType < DATA_TYPE_TEXT)) {
		//  Paired Data 
		recordSize->pdNumberCurves = internalHeader[INT_HEAD_pdNumberCurves];
		recordSize->pdNumberOrdinates = internalHeader[INT_HEAD_pdNumberOrdinates];
		recordSize->pdBoolIndependentIsXaxis = internalHeader[INT_HEAD_pdBoolIndependentIsXaxis];
		recordSize->pdLabelsLength = internalHeader[INT_HEAD_pdLabelsLength];
		recordSize->pdPrecision = internalHeader[INT_HEAD_pdPrecision];
	}
	else if ((recordSize->dataType >= DATA_TYPE_TEXT) && (recordSize->dataType < 400)) {
	}

	if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zreadInfo_ID, "Exit zgetRecordSize7, Status: ", status);
	}

	return status;
}


