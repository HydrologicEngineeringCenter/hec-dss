#include "heclib.h"

/**
*  Function:	ztsGetSizes
*
*  Use:			Public
*
*  Description:	Gets size information about a time series record or data set (series of records) blocks
*
*  Declaration: int ztsGetSizes(long long *ifltab, zStructTimeSeries *tss, zStructRecordSize *recordSize);
*
*  Parameters:	long long *ifltab
*					The file table array, similar to a handle number
*
*				const char* pathname
*					The pathname of the record or data set to get sizes from.
*
*				zStructTimeSeries *tss
*					A pointer to a time series struct that identifies the start and end dates/times
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

int ztsGetSizes(long long *ifltab, zStructTimeSeries *tss, zStructRecordSize *timeSeriesRecordSizes)
{
	int status;

	//  get sizes internal returns number in block, not number
	//  in time window.  Report back number in time window, if set
	if (zgetVersion(ifltab) == 6) {
		status = ztsGetSizes6(ifltab, tss, timeSeriesRecordSizes);
	}
	else {
		status = ztsGetSizesInternal (ifltab, tss->timeWindow,  timeSeriesRecordSizes);
	}

	if (status >= 0) {
		return STATUS_OKAY;
	}
	return status;
}


void ztsgetsizes_(long long *ifltab, const char *pathname,
						const char *startDate, const char *startTime,
						const char *endDate, const char *endTime,
						int *numberValues, int *valueSize,
						int *qualityElementSize, int *inoteElementSize,
						int *totalLengthCnotesInRecords, int *userHeaderNumber, int *status,
						size_t pathLen, size_t startDateLen, size_t startTimeLen,
						size_t endDateLen, size_t endTimeLen)
{

	char *path;
	char *sDate;
	char *sTime;
	char *eDate;
	char *eTime;

	zStructRecordSize timeSeriesRecordSizes;
	zStructTimeSeries *tss;

	path = stringFortToC(pathname, pathLen);
	sDate = stringFortToC(startDate, startDateLen);
	sTime = stringFortToC(startTime, startTimeLen);
	eDate = stringFortToC(endDate, endDateLen);
	eTime = stringFortToC(endTime, endTimeLen);

	timeSeriesRecordSizes.pathname = path;

	tss = zstructTsNewTimes(path, sDate, sTime, eDate, eTime);
	if (!tss) {
		*status = zerrorProcessing(ifltab, DSS_FUNCTION_internalUtility_ID,
										zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
										zdssErrorSeverity.MEMORY_ERROR,
										path, "Allocating time series struct in ztsgetsizes_");
		return;
	}
	if (tss->startJulianDate != UNDEFINED_TIME) {
		ztsProcessTimes(ifltab, tss, 1);
	}

	timeSeriesRecordSizes.pathname = path;
	*status = ztsGetSizes (ifltab, tss, &timeSeriesRecordSizes);
	if (*status == 0) {
		*numberValues = timeSeriesRecordSizes.numberValues;
		*valueSize = timeSeriesRecordSizes.tsValueSize;
		*qualityElementSize = timeSeriesRecordSizes.tsQualityElementSize;
		*inoteElementSize = timeSeriesRecordSizes.tsInotesElementSize;
		*totalLengthCnotesInRecords = timeSeriesRecordSizes.tsCnotesLength;
		*userHeaderNumber = timeSeriesRecordSizes.userHeaderNumber;
	}

	free(path);
	free(sDate);
	free(sTime);
	free(eDate);
	free(eTime);
	zstructFree(tss);
}

