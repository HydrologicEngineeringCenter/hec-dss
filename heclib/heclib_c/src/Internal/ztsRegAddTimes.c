#include <stdlib.h>

#include "hecdss7.h"


/**
*  Function:	ztsRegAddTimes
*
*  Use:			Private
*
*  Description:	 A small utility function to add the time array to a time series struct
*					for regular interval data
*
*  Declaration: void ztsRegAddTimes(zStructTimeSeries *tss);
*
*  Parameters:	zStructTimeSeries *tss
*					The time series struct.  Must have a valid start date, time, number
*					of values and regular time interval.  Cannot have a pre-existing time array

*
*
*	Returns:	status
*
*
*	Author:			Bill Charley, 2016
*
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int ztsRegAddTimes(zStructTimeSeries *tss)
{
	int i;
	int julianStart;
	int julianEnd;
	int secondsEnd;
	int startTimeSeconds;
	int timeGranularity;
	int interval;
	long long itemp[2];


	if (tss->timeIntervalSeconds < 1) {
		//  No time interval to work with
		//  (This should have been done by process times)
		return STATUS_NOT_OKAY;
	}
	if (tss->times) {
		//  The times array is already allocated
		//  We don't know the size or what's in it to do anything
		//  (It should be null when this function is called.)
		return STATUS_OKAY;;
	}
	//  Valid times to work with?
	if (!tss->timeWindow)
		return STATUS_NOT_OKAY;

	if (tss->timeWindow->startJulian == UNDEFINED_TIME) {
		return STATUS_NOT_OKAY;
	}
	if (tss->timeWindow->startTimeSeconds < 0) {
		return STATUS_NOT_OKAY;
	}
	if (tss->timeWindow->numberValues < 1) {
		return STATUS_NOT_OKAY;
	}
	if (tss->numberValues < 1) {
		return STATUS_NOT_OKAY;
	}
	tss->times = (int *)calloc(tss->numberValues, 4);
	if (!tss->times) {
		itemp[0] = 0;
		itemp[1] = 0;
		return zerrorProcessing(itemp, DSS_FUNCTION_ztsRetrieveReg_ID,
			zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, (tss->numberValues * 4), 0,
			zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for times array");
	}
	tss->allocated[zSTRUCT_TS_times] = 1;
	julianStart = tss->timeWindow->startJulian;
	startTimeSeconds = tss->timeWindow->startTimeSeconds;
	timeGranularity = tss->timeGranularitySeconds;
	//  Adjust to correct offset
	if (tss->timeOffsetSeconds < 0) tss->timeOffsetSeconds = 0;
	ztsOffsetAdjustToOffset(tss->timeOffsetSeconds, tss->timeIntervalSeconds, &julianStart, &startTimeSeconds);

	if (timeGranularity < 1) timeGranularity = MINUTE_GRANULARITY;
	if (tss->timeIntervalSeconds <= SECS_IN_1_DAY) {
		//  Time interval a day or less.  We can just add times
		incrementTime(tss->timeIntervalSeconds, 0, julianStart, startTimeSeconds, &julianEnd, &secondsEnd);
		if (tss->timeOffsetSeconds != 0) {
			ztsOffsetAdjustToOffset(tss->timeOffsetSeconds, tss->timeIntervalSeconds, &julianEnd, &secondsEnd);
		}
		tss->times[0] = ((julianEnd - tss->julianBaseDate) * (SECS_IN_1_DAY / timeGranularity)) + (secondsEnd / timeGranularity);
		interval = tss->timeIntervalSeconds / timeGranularity;
		for (i = 1; i < tss->timeWindow->numberValues; i++) {
			tss->times[i] = tss->times[i - 1] + interval;
		}
	}
	else {
		for (i = 0; i < tss->timeWindow->numberValues; i++) {
			incrementTime(tss->timeIntervalSeconds, i, julianStart, startTimeSeconds, &julianEnd, &secondsEnd);
			if (tss->timeOffsetSeconds != 0) {
				ztsOffsetAdjustToOffset(tss->timeOffsetSeconds, tss->timeIntervalSeconds, &julianEnd, &secondsEnd);
			}
			tss->times[i] = ((julianEnd - tss->julianBaseDate) * (SECS_IN_1_DAY / timeGranularity)) + (secondsEnd / timeGranularity);
		}
	}
	return STATUS_OKAY;
}

