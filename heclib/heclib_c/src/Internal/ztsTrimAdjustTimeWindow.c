#include "heclib.h"
#include "hecdssInternal.h"
#include "zStructTsTimeWindow.h"
#include "zStructTimeSeries.h"

/**
*  Function:	ztsTrimAdjustTimeWindow
*
*  Use:			Private
*
*  Description:	Resets the time window in a zStructTimeSeries to reflect trimmed data
*
*  Declaration: void ztsTrimAdjustTimeWindow(long long *ifltab, zStructTimeSeries *tss, int firstValid, int lastValid);
*
*  Parameters:	long long *ifltab
*					The file table array, similar to a handle number
*
*				zStructTimeSeries *tss
*					The time series struct.
*
*				int firstValid
*					The position of the first valid data item in the data array
*
*				int lastValid
*					The position of the last valid data item in the data array
*
*
*	Returns:	none.
*
*	Remarks:	A helper function to ztsTrim()
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


void ztsTrimAdjustTimeWindow(long long *ifltab, zStructTimeSeries *tss, int firstValid, int lastValid)
{

	int jul;
	int itime;
	int number;

	if (!tss->timeWindow) {
		ztsProcessTimes(ifltab, tss, 0);
	}

	if (tss->timeWindow->intervalSeconds > 0) {
		if (firstValid > 0) {
			//  Include base date to be sure uneven intervals are correct
			jul = tss->timeWindow->startJulian + tss->julianBaseDate;
			itime = tss->timeWindow->startTimeSeconds;
			incrementTime(tss->timeWindow->intervalSeconds, firstValid,
				jul, itime,
				&tss->startJulianDate, &tss->startTimeSeconds);
			tss->startJulianDate -= tss->julianBaseDate;
		}

		if (lastValid < (tss->numberValues - 1)) {
			//  number to increment - will be negative
			number = lastValid - tss->numberValues + 1;
			jul = tss->timeWindow->endJulian + tss->julianBaseDate;
			itime = tss->timeWindow->endTimeSeconds;
			incrementTime(tss->timeWindow->intervalSeconds, number,
				jul, itime,
				&tss->endJulianDate, &tss->endTimeSeconds);
			tss->endJulianDate -= tss->julianBaseDate;
		}
	}
	else if (tss->times) {
		if (firstValid > 0) {
			jul = tss->julianBaseDate;
			itime = tss->times[firstValid];
			cleanTime(&jul, &itime, tss->timeGranularitySeconds);
			itime *= tss->timeGranularitySeconds;
			tss->startJulianDate = jul;
			tss->startTimeSeconds = itime;
		}

		if (lastValid < (tss->numberValues - 1)) {
			jul = tss->julianBaseDate;
			itime = tss->times[lastValid];
			cleanTime(&jul, &itime, tss->timeGranularitySeconds);
			itime *= tss->timeGranularitySeconds;
			tss->endJulianDate = jul;
			tss->endTimeSeconds = itime;
		}
	}

	tss->numberValues = lastValid - firstValid + 1;

	tss->timeWindow->startJulian = tss->startJulianDate;
	tss->timeWindow->startTimeSeconds = tss->startTimeSeconds;
	tss->timeWindow->endJulian = tss->endJulianDate;
	tss->timeWindow->endTimeSeconds = tss->endTimeSeconds;
	tss->timeWindow->numberValues = tss->numberValues;


}

