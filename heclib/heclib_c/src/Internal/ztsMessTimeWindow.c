#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "heclib.h"
#include "zStructTsTimeWindow.h"
#include "zStructTimeSeries.h"

/**
*  Function:	ztsMessTimeWindow
*
*  Use:			Private
*
*  Description:	Print debug messages with information about the time window specified / calculated
*
*  Declaration: void ztsMessTimeWindow(long long *ifltab, zStructTimeSeries *tss);
*
*  Parameters:	long long *ifltab
*					The file table array, similar to a handle number
*
*				zStructTimeSeries *tss
*					The time series struct.

*
*	Returns:	None
*
*	Remarks:	The difference between the time window in tss and tss->timeWindow
*					is that the time window in tss is user input and does not have to be complete.
*					tss->timeWindow is processed and cleaned.  It is complete.
*
*	See Also:	ztsDateMessage
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


void ztsMessTimeWindow(long long *ifltab, int functionId, zStructTimeSeries *tss)
{
	char cdate[20];
	char ctime[13];
	char messageString[100];

	int julianBaseDate;
	int startJulian;
	int startTimeSeconds;
	int endJulian;
	int endTimeSeconds;
	int timeOffsetSeconds;
	int numberValues;


	julianBaseDate = tss->julianBaseDate;
	if (!tss->timeWindow) {
		zmessageDebug(ifltab, functionId, "Computed time window does not exist; Input times", "");
		startJulian = tss->startJulianDate;
		startTimeSeconds = tss->startTimeSeconds;
		endJulian = tss->endJulianDate;
		endTimeSeconds = tss->endTimeSeconds;
		timeOffsetSeconds = tss->timeOffsetSeconds;
		numberValues = tss->numberValues;
	}
	else {
		zmessageDebug(ifltab, functionId, "Computed time window", "");
		startJulian = tss->timeWindow->startJulian;
		startTimeSeconds = tss->timeWindow->startTimeSeconds;
		endJulian = tss->timeWindow->endJulian;
		endTimeSeconds = tss->timeWindow->endTimeSeconds;
		timeOffsetSeconds = tss->timeWindow->timeOffsetSeconds;
		numberValues = tss->timeWindow->numberValues;
	}

	zmessageDebugInt(ifltab, functionId, "startJulian-------->: ", startJulian);
	zmessageDebugInt(ifltab, functionId, "startTimeSeconds--->: ", startTimeSeconds);
	zmessageDebugInt(ifltab, functionId, "endJulian---------->: ", endJulian);
	zmessageDebugInt(ifltab, functionId, "endTimeSeconds----->: ", endTimeSeconds);
	if ((tss->timeWindow) && (tss->timeWindow->intervalSeconds > 0)) {
		zmessageDebugInt(ifltab, functionId, "Number Values------>: ", numberValues);
	}
	if (tss->times) {
		zmessageDebugInt(ifltab, functionId, "First value time (raw): ", tss->times[0]);
	}

	julianToDate(julianBaseDate, 104, cdate, sizeof(cdate));
	_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "Base date: %s, (julian: %d)",
		cdate, julianBaseDate);
	zmessageDebug(ifltab, functionId, messageString, "");

	if (isTimeDefined(startJulian, startTimeSeconds)) {
		julianToDate(startJulian, 104, cdate, sizeof(cdate));
		secondsToTimeString(startTimeSeconds, 0, 2, ctime, sizeof(ctime));
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "Start date: %s, (julian: %d)",
			cdate, startJulian);
		zmessageDebug(ifltab, functionId, messageString, "");
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "Start time: %s, (seconds: %d)",
			ctime, startTimeSeconds);
		zmessageDebug(ifltab, functionId, messageString, "");
	}
	else {
		zmessageDebug(ifltab, functionId, "Start date and time are UNDEFINED", "");
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "Start julian: %d, Start time: %d",
			startJulian, startTimeSeconds);
		zmessageDebug(ifltab, functionId, messageString, "");
	}

	if (isTimeDefined(endJulian, endTimeSeconds)) {
		julianToDate(endJulian, 104, cdate, sizeof(cdate));
		secondsToTimeString(endTimeSeconds, 0, 2, ctime, sizeof(ctime));
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "End date:   %s, (julian: %d)",
			cdate, endJulian);
		zmessageDebug(ifltab, functionId, messageString, "");
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "End time:   %s, (seconds: %d)",
			ctime, endTimeSeconds);
		zmessageDebug(ifltab, functionId, messageString, "");
	}
	else {
		zmessageDebug(ifltab, functionId, "End date and time are UNDEFINED", "");
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "End julian: %d, End time: %d",
			endJulian, endTimeSeconds);
		zmessageDebug(ifltab, functionId, messageString, "");
	}

	_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "Time offset: %d seconds",
		timeOffsetSeconds);
	zmessageDebug(ifltab, functionId, messageString, "");
	if ((tss->timeWindow) && (tss->timeWindow->intervalSeconds > 0)) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "Number of values: %d",
			numberValues);
		zmessageDebug(ifltab, functionId, messageString, "");
	}

	//  End of non-processed times?
	if (!tss->timeWindow) {
		return;
	}

	_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "Time interval: %d seconds",
		tss->timeWindow->intervalSeconds);
	zmessageDebug(ifltab, functionId, messageString, "");

	_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "Block size: %d",
		tss->timeWindow->blockSize);
	zmessageDebug(ifltab, functionId, messageString, "");
	if (isTimeDefined(tss->timeWindow->startBlockJulian, 1) && (tss->timeWindow->startBlockJulian != 0)) {
		julianToDate(tss->timeWindow->startBlockJulian, 104, cdate, sizeof(cdate));
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "Start block date: %s, (julian: %d)",
			cdate, tss->timeWindow->startBlockJulian);
		zmessageDebug(ifltab, functionId, messageString, "");
	}
	else {
		zmessageDebug(ifltab, functionId, "Start block julian is UNDEFINED", "");
	}

	if (isTimeDefined(tss->timeWindow->endBlockJulian, 1) && (tss->timeWindow->endBlockJulian != 0)) {
		julianToDate(tss->timeWindow->endBlockJulian, 104, cdate, sizeof(cdate));
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "End block date:   %s, (julian: %d)",
			cdate, tss->timeWindow->endBlockJulian);
		zmessageDebug(ifltab, functionId, messageString, "");
	}
	else {
		zmessageDebug(ifltab, functionId, "End block julian is UNDEFINED" ,"");
	}

	zmessageDebugInt(ifltab, functionId, "Internal startJulian-------->: ", tss->timeWindow->startJulian);
	zmessageDebugInt(ifltab, functionId, "Internal startTimeSeconds--->: ", tss->timeWindow->startTimeSeconds);
	zmessageDebugInt(ifltab, functionId, "Internal endJulian---------->: ", tss->timeWindow->endJulian);
	zmessageDebugInt(ifltab, functionId, "Internal endTimeSeconds----->: ", tss->timeWindow->endTimeSeconds);
}


