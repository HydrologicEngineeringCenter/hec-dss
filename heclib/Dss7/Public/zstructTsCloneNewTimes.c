#include <string.h>

#include "heclib.h"
#include "hecdssInternal.h"

#include "zStructTimeSeries.h"

/*
	Description


	You must not reuse this strut.  Make a new one for every dataset.
	Call zstructFree after the storage is complete

	pathname:		The pathname of the data to store or retrieve.  The E part specifies time granularity and interval of data

	startDate:		date of the first value, e.g. "04Jul2004".  (Multiple date formats are supported.)

	starttime:		corresponding startTime of the first value, e.g. "0700" or "07:00:00"


*/

zStructTimeSeries* zstructTsCloneNewTimes(zStructTimeSeries *tss, const char* pathname, 
										const char* startDate, const char* startTime,
										const char* endDate, const char* endTime)
{
	zStructTimeSeries *tssClone;


	tssClone = zstructTsClone(tss, pathname);
	if (!tssClone) return (zStructTimeSeries*)0;

	//  Reset all times to undefined
	//  Only for regular interval data do we remove the time array.
	if (tssClone->times && (tssClone->timeIntervalSeconds > 0)) {
		free(tssClone->times);
		tssClone->allocated[zSTRUCT_TS_times] = 0;
		tssClone->times = (int *)0;
	}

	tssClone->julianBaseDate =			0;
	tssClone->startJulianDate =			UNDEFINED_TIME;
	tssClone->startTimeSeconds =		UNDEFINED_TIME;
	tssClone->endJulianDate =			UNDEFINED_TIME;
	tssClone->endTimeSeconds =			UNDEFINED_TIME;
	tssClone->timeGranularitySeconds =	0;
	tssClone->timeIntervalSeconds =		0;
	tssClone->timeOffsetSeconds =		0;
	tssClone->boolRetrieveAllTimes =	0;
	tssClone->boolPattern = 0;

	if (startDate && (strlen(startDate) > 0)) {
		tssClone->startJulianDate = dateToJulian(startDate);
	}
	if (startTime && (strlen(startTime) > 0)) {
		tssClone->startTimeSeconds = timeStringToSeconds(startTime);
	}

	if (endDate && (strlen(endDate) > 0)) {
		tssClone->endJulianDate = dateToJulian(endDate);
	}
	if (endTime && (strlen(endTime) > 0)) {
		tssClone->endTimeSeconds = timeStringToSeconds(endTime);
	}

	return tssClone;
}

