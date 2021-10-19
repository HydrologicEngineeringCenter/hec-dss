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

zStructTimeSeries* zstructTsNewTimes(const char* pathname, const char* startDate, const char* startTime,
											const char* endDate, const char* endTime)
{
	zStructTimeSeries *tss;


	tss = zstructTsNew(pathname);

	if (startDate && (strlen(startDate) > 0)) {
		tss->startJulianDate = dateToJulian(startDate);
	}
	if (startTime && (strlen(startTime) > 0)) {
		tss->startTimeSeconds = timeStringToSeconds(startTime);
	}

	if (endDate && (strlen(endDate) > 0)) {
		tss->endJulianDate = dateToJulian(endDate);
	}
	if (endTime && (strlen(endTime) > 0)) {
		tss->endTimeSeconds = timeStringToSeconds(endTime);
	}


	return tss;
}

