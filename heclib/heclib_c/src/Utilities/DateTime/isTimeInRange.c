#include "hecdssInternal.h"
#include "heclibDate.h"
#include "zStructTsTimeWindow.h"

/**
*  Function:	isTimeInRange
*
*  Use:			Private
*
*  Description:	 Determine if the time value given is in range of the specified time window.
*					Used in irregular-interval processes
*
*  Declaration: int isTimeInRange(int timeValue, int *lastValueStatus, int timeGranularitySeconds,
*								  int baseDate, ztsTimeWindow *timeWindow);
*
*  Parameters:	int timeValue
*					The time value to compare, in timeGranularitySeconds (minutes or seconds) from baseDate.
*
*				int *lastValueStatus
*					(Input and output) Flag indicating if value is before, in or after
*					time window.  Uses as input and returns for this value
*						*lastValueStatus < 0:   previous time was before time window
*						*lastValueStatus == 0:  previous time was within time window
*						*lastValueStatus > 0:   previous time was after time window (and probably break)
*
*				int timeGranularitySeconds
*					The number of seconds a unit in timeValue represents, usually
*					MINUTE_GRANULARITY (60) or SECOND_GRANULARITY (1)
*
*				int baseDate
*					The julian base date (days since Jan 01, 1900) for this time.
*
*				ztsTimeWindow *timeWindow
*					The time window to compare to.
*
*
*  Returns:		boolInRange
*		 			Zero (false), if not within range
*					One (true) if within range
*
*	Remarks:	This function is a convenience function used to quickly test if a time is
*					within the requested time window, for irregular interval date.
*					timeGranularitySeconds is usually 60 for minutes,
*					but often 1 for seconds, and also can be larger
*					for very long time spans - HOUR_GRANULARITY (3600) for hours
*                   ory DAY_GRANULARITY (86400) for days
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int isTimeInRange(int timeValue, int *lastValueStatus, int timeGranularitySeconds,
				 int baseDate, ztsTimeWindow *timeWindow)
{

	long long jul;
	long long ltime;


	//  Determine if it is before or after, otherwise it is within
	if (timeGranularitySeconds < 1) timeGranularitySeconds = MINUTE_GRANULARITY;
	if (*lastValueStatus < 0) {
		// Is it before?
		jul = timeWindow->startJulian - baseDate;
		ltime = (jul * (long long)(SECS_IN_1_DAY /timeGranularitySeconds)) + (timeWindow->startTimeSeconds / timeGranularitySeconds);
		if ((long long)timeValue < ltime) {
			return 0;
		}
	}

	// Is it after?
	jul = (long long)(timeWindow->endJulian - baseDate);
	ltime = (jul * (long long)(SECS_IN_1_DAY /timeGranularitySeconds)) + (timeWindow->endTimeSeconds / timeGranularitySeconds);
	if ((long long)timeValue > ltime) {
		*lastValueStatus = 1;
		return 0;
	}


	//  Neither before or after, so it must be within the range
	*lastValueStatus = 0;
	return 1;
}

