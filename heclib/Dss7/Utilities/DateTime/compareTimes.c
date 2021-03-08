#include "heclib.h"
#include "heclibDate.h"

/**
*  Function:	compareTimes
*
*  Use:			Semi-public
*
*  Description:	 Compares two date/times and returns:
*					-1 if first date/time is before second
*					0 if times are the same
*				    1 if first date/time is after second
*
*  Declaration: int compareTimes(int julianFirst, int secondsFirst, int julianBaseFirst,
*								 int julianSecond, int secondsSecond, int julianBaseSecond);
*
*  Parameters:	int julianFirst
*					The Julian date of the first date/time to compare.
*
*				int timeFirst
*					The first time in timeGranularitySeconds (60 for minutes, 1 for seconds)
*					to compare.
*
*				int julianBaseFirst
*					The Julian base date for the first date.  Usually 0 (zero),
*					unless you are comparing large times used in statistical analysis.
*
*				int timeGranularitySecondsFirst
*					The number of seconds a unit in *itime represents, usually
*					MINUTE_GRANULARITY (60) or SECOND_GRANULARITY (1)
*
*				int julianSecond
*					The Julian date of the second date/time to compare against.
*
*				int timeSecond
*					The Second time in timeGranularitySeconds (60 for minutes, 1 for seconds)
*					to compare.
*
*				int julianBaseSecond
*					The Julian base date for the second date.  Usually 0 (zero)
*
*				int timeGranularitySecondsSecond
*					The number of seconds a unit in *itime represents, usually
*					MINUTE_GRANULARITY (60) or SECOND_GRANULARITY (1)
*
*
*  Returns:		status
*		 			-1 if first date/time is before second
*					0 if times are the same
*				    1 if first date/time is after second
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int compareTimes(int julianFirst, int timeFirst, int julianBaseFirst, int timeGranularitySecondsFirst,
				 int julianSecond, int timeSecond, int julianBaseSecond, int timeGranularitySecondsSecond)
{
	int jul1;
	int jul2;
	int time1;
	int time2;

	jul1 = julianFirst + julianBaseFirst;
	time1 = timeFirst;
	cleanTime(&jul1, &time1, timeGranularitySecondsFirst);
	//  Be sure we are in the same time units (seconds)
	if (timeGranularitySecondsFirst > 1) time1 *= timeGranularitySecondsFirst;

	jul2 = julianSecond + julianBaseSecond;
	time2 = timeSecond;
	cleanTime(&jul2, &time2, timeGranularitySecondsSecond);
	if (timeGranularitySecondsSecond > 1) time2 *= timeGranularitySecondsSecond;

	if (jul1 < jul2) return -1;
	if (jul1 > jul2) return 1;

	//  Days are the same - compare seconds
	if (time1 < time2) return -1;
	if (time1 > time2) return 1;

	// same times
	return 0;
}

