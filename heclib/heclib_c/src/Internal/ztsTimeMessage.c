#include "hecdssInternal.h"
#include "zdssMessages.h"

/**
*  Function:	ztsTimeMessage
*
*  Use:			Private
*
*  Description:	Convience function to print debug messages with time information.
*
*  Declaration: void ztsTimeMessage(long long *ifltab, const char *message, int timeMinOrSec, int timeGranularitySeconds, int julianBaseDate)
*
*  Parameters:	long long *ifltab
*					The file table array, similar to a handle number
*
*				const char *message
*					The message to print.
*
*				int timeMinOrSec
*					The time value to convert to character, given either in minutes since baseDate, or
*					seconds since baseDate.
*					"1" is one second past midnight.  "0" doesn't exist, as midnight belongs to the end
*					of the day, by convention, and midnight is the number SECS_IN_1_DAY.
*					secondsPastMidnight varies from 1 to SECS_IN_1_DAY.
*
*				int timeGranularitySeconds
*					The number of seconds each unit in timeMinOrSec represents,
*					MINUTE_GRANULARITY (60) or SECOND_GRANULARITY (1)
*
*				int julianBaseDate
*					The Julian base date, which when appropriatly aded to the time, give the full correct
*					date and time.
*
*	Returns:	None
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

void ztsTimeMessage(long long *ifltab, const char *message, int timeMinOrSec, int timeGranularitySeconds, int julianBaseDate)
{
	int numberInDay;
	int days;
	int timeOfDay;
	int julian;
	long long granularity;


	//  Convert minutes or seconds to days and seconds
	granularity = (long long)timeGranularitySeconds;
	if (granularity < 1) granularity = MINUTE_GRANULARITY;
	numberInDay = (int)(SECS_IN_1_DAY / granularity);
	days = timeMinOrSec / numberInDay;
	julian = julianBaseDate + days;

	//  Now start day and seconds
	timeOfDay = timeMinOrSec - (days * numberInDay);
	if (timeOfDay < 1) {
		julian--;
		timeOfDay += numberInDay;
	}
	if (granularity == MINUTE_GRANULARITY) timeOfDay *= SECS_IN_1_MINUTE;

	ztsDateMessage(ifltab, DSS_FUNCTION_ztsStore_ID, message, julian, timeOfDay);
}

