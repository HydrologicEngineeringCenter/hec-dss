#include <string.h>

#include "hecdssInternal.h"
#include "heclib.h"

/**
*  Function:	getDateAndTime
*
*  Use:			Public
*
*  Description:	Returns the date and time from params in a time series struct.
*
*  Declaration: int getDateAndTime(int time, int timeGranularitySeconds, int julianBaseDate,
				   char *dateString, int sizeOfDateString, char* hoursMins, int sizeofHoursMins);
*
*  Parameters:	int time
*					The time value to convert to character, given in timeGranularitySeconds since baseDate.
*					"1" is one second past midnight.  "0" doesn't exist, as midnight belongs to the end
*					of the day, by convention, and midnight is the number SECS_IN_1_DAY.
*					secondsPastMidnight varies from 1 to SECS_IN_1_DAY.
*
*				int timeGranularitySeconds
*					The number of seconds each unit in time represents,
*					MINUTE_GRANULARITY (60) or SECOND_GRANULARITY (1) or 
*					HOUR_GRANULARITY (SECS_IN_1_HOUR) or DAY_GRANULARITY (SECS_IN_1_DAY)
*
*				int julianBaseDate
*					The Julian base date, which when appropriatly aded to the time, give the full correct
*					date and time.
*
*				char dateString[20]
*					The character string to return the date in.  Must be dimensioned to 13.
*
*				int sizeOfDateString
*					The length of the string.  Must be at least 13.
*
*				char hoursMins[9]
*					The character string to return the hours and minutes in.  Must be dimensioned to 9.
*
*				int sizeofHoursMins
*					The length of the string.  Must be at least 9.
*
*
*	Returns:	None
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int getDateAndTime(int time, int timeGranularitySeconds, int julianBaseDate,
				   char *dateString, int sizeOfDateString, char* hoursMins, int sizeofHoursMins)
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
	days = time / numberInDay;
	julian = julianBaseDate + days;

	//  Now start day and seconds
	timeOfDay = time - (days * numberInDay);
	if (timeOfDay < 1) {
		julian--;
		timeOfDay += numberInDay;
	}

	if (timeGranularitySeconds == SECOND_GRANULARITY) {
		secondsToTimeString(timeOfDay, 0, 2, hoursMins, sizeofHoursMins);
	}
	else if (timeGranularitySeconds == MINUTE_GRANULARITY) {
		minutesToHourMin(timeOfDay, hoursMins, sizeofHoursMins);
	}
	else {
		//  Get minutes
		timeOfDay *= (timeGranularitySeconds / SECS_IN_1_MINUTE);
		minutesToHourMin(timeOfDay, hoursMins, sizeofHoursMins);
	}
	julianToDate(julian, 4, dateString, sizeOfDateString);

	return 0;
}

