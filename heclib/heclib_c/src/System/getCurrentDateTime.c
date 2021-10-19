#include <sys/timeb.h>
#include <time.h>

#include "heclibDate.h"

/**
*  Function:	getCurrentDateTime
*
*  Use:			Public
*
*  Description:	Returns the current local system time in Julian days, seconds past midnight and mills past second
*					Local system time means that the time zone is included, but daylight time is not.
*
*  Declaration: void getCurrentDateTime (int *julian, int *secondsPastMidnight, int *millsPastSecond);
*
*  Parameters:	int *julian (output)
*					Current date in days since 01Jan1900, with 01Jan1900 being day 1 (31Dec1899 is day 0)
*					This is the standard day count used throught DSS and can be negative (or large).
*					An undefined day is the number "-2147483647".  Note, 0 (zero) is valid day and used a lot!
*
*				int *secondsPastMidnight (output)
*					The current time of day in seconds past midnight.
*					"1" is one second past midnight.  "0" doesn't exist, as midnight belongs to the end
*					of the day, by convention, and midnight is the number "86400".
*					secondsPastMidnight varies from 1 to 86400.
*
*				int *millsPastSecond (output)
*					The current time second fraction in mills past the secondsPastMidnight
*					variable.  millsPastSecond varies from 0 to 999.
*
*
*	Returns:	None
*
*	Remarks:	This is a system dependent function, although all systems have similar functions
*
*	See Also:	getCurrentDateTimeString()
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

void getCurrentDateTime (int *julian, int *secondsPastMidnight, int *millsPastSecond)
{
	long long seconds;
	long long days;

#ifdef _MSC_VER
	struct _timeb timebuffer;

    _ftime64_s( &timebuffer );
	seconds = timebuffer.time - (long long)(timebuffer.timezone * 60);
	if (timebuffer.dstflag) {
		//  Only do USA DST (60 minutes)
		seconds += 3600;
	}
	*millsPastSecond = timebuffer.millitm;

#else
	struct timespec spec;

	clock_gettime(CLOCK_REALTIME, &spec);
	seconds = (long long)spec.tv_sec;
	*millsPastSecond = spec.tv_nsec / 1.0e6; // Convert nanoseconds to milliseconds
#endif

	days = seconds/86400;
	*julian = (int)days + 25568;   //  25568 is julian day for 01Jan1970
	*secondsPastMidnight = (int)(seconds - (days * 86400));


}

void getcurrentdatetime_(int *julian, int *secondsPastMidnight, int *millsPastSecond)
{
	getCurrentDateTime(julian, secondsPastMidnight, millsPastSecond);
}

