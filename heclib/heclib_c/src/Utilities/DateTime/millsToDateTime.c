#include "heclibDate.h"
#include "standardIntervals.h"

void millsToDateTime(long long mills, char *dateString, char *timeString, size_t sizeofDateString, size_t sizeofTimeString)
{
	//  Takes milli-seconds since 01Jan1970 and converts it to a date and time character string
	//
	int julian;
	int secondsPastMidnight;
	int millsPastSecond;
	long long secs;
	long long jul;
	
	secs = mills / 1000L;
	millsPastSecond = (int)(mills - (secs * 1000L));
	jul = secs / SECS_IN_1_DAY;  // Since 01Jan1970
	secondsPastMidnight = (int)(secs - (jul * SECS_IN_1_DAY));
	julian = (int)jul + JUL_01JAN1970;  //  Since 01Jan1900

	julianToDate(julian, 4, dateString, sizeofDateString);
	secondsToTimeString(secondsPastMidnight, millsPastSecond, 3, timeString, sizeofTimeString);

}
