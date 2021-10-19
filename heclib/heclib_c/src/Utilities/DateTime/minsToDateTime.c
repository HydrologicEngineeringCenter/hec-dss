#include "heclibDate.h"

void minsToDateTime(int minsSince1900, char *dateString, char *timeString, size_t sizeofDateString, size_t sizeofTimeString)
{
	//  Takes minutes since 01Jan1900 and converts it to a date and time character string
	//
	int julian;
	int minsSinceMidnight;
	

	julian = minsSince1900 / 1440;
	minsSinceMidnight = minsSince1900 - (julian * 1440);
	julianToDate(julian, 4, dateString, sizeofDateString);
	minutesToHourMin(minsSinceMidnight, timeString, sizeofTimeString);

}

