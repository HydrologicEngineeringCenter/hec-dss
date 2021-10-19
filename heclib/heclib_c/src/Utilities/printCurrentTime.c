#include <stdio.h>

#include "heclibDate.h"


//  Routine to print current date and time
void printCurrentTime(int lineFeed)
{
	char date[30];
	char time[30];
	int julian, secondsPastMidnight, millsPastSecond;

	getCurrentDateTime (&julian, &secondsPastMidnight, &millsPastSecond);
	getDateTimeString (julian, date, sizeof(date), 0, secondsPastMidnight, time, sizeof(time), 2);

	printf ("%s, %s.%d", date, time, millsPastSecond);
	if (lineFeed) {
		printf("\n");
	}

}

