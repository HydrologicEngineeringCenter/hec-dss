#include <string.h>

#include "heclib7.h"
#include "hecdssInternal.h"



void secondstotimestring_(int *seconds, int *millsPastSecond, int *timeStyle, char *timeString, size_t lenTimeString)
{
	char tString[20];

	secondsToTimeString(*seconds, *millsPastSecond, *timeStyle, tString, sizeof(tString));
	stringCToFort(timeString, lenTimeString, tString);
}

