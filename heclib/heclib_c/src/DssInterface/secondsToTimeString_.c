#include <string.h>

#include "heclib7.h"
#include "hecdssInternal.h"
#include "fortran_string_len_size.h"


void secondstotimestring_(int *seconds, int *millsPastSecond, int *timeStyle, char *timeString, slen_t lenTimeString)
{
	char tString[20];

	secondsToTimeString(*seconds, *millsPastSecond, *timeStyle, tString, sizeof(tString));
	stringCToFort(timeString, lenTimeString, tString);
}

