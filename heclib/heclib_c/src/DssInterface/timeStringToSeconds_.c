#include <string.h>

#include "heclib7.h"
#include "hecdssInternal.h"




int timestringtoseconds_(const char *timeString, size_t lenTimeString)
{
	char tString[9];

	copyAndTrim(tString, sizeof(tString), timeString, lenTimeString);

	return timeStringToSeconds(tString);
}

