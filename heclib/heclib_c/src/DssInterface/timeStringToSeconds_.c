#include <string.h>

#include "heclib7.h"
#include "hecdssInternal.h"
#include "fortran_string_len_size.h"



int timestringtoseconds_(const char *timeString, slen_t lenTimeString)
{
	char tString[9];

	copyAndTrim(tString, sizeof(tString), timeString, lenTimeString);

	return timeStringToSeconds(tString);
}

