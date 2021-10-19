#include <string.h>

#include "hecdssInternal.h"
#include "heclib.h"

void zgetinterval_(int *intervalMins, char *Epart, int *numberData,
				   int *status, size_t lengthEpart)
{
	// ************* UNTESTED and LIMITED ******************
	char cepart[MAX_PART_SIZE];
	size_t len;

	len = sizeof(cepart)-1;
	if (*status == 1) {
		copyAndTrim(cepart, len, Epart, lengthEpart);
		*status = ztsGetStandardInterval(7,intervalMins, cepart, len, status);
	}
	else {
		*status = ztsGetStandardInterval(7,intervalMins, cepart, len, status);
		///   FIX ME copytofort should return string length
		stringCToFort(Epart, lengthEpart,  cepart);
	}

}

