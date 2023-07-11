#include <string.h>

#include "heclib7.h"
#include "hecdssInternal.h"

void minutesToHourMin(int minutes, char *hoursMins, size_t lenHoursMins)
{
	//  Takes minutes and converts it to a time character string, such as "0830"
	//

	int upper;
	int lower;
	int ihr, imin;


	ihr = minutes / SECS_IN_1_MINUTE;
	imin = minutes - (ihr * SECS_IN_1_MINUTE);

	if (lenHoursMins < 5) {
		return;
	}

	upper = ihr / 10;
	lower = ihr - (upper * 10);
	//  48 is ASCII '0'
	upper += 48;
	lower += 48;
	hoursMins[0] = upper;
	hoursMins[1] = lower;

	upper = imin / 10;
	lower = imin - (upper * 10);
	upper += 48;
	lower += 48;
	hoursMins[2] = upper;
	hoursMins[3] = lower;

	hoursMins[4] = '\0';
}

void minutestohourmin_(int *minutes, char *hoursMins, size_t lenHoursMins)
{
	char tString[5];

	minutesToHourMin(*minutes, tString, sizeof(tString));
	stringCToFort(hoursMins, lenHoursMins, tString);
}

