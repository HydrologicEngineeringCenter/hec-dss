#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"
#include "hecdssFort.h"

/**
*  Function:	ztsGetFirstLastRecordTimes
*
*  Use:			Semi-Public
*
*  Description:	For a single time series record, get the start and ending dates for valid data, and, optionally, times.
*
*  Declaration: int ztsGetFirstLastRecordTimes (long long *ifltab, const char *pathname,
*												int *firstJulian, int *firstSeconds,
*												int *lastJulian, int *lastSeconds,
*												int boolGetSecondsAlso);
*  Parameters:	long long *ifltab
*					The file table array, similar to a handle number
*
*				const char* pathname
*					The pathname of the record to querry.
*
*				int *firstJulian
*					Returns the julian date of the first valid value
*
*				int *firstSeconds
*					Returns the time in seconds from midnight of julian date of the first valid value,
*					if boolGetSecondsAlso is set to 1.
*
*				int *lastJulian
*					Returns the julian date of the last valid value
*
*				int *lastSeconds
*					Returns the time in seconds from midnight of julian date of the last valid value,
*					if boolGetSecondsAlso is set to 1.
*
*				int boolGetSecondsAlso
*					A boolean flag passed in to have the seconds returned also (slower).
*					Set to 0 (zero) not to return seconds, 1 to return seconds.
*					Additional resources are used to return seconds.
*
*
*
*	Returns:	int status
*					STATUS_OKAY
*					error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*					Returns STATUS_NOT_OKAY (-1) if not time series
*
*	Remarks:	Works for both regular and irregular interval data
*					Getting just the dates is much more efficient then times also,
*					as the dates are in the pathname bin, but times have to be read
*					from the info block.  (i.e., if you don't need times, set boolGetSecondsAlso = 0)
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int ztsGetFirstLastRecordTimes(long long *ifltab, const char *pathname,
								int *firstJulian, int *firstSeconds,
								int *lastJulian, int *lastSeconds,
								int boolGetSecondsAlso)
{

	int dataType;
	int catSort;
	int status;
	char cdum[5];
	int idum1, idum2, found;
	long long *info;

	*firstJulian = 0;
	*firstSeconds = 0;
	*lastJulian = 0;
	*lastSeconds = 0;


	if (zgetVersion(ifltab) == 6) {
		ztsinfo_(ifltab, pathname, firstJulian, firstSeconds,
			lastJulian, lastSeconds, cdum, cdum, &idum1, &idum2, &found, strlen(pathname), 0, 0);
		if (found) {
			*firstSeconds *= SECS_IN_1_MINUTE;
			*lastSeconds *= SECS_IN_1_MINUTE;
			return STATUS_RECORD_FOUND;
		}
		return STATUS_RECORD_NOT_FOUND;
	}

	status = zcheck(ifltab, pathname);
	if (status != STATUS_RECORD_FOUND) {
		return status;
	}

	i8toi4(ifltab[zdssKeys.kbinTypeAndCatSort], &dataType, &catSort);
	if ((dataType < 100) || (dataType > 199)) {
		return STATUS_NOT_OKAY;
	}

	if (boolGetSecondsAlso) {
		status = zreadInfo(ifltab, pathname, 0);
		if (status != STATUS_RECORD_FOUND) {
			return status;
		}
		info = (long long *)ifltab[zdssKeys.kinfo];
		i8toi4(info[zdssInfoKeys.kinfoFirstDate], firstJulian, firstSeconds);
		i8toi4(info[zdssInfoKeys.kinfoLastDate], lastJulian, lastSeconds);
		if (*firstSeconds == 0) {
			*firstJulian -= 1;
			*firstSeconds = SECS_IN_1_DAY;
		}
		if (*lastSeconds == 0) {
			*lastJulian -= 1;
			*lastSeconds = SECS_IN_1_DAY;
		}
	}
	else {
		i8toi4(ifltab[zdssKeys.kbinDates], firstJulian, lastJulian);
	}

	return STATUS_OKAY;
}

