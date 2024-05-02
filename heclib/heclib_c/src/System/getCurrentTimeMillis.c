#include <sys/timeb.h>
#include <time.h>

#include "heclib7.h"

/**
*  Function:	getCurrentTimeMillis
*
*  Use:			Public
*
*  Description:	Returns the current local system time in milliseconds since Jan 01, 1970, as a long long.
*					Local system time means that the time zone is included, but daylight time is not.
*
*  Declaration: long long getCurrentTimeMillis();
*
*  Parameters:	None
*
*
*	Returns:	long long time
*					The local system clock time, in milliseconds since Jan 01, 1970.
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

long long getCurrentTimeMillis()
{
	long long ltime;

#ifdef _MSC_VER
	struct _timeb timebuffer;

	_ftime64_s( &timebuffer );
	ltime = (timebuffer.time - (long long)(timebuffer.timezone * SECS_IN_1_MINUTE));
	ltime *= 1000L;
	ltime += (long long)timebuffer.millitm;
	if (timebuffer.dstflag) {
		//  Only do USA DST (60 minutes)
		ltime += SECS_IN_1_HOUR * 1000L;
	}
#else
	struct timespec spec;

	clock_gettime(CLOCK_REALTIME, &spec);
	ltime = spec.tv_nsec / 1.0e6; // Convert nanoseconds to milliseconds
	ltime += (long long)spec.tv_sec * 1000L;  //  Add current seconds since 1970, * 1000
#endif
	return ltime;
}

