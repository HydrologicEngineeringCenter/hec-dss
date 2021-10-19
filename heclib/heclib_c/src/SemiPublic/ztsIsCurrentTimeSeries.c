#include <string.h>

#include "hecdssInternal.h"
#include "heclib.h"
#include "zStructTsTimeWindow.h"

/**
*  Function:	ztsIsCurrentTimeSeries
*
*  Use:			Semi-private
*
*  Description:	Deterimine if a data set for a time series pathname is for the current time;
*					i.e., does the current time fall somewhere between the start and end of the block
*
*  Declaration:int ztsIsCurrentTimeSeries(long long *ifltab, char* pathname) ;
*
*  Parameters:	long long *ifltab
*					The integer file table array passed among DSS functions.  Used to get DSS version
*
*				char *pathname
*					The time series pathname to check
*
*
*  Returns:		pathname with valid E part, if time series
*				int status:
*					 0:  Not in range or not time series.
*					 1:  Yes, in range
*
*
*	Remarks:	Used in copying or squeezing to determine if record might be expanding or not
*					If it had expanded, but not in current range, then assume won't expand
*					and don't allocate extra space; otherwise allocate space.
*
*
*	Author:			Bill Charley
*	Date:			2013
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int ztsIsCurrentTimeSeries(long long *ifltab, char* pathname)
{
	int istat;
	ztsTimeWindow timeWindow;
	int julianCurrent;
	int seconds;
	int millsPastSecond;

	timeWindow.startJulian = UNDEFINED_TIME;
	timeWindow.startTimeSeconds = -1;
	timeWindow.endJulian = UNDEFINED_TIME;
	timeWindow.endTimeSeconds = -1;

	istat = ztsGetPathTimeWindow(zgetVersion(ifltab), pathname, strlen(pathname), &timeWindow);
	if (istat < 0) {
		return 0;
	}

	getCurrentDateTime (&julianCurrent, &seconds, &millsPastSecond);

	if (julianCurrent < timeWindow.startJulian) {
		return 0;
	}
	if (julianCurrent > timeWindow.endJulian) {
		return 0;
	}
	//  We are within the current time range
	return 1;
}

