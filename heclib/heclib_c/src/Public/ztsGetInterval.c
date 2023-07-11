
#include "heclib.h"


/**
*  Function:	ztsGetInterval
*
*  Use:			Public
*
*  Description:	 A convienence function for ztsGetStandardInterval.
*					Returns the time interval for a regular time series pathname,
*					or block size for an irregular times series pathname.  Returns 0, if not time series.
*					For DSS Version 6, the interval is in seconds.
*					For DSS Version 7, the interval is in minutes.
*					For irregular interval, the block size is negative
*
*  Declaration: int ztsGetInterval(int dssVersion, const char *pathname);
*
*  Parameters:	int dssVersion
*					The DSS Version the results are for.  Must either be 6 or 7.  (Versions have different intervals)
*
*				const char *pathname
*					The pathname to get the interval from
*
*				size_t sizeofEpart (input)
*					The size of the E part, used when returning the E part from the interval.
*
*
*
*	Returns:	time interval, seconds for DSS-7 and minutes for DSS-6, or the block size as a negative number
*				0, if not time series
*				1 for irregular interval
*
*
*  Standard Intervals:
*					Name			Seconds
*					"1Year"			31536000	(365 days)
*					"1Month"		2592000		(30 days)
*					"Semi-Month"	1296000		(15 days)
*					"Tri-Month"		864000		(10 days)
*					"1Week"			604800		(7 days, EOP Saturday, 2400 (7))
*					"1Day"			86400
*					"12Hour"		43200
*					"8Hour"			28800
*					"6Hour"			21600
*					"4Hour"			14400
*					"3Hour"			10800
*					"2Hour"			7200
*					"1Hour"			3600
*					"30Minute"		1800
*					"20Minute"		1200
*					"15Minute"		900
*					"12Minute"		720
*					"10Minute"		600
*					"6Minute"		360
*					"5Minute"		300
*					"4Minute"		240
*					"3Minute"		180
*					"2Minute"		120
*					"1Minute"		60
*					"30Second"		30
*					"20Second"		20
*					"15Second"		15
*					"10Second"		10
*					"6Second"		6
*					"5Second"		5
*					"4Second"		4
*					"3Second"		3
*					"2Second"		2
*					"1Second"		1
*					"IR-Century"	-5
*					"IR-Decade"		-4
*					"IR-Year"		-3
*					"IR-Month"		-2
*					"IR-Day"		-1
*
*
*
*	Author:			Bill Charley, 2014
*
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int ztsGetInterval(int dssVersion, const char *pathname)
{
	int istat;
	int len;
	int operation;
	int interval;
	char ePart[MAX_PART_SIZE];

	len = zpathnameGetPart (pathname, 5, ePart, sizeof(ePart));
	if (len > 3) {
		operation = EPART_TO_SECONDS;
		istat = ztsGetStandardInterval(dssVersion, &interval, ePart, sizeof(ePart), &operation);
		if ((istat == 0) || (istat == 1)) {
			return interval;
		}
	}
	return 0;
}


