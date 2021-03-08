#include <string.h>

#include "heclib7.h"
#include "hecdssInternal.h"
#include "heclibDate.h"

/**
*  Function:	timeStringToSecondsMills
*
*  Use:			Public
*
*  Description:	Converts a character time to seconds and fractions of a second, in milliseconds, past midnight.
*
*  Declaration: float timeStringToSecondsMills(const char *timeString);
*
*  Parameters:	const char *timeString
*					The time string to parse.  Examples of what can be parsed are provided in the table below
*
*
*	Returns:	secondsPastMidnight as a float, with the fractional portion being 3 digit millisecond.
*					secondsPastMidnight varies from 0.000 to 86400.000. A time of zero is allowed.
*					If the string was not able to be parsed, -1.0 is returned.
*
*
* Time Styles:
*
*      Style  Example				Returns
*
*          0:  0830               30600.000
*          1:  08:30              30600.000
*          2:  08:30:43           30643.000
*          3:  08:30:43.076       30643.076

*
*  Remarks:	Valid times range from "00:00:00.000" to "24:00:00.000".  Outside of this range, -1 will be returned.
*				No style is needed as this will attempt to parse, regardless of style.
*				Not intended to identify a string as a time; a valid time is assumed.
*
* See Also: timeStringToSeconds()
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

float timeStringToSecondsMills(const char *timeString)
{
	int itime;
	float ftime;
	int idigits[9];
	char ich;
	int len;
	int i;
	int icount;
	int ihr, imin, isec;
	int mills;
	int milliFound;

	len = (int)strlen(timeString);

	//  Convert numeric characters into integers
	icount = 0;
	milliFound = -1;
	for (i=0; i<len; i++) {
		ich = timeString[i];
		if ((ich > 47) && (ich < 58)) {
			idigits[icount++] = ich - 48;
			if (icount >= 9) break;
		}
		else if (ich == 46) {
			milliFound = icount;
		}
	}

	if (icount < 3) {
		return STATUS_NOT_OKAY;
	}

	//  Process milliseconds
	mills = 0;
	if (milliFound > 0) {
		for (i=milliFound; i<icount; i++) {
			mills = (mills * 10) + idigits[i];
		}
		icount = milliFound;
	}


	//  Determine if we have an odd or even number of digits
	if (isOdd(icount)) {
		ihr = idigits[0];
		imin = (idigits[1] * 10)  + idigits[2];
		if (icount == 5) {
			isec = (idigits[3] * 10)  + idigits[4];
		}
		else {
			isec = 0;
		}
	}
	else {
		ihr = (idigits[0] * 10) + idigits[1];
		imin = (idigits[2] * 10)  + idigits[3];
		if (icount == 6) {
			isec = (idigits[4] * 10)  + idigits[5];
		}
		else {
			isec = 0;
		}
	}
	itime = (ihr * 3600) + (imin * 60) + isec;
	ftime = (float)itime + ((float)mills * (float)0.001);
	return ftime;
}

