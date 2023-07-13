#include <string.h>

#include "heclibDate.h"
#include "standardIntervals.h"

/**
*  Function:	secondsToTimeString
*
*  Use:			Public
*
*  Description:	Returns a character time from the time in seconds past midnight, and optionally, mills past second
*
*  Declaration: void secondsToTimeString(int secondsPastMidnight, int millsPastSecond,
*					char *timeString, size_t sizeofTimeString, int timeStyle);
*
*  Parameters:	int secondsPastMidnight
*					The time of day in seconds past midnight.
*					"1" is one second past midnight.  Midnight is the number SECS_IN_1_DAY.
*					0 (zero) is allowed for Beginning of Period items.
*					secondsPastMidnight varies from 0 to SECS_IN_1_DAY.
*
*				int millsPastSecond
*					The time second fraction in mills past the secondsPastMidnight
*					variable.  millsPastSecond varies from 0 to 999.
*
*				int timeStyle
*					The style code of how the time should be returned, as given below in the table.
*
*				char *timeString (output)
*					A string to contain the complete time.  Must be dimensioned to hold string with null term.
*					Example time string: "08:23:12".
*
*				size_t sizeofTimeString
*					The size of timeString.  Should be at least the size shown in the table below.
*
*
*	Returns:	None
*
*
* Time Style Codes:
*
*      Style  Example     Minimum size of timeString
*
*          0:  0830               5
*          1:  08:30              6
*          2:  08:30:43           9
*          3:  08:30:43.076      13

*
*  Remarks:	Does not "clean time", assumes that you are passing in valid times.
*			Invalid styles, times, or lengths returns a null in the first char.
*			If first character is "0", that is always returned (no option to remove.)
*
* See Also: timeStringToSeconds()
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

void secondsToTimeString(int secondsPastMidnight, int millsPastSecond, int timeStyle,
						 char *timeString, size_t sizeofTimeString)
{
	int upper;
	int mid;
	int lower;
	int ihr, imin, isec;
	int icount;


	//  Time string must have a minimum of 5 characters
	if (sizeofTimeString < 5) {
		return;
	}

	//  Check for valid style and seconds
	//  Even though 0 seconds is not valid, we'll let it ride.
	if ((timeStyle < 0) || (timeStyle > 3) ||
	  (secondsPastMidnight < 0) || (secondsPastMidnight > SECS_IN_1_DAY)) {
		timeString[0] = '\0';
		return;
	}

	ihr = secondsPastMidnight / SECS_IN_1_HOUR;
	imin = (secondsPastMidnight - (ihr * SECS_IN_1_HOUR)) / SECS_IN_1_MINUTE;
	isec = secondsPastMidnight - (ihr * SECS_IN_1_HOUR) - (imin * SECS_IN_1_MINUTE);
	icount = 0;

	//  Hour portion
	upper = ihr / 10;
	lower = ihr - (upper * 10);
	//  48 is ASCII '0'
	upper += 48;
	lower += 48;
	timeString[icount++] = upper;
	timeString[icount++] = lower;
	if (timeStyle != 0) {
		timeString[icount++] = ':';
	}

	//  Minute portion
	upper = imin / 10;
	lower = imin - (upper * 10);
	upper += 48;
	lower += 48;
	timeString[icount++] = upper;
	timeString[icount++] = lower;

	//  Need at least 9 characters for times following
	if ((timeStyle < 2) || (sizeofTimeString < 9)) {
		if ((int)sizeofTimeString > icount) {
			timeString[icount++] = '\0';
		}
		return;
	}

	//  Seconds
	timeString[icount++] = ':';
	upper = isec / 10;
	lower = isec - (upper * 10);
	upper += 48;
	lower += 48;
	timeString[icount++] = upper;
	timeString[icount++] = lower;

	if ((timeStyle < 3) || (sizeofTimeString < 13)
		|| (millsPastSecond > 999) || (millsPastSecond < 0)) {
		if ((int)sizeofTimeString > icount) {
			timeString[icount++] = '\0';
		}
		return;
	}

	//  Mills  (".748")
	timeString[icount++] = '.';
	upper = millsPastSecond / 100;
	mid = (millsPastSecond - (upper * 100)) / 10;
	lower = millsPastSecond - (upper * 100) - (mid * 10);
	//  48 is ASCII '0'
	upper += 48;
	mid += 48;
	lower += 48;
	timeString[icount++] = upper;
	timeString[icount++] = mid;
	timeString[icount++] = lower;
	timeString[icount] = '\0';

}

