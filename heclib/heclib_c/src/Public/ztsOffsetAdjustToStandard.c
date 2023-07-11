#include "heclib7.h"

/**
*  Function:	ztsOffsetAdjustToStandard
*
*  Use:			public
*
*  Description:	Takes a date / time and adjusts it to the standard date / time for that interval.
*				Returns the time offset
*
*  Declaration: int ztsOffsetAdjustToStandard(int intervalSeconds, int *julian, int *seconds);
*
*  Parameters:	int intervalSeconds
*					The time interval, in seconds.  This must be a standard interval, as shown below.
*
*				int julianStart (input - output)
*					The julian day.
*
*				int secondsStart (input - output)
*					The time, in seconds.
*
*
*	Returns:	The time offset from standard time for that interval
*
*	Remarks:	The time offset is given in seconds from the START of the interval.
*				This is because for intervals of different length (e.g., monthly),
*				the end of the interval is ragged and the day would be different for
*				each month.
*	Comments:
*				DSS "standard time" is considered End of Period (EOP),
*				e.g., midnight or 2400 hours for daily data.
*				If, on standard time, the offset is zero.
*				If not, then the offset is time from the standard time
*				to the reported time (for compatibility with version 6).
*				For example, for daily data, if the data is reported at
*				0100 (1 a.m.), the offset is 1 hour, or 3600 seconds.
*				If data is reported at 2300 (11 p.m.), the offset is
*				23 hours or 23 * 3600 = 82800 seconds.
*				(The offset is not the difference between the reported time
*				to the standard time, but standard to reported.)
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
*
*
*
*
*	Author:			Bill Charley, 2014
*
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int ztsOffsetAdjustToStandard(int intervalSeconds, int *julian, int *seconds)
{


	int offsetSeconds;
	int year, month, day;


	if (intervalSeconds < 1) return STATUS_NOT_OKAY;

	cleanTime(julian, seconds, SECOND_GRANULARITY);
	offsetSeconds = ztsOffset(intervalSeconds, *julian, *seconds);

	if (intervalSeconds <= SECS_IN_1_WEEK) {
		// Weekly or less (most common case)
		//  A direct computation
		if (offsetSeconds > 0) {
			*seconds += intervalSeconds - offsetSeconds;
			cleanTime(julian, seconds, SECOND_GRANULARITY);
		}
		return offsetSeconds;
	}


	//  Tri-monthly and above is when we have to start dealing with the
	//  odd end of period dates
	//  For example, the end of the month may be the 29, 30 or 31st.

	//  Convert to year, month, day
	julianToYearMonthDay (*julian, &year, &month, &day);

	if (intervalSeconds <= SECS_IN_1_MONTH) {
		//  Monthly or monthly divisible interval
		if (intervalSeconds == SECS_IN_TRI_MONTH) {
			//  Tri-monthly
			//  Generally reported for 10th, 20th and 30th
			if (day <= 10) {
				day = 10;
			}
			else if (day <= 20) {
				day = 20;
			}
			else {
				day = 28;
			}
		}
		else if (intervalSeconds == SECS_IN_SEMI_MONTH) {
			//  Semi-monthly
			if (day <= 15) {
				day = 15;
			}
			else {
				day = 28;
			}
		}
		else if (intervalSeconds == SECS_IN_1_MONTH) {
			//  Monthly
			day = 28;
		}
		else {
			return STATUS_NOT_OKAY;
		}
		if (day >= 25) {
			month++;
			*julian = yearMonthDayToJulian(year, month, 1) - 1;
		}
		else {
			*julian = yearMonthDayToJulian(year, month, day);
		}
	}
	else if (intervalSeconds == SECS_IN_1_YEAR) {
		//  Yearly
		*julian = yearMonthDayToJulian(year, 12, 31);
	}
	else {
		//  Unrecognized interval
		return STATUS_NOT_OKAY;
	}
	*seconds = SECS_IN_1_DAY;
	return offsetSeconds;
}

