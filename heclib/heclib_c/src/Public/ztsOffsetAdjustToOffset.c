#include "hecdss7.h"

/**
*  Function:	ztsOffsetAdjustToOffset
*
*  Use:			public
*
*  Description:	Takes a date / time and adjusts it to the date / time for the offset given.
*
*  Declaration: int ztsOffsetAdjustToOffset(int offsetSeconds, int intervalSeconds, int *julian, int *seconds);
*
*  Parameters:	int offsetSeconds
*					The time offset to adjust to, in seconds.
*
*				int intervalSeconds
*					The time interval, in seconds.  This must be a standard interval, as shown below.
*
*				int julianStart (input - output)
*					The julian day.
*
*				int secondsStart (input - output)
*					The time, in seconds.
*
*
*	Returns:	int status:
*					STATUS_OKAY
*					STATUS_NOT_OKAY - invalid interval.
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

int ztsOffsetAdjustToOffset(int offsetSeconds, int intervalSeconds, int *julian, int *seconds)
{
	int year, month, day;
	int status;

	if (intervalSeconds < 1) return STATUS_NOT_OKAY;

	//  First adjust to standard
	ztsOffsetAdjustToStandard(intervalSeconds, julian, seconds);

	if (offsetSeconds == 0) {
		return STATUS_OKAY;
	}

	//  Decrement the date/time by one period
	status = incrementTime(intervalSeconds, -1, *julian, *seconds, julian, seconds);

	//  Add the offset
	*seconds += offsetSeconds;

	//  Clean up the date/time, and we are done!
	cleanTime(julian, seconds, SECOND_GRANULARITY);

	//  If yearly and in a leap year, need to adjust
	if (intervalSeconds == SECS_IN_1_YEAR) {
		julianToYearMonthDay (*julian, &year, &month, &day);
		if (isLeapYear(year)) {
			if (year > 0) {
				//  Need to account for leap day
				if (month > 2) {
					(*julian)++;
				}
			}
			else {
				if (month > 2) {
					(*julian)--;
				}
			}
		}
	}

	return status;
}

