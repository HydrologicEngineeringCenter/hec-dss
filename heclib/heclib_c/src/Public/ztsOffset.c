#include "hecdss7.h"


/**
*  Function:	ztsOffset
*
*  Use:			public
*
*  Description:	 Computes the time offset between a date/time and the "standard DSS EOP"
*					date and time for the given interval.
*
*  Declaration: int ztsOffset(int intervalSeconds, int julian, int seconds);
*
*  Parameters:	int intervalSeconds
*					The time interval, in seconds.  This must be a standard interval, as shown below.
*
*				int julianStart
*					The julian day.
*
*				int secondsStart
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

int ztsOffset(int intervalSeconds, int julian, int seconds)
{


	int offsetSeconds;
	int year, month, day;
	int jul;
	int julBeg;
	int secs;
	int number;
	int leap;


	if (intervalSeconds < 1) return STATUS_NOT_OKAY;

	//  Copy and clean
	jul = julian;
	secs = seconds;
	cleanTime(&jul, &secs, SECOND_GRANULARITY);


	if (intervalSeconds <= SECS_IN_1_DAY) {
		// Daily or less (most common case)
		number = secs / intervalSeconds;
		offsetSeconds = secs - (number * intervalSeconds);
		return offsetSeconds;
	}

	if (intervalSeconds == SECS_IN_1_WEEK) {
		// Weekly (EOP is Saturday night at midnight)
		day = dayOfWeek(jul);
		offsetSeconds = ((day - 1) * SECS_IN_1_DAY) + secs;
		if (offsetSeconds == SECS_IN_1_WEEK) offsetSeconds = 0;
		return offsetSeconds;
	}

	//  Tri-monthly and above is when we have to start dealing with the
	//  odd end of period dates
	//  For example, the end of the month may be the 29, 30 or 31st.

	//  Convert to year, month, day
	julianToYearMonthDay (jul, &year, &month, &day);

	if (intervalSeconds <= SECS_IN_1_MONTH) {
		//  Monthly or monthly divisible interval
		//  If we are at (or near) end of month, offset is zero.
		//  "Close" to the end of month is not supported
		//  e.g., we assume that noon on last day of the
		//  month has no offset.  We're only concerned with
		//  data from first to mid-month
		if (month != 2) {
			if ((day == 30) || (day == 31)) {
				return 0;
			}
		}
		else {
			if (day >= 28) {
				//  February
				return 0;
			}
		}
		if (intervalSeconds == SECS_IN_TRI_MONTH) {
			//  Tri-monthly
			//  Generally reported for 10th, 20th and 30th
			//  But allow 2400 hrs on 9th, etc.
			if ((day == 9) || (day == 10)) {
				return 0;
			}
			if ((day == 19) || (day == 20)) {
				return 0;
			}
			if (day > 20) {
				day -= 20;
			}
			else if (day > 10) {
				day -= 10;
			}
			jul = yearMonthDayToJulian(year, month, day);
		}
		else if (intervalSeconds == SECS_IN_SEMI_MONTH) {
			//  Semi-monthly
			//  Generally reported for 15th and 30th
			if ((day == 14) || (day == 15)) {
				return 0;
			}
			if (day > 15) {
				day -= 15;
			}
			jul = yearMonthDayToJulian(year, month, day);
		}
		else if (intervalSeconds == SECS_IN_1_MONTH) {
			//  Monthly - computation done below
		}
		else {
			return STATUS_NOT_OKAY;
		}
		//  Go to first day of month and compute value
		julBeg = yearMonthDayToJulian(year, month, 1);
		//  Note - clean at start ensures that we are not BOP
		offsetSeconds = ((jul - julBeg) * SECS_IN_1_DAY) + secs;
	}
	else if (intervalSeconds == SECS_IN_1_YEAR) {
		//  Yearly (disregard time if on last day)
		if ((month == 12) && (day == 31)) {
			return 0;
		}
		//  Go to first day of month and compute value
		julBeg = yearMonthDayToJulian(year, 1, 1);
		//  If we are in a leap year, remove leap day
		leap = 0;
		if (isLeapYear(year)) {
			if (year > 0) {
				//  Need to account for leap day
				if (month > 2) {
					leap = 1;
				}
			}
			else {
				if (month > 2) {
					leap = -1;
				}
			}
		}

		offsetSeconds = ((jul - julBeg - leap) * SECS_IN_1_DAY) + secs;
	}
	else {
		//  Unrecognized interval
		return STATUS_NOT_OKAY;
	}

	return offsetSeconds;
}

