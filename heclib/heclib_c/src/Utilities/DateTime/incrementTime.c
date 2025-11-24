#include <string.h>
#include <stdlib.h>

#include "heclib7.h"
#include "hecdssInternal.h"

/**
*  Function:	incrementTime
*
*  Use:			public
*
*  Description:	 Increments a Julian date and time (in seconds) by a number of periods for a standard interval.
*
*  Declaration: int incrementTime(int intervalSeconds, int numberPeriods,
*								  int julianStart, int secondsStart,
								  int *julianEnd, int *secondsEnd);
*
*  Parameters:	int intervalSeconds
*					The time interval, in seconds.  This must be a standard interval, as shown below.
*
*				int numberPeriods
*					The number of periods to increment by.  This may be positive, negative or zero.
*
*				int julianStart
*					The julian day to increment from.
*
*				int secondsStart
*					The time, in seconds, of the day to increment from.
*
*				int *julianEnd (output)
*					The resulting julian day after incrementing.
*
*				int *secondsEnd (output)
*					The time, in seconds, of the day after incrementing.
*					Will be   0 < secondsEnd <= SECS_IN_1_DAY
*
*	Returns:	STATUS_OKAY:  Successful
*				STATUS_NOT_OKAY:  Invalid interval
*
*	Remarks:	"Cleans" output time, so that seconds are within 1 day.
*				For monthly, yearly, etc. intervals, the date/time is incremented
*				according to a logical interval.  For example, for a monthly
*				interval the date/time is increment by months, not 30 days
*				(e.g., Jan 31 to Feb 28 to Mar 31 to Apr 30).
*				Accounts for "offsets", for example daily data at 8:00 am,
*				and monthly data occurring on the 5th of the month
*				(e.g., Jan 5 to Feb 5 to Mar 5 to Apr 5).
*				For intervals of tri-month and above, any time near the end of the period is
*				considered "End of Period".  So, 29 January is seen as month end of period with no offset.
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
*	Author:			Originally written in Fortran by Bill Charley, April 1993
*					Updated by Bill Charley, 2014
*
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int incrementTime(int intervalSeconds, int numberPeriods,
				  int julianStart, int secondsStart,
				  int *julianEnd, int *secondsEnd)
{


	long long intervalMinutes;
	int offsetSeconds;
	int imonth;
	int iremain;
	int year, month, day;
	long long lseconds;
	long long longNumber;


	if (intervalSeconds < 1) return STATUS_NOT_OKAY;

	//  Set end to start
	*julianEnd = julianStart;
	*secondsEnd = secondsStart;


	//  If the interval is less than tri-monthly, then it is a simple addition
	//  Check for less than 10 days
	if (intervalSeconds < SECS_IN_TRI_MONTH) {
		// Do computations in 64 bit math in case we end up with very large numbers
		lseconds = ((long long)intervalSeconds * (long long)numberPeriods) + (long long)*secondsEnd;
		if ((lseconds < -SECS_IN_1_DAY) || (lseconds > SECS_IN_1_DAY)) {
			day = (int)(lseconds / SECS_IN_1_DAY);
			iremain = (int)(lseconds - ((long long)day * SECS_IN_1_DAY));
			if (iremain < 1) {
				day--;
				iremain += SECS_IN_1_DAY;
			}
			*julianEnd += day;
			*secondsEnd = iremain;
		}
		else {
			//  less than a day, just add.
			*secondsEnd += (intervalSeconds * numberPeriods);
		}
		cleanTime(julianEnd, secondsEnd, SECOND_GRANULARITY);
		return STATUS_OKAY;
	}


	//  Tri-monthly and above is when we have to start dealing with the
	//  odd end of period dates
	//  For example, the end of the month may be the 29, 30 or 31st.
	intervalMinutes = intervalSeconds / SECS_IN_1_MINUTE;

	//  Compute any offset.  EOP rules.  We will adjust for this later
	//  (e.g., if yearly and date is 25Dec1950, a 60 year increment needs to be 25Dec2010)
	offsetSeconds = ztsOffsetAdjustToStandard(intervalSeconds, julianEnd, secondsEnd);

	//  Get the day, month and year
	julianToYearMonthDay (*julianEnd, &year, &month, &day);

	//  Adjust dates, using standard times; offset holds the time within the period.
	if (intervalMinutes == MINS_IN_TRI_MONTH) {
		//  Tri-monthly (nominal 10 days)
		imonth = numberPeriods / 3;
		iremain = numberPeriods - (imonth * 3);
		month += imonth;
		day += (iremain * 10);
		if (day < 1) {
			month--;
			day += 30;
		}
		if (day > 31) {
			month++;
			day -= 30;
		}
		if (month < 1) {
			year--;
			month += 12;
		}
		//  Since this is a standard date, adjust day to be standard also
		//  (taking care of different number of days in a month)
		//  day can only be 10, 20 or end of month for standard
		if (day > 25) {
			day = 28;
		}
		else if (day > 15) {
			day = 20;
		}
		else {
			day = 10;
		}
	}
	else if (intervalMinutes == MINS_IN_SEMI_MONTH) {
		//  Semi-monthly
		imonth = numberPeriods / 2;
		iremain = numberPeriods - (imonth * 2);
		month += imonth;
		day += (iremain * 15);
		if (day < 1) {
			month--;
			day += 30;
		}
		if (day > 31) {
			month++;
			day -= 30;
		}
		if (month < 1) {
			year--;
			month += 12;
		}
		//  Since this is a standard date, adjust day to be standard too
		//  (taking care of different number of days in a month)
		//  day can only be 15 or end of month for standard
		if (day > 25) {
			day = 28;
		}
		else {
			day = 15;
		}
	}
	else if (llabs(intervalMinutes - MINS_IN_1_MONTH) <= 2 * MINS_IN_1_DAY) {
		//  Monthly
		month += numberPeriods;
		//  Day is always end of month for standard
		day = 28;
	}
	else if (llabs(intervalMinutes - MINS_IN_1_YEAR) <= MINS_IN_1_DAY) {
		//  Yearly
		year += numberPeriods;
		//  Always new year's eve for standard
		month = 12;
		day = 31;
	}
	else {
		//  Unknown - just do straight day computation
		//  Need to use long long math for large increments
		longNumber = (((long long)intervalMinutes / MINS_IN_1_DAY) * (long long)numberPeriods);
		day += (int)longNumber;
	}

	*julianEnd = yearMonthDayToJulian (year, month, day);
	if (offsetSeconds) {
		ztsOffsetAdjustToOffset(offsetSeconds, intervalSeconds, julianEnd, secondsEnd);
	}
	else {
		//  This will take care of things like different number of days in months, etc.
		ztsOffsetAdjustToStandard(intervalSeconds, julianEnd, secondsEnd);
	}
	return STATUS_OKAY;
}

