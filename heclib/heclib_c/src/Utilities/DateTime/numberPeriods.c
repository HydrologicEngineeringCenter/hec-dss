#include <string.h>

#include "heclib7.h"
#include "hecdssInternal.h"

/**
*  Function:	numberPeriods
*
*  Use:			public 
*
*  Description:	 Computes the number of periods between two dates/times for a standard interval.
*
*  Declaration: int numberPeriods(int intervalSeconds, 
*								  int julianStart, int secondsStart, 
*								  int julianEnd, int secondsEnd);
*
*  Parameters:	int intervalSeconds
*					The time interval, in seconds.  This must be a standard interval, as shown below.
*
*				int julianStart
*					The Julian day of the start.
*
*				int secondsStart
*					The time, in seconds, of the day of the start.
*
*				int julianEnd
*					The Julian day of the end.
*
*				int secondsEnd
*					The time, in seconds, of the day of the end.

*
*	Returns:	number of periods between the two dates/times for the given interval 
*		
*	Remarks:	Truncates any remainder
*				The number of periods may be positive, negative or zero.
*				Cleans time before computing, so you can have, for example seconds of 50 days
*				Does not check for a standard interval
*				An interval < 1 is returned STATUS_NOT_OKAY (which is also a valid number of periods)
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

int numberPeriods(int intervalSeconds, 
				  int julianStart, int secondsStart, 
				  int julianEnd, int secondsEnd)
{


	int intervalMinutes;
	int numberPeriods;
	int days;
	int seconds;
	int yearStart, monthStart, dayStart;
	int yearEnd, monthEnd, dayEnd;
	long long lseconds;


	if (intervalSeconds < 1) return STATUS_NOT_OKAY;


	//  This requires days and seconds to be normal
	cleanTime(&julianStart, &secondsStart, SECOND_GRANULARITY);
	cleanTime(&julianEnd, &secondsEnd, SECOND_GRANULARITY);

	//  If the interval is less than tri-monthly, then it is a simple calculation
	//  Check for less than 10 days
	if (intervalSeconds < 864000) {
		days = julianEnd - julianStart;
		seconds = secondsEnd - secondsStart;
		//  This is what we'd like to do:
		//  numberPeriods = ((days * 86400) + seconds) / intervalSeconds;

		//  For daily or less intervals, the interval is evenly divisible
/*		if (intervalSeconds <= 86400) {
			numberPeriods = (int)((long long)days * (86400L / (long long)intervalSeconds));
			numberPeriods += (seconds / intervalSeconds);
			if (seconds < 0) {
				//  If less than zero and not evenly divisible, we need to subtract one
				///  ...
			}
		}
		else { */
			//  Weekly, may not be evenly divisble 
			// Do computations in 64 bit math in case we end up with very large numbers
			lseconds = ((long long)days * 86400L) + (long long)seconds;
			numberPeriods = (int)(lseconds / (long long)intervalSeconds);
	//	}
	}
	else {
		//  Tri-monthly or larger.  We have to compute on an interval
		//  basis, since intervals are un-even.  Seconds don't count.
		intervalMinutes = intervalSeconds / 60;
		julianToYearMonthDay (julianStart, &yearStart, &monthStart, &dayStart);
		julianToYearMonthDay (julianEnd, &yearEnd, &monthEnd, &dayEnd);

		if (intervalMinutes == 14400) {
			//  Tri-monthly (nominal 10 days)
			numberPeriods = ((yearEnd - yearStart) * 36) +
							((monthEnd - monthStart) * 3) +
							((dayEnd - dayStart) / 8);
		}
		else if (intervalMinutes == 21600) {
			//  Semi-monthly
			numberPeriods = ((yearEnd - yearStart) * 24) +
							((monthEnd - monthStart) * 2) +
							((dayEnd - dayStart) / 13);
		}
		else if ((intervalMinutes > 40000) && (intervalMinutes < 45000)) {
			//  Monthly
			numberPeriods = ((yearEnd - yearStart) * 12) +
							(monthEnd - monthStart) +
							((dayEnd - dayStart) / 27);
		}
		else if ((intervalMinutes > 520000) && (intervalMinutes < 530000)) {
			//  Yearly
			numberPeriods = (yearEnd - yearStart) +
							((monthEnd - monthStart) +
							((dayEnd - dayStart) / 28) / 12);
		}
		else {
			//  Unknown - just do straight day computation 
			days = julianEnd - julianStart;
			seconds = secondsEnd - secondsStart;
			lseconds = ((long long)days * 86400L) + (long long)seconds;
			numberPeriods = (int)(lseconds / (long long)intervalSeconds);
		}
	}
	return numberPeriods;
}
