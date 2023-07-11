#include "hecdssInternal.h"


/**
*  Function:	ztsRegGetBlockStart
*
*  Use:			Private
*
*  Description:	Gets the juilan start date of a regular interval time series standard block, based on the interval
*
*  Declaration: int ztsRegGetBlockStart(int julianDate, int intervalSeconds, int *blockSize);
*
*  Parameters:	int julianDate
*					Julian date that is within the block returned
*
*				int intervalSeconds
*					The standard interval, given in seconds
*
*				int *blockSize (output)
*					Returns the block size
*
*
*	Returns:	int startBlockJulian
*					The julian date of the start of the block
*
*
*	Remarks:	blockSize:
*					1:  Daily block
*					2:  Monthly block
*					3:  Yearly block
*					4:  Decade block
*					5:  Century block
*
*
*  Standard Intervals:
*					Name			Seconds		Block size
*					"1Year"			31536000	1 Century
*					"1Month"		2592000		1 Decade
*					"Semi-Month"	1296000		1 Decade
*					"Tri-Month"		864000		1 Decade
*					"1Week"			604800		1 Decade
*					"1Day"			86400		1 Year
*					"12Hour"		43200		1 Month
*					"8Hour"			28800		1 Month
*					"6Hour"			21600		1 Month
*					"4Hour"			14400		1 Month
*					"3Hour"			10800		1 Month
*					"2Hour"			7200		1 Month
*					"1Hour"			3600		1 Month
*					"30Minute"		1800		1 Month
*					"20Minute"		1200		1 Month
*					"15Minute"		900			1 Month
*					"12Minute"		720			1 Day
*					"10Minute"		600			1 Day
*					"6Minute"		360			1 Day
*					"5Minute"		300			1 Day
*					"4Minute"		240			1 Day
*					"3Minute"		180			1 Day
*					"2Minute"		120			1 Day
*					"1Minute"		60			1 Day
*					"30Second"		30			1 Day
*					"20Second"		20			1 Day
*					"15Second"		15			1 Day
*					"10Second"		10			1 Day
*					"6Second"		6			1 Day
*					"5Second"		5			1 Day
*					"4Second"		4			1 Day
*					"3Second"		3			1 Day
*					"2Second"		2			1 Day
*					"1Second"		1			1 Day
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*/

int ztsRegGetBlockStart(int julianDate, int intervalSeconds, int *blockSize)
{
	int day, month, year;
	int startBlockJulian;

	if (intervalSeconds < SECS_IN_15_MINUTE) {  //  < 15 minutes
		//  Daily block; use start of day
		startBlockJulian = julianDate;
		*blockSize = BLOCK_1_DAY;
	}
	else {
		//  Larger than daily.  We'll need to get the specific values
		julianToYearMonthDay(julianDate, &year, &month, &day);

		if (intervalSeconds < SECS_IN_1_DAY) {  //  < 1 day
			//  Monthly block; use start of month
			day = 1;
			*blockSize = BLOCK_1_MONTH;
		}
		else if (intervalSeconds < SECS_IN_1_WEEK) { //  < 1 week
			//  Yearly block; use start of year
			day = 1;
			month = 1;
			*blockSize = BLOCK_1_YEAR;
		}
		else if (intervalSeconds < SECS_IN_1_YEAR) {  //  < 1 year
			//  Decade block; use start of decade
			day = 1;
			month = 1;
			year /= 10;
			year *= 10;
			*blockSize = BLOCK_1_DECADE;
		}
		else  {
			//  Century block; use start of century
			day = 1;
			month = 1;
			year /= 100;
			year *= 100;
			*blockSize = BLOCK_1_CENTURY;
		}

		//  Now, put the dates back together
		startBlockJulian = yearMonthDayToJulian(year, month, day);
	}

	return startBlockJulian;
}

