#include <stdlib.h>

#include "heclibDate.h"

/**
*  Function:	julianToYearMonthDay
*
*  Use:			Public
*
*  Description:	Takes a Julian day date and converts it to year, month, and day.
*
*  Declaration: void julianToYearMonthDay (int julian, int *year, int *month, int *day)
*
*  Parameters:	int julianDate
*					Julian date to convert, in days since 01Jan1900, with 01Jan1900 being day 1 (31Dec1899 is day 0)
*					This is the standard day count used throughout DSS and can be negative (or large).
*					An undefined day is the number "-2147483647".  Note, 0 (zero) is valid day and used a lot!
*
*				int *year (output)
*					The (4 digit) year of the Julian date.
*
*				int *month (output)
*					The month of the Julian date, where 1 is January, 12 is December.
*
*				int *day (output)
*					The day of the Julian date, where 1 is the first day of the month.
*
*
*	Returns:	None
*
*	Remarks:	Uses standard conventions for month and day; For example 01Jan1980 is day 1, month 1, year 1980.
*
*
* See Also: julianToYearMonthDay(), yearMonthDayToDate(), secondsToTimeString()
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int julianToYearMonthDay (int julian, int *year, int *month, int *day)
{
	//  Calculate the year by dividing the approximate days in a year,
	//  then looking forward until the Julian of Jan 1 on the next year
	//  is greater than the value passed in.
	//

	int i;
	int jula;
	int julb;
	int julc;
	int juld;
	int iyear;
	int iyr;
	int imonth;
	int iday;
	int found;


	julc = julian + JULIAN_BASE_DATE;
	iyear = (int) ((float)julc / 365.2425);
	found = 0;
	if (iyear > 0) {
		for (i = 0; i < 100; i++) {
			jula = yearMonthDayToJulian(iyear, 1, 1);
			if (jula > julian) {
				found = 1;
				break;
			}
			iyear++;
		}
		if (!found) {
			return -1;
		}
		//  We have found the following year, so back up one
		iyear--;
	}
	else {
		//  Negative years (BC)
		for (i = 0; i < 100; i++) {
			jula = yearMonthDayToJulian(iyear, 1, 1);
			if (julian >= jula) {
				found = 1;
				break;
			}
			iyear--;
		}
		if (!found) {
			return -1;
		}
	}

	//  Calculate the month by computing an approximate month on the short side,
	//  then looking forward until the Julian of the next month
	//  is greater than the value passed in.
	if (iyear > -1) {
		julb = yearMonthDayToJulian(iyear, 1, 1);
		//juld = __min(12, ((julian - julb + 28) / 28));
		juld = (julian - julb + 28) / 28;
		if (juld > 12) juld = 12;
		found = 0;
		imonth = juld;
		for (i = juld; i <= 12; i++) {
			jula = yearMonthDayToJulian(iyear, i, 1);
			if (jula > julian) {
				imonth = i;
				found = 1;
				break;
			}
		}
		if (found)
			imonth--;
	}
	else {
		imonth = 0;
		for (i = 12; i >= 0; i--) {
			jula = yearMonthDayToJulian(iyear, i, 1);
			if (julian >= jula) {
				imonth = i;
				break;
			}
		}
	}

	//  For negative years, the month can go to zero at the year boundary
	if ((imonth <= 0) || (imonth > 12)){
		iyr = (imonth/12) - 1;
		imonth += (-iyr * 12);
		iyear += iyr;
	}

	//  We have the month and year. The day is the difference between
	//  the Julian and the start of the month + 1
	jula = yearMonthDayToJulian(iyear, imonth, 1);
	iday = julian - jula + 1;
	//  Done
	*year = iyear;
	*month = imonth;
	*day = iday;
	return 0;
}

