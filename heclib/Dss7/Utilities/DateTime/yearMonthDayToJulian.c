#include <stdlib.h>

#include "heclibDate.h"

/**
*  Function:	yearMonthDayToJulian
*
*  Use:			Public
*
*  Description:	Takes a integer year, month, day date and converts it to a Julian day.
*
*  Declaration: int yearMonthDayToJulian (int year, int month, int day);
*
*  Parameters:	int year
*					The year of the date.  This must be a four digit number (unless the real date is otherwise). 
*
*				int month
*					The integer month of the date.  1 corresponds to January, 2 to February, etc.
*					Although this is generally a number between 1 and 12, this function will
*					adjust the Julian date according to the number of months (clean it up).
*
*				int day
*					The integer day of the date.  Generally a number number between 1 and 31,
*					but this function will provide the correct Julian day if not.
*					
*
*	Returns:	Julian day, in days since 01Jan1900, with 01Jan1900 being day 1 (31Dec1899 is day 0)
*					This is the standard day count used throughout DSS and can be negative (or large).
*				UNDEFINED_TIME "-2147483647" if error. 
*				Note, 0 (zero) is valid day and used a lot!
*
*
*	Remarks:	This function is used to add months / days and clean up the date
*				For example, if you wanted to add 500 months to the date, 
*				just set month to 500 (actually +500).  Does negative also.
*
*	See Also:  yearMonthDayToDate(), dateToJulian()
*
*	Author:			Art Pabst, Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int yearMonthDayToJulian (int year, int month, int day)
{
	int NDAY[12] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};
	int intYear_1;
	int iyears;
	int numberOfLeapDays;
	int leapCheck;

	//  Fix up months, if off
	//  Note - 1 = January (not 0).  We use common English conventions here
	if ((month < 1) || (month > 12)) {
		//  Need to adjust
		iyears = month / 12;
		month = month - (iyears * 12);
		year += iyears;
		if (month < 1) {
			year--;
			month = month + 12;
		 }
	}

	if (year > 4) {
		intYear_1 = year - 1;
		numberOfLeapDays = (intYear_1 / 4) + (intYear_1 / 400) - (intYear_1 / 100);
	}
	else if (year > 0) {
		numberOfLeapDays = 1;
	}
	else {
		intYear_1 = year + 1;
		numberOfLeapDays = (intYear_1 / 4) + (intYear_1 / 400) - (intYear_1 / 100);
	}
	
	
	//  Check to see if it's a leap year
	leapCheck = 0;
	if (isLeapYear(year)) {
		if (year >= 0) {
			//  Need to account for leap day
			if (month > 2) {
				leapCheck = 1;
			}
		}
		else {
			if (month < 3) {
				leapCheck = -1;
			}
		}			
	}
	
	if (month < 1 || month > 12) {
		return UNDEFINED_TIME;
	} else {
		return (year * 365) + numberOfLeapDays + (NDAY[month - 1]) + day + leapCheck - JULIAN_BASE_DATE;
	}	
}
