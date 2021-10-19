#include <string.h>

#include "heclibDate.h"
#include "hecdssInternal.h"

/**
*  Function:	dateToJulian
*
*  Use:			public
*
*  Description:	 Takes a character date, in a variety of styles, and converts it to Julian.
*				 The styles of dates is not exhaustive, but this does include styles that other date parsing
*				 functions do not have.  It does not matter if the date is in upper or lower case, all numeric, etc.
*
*  Declaration: int dateToJulian(const char *dateString);
*
*  Parameters:	const char *dateString
*					The date to parse.  This can be in a variety of formats.  See example table below.
*					If no year is given, the current year is assumed.  If no day is given, the first of the month is assumed.
*
*	Returns:	Julian date or
*		 		UNDEFINED_TIME  (-2147483647):  Invalid date
*
* Remarks:  See dateToYearMonthDay() for more info.
*
*  Example dates:  All of the dates from yearMonthDayToDate()
*
*  Alpha numeric dates:	June 2, 1985; JUNE 2, 85; 2 June 1985; 02Jun85;  Jun 85
*  Numeric dates:  6/2/85; 6-2-1985; 1985-06-02
*
*
* See Also: dateToYearMonthDay()
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int dateToJulian(const char *dateString)
{
	//  Takes a date character string, such as "23Feb2002",
	//  and converts it to Julian day date
	//
	int julian;
	int ierror;
	int year;
	int month;
	int day;

	ierror = dateToYearMonthDay(dateString, &year, &month, &day);

	if (ierror != 0) {
		return UNDEFINED_TIME;
	}

	//  If no day given, assume 1st day of month
	if (day == 0) {
		day = 1;
	}
	if ((year < 100) && (strlen(dateString) == 7)) {
		//  Add century number
		year = addCentury(year);
	}

	julian = yearMonthDayToJulian (year, month, day);
	return julian;
}

