#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#include "heclib.h"
#include "heclibDate.h"

/**
*  Function:	dateToYearMonthDay
*
*  Use:			public
*
*  Description:	 Takes a character date, in a variety of styles, and converts it into integer year, month, day.
*				 The styles of dates is not exhaustive, but this does include styles that other date parsing
*				 functions do not have.  It does not matter if the date is in upper or lower case, all numeric, etc.
*
*  Declaration: int dateToYearMonthDay(const char *dateString, int *year, int *month, int *day);
*
*  Parameters:	const char *dateString
*					The date to parse.  This can be in a variety of formats.  See example table below.
*
*				int *year (output)
*					The year of the date.  This will be a four digit year (e.g., 1982 instead of 82).
*					If no year is given,  zero(0) is returned.
*
*				int *month (output)
*					The month of the Julian date, where 1 is January, 12 is December.
*					The month (in dateString) is the only parameter that has to be included.
*
*				int *day (output)
*					The day of the Julian date, where 1 is the first day of the month.
*					If no day is given,  zero(0) is returned.

*	Returns:
*			STATUS_OKAY:  Successful
*			STATUS_NOT_OKAY:  Invalid date
*
* Remarks:  A rather complex routine that tries a variety of parsing.
*			Based on julymd() from original heclib
*
*  Example dates:  All of the dates from yearMonthDayToDate()
*
*  Alpha numeric dates:	June 2, 1985; JUNE 2, 85; 2 June 1985; 02Jun85;  Jun 85
*  Numeric dates:  6/2/85; 6-2-1985; 1985-06-02
*
*
*	Author:			Originally written in Fortran by Bill Charley, circa 1980
*					Translated to Java by Gerhard Kruger, circa 2004
*					Translated to C by Bill Charley, 2012
*
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int dateToYearMonthDay(const char *dateString, int *year, int *month, int *day)
{


	int i;
	int idx;
	int idx2;
	int len;
	int iyear;
	int imonth;
	int iday;
	char charDate[30];
	char string[10];

	char *slash;
	char *dash;
	char *last_dash;
	char *pos;
	int negativeYear = 0;

	//
	// Make a local copy of charDate
	//
	stringCopy(charDate, sizeof(charDate), dateString, _TRUNCATE);
	upperCase(charDate);
	//
	// Check that charDate is between 3 and 30 characters long
	//
	len = (int)strlen(charDate);
	if (len > 30 || len < 3) {
		return STATUS_NOT_OKAY;
	}

	//
	// Check if the date is an all numeric style date format
	// (e.g., yyyy-mm-dd or mm/dd/yy)or a text/numeric style
	// format (e.g. March 3, 1986 or 3 Mar 86)
	// 02Jan-1000 is okay, though
	//
	slash = strchr(charDate, '/');
	dash = strchr(charDate, '-');
	if (slash) {
		idx = (int)(slash - charDate) + 1;
		//  Disallow both a dash and slash (unless dash is negative year)
		last_dash = strrchr(charDate, '-');
		pos = reverseNonNumeric(charDate);
		negativeYear = last_dash == pos && last_dash == dash;

		if (dash && !negativeYear)
			return STATUS_NOT_OKAY;
		else
			dash = 0;
	}
	else if (dash) {
		idx = (int)(dash - charDate) + 1;
		//  yyyy-mm-dd is usgs style, but ddMMM-yyyy is a valid date with a negative year
		if (idx > 5) {
			dash = 0;
			idx = 0;
		}
	}
	else {
		idx = 0;
	}
	if (idx > 0) {
		if (idx == 5) {
			//
			// This is a USGS style date yyyy-mm-dd
			// mm and dd must be two digit numbers
			if (strlen(charDate) != 10) return STATUS_NOT_OKAY;
			//  Make sure month is 2 char
			idx +=2;
			if (charDate[idx] != '-')  return STATUS_NOT_OKAY;

			stringCopy(string, sizeof(string), charDate, (size_t)4);
			iyear = atoi(string);
			//  Invalid year?
			if (iyear == 0) return STATUS_NOT_OKAY;

			stringCopy(string, sizeof(string), &charDate[5],(size_t) 2);
			imonth = atoi(string);
			//  Invalid month?
			if (imonth == 0) return STATUS_NOT_OKAY;

			stringCopy(string, sizeof(string), &charDate[8], (size_t)2);
			iday = atoi(string);
			//  Invalid day?
			if (iday == 0) return STATUS_NOT_OKAY;
			//  Good!
		}
		else if ((idx == 2) || (idx == 3)) {
			//
			// The date is in an all numeric format with the mm/dd/yy
			// parsed by either a dash or a slash
			//
			stringCopy(string, sizeof(string), charDate, (size_t)(idx-1));
			imonth = atoi(string);
			//  Invalid month?
			if (imonth == 0) return STATUS_NOT_OKAY;

			if (dash) {
				pos = strchr(&charDate[idx], '-');
			}
			else {
				pos = strchr(&charDate[idx], '/');
			}
			//  No second delimiter?
			if (pos == 0) return STATUS_NOT_OKAY;

			i = (int)(pos - &charDate[idx]);
			stringCopy(string, sizeof(string), &charDate[idx], (size_t)i);
			iday = atoi(string);
			//  Invalid day?
			if (iday == 0) return STATUS_NOT_OKAY;

			idx += i;
			idx++;
			stringCopy(string, sizeof(string), &charDate[idx], _TRUNCATE);
			iyear = atoi(string);
			//  Invalid year?
			if (iyear == 0) return STATUS_NOT_OKAY;
			//  Good!
		}
		else {
			//  Unrecongnized date
			return STATUS_NOT_OKAY;
		}

	}
	else {
		// Must be a normal style date (e.g. March 3, 1986 or 3 Mar 86)
		//
		// The year (if it exists) is at the end of all valid date
		// strings, therefore, extract the Year from the end of charDate
		//
		//  Use custom function to find last non-numeric character
		pos = reverseNonNumeric(charDate);
		if (pos == 0) {
			//  Okay to not have a year.  Patterns don't use year, just set to zero.
			iyear = 0;
		}
		else {
			if (pos[0] == '-') {
				//  Allow a negative year
				pos--;
			}
			idx = (int)(pos - charDate) + 1;
			stringCopy(string, sizeof(string), &charDate[idx], _TRUNCATE);
			iyear = atoi(string);
			//  Invalid year?
			if (iyear == 0) {
				if (string[0] != '0') {
					return STATUS_NOT_OKAY;
				}
			}
			//  Try to filter out some bogus dates
			if (idx > 9) {
				if ((int)pos[0] != 32) {
					if ((int)pos[0] < 43) {  // ! " # $ % & ' ( ) *
						return STATUS_NOT_OKAY;
					}
					if (((int)pos[0] > 57) && ((int)pos[0] < 65)) {   //  : ; < = > ? @
						return STATUS_NOT_OKAY;
					}
				}
			}
			//  Now that we have the year, truncate it from the string
			charDate[idx] = '\0';
		}

		//
		// NEXT search for a valid day
		//
		iday = findInt(charDate, &idx, &idx2);
		//  Its okay not to have a day given (returned as zero
		if (idx2 == 0) {
			iday = 0;
		}
		else if ((iday < 0) || (iday > 31)) {
			return STATUS_NOT_OKAY;
		}

		//
		// NEXT search the string for a valid month
		//
		imonth = 0;
		for (i=0; i<12; i++) {
			//  Just search for first 3 characters of each month
			stringCopy(string, sizeof(string), LONG_MONTH_NAMES[i], (size_t)3);
			upperCase(string);
			pos = strstr(charDate, string);
			if (pos != NULL) {
				imonth = i + 1;
				break;
			}
		}
		if (imonth == 0) {
			//  No Match for the Month Found - Return
			return STATUS_NOT_OKAY;
		}
	}
	//
	// A sucessful Date Format was matched, set errorCode to zero...
	//
	*year = iyear;
	*month = imonth;
	*day = iday;

	return STATUS_OKAY;
}

