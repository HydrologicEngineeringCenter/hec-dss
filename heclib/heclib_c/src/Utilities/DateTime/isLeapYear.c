#include "heclibDate.h"

/**
*  Function:	isLeapYear
*
*  Use:			public
*
*  Description:	 Checks if a given year is a leap year.
*
*  Declaration: int isLeapYear (int year)
*
*  Parameters:	int year
*					The four digit year to check (e.g., 1969).
*
*  Returns:		leapYear
*		 			A boolean set to 1 if it is a leap year, or 0 if not.
*
*	Author:			Originally written in Fortran by Art Pabst, circa 1980
*					Translated to Java by Gerhard Kruger, circa 2004
*					Translated to C by Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int isLeapYear (int year)
{
	if ((year % 400 == 0) || ((year % 4 == 0) && (year % 100 != 0))) {
		return 1;
	}
	return 0;
}

