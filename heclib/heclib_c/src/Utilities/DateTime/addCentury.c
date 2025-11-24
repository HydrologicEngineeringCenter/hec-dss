#include "heclibDate.h"

/**
*  Function:	addCentury
*
*  Use:			Public
*
*  Description:	If the year is a two digit addCentury, this will add the current century
*
*  Declaration: int addCentury (int year);
*
*  Parameters:	int year.
*
*  Returns:
*				year with current century added
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int addCentury (int year)
{
	//  We could do something fancy, like get the current date, strip off the
	//  first two digits that make up the century value, then multiply by 100
	//  and add to the year... but that won't make any difference until
	//  the year 3000, and I don't think this code will be around by then

	//  So just add 2000.
	if (year < 100) {
		year += 2000;
	}
	return year;

}

