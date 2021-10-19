#include "heclib.h"

/**
*  Function:	isTimeDefined
*
*  Use:			Semi-private
*
*  Description:	 A convenience function to check if date / times are set defined.
*
*  Declaration: int isTimeDefined(int julianDate, int timeSeconds);
*
*  Parameters:	int julianDate
*					Julian date, in days since 01Jan1900, with 01Jan1900 being day 1 (31Dec1899 is day 0)
*					This is the standard day count used throughout DSS and can be negative (or large).
*
*				int timeSeconds
*					The time in minutes or seconds
*
*  Returns:		0:  (false) Not defined
*				1:  (true)  Defined.
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int isTimeDefined(int julianDate, int timeSeconds)
{
	if (julianDate == UNDEFINED_TIME) return 0;
	if (timeSeconds < 0) return 0;
	return 1;
}

