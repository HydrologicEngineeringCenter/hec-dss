#include "heclib.h"

/**
*  Function:	cleanTime
*
*  Use:			Semi-public 
*
*  Description:	 Adjusts a date and time passed to be within valid range.  Adjusts time so that
*					0 <= minutes < 1440 or 0 <= seconds < 86400.
*
*  Declaration: int cleanTime(int *julianDate, int *itime, int timeGranularitySeconds)
*
*  Parameters:	int *julianDate
*					Julian date, in days since 01Jan1900, with 01Jan1900 being day 1 (31Dec1899 is day 0)
*					This is the standard day count used throughout DSS and can be negative (or large).
*
*				int *itime
*					The time in timeGranularitySeconds (60 for minutes, 1 for seconds)
*
*				int timeGranularitySeconds
*					The number of seconds a unit in *itime represents, usually
*					MINUTE_GRANULARITY (60) or SECOND_GRANULARITY (1)
*
*
*  Returns:		status
*		 			Zero (false), if date is undefined
*					One (true) if the date/time is defined
*
*	Remarks:	This function allows one to add or subtract from time, then adjust to standard date and time
*					For example, if you added 2880 minutes to the time, it will add 2 days to julianDate, and 
*					subtract 2880 minutes from time (assuming time is within range.)
*					Should accommodate large times
*
*		
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int cleanTime(int *julianDate, int *itime, int timeGranularitySeconds)
{
	//  Force 64-bit math
	long long days;
	long long dayLength;
	long long ltime;
	long long granularity;


	if (*julianDate == UNDEFINED_TIME) return 0;

	//  0, 0 is an undefined time also
	if ((*julianDate == 0) && (*itime == 0)) {
		*julianDate = UNDEFINED_TIME;
		*itime = -1;
		return 0;
	}

	//  Test the 99% case, for speed mainly
	if (timeGranularitySeconds == MINUTE_GRANULARITY) {
		if ((*itime > 0) && (*itime <= 1440)) return 1;
	}
	else if (timeGranularitySeconds == SECOND_GRANULARITY) {
		if ((*itime > 0) && (*itime <= 86400)) return 1;
	}

	granularity = (long long)timeGranularitySeconds;
	if (granularity < 1) granularity = MINUTE_GRANULARITY;  //  default, but this should be set anyway  

	if (granularity == MINUTE_GRANULARITY) {
		dayLength = 1440L;
	}
	else if (granularity == SECOND_GRANULARITY) {
		dayLength = 86400L;
	}
	else {
		dayLength = 86400L / granularity;
	}

	ltime = (long long)*itime;
	if ((ltime > 0) && ((ltime <= dayLength) && (ltime > 0))) {
		return 1;
	}

	days = ltime / dayLength;
	if (days > 0) {
		*itime = (int)(ltime - (days * dayLength));
		*julianDate += (int)days;
	}
	else {  // days are either negative or zero.
		*itime = (int)(ltime - (days * dayLength) + dayLength);			
		*julianDate += (int)days - 1;  //  This cancels out the +dayLength above		
	}
	if (*itime == 0) {  //  If on day boundary, readjust
		*itime = (int)dayLength;
		*julianDate -= 1;
	}
	return 1;
}

