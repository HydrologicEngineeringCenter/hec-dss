#include <string.h>

#include "hecdssInternal.h"

/**
*  Function:	secondsToTimeString
*
*  Use:			Public
*
*  Description:	Converts a character time to seconds past midnight.
*
*  Declaration: int timeStringToSeconds(const char *timeString);
*
*  Parameters:	const char *timeString
*					The time string to parse.  Examples of what can be parsed are provided in the table below
*
*
*	Returns:	secondsPastMidnight.  secondsPastMidnight varies from 0 to SECS_IN_1_DAY. A time of zero is allowed
*
*
* Time Styles:
*
*      Style  Example				Returns
*
*          0:  0830               30600.000
*          1:  08:30              30600.000
*          2:  08:30:43           30643.000
*
*
*  Remarks:	Does not parse or look for mills.
*
* See Also: timeStringToSecondsMills()
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int timeStringToSeconds(const char *timeString)
{
	//  Takes a time character string, such as "2400" or "08:30:00"
	//  and converts it to seconds.
	//
	float fseconds;

	fseconds = timeStringToSecondsMills(timeString);

	return (int)fseconds;
}

