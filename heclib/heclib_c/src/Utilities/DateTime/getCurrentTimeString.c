#include "heclibDate.h"

/**
*  Function:	getCurrentTimeString
*
*  Use:			Public
*
*  Description:	Returns the current day time with milli seconds in a character string, e.g., "08:23:12.084"
*
*  Declaration: void getCurrentTimeString (char *timeString, size_t sizeOfTimeString);
*
*  Parameters:	char timeString[13]
*					The character string to return the time in.  Must be dimensioned to 13.
*
*				size_t sizeOfTimeString
*					The length of the string.  Must be at least 13.
*
*
*	Returns:	None
*
*   Note:		For debug preamble.
*
*	See Also:	getCurrentDateTime()
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

void getCurrentTimeString (char *timeString, size_t sizeOfTimeString)
{

	int julian;
	int secondsPastMidnight;
	int mills;

    getCurrentDateTime (&julian, &secondsPastMidnight, &mills);
	secondsToTimeString(secondsPastMidnight, mills, 3, timeString, sizeOfTimeString);

}

