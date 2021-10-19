#include "heclibDate.h"

/**
*  Function:	getCurrentDateString
*
*  Use:			Public
*
*  Description:	Returns the current date in a character string, e.g., "02Jun1985"
*
*  Declaration: void getCurrentDateString (char *dateString, size_t sizeOfDateString);
*
*  Parameters:	char dateString[13]
*					The character string to return the date in.  Must be dimensioned to 10.
*
*				size_t sizeOfDateString
*					The length of the string.  Must be at least 10.
*
*
*
*	See Also:	getCurrentDateTimeString(), getCurrentTimeString()
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

void getCurrentDateString (char *dateString, size_t sizeofDateString)
{

	int julian;
	int secondsPastMidnight;
	int mills;
	int style;

	style = 4;  //  02Jun1985
    getCurrentDateTime (&julian, &secondsPastMidnight, &mills);
	julianToDate(julian, style, dateString, sizeofDateString);

}

