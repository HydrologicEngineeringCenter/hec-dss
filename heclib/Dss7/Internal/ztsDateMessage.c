#include <stdio.h>

#include "heclib7.h"
#include "hecdssInternal.h"

/**
*  Function:	ztsDateMessage
*
*  Use:			Private
*
*  Description:	 A convenience function to print a date and time for debug.  The date will be displayed both in normal and julian.
*
*  Declaration: void ztsDateMessage(long long *ifltab, int functionID, const char *message, int julian, int seconds);
*
*  Parameters:	long long ifltab
*					The integer array dimensioned to int*8 [250] that contains file specific information.
*
*				int functionID
*					The function identifier of the calling function.  This is defined in header file "zdssMessages.h"
*
*				const char *message
*					A string containing a message to display before the date / time
*
*				int julian
*					The Julian date to display both in regular and Julian
*
*				int seconds
*					Seconds past midnight for the time portion of the date and time
*
*
*  Returns:		None
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

void ztsDateMessage(long long *ifltab, int functionID, const char *message, int julian, int seconds)
{
	char messageString[60];
	char cDate[20];
	char cTime[12];

	if (julian == UNDEFINED_TIME) {
		stringCopy (cDate, sizeof(cDate), "undefined\0", (size_t)10);
	}
	else {
		julianToDate(julian, 4, cDate, sizeof(cDate));
	}
	if (seconds < 0) {
		stringCopy (cTime, sizeof(cTime), "undefined\0", (size_t)10);
	}
	else {
		secondsToTimeString(seconds, 0, 2, cTime, sizeof(cTime));
	}
	_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %s  %s  (%d, %d)",
				cDate, cTime, julian, seconds);
	zmessageDebug(ifltab, functionID, message, messageString);
}

