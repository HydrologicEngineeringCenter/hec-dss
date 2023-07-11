#include <stdio.h>

#include "hecdssInternal.h"


/**
*  Function:	ztsIrregMessage
*
*  Use:			Private
*
*  Description:	Write a message for irregular interval time series, displaying correct date/time (from seconds)
*
*  Declaration: void ztsIrregMessage(long long *ifltab, int functionID, const char *message, int itime,
*									 int timeGranularitySeconds, int baseDate, int *value, int sizeValue, int boolDebugMessage);
*
*
*  Parameters:	long long *ifltab:  The ifltab array
*
*				int functionID:  The int identifying the calling function (from struct DssFunctions)
*
*				const char *message:  Message to display along with date/time, value
*
*				int itime:  The time, in seconds from the base date.
*							This is the time in the time array stored for irregular interval data
*
*				int timeGranularitySeconds
*					The number of seconds a unit in itime represents, usually
*					MINUTE_GRANULARITY (60) or SECOND_GRANULARITY (1)
*
*				int baseDate:  The base date, in julian days.  This is the date in the records header and is often 0.
*
*				int *value:  The value at this time.  Usually a float or double (after conversion)
*
*				int sizeValue:  The number of words value takes; 1 means it's a float, 2 means it's a double
*
*				int boolDebugMessage:  A boolean indicating if this should be written as a debug message or normal message
*							0 means normal message, 1 means debug message
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

void tod(int *val, int *d)
{
	d[0] = val[0];
	d[1] = val[1];
}

void ztsIrregMessage(long long *ifltab, int functionID, const char *message, int itime, int timeGranularitySeconds,
					 int baseDate, int *value, int sizeValue, int boolDebugMessage)
{
	double d;
	float f;
	char cdate[20];
	char ctime[12];
	char messageString[50];
	int julian;
	int seconds;
	long long dayLength;

	//  Figure out the julian day and seconds after midnight
	dayLength = SECS_IN_1_DAY / timeGranularitySeconds;
	julian = (itime / (int)dayLength);
	seconds = itime - (julian * (int)dayLength);
	if (seconds == 0) {
		seconds = SECS_IN_1_DAY;
		julian--;
	}
	else {
		seconds *= SECS_IN_1_MINUTE;
	}
	julian += baseDate;

	julianToDate(julian, 4, cdate, sizeof(cdate));
	secondsToTimeString(seconds, 0, 2, ctime, sizeof(ctime));

	if (sizeValue == 1) {
		//  Print as a float
		f = *(float *)value;
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %s  %s;  %f",
			cdate, ctime, f);
	}
	else {
		//  print as a double
		tod(value, (void *)&d);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %s  %s;  %f",
			cdate, ctime, d);
	}
	if (boolDebugMessage) {
		zmessageDebug(ifltab, functionID, message, messageString);
	}
	else {
		zmessage2(ifltab, message, messageString);
	}
}

