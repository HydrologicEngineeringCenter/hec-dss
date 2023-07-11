#include <string.h>

#include "hecdss7.h"

/**
*  Function:	ztsGetEPartFromInterval
*
*  Use:			Public
*
*  Description:	 Gets the (standard) E part from a time interval in seconds for time series data.
*					A convienence function that calls ztsGetStandardInterval
*
*  Declaration: int ztsGetEPartFromInterval(int intervalSeconds, char ePart, size_t sizeofEpart);
*
*  Parameters:
*				int intervalSeconds (input)
*					The time interval, in seconds.
*
*				char *Epart (output)
*					Returns with the E part for that time interval
*
*				size_t sizeofEpart (input)
*					The size of the E part, used when returning the E part from the interval.
*
*
*	Returns:	0 for regular interval
*				1 for irregular interval
*				STATUS_NOT_OKAY for error or end of list
*
*
*  Standard Intervals:
*					Name			Seconds
*					"1Year"			31536000	(365 days)
*					"1Month"		2592000		(30 days)
*					"Semi-Month"	1296000		(15 days)
*					"Tri-Month"		864000		(10 days)
*					"1Week"			604800		(7 days, EOP Saturday, 2400 (7))
*					"1Day"			86400
*					"12Hour"		43200
*					"8Hour"			28800
*					"6Hour"			21600
*					"4Hour"			14400
*					"3Hour"			10800
*					"2Hour"			7200
*					"1Hour"			3600
*					"30Minute"		1800
*					"20Minute"		1200
*					"15Minute"		900
*					"12Minute"		720
*					"10Minute"		600
*					"6Minute"		360
*					"5Minute"		300
*					"4Minute"		240
*					"3Minute"		180
*					"2Minute"		120
*					"1Minute"		60
*					"30Second"		30
*					"20Second"		20
*					"15Second"		15
*					"10Second"		10
*					"6Second"		6
*					"5Second"		5
*					"4Second"		4
*					"3Second"		3
*					"2Second"		2
*					"1Second"		1
*					"IR-Century"	-5
*					"IR-Decade"		-4
*					"IR-Year"		-3
*					"IR-Month"		-2
*					"IR-Day"		-1
*
*
*
*	Author:			Bill Charley, 2014
*
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int ztsGetEPartFromInterval(int intervalSeconds, char *ePart, size_t sizeofEpart)
{
	int status;
	int operation;

	operation = SECONDS_TO_EPART;
	status = ztsGetStandardInterval(7, &intervalSeconds, ePart, sizeofEpart, &operation);
	return status;
}


