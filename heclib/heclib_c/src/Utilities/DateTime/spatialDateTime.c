#include <string.h>

#include "heclib.h"

/**
*  Function:	spatialDateTime
*
*  Use:			Public
*
*  Description:	Takes a date / time string in the form 01DEC2016:0000 and returns the Julian
*					date and time in seconds.  Note "0000" is returned as zero and "2400" as SECS_IN_1_DAY
*
*  Declaration: int spatialDateTime(char *dateTimeString, int *julian, int *seconds);
*
*  Parameters:	char *dateTimeString
*					A character string containing the spatial style date and time to parse (e.g., 01DEC2016:0000)
*					Must have a colon between the date and time.
*
*				int *julian
*					Returns the Julian date from the string, or UNDEFINED_TIME if cannot be determined
*
*				int *seconds
*					Returns the time portion in seconds, or < 0 if cannot be determined
*					The time will range from 0 to SECS_IN_1_DAY, inclusive
*
*
*	Returns:	STATUS_OKAY if correctly parsed
*				STATUS_NOT_OKAY if invalid spatial date time string.
*
*	Remarks:	Don't use this function to determine if it is a correct string
*					Check your string length before calling, as a empty string is valid for spatial data.
*
*	Author:			Bill Charley
*	Date:			2017
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int spatialDateTime(char *dateTimeString, int *julian, int *seconds)
{
	char *string;
	int colon;
	int i;
	int len;


	*julian = UNDEFINED_TIME;
	*seconds = UNDEFINED_TIME;

	len = (int)strlen(dateTimeString);
	if (len < 10) {
		return STATUS_NOT_OKAY;
	}

	colon = 0;
	for (i=0; i<len; i++) {
		if (dateTimeString[i] == ':') {
			colon = i;
			break;
		}
	}

	if (i == 0) {
		return STATUS_NOT_OKAY;
	}

	string = mallocAndCopy(dateTimeString);
	string[colon] = '\0';

	*julian = dateToJulian(string);
	if (*julian == UNDEFINED_TIME) {
		free(string);
		return STATUS_NOT_OKAY;
	}

	colon++;
	*seconds = timeStringToSeconds(&string[colon]);
	free(string);
	if (*seconds < 0) {
		return STATUS_NOT_OKAY;
	}

	return STATUS_OKAY;
}

