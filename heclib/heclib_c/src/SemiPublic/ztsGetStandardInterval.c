#include <string.h>

#include "heclib7.h"

/**
*  Function:	ztsGetStandardInterval
*
*  Use:			Public
*
*  Description:	 Get a standard DSS time interval in seconds from the E part, or the E part from the interval in seconds.
*				 Also will return a list of standard intervals.
*
*  Declaration: int ztsGetStandardInterval(int dssVersion, int *intervalSeconds, char *Epart, size_t sizeofEpart, int *operation);
*
*  Parameters:	int dssVersion
*					The DSS Version the results are for.  Must either be 6 or 7.  (Versions have different intervals)
*
*				int *intervalSeconds (input or output)
*					If operation is EPART_TO_SECONDS_TO_EPART or EPART_TO_SECONDS, this is returned with the time interval, in seconds from the E part.
*					If operation is SECONDS_TO_EPART, the E part is returned using this time interval.
*					If operation is BEGIN_ENUMERATION or CONTINUE_ENUMERATION, this is returned with the a time interval from the list.
*
*				char *Epart (input or output)
*					The E part of the pathname, either determined from the interval, or used to determine the interval
*
*				size_t sizeofEpart (input)
*					The size of the E part, used when returning the E part from the interval.
*
*				int *operation
*					A flag telling ztsGetStandardInterval7 what to do, set operation to:
*						EPART_TO_SECONDS_TO_EPART (0) to go from char Epart to intervalSeconds AND change the E part to the standard for that interval.
*						EPART_TO_SECONDS          (1) to go from char Epart to intervalSeconds
*						SECONDS_TO_EPART          (2) to go from intervalSeconds to char Epart
*						BEGIN_ENUMERATION         (3) to begin a list of valid E parts (intervals used to keep track)
*						CONTINUE_ENUMERATION      (4) In a list of valid E parts (returns -1 at end)
*
*
*	Returns:	0 for regular interval
*				1 for irregular interval
*				STATUS_NOT_OKAY for error or end of list
*
*	Remarks:	For a operation set to EPART_TO_SECONDS_TO_EPART or EPART_TO_SECONDS, the function tries to recognized the E part to get the interval.
*				When operation is set to EPART_TO_SECONDS_TO_EPART, the standard E part is returned in Epart.  For example, if you pass
*				an E part of "1MON", the E part will be returned with "1Month".
*				To get a list of intervals, begin operation with BEGIN_ENUMERATION.  ztsGetStandardInterval7 will return
*				the interval in seconds and the E part for the first valid interval, as well as change operation to CONTINUE_ENUMERATION. Subsequent
*               calls (without changing operation), will enumerate the valid intervals, including irregular interval and pseudo-regular, returning
*               the interval in seconds and E part for each one. When the enumeration is exhausted,  STATUS_NOT_OKAY is returned (see example).
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
*	Example - print a list of standard intervals:
*
*		int intervalSeconds, operation, version;
*		char Epart[20];
*
*		operation = BEGIN_ENUMERATION;
*		version = zgetVersion(ifltab);
*		while (ztsGetStandardInterval(version, &intervalSeconds, Epart, sizeof(Epart), &operation) == STATUS_OKAY) {
*			if (intervalSeconds > 0) {
*				printf("Regual Interval = %s,  intervalSeconds = %d\n", Epart, intervalSeconds);
*			}
*			else {
*				printf("Irregual Interval = %s\n", Epart);
*			}
*		}
*
*
*
*	Author:			Bill Charley, 2014
*
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int ztsGetStandardInterval(int dssVersion, int *intervalSeconds, char *Epart, size_t sizeofEpart, int *operation)
{
	int *secondsInInterval;

	int i;
	int len1;
	int len2;
	int same;
	int pseudoRegular;
	int dim;


	if (dssVersion == 6) {
		secondsInInterval = secondsInInterval6;
		dim = sizeof(secondsInInterval6) / sizeof(secondsInInterval6[0]);
	}
	else {
		secondsInInterval = secondsInInterval7;
		dim = sizeof(secondsInInterval7) / sizeof(secondsInInterval6[7]);
	}

	if ((*operation == EPART_TO_SECONDS) || (*operation == EPART_TO_SECONDS_TO_EPART)) {
		//  set operation = EPART_TO_SECONDS_TO_EPART to go from char Epart to intervalSeconds AND return
		//  the standard E part for that interval.
		//  set operation = EPART_TO_SECONDS To go from char Epart to intervalSeconds
		len2 = (int)strlen(Epart);
		if (Epart[0] == '~') {
			pseudoRegular = 1;
			len2--;
			sizeofEpart--;
		}
		else {
			pseudoRegular= 0;
		}
		for (i=0; i<dim; i++) {
			if (dssVersion == 6) {
				len1 = (int)strlen(eParts6[i]);
				if (len2 < len1) {
					len1 = len2;
				}
				same = zstringCompare(&Epart[pseudoRegular], eParts6[i], (size_t)len1);
			}
			else {
				len1 = (int)strlen(eParts7[i]);
				if (len2 < len1) {
					len1 = len2;
				}
				same = zstringCompare(&Epart[pseudoRegular], eParts7[i], (size_t)len1);
			}
			if (same) {
				if (pseudoRegular) {
					//  If quasi-regular interval (i.e., irregular), return the block size
					if (dssVersion == 6) {
						if (secondsInInterval[i] < SECS_IN_30_MINUTES) {  //  e.g., minute data
							//  Less than 30 minutes (SECS_IN_30_MINUTES seconds) goes in to blocks of 1 day
							*intervalSeconds = -BLOCK_1_DAY;
						}
						else if (secondsInInterval[i] < SECS_IN_1_DAY) {  //  e.g., hourly data
							//  Less than 6 hours (SECS_IN_1_DAY seconds) goes in to blocks of 1 month
							*intervalSeconds = -BLOCK_1_MONTH;
						}
						else if (secondsInInterval[i] < SECS_IN_1_WEEK) {   //  e.g., daily data
							//  Less than 1 week goes in to blocks of 1 year
							*intervalSeconds = -BLOCK_1_YEAR;
						}
						else if (secondsInInterval[i] < SECS_IN_1_MONTH) {  //  e.g., monthly data
							//  1 month and less (2,592,000 seconds) goes in to blocks of 1 decade
							*intervalSeconds = -BLOCK_1_DECADE;
						}
						else { //if (secondsInInterval[i] <= SECS_IN_1_YEAR) {  //  e.g., yearly
							//  Above one month goes into blocks of 1 century
							*intervalSeconds = -BLOCK_1_CENTURY;
						}
					}
					else {
						if (secondsInInterval[i] < SECS_IN_30_MINUTES) {  //  e.g., minute data
							//  Less than 30 minutes INTVL_30_MINUTEseconds) goes in to blocks of 1 day
							*intervalSeconds = -BLOCK_1_DAY;
						}
						else if (secondsInInterval[i] < SECS_IN_6_HOURS) {  //  e.g., hourly data
							//  DSS-7 change, 6, 8, 12 hours goes into blocks of a year
							//  DSS-6 had 6, 8, 12 hours going into blocks of a month
							//  Less than 6 hours (21,600 seconds) goes in to blocks of 1 month
							*intervalSeconds = -BLOCK_1_MONTH;
						}
						else if (secondsInInterval[i] < SECS_IN_1_WEEK) {   //  e.g., daily data
							//  Less than 1 week goes in to blocks of 1 year
							*intervalSeconds = -BLOCK_1_YEAR;
						}
						else if (secondsInInterval[i] < SECS_IN_1_MONTH) {  //  e.g., monthly data
							//  1 month and less (2,592,000 seconds) goes in to blocks of 1 decade
							*intervalSeconds = -BLOCK_1_DECADE;
						}
						else { //if (secondsInInterval[i] <= SECS_IN_1_YEAR) {  //  e.g., yearly
							   //  Above one month goes into blocks of 1 century
							*intervalSeconds = -BLOCK_1_CENTURY;
						}
					}
				}
				else {
					*intervalSeconds = secondsInInterval[i];
				}
				if (*operation == EPART_TO_SECONDS_TO_EPART) {
					if (dssVersion == 6) {
						stringCopy(&Epart[pseudoRegular], sizeofEpart, eParts6[i], strlen(eParts6[i]));
					}
					else {
						stringCopy(&Epart[pseudoRegular], sizeofEpart, eParts7[i], strlen(eParts7[i]));
					}
				}
				if (*intervalSeconds > 0) {
					return 0;
				}
				else {
					return 1;
				}
			}
		}
	}
	else if (*operation == SECONDS_TO_EPART) {
		//  set operation = SECONDS_TO_EPART To go from intervalSeconds to char Epart
		//  Note, quasi-interval is not returned here for irregular, only block size
		for (i=0; i<dim; i++) {
			if (*intervalSeconds == secondsInInterval[i]) {
				if (dssVersion == 6) {
					stringCopy(Epart, sizeofEpart, eParts6[i], strlen(eParts6[i]));
				}
				else {
					stringCopy(Epart, sizeofEpart, eParts7[i], strlen(eParts7[i]));
				}
				return 0;
			}
		}
	}
	else if (*operation == BEGIN_ENUMERATION) {
		//  set operation = BEGIN_ENUMERATION to begin a list of valid E parts (intervals used to keep track)
		*intervalSeconds = secondsInInterval[0];
		if (dssVersion == 6) {
			stringCopy(Epart, sizeofEpart, eParts6[0], strlen(eParts6[0]));
		}
		else {
			stringCopy(Epart, sizeofEpart, eParts7[0], strlen(eParts7[0]));
		}
		*operation = CONTINUE_ENUMERATION;
		return 0;
	}
	else if (*operation == CONTINUE_ENUMERATION) {
		//  set operation = CONTINUE_ENUMERATION In a list of valid E parts (returns -1 at end)
		for (i=0; i<dim; i++) {
			if (*intervalSeconds == secondsInInterval[i]) {
				if (i == (dim-1)) {
					*intervalSeconds = 0;
					*operation = -1;
					return STATUS_NOT_OKAY;
				}
				if (dssVersion == 6) {
					stringCopy(Epart, sizeofEpart, eParts6[i+1], strlen(eParts6[i+1]));
				}
				else {
					stringCopy(Epart, sizeofEpart, eParts7[i+1], strlen(eParts7[i+1]));
				}
				*intervalSeconds = secondsInInterval[i+1];
				return 0;
			}
		}
	}
	return STATUS_NOT_OKAY;

}


