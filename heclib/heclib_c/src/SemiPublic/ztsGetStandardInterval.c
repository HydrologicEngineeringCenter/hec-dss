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
*  Declaration: int ztsGetStandardInterval(int dssVersion, int *intervalSeconds, char *Epart, size_t sizeofEpart, int *flagDirection);
*
*  Parameters:	int dssVersion
*					The DSS Version the results are for.  Must either be 6 or 7.  (Versions have different intervals)
*
*				int *intervalSeconds (input or output)
*					If flagDirection is 0 or 1, this is returned with the time interval, in seconds from the E part.
*					If flagDirection is 2, the E part is returned using this time interval.
*					If flagDirection is 3 or 4, this is returned with the a time interval from the list.
*
*				char *Epart (input or output)
*					The E part of the pathname, either determined from the interval, or used to determine the interval
*
*				size_t sizeofEpart (input)
*					The size of the E part, used when returning the E part from the interval.
*
*				int *flagDirection
*					A flag telling ztsGetStandardInterval7 what to do, set flagDirection to:
*						0 to go from char Epart to intervalSeconds AND change the E part to the standard for that interval.
*						1 to go from char Epart to intervalSeconds
*						2 to go from intervalSeconds to char Epart
*						3 to begin a list of valid E parts (intervals used to keep track)
*						4 In a list of valid E parts (returns -1 at end)
*
*
*	Returns:	0 for regular interval
*				1 for irregular interval
*				STATUS_NOT_OKAY for error or end of list
*
*	Remarks:	For a flagDirection set to 0 or 1, the function tries to recognized the E part to get the interval.
*				When flagDirection is set to 0, the standard E part is returned in Epart.  For example, if you pass
*				an E part of "1MON", the E part will be returned with "1Month".
*				To get a list of intervals, begin flagDirection with 3.  ztsGetStandardInterval7 will return
*				the interval in seconds and the E part for each valid interval, including irregular interval and pseudo-regular.
*				At the end of the list, a STATUS_NOT_OKAY is returned (see example)
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
*		int intervalSeconds, flagDirection, version;
*		char Epart[20];
*
*		flagDirection = 3;
*		version = zgetVersion(ifltab);
*		while (ztsGetStandardInterval(version, &intervalSeconds, Epart, sizeof(Epart), &flagDirection) == STATUS_OKAY) {
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

int ztsGetStandardInterval(int dssVersion, int *intervalSeconds, char *Epart, size_t sizeofEpart, int *flagDirection)
{
	static const char eParts7[][12] = {
		"1Year",      "1Month",    "Semi-Month", "Tri-Month",
		"1Week",      "1Day",      "12Hour",     "8Hour",
		"6Hour",      "4Hour",     "3Hour",      "2Hour",
		"1Hour",      "30Minute",  "20Minute",   "15Minute",
		"12Minute",   "10Minute",  "6Minute",    "5Minute",
		"4Minute",    "3Minute",   "2Minute",    "1Minute",
		"30Second",   "20Second",  "15Second",   "10Second",
		"6Second",    "5Second",   "4Second",    "3Second",
		"2Second",    "1Second",
		"IR-Century", "IR-Decade", "IR-Year",	"IR-Month",  "IR-Day"};

	//  Longest time interval allowed for regular interval = 1Year
	//  1Year = 365 Days = 8760 hours = 525600 minutes = 31536000 seconds
	static int secondsInInterval7[39] = {
		31536000,     2592000,     1296000,      864000,
		604800,       86400,       43200,        28800,
		21600,        14400,       10800,        7200,
		3600,         1800,        1200,         900,
		720,          600,         360,          300,
		240,          180,         120,          60,
		30,           20,          15,           10,
		6,            5,           4,            3,
		2,            1,
		-5,          -4,          -3,           -2,      -1};

	static const char eParts6[][12] = {
		"1YEAR",      "1MON",    "SEMI-MONTH", "TRI-MONTH",
		"1WEEK",      "1DAY",        "12HOUR",     "8HOUR",
		"6HOUR",     "4HOUR",         "3HOUR",     "2HOUR",
		"1HOUR",     "30MIN",         "20MIN",     "15MIN",
		"12MIN",	 "10MIN",          "6MIN",      "5MIN",
		"4MIN",       "3MIN",          "2MIN",      "1MIN",
		"IR-CENTURY", "IR-DECADE",  "IR-YEAR",	"IR-MONTH",  "IR-DAY"};

	//  Longest time interval allowed for regular interval = 1Year
	//  1Year = 365Days = 8760 hour = 525600 minutes = 31536000 seconds
	static int secondsInInterval6[39] = {
		31536000,     2592000,     1296000,      864000,
		604800,       86400,       43200,        28800,
		21600,        14400,       10800,        7200,
		3600,         1800,        1200,         900,
		720,          600,         360,          300,
		240,          180,         120,          60,
		-5,          -4,          -3,           -2,      -1};



	int *secondsInInterval;

	int i;
	int len1;
	int len2;
	int same;
	int pseudoRegular;
	int dim;


	if (dssVersion == 6) {
		secondsInInterval = secondsInInterval6;
		dim=29;
	}
	else {
		secondsInInterval = secondsInInterval7;
		dim=39;
	}

	if ((*flagDirection == 1) || (*flagDirection == 0)) {
		//  set flagDirection = 0 to go from char Epart to intervalSeconds AND return
		//  the standard E part for that interval.
		//  set flagDirection = 1 To go from char Epart to intervalSeconds
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
						if (secondsInInterval[i] < 1800) {  //  e.g., minute data
							//  Less than 30 minutes (1800 seconds) goes in to blocks of 1 day
							*intervalSeconds = -1;
						}
						else if (secondsInInterval[i] < 86400) {  //  e.g., hourly data
							//  Less than 6 hours (86400 seconds) goes in to blocks of 1 month
							*intervalSeconds = -2;
						}
						else if (secondsInInterval[i] < 604800) {   //  e.g., daily data
							//  Less than 1 week (604800 seconds) goes in to blocks of 1 year
							*intervalSeconds = -3;
						}
						else if (secondsInInterval[i] < 2592000) {  //  e.g., monthly data
							//  1 month and less (2,592,000 seconds) goes in to blocks of 1 decade
							*intervalSeconds = -4;
						}
						else { //if (secondsInInterval[i] <= 31536000) {  //  e.g., yearly
							//  Above one month goes into blocks of 1 century
							*intervalSeconds = -5;
						}
					}
					else {
						if (secondsInInterval[i] < 1800) {  //  e.g., minute data
							//  Less than 30 minutes (1800 seconds) goes in to blocks of 1 day
							*intervalSeconds = -1;
						}
						else if (secondsInInterval[i] < 21600) {  //  e.g., hourly data
							//  DSS-7 change, 6, 8, 12 hours goes into blocks of a year
							//  DSS-6 had 6, 8, 12 hours going into blocks of a month
							//  Less than 6 hours (21,600 seconds) goes in to blocks of 1 month
							*intervalSeconds = -2;
						}
						else if (secondsInInterval[i] < 604800) {   //  e.g., daily data
							//  Less than 1 week (604800 seconds) goes in to blocks of 1 year
							*intervalSeconds = -3;
						}
						else if (secondsInInterval[i] < 2592000) {  //  e.g., monthly data
							//  1 month and less (2,592,000 seconds) goes in to blocks of 1 decade
							*intervalSeconds = -4;
						}
						else { //if (secondsInInterval[i] <= 31536000) {  //  e.g., yearly
							   //  Above one month goes into blocks of 1 century
							*intervalSeconds = -5;
						}
					}
				}
				else {
					*intervalSeconds = secondsInInterval[i];
				}
				if (*flagDirection == 0) {
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
	else if (*flagDirection == 2) {
		//  set flagDirection = 2 To go from intervalSeconds to char Epart
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
	else if (*flagDirection == 3) {
		//  set flagDirection = 3 to begin a list of valid E parts (intervals used to keep track)
		*intervalSeconds = secondsInInterval[0];
		if (dssVersion == 6) {
			stringCopy(Epart, sizeofEpart, eParts6[0], strlen(eParts6[0]));
		}
		else {
			stringCopy(Epart, sizeofEpart, eParts7[0], strlen(eParts7[0]));
		}
		*flagDirection = 4;
		return 0;
	}
	else if (*flagDirection == 4) {
		//  set flagDirection = 4 In a list of valid E parts (returns -1 at end)
		for (i=0; i<dim; i++) {
			if (*intervalSeconds == secondsInInterval[i]) {
				if (i == (dim-1)) {
					*intervalSeconds = 0;
					*flagDirection = -1;
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


