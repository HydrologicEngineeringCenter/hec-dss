#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "hecdss7.h"
#include "hecdssInternal.h"
#include "zerrorCodes.h"

/**
*  Function:	ztsProcessTimes
*
*  Use:			Private
*
*  Description:	Computes a variety of time parameters from a pathname or dates and times and fills in the timeWindow struct (see below)
*
*  Declaration: int ztsProcessTimes(long long *ifltab, zStructTimeSeries *tss, int boolStore);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.  
*					This should be considered as a “handle” array and must be passed to any function that accesses that file.
*
*				zStructTimeSeries *tss
*					The time series struct to process.  This function will file out the timeWindow struct within it.
*
*				int boolStore
*					A flag to indicate if the data set is being stored (or retrieved.)
*					Set to one when storing, zero when retrieving 
*
*				
*
*	Returns:	int status 
*					STATUS_NOT_OKAY for error 
*					0 for regular interval time series 
*					1 for irregular interval time series
*	
*	Remarks:		This function takes a pathname and optionally a time window
*					and computes if regular or irregular, the start time,
*					the end time and other time parameters.
*					If startDate, startTime, etc. are passed in, those take precedence.
*					If not, then a date range in the D part will define the time window
*					(e.g., .../12JUL2002 - 13SEP2004/1DAY/...)
*					If not, then the D part and E part will define the time window
*					for that single record.
*
*					If regular interval, will compute
*						time interval
*						number values
*						start block
*						end block
*
*					If irregular interval, will compute
*						block size (e.g., month, year)
*						start block
*						end block
*
*					This function is meant to be called only once for a time series struct
*					using the internal ztsTimeWindow struct to determine if it has or not.
*					Calling additional times just returns.
*
*	Note:			Note:  numberValues = (numberPeriods - 1)
*					(1 value would have same start and end time, or zero periods)
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int ztsProcessTimes(long long *ifltab, zStructTimeSeries *tss, int boolStore)
{

	int timeWindowDefined;
	int flag;
	int len;
	size_t pathSize;
	int status;
	int startDefined;
	int endDefined;
	int version;
	int boolIrreg;
	int jul;
	int days;

	char *path;
	char blockDate[MAX_PART_SIZE];
	char dPart[MAX_PART_SIZE];
	char ePart[MAX_PART_SIZE];


	if (!tss->timeWindow) {
		len = sizeof(ztsTimeWindow);	
		tss->timeWindow = (ztsTimeWindow*)calloc((size_t)len, BYTE_SIZE);
		if (!tss->timeWindow) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_ztsProcessTimes_ID, 
									zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, len, 0, 
									zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "time window array");
		}
		tss->allocated[zSTRUCT_TS_timeWindow] = 1;		
	}
	else {
		//  This function is meant to be called only once for a time object.
		if (tss->timeWindow->intervalSeconds > 0) {
			return 0;
		}
		else {
			return 1;
		}
	}


	//  For older code, "undefined" times might be passed in as zero (not undefined)
	if (tss->julianBaseDate == 0) {
		if (((tss->startJulianDate == 0) && (tss->endJulianDate == 0)) ||
		((tss->startJulianDate == -1) && (tss->endJulianDate == -1))) {
			tss->startJulianDate = UNDEFINED_TIME;
			tss->startTimeSeconds = -1;
			tss->endJulianDate = UNDEFINED_TIME;
			tss->endTimeSeconds = -1;
			startDefined = 0;
			endDefined = 0;
		}
		else {
			//  Clean the times, if defined
			startDefined = cleanTime(&tss->startJulianDate, &tss->startTimeSeconds, SECOND_GRANULARITY);
			endDefined = cleanTime(&tss->endJulianDate, &tss->endTimeSeconds, SECOND_GRANULARITY);
		}
	}
	else {
		//  If the base date is set, then dates will most likely be zero		
		if (tss->endJulianDate == UNDEFINED_TIME) {
			endDefined = 0;
		}
		else if ((tss->endJulianDate == 0) && (tss->endTimeSeconds == 0)) {
			endDefined = 0;
		}
		else {
			endDefined = 1;
		}
		if (tss->startJulianDate == UNDEFINED_TIME) {
			startDefined = 0;
		}
		else {
			if (!endDefined && ((tss->startJulianDate == 0) && (tss->startTimeSeconds == 0))) {
				startDefined = 0;
			}
			else {
				startDefined = 1;
			}
		}
	}

	if (startDefined && endDefined) {
		if (tss->endJulianDate < tss->startJulianDate) {
			endDefined = 0;
		}
	}

	if (boolStore && (tss->processedAlready == 1)) {
		//  Is this a copied record, with the start being the time window,
		//  not necessarily the first data time?
		if (tss->timeIntervalSeconds > 0) {
			ztsOffsetAdjustToOffset(tss->timeOffsetSeconds, tss->timeIntervalSeconds, &tss->startJulianDate, &tss->startTimeSeconds);
		}
	}
	

	//  Clean the pathname and place into a char array that we can change
	pathSize = strlen(tss->pathname);
	pathSize += 20;
	path = (char *)calloc((size_t)pathSize, CHAR_SIZE);
	if (!path) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsProcessTimes_ID, 
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, (int)pathSize, 0, 
								zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "");
	}
	zpathnameClean(path, pathSize, tss->pathname);

	if (startDefined && endDefined) {
		timeWindowDefined = 1;
	}
	else {
		timeWindowDefined = 0;
	}

	//  Get the E part (time interval) from the pathname
	len = zpathnameGetPart (path, 5, ePart, sizeof(ePart));
	if (len < 4) {
		//  Not a valid length for a time series interval 				
		if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_GENERAL) || zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_GENERAL)) {
			zmessage2(ifltab, "Invalid E part for time series: ", path);;
		}
		free(path);
		return STATUS_NOT_OKAY;
	}
	//  Get the time interval 
	flag = 0;
	version = zgetVersion(ifltab);
	status = ztsGetStandardInterval(version, &tss->timeWindow->intervalSeconds, ePart, sizeof(ePart), &flag);
	 if (status == 0) {		
		 boolIrreg = 0;
		 tss->timeIntervalSeconds = tss->timeWindow->intervalSeconds;
		 if (tss->numberValues > 0) {
			 //  Regular interval data is always based on the number of values, not the end date
			 tss->endJulianDate = UNDEFINED_TIME;
			 tss->endTimeSeconds = -1;
			 endDefined = 0;
			 timeWindowDefined = 0;
		 }
	 }
	 else if (status == 1) {
		 //  Irregular Interval - block size sent back in interval, as negative
		 tss->timeWindow->blockSize = -tss->timeWindow->intervalSeconds;
		 boolIrreg = 1;
		 tss->timeIntervalSeconds = 0;
	 }
	 else {
		//  Not a valid time series interval
		 if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_GENERAL) || zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_GENERAL)) {
			zmessage2(ifltab, "Invalid time series interval: ", path);
		 }
		 free(path);
		 return STATUS_NOT_OKAY;
	}
	//  Put the standardized E part (back) into the pathname
	zpathnameSetPart (path, pathSize, ePart, 5);

	//  Should we query the database to retrieve all times?
	if (tss->boolRetrieveAllTimes && !boolStore) {
		status = ztsGetDateRange(ifltab, path, 1, &tss->startJulianDate, &tss->endJulianDate);
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_ztsProcessTimes_ID);
		}
		if (status == STATUS_RECORD_FOUND) {
			if (tss->timeIntervalSeconds >= 60) {
				tss->startTimeSeconds = 60;
			}
			else {
				tss->startTimeSeconds = 1;
			}
			tss->endTimeSeconds = (1440 * 60);
			tss->numberValues = 0;
			if (tss->timeIntervalSeconds > 0) {
				ztsOffsetAdjustToStandard(tss->timeIntervalSeconds, &tss->startJulianDate, &tss->startTimeSeconds);
				ztsOffsetAdjustToStandard(tss->timeIntervalSeconds, &tss->endJulianDate, &tss->endTimeSeconds);
			}
			startDefined = 1;
			endDefined = 1;
		}
	}

	//  Set any predefined times 
	tss->timeWindow->startJulian = tss->startJulianDate;
	tss->timeWindow->startTimeSeconds = tss->startTimeSeconds;
	tss->timeWindow->endJulian = tss->endJulianDate;
	tss->timeWindow->endTimeSeconds = tss->endTimeSeconds;
	tss->timeWindow->numberValues = tss->numberValues;
	tss->timeWindow->startBlockJulian = UNDEFINED_TIME;
	tss->timeWindow->endBlockJulian = UNDEFINED_TIME;

	//  Check for a time pattern
	len = zpathnameGetPart (path, 4, dPart, sizeof(dPart));
	if (len > 4) {
		dPart[4] = '\0';
		if (zstringCompare(dPart, "TS-P", strlen(dPart))) {
			//  A time pattern data set
			tss->boolPattern = 1;
			if ((tss->pathnameInternal) && (tss->allocated[zSTRUCT_pathnameInternal])) free(tss->pathnameInternal);
			tss->pathnameInternal = mallocAndCopy(path);
			tss->allocated[zSTRUCT_pathnameInternal] = 1;
			if ((tss->timeGranularitySeconds == 0) && boolStore) {
				if ((tss->timeWindow->intervalSeconds > 0) && (tss->timeWindow->intervalSeconds < 60)) {
					tss->timeGranularitySeconds = SECOND_GRANULARITY;  //  Max days = LONG_MAX / 86400 = 24,855
				}
				else {
					tss->timeGranularitySeconds = MINUTE_GRANULARITY;  //  Should have been set by user
				}
			}
			free(path);
			return status;
		}
	}

	//  If we are retrieving data, check to see if time window is implicit in
	//  the pathname.  (Explicit times must be given for storing)
	if (!boolStore) {
		if (!startDefined && !endDefined) {
			//  Make sure the E part is standard, and
			//  Get the time window specified in the D part of the pathname
			timeWindowDefined = ztsGetPathTimeWindow(zgetVersion(ifltab), path, pathSize, tss->timeWindow);
			if (timeWindowDefined < 0) {
				//  Fix me
				free(path);
				return timeWindowDefined;
			}
			//  If no time window, get it from the file (slow)
			if (!timeWindowDefined) {
				status = ztsGetDateRange(ifltab, path, 1, &tss->timeWindow->startJulian, &tss->timeWindow->endJulian);
				if (status == STATUS_OKAY) {
					if (tss->timeIntervalSeconds > 0) {
						tss->timeWindow->startTimeSeconds = tss->timeWindow->intervalSeconds;
						tss->timeWindow->endTimeSeconds = 1440 * 60;
						ztsOffsetAdjustToStandard(tss->timeIntervalSeconds, &tss->timeWindow->startJulian, &tss->timeWindow->startTimeSeconds);
						ztsOffsetAdjustToStandard(tss->timeIntervalSeconds, &tss->timeWindow->endJulian, &tss->timeWindow->endTimeSeconds);
					}
					else {
						tss->timeWindow->startTimeSeconds = 1;
						tss->timeWindow->endTimeSeconds = 1440 * 60;
					}
				}
			}
		}
	}
	else {  //  if (boolStore) {
		//  If dates not defined, use the time array and base date to define.
		if (tss->times) {
			if (!startDefined && (tss->numberValues > 0)) {
				tss->timeWindow->startJulian = tss->julianBaseDate;
				tss->timeWindow->startTimeSeconds = tss->times[0];
				cleanTime(&tss->timeWindow->startJulian, &tss->timeWindow->startTimeSeconds, tss->timeGranularitySeconds);
				if (tss->timeGranularitySeconds > 0) {
					tss->timeWindow->startTimeSeconds *= tss->timeGranularitySeconds;
				}
				else {
					tss->timeWindow->startTimeSeconds *= 60;  //  Default is one minute
				}
				startDefined = 1;
				if (!isTimeDefined(tss->startJulianDate, tss->startTimeSeconds)) {
					tss->startJulianDate = tss->timeWindow->startJulian;
					tss->startTimeSeconds = tss->timeWindow->startTimeSeconds;
				}	
			}
			if (!endDefined && (tss->numberValues > 0)) {
				tss->timeWindow->endJulian = tss->julianBaseDate;
				tss->timeWindow->endTimeSeconds = tss->times[tss->numberValues-1];
				cleanTime(&tss->timeWindow->endJulian, &tss->timeWindow->endTimeSeconds, tss->timeGranularitySeconds);
				if (tss->timeGranularitySeconds > 0) {
					tss->timeWindow->endTimeSeconds *= tss->timeGranularitySeconds;
				}
				else {
					tss->timeWindow->endTimeSeconds *= 60;  //  Default is one minute
				}
				endDefined = 1;
				if (!isTimeDefined(tss->endJulianDate, tss->endTimeSeconds)) {
					tss->endJulianDate = tss->timeWindow->endJulian;
					tss->endTimeSeconds = tss->timeWindow->endTimeSeconds;
				}		
			}		
			if (startDefined && endDefined) timeWindowDefined = 1;
		}
	}

	if (isTimeDefined(tss->timeWindow->startJulian, tss->timeWindow->startTimeSeconds)) {
		if (tss->timeWindow->intervalSeconds > 0) {
			//   Regular. 
			if (!(startDefined && endDefined)) {
				if (tss->numberValues > 0) {
				//  Number periods is one less than number values
					incrementTime(tss->timeWindow->intervalSeconds, (tss->numberValues - 1), 
						tss->timeWindow->startJulian, tss->timeWindow->startTimeSeconds, 
						&tss->timeWindow->endJulian, &tss->timeWindow->endTimeSeconds);	
					if (!isTimeDefined(tss->startJulianDate, tss->startTimeSeconds)) {
						tss->startJulianDate = tss->timeWindow->startJulian;
						tss->startTimeSeconds = tss->timeWindow->startTimeSeconds;
						startDefined = 1;
					}
					if (!isTimeDefined(tss->endJulianDate, tss->endTimeSeconds)) {
						tss->endJulianDate = tss->timeWindow->endJulian;
						tss->endTimeSeconds = tss->timeWindow->endTimeSeconds;
						endDefined = 1;
					}				
				}			
			}
			//   Make sure the internal time window is on standard boundaries....
			tss->timeWindow->timeOffsetSeconds = ztsOffsetAdjustToStandard(tss->timeWindow->intervalSeconds, 
				&tss->timeWindow->startJulian, &tss->timeWindow->startTimeSeconds);

			//  Now adjust end time to standard boundaries
			ztsOffsetAdjustToStandard(tss->timeWindow->intervalSeconds, &tss->timeWindow->endJulian, &tss->timeWindow->endTimeSeconds);
			//  Get the start date of the first block
			jul = tss->timeWindow->startJulian;
			tss->timeWindow->startBlockJulian = ztsRegGetBlockStart(jul, tss->timeWindow->intervalSeconds, &tss->timeWindow->blockSize);
			//  And the start date of the last block
			tss->timeWindow->endBlockJulian = ztsRegGetBlockStart(tss->timeWindow->endJulian, tss->timeWindow->intervalSeconds, &tss->timeWindow->blockSize);
			if (tss->timeWindow->numberValues <= 0) {
				tss->timeWindow->numberValues = numberPeriods(tss->timeWindow->intervalSeconds, tss->timeWindow->startJulian, tss->timeWindow->startTimeSeconds, 
					tss->timeWindow->endJulian, tss->timeWindow->endTimeSeconds) + 1;
				if (tss->timeWindow->numberValues <= 0) {
					if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_GENERAL) || zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_GENERAL)) {
						zmessage(ifltab, "Number of values for time series data less than one");
						ztsMessTimeWindow(ifltab, DSS_FUNCTION_ztsProcessTimes_ID, tss);
					 }
					free(path);
					return STATUS_NOT_OKAY;
				}
			}
			//  Do we have enough space for this time window?
			if ((tss->numberValues > 0) && (tss->numberValues < tss->timeWindow->numberValues)) {
				//  No - adjust end time for space
				//  Number periods is one less than number values
				tss->numberValues--;
				incrementTime(tss->timeWindow->intervalSeconds, tss->numberValues, 
					tss->timeWindow->startJulian, tss->timeWindow->startTimeSeconds, 
					&tss->timeWindow->endJulian, &tss->timeWindow->endTimeSeconds);			
			}
		}
		else {
			//  Irregular data
			//  Get the start date of the first block
			jul = tss->timeWindow->startJulian;
			tss->timeWindow->startBlockJulian = ztsIrregGetBlockStart(jul, tss->timeWindow->blockSize);
			//  And the start date of the last block
			tss->timeWindow->endBlockJulian = ztsIrregGetBlockStart(tss->timeWindow->endJulian, tss->timeWindow->blockSize);	
			tss->timeWindow->numberValues = tss->numberValues;
		}
	}

	if (tss->timeWindow->startBlockJulian != UNDEFINED_TIME) {
		julianToDate(tss->timeWindow->startBlockJulian, 104, blockDate, sizeof(blockDate));
		//  Put the start block date - D part (back) into the pathname
		zpathnameSetPart (path, pathSize, blockDate, 4);
	}

	if (!startDefined) {
		tss->startJulianDate = tss->timeWindow->startJulian;
		tss->startTimeSeconds = tss->timeWindow->startTimeSeconds;
		startDefined = 1;
	}
	
	if (!endDefined) {
		tss->endJulianDate = tss->timeWindow->endJulian;
		tss->endTimeSeconds = tss->timeWindow->endTimeSeconds;
		//  (End is always at the standard time of the end of the period, so no adjustment needed.)
		endDefined = 1;
	}
	if (startDefined && endDefined) {
		timeWindowDefined = 1;
	}
	if (tss->numberValues <= 0) tss->numberValues = tss->timeWindow->numberValues;
	
	//  If no time granularity defined, then define it now.
	//  This is what each unit in the times array will represent.
	//  The default is minutes; however it could also be seconds, hours and days (for long time spans)
	//  Based on largest int without overflowing
	//  LONG_MAX = 2**31 -1 = 2,147,483,647  
	//  That's 4085 years, or 1,491,308 days or 2,147,483,647 minutes
	//  timeGranularitySeconds should always be specified when storing data
	if (timeWindowDefined && !tss->timeGranularitySeconds) {
		if ((tss->timeWindow->intervalSeconds > 0) && (tss->timeWindow->intervalSeconds < 60)) {
			tss->timeGranularitySeconds = SECOND_GRANULARITY;  //  Max days = LONG_MAX / 86400 = 24,855
		}
		else if (boolIrreg && boolStore)  {
			tss->timeGranularitySeconds = MINUTE_GRANULARITY;  //  Should have been set by user
		}
		else {
			days = tss->timeWindow->endJulian - tss->timeWindow->startJulian;
			if (days < 1000000) {			//  1,000,000 Max days = LONG_MAX / 1440 = 1,491,308
				tss->timeGranularitySeconds = MINUTE_GRANULARITY;   // 99.9% of the time
			}
			else if  (days < 30000000) {		//   30,000,000	Max days = LONG_MAX / 60 = 35,791,394
				tss->timeGranularitySeconds = HOUR_GRANULARITY;   // .1% of the time
			}
			else {						//  Max days = LONG_MAX / 1
				tss->timeGranularitySeconds = DAY_GRANULARITY;  //  For use in statistical analysis
			}
		}
	}

	//  Do we need to set a base date for large time spans or times not near 1900 or second granularity?
	if (!boolStore && timeWindowDefined && (tss->julianBaseDate == 0) && (tss->timeWindow->startBlockJulian != 0)) {
		if (tss->timeGranularitySeconds == SECOND_GRANULARITY) {
			tss->julianBaseDate = tss->timeWindow->startBlockJulian;
		}
		else if (tss->timeGranularitySeconds == MINUTE_GRANULARITY) {
			//  Julian 0 is the year 1900
			//  Approx year 900 to 5000 uses base date of 0, otherwise start block date
			if ((tss->timeWindow->startJulian < (-365*1000)) || (tss->timeWindow->startJulian > (365*3100)) || 
				(tss->timeWindow->endJulian > (365 * 3200)) ) {
				tss->julianBaseDate = tss->timeWindow->startBlockJulian;
			}
		}
		else {	//  If the granularity is not standard, then use the start block as the base date.
			tss->julianBaseDate = tss->timeWindow->startBlockJulian;
		}
	}

	tss->timeOffsetSeconds = tss->timeWindow->timeOffsetSeconds;

	if ((tss->pathnameInternal) && (tss->allocated[zSTRUCT_pathnameInternal])) free(tss->pathnameInternal);
	tss->pathnameInternal = mallocAndCopy(path);
	tss->allocated[zSTRUCT_pathnameInternal] = 1;

	if (tss->timeWindow->intervalSeconds > 0) {
		status =  0;
	}
	else {
		status =  1;
	}

	//  Was no time window given (and thus an error)?
	if (!timeWindowDefined) {
		if (!tss->boolPattern) {
			if (!isTimeDefined(tss->timeWindow->startJulian, tss->timeWindow->startTimeSeconds))
				status = STATUS_NOT_OKAY;
			if (!isTimeDefined(tss->timeWindow->endJulian, tss->timeWindow->endTimeSeconds))
				status = STATUS_NOT_OKAY;
			if (status == STATUS_NOT_OKAY) {
				if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_GENERAL) || zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_GENERAL)) {
					zmessage2(ifltab, "No time window defined for time series data for record: ", tss->pathname);
					zmessage(ifltab, "Time window information passed in follows:");
					ztsMessTimeWindow(ifltab, DSS_FUNCTION_ztsProcessTimes_ID, tss);
					zmessage(ifltab, " ");
				}
			}
		}
	}

	if (boolStore) {
		tss->processedAlready = 2;
	}
	else {
		//  For retrieval
		tss->processedAlready = 1;
	}
	free(path);
	return status;
}
