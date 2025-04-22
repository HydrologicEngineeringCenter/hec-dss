#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "heclib.h"
#include "zdssKeys.h"
#include "zdssLocking.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "zStructTsTimeWindow.h"
#include "zerrorCodes.h"

/**
*  Function:	ztsStoreIrreg7
*
*  Use:			Private (use ztsStore for public access)
*
*  Description:	Write an irregular-interval time series data set to a version 7 DSS file.
*
*  Declaration: int ztsStoreIrreg7(long long *ifltab, zStructTimeSeries *tss, int storageFlag);
*
*  Parameters:	long long *ifltab
*					The file table array, similar to a handle number
*
*				zStructTimeSeries *tss
*					The time series struct that contains all data and information to store.
*					See ztsStore for definition of zStructTimeSeries
*
*				int storageFlag
*					0:  Merge data sets together
*					1:  Replace data set (preferred)
*
*
*	Returns:	int status
*					STATUS_OKAY
*					error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*	Remarks:	ztsStore() is the public function.  Do not call this (ztsStoreIrreg7), as it is
*					internal and arguments are subject to change.
*				This function blocks data sets into the standard single record conventions, and
*					merges/stores data in DSS version 7.
*
*	Notes:		Internally, times are always stored in seconds, except for IR-Century.
*					For IR-Century, the int size would be exceeded, so the granularity is limited to minutes.
*					Times are stored relative to the base date, the date in the "D" part of the pathname.
*					The time associated with the end of of the first day (e.g., 01Jan2000)
*					would be SECS_IN_1_DAY (24 hours * 60 mins/hr * 60 seconds/min)
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int ztsStoreIrreg7(long long *ifltab, zStructTimeSeries *tss, int storageFlag)
{
	int status;
	int *values;
	int valueSize;
	int *profileDepths;
	int profileDepthsSize;

	int startJulian, startSeconds;
	int endJulian;
	int blockSize;
	int julianBlockDate;
	int julianNextBlockDate;
	int boolFromStartOfBlock;
	int boolToEndOfBlock;
	int currentPosition;
	int timeBaseToStart;
	int timeBaseToEnd;
	int time;
	int dataType;
	int valueElementSize;
	int profileDepthsNumber;
	int timeGranularity;

	int ipos;
	int jpos;
	int kpos;
	int icount;
	int npos;
	int lengthCNotesRemaining;
	int i;
	int numberToStore;
	int julian;
	int seconds;
	long long ltime;

	int internalHeader[INT_HEAD_SIZE];
	int internalHeaderArraySize  = INT_HEAD_SIZE;
	int boolReadBlock;
	int *itimes;

	char messageString[100];
	char path[MAX_PATHNAME_LENGTH];
	char blockDate[20];
	char nextBlockDate[20];

	int buffer[1];
	long long bufferControl[4];
	buffer[0] = 0;
	bufferControl[BUFF_SIZE] = 0;
	bufferControl[BUFF_STAT] = 0;
	bufferControl[BUFF_ADDRESS] = 0;
	bufferControl[BUFF_INTS_USED] = 0;



	//  Check for correct DSS Version
	if (zgetVersion(ifltab) != 7) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
								zgetVersion((void *)ifltab), 0, zdssErrorSeverity.WARNING, tss->pathname, "");
	}

	if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessage(ifltab, " ");
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, "Handle: ", zhandle(ifltab));
		zmessageDebug(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, "Pathname: ", tss->pathname);
	}

	//  Cannot store both character and integer notes (only one or the other - occupy same space)
	if ((tss->cnotesLengthTotal > 0) && (tss->inoteElementSize > 0)) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, zdssErrorCodes.BOTH_NOTE_KINDS_USED,
								0, 0, zdssErrorSeverity.WARNING, tss->pathname, "");
	}
	itimes = 0;

	//  Initialize the internal header array
	for (i=0; i<internalHeaderArraySize ; i++) {
		internalHeader[i] = 0;
	}

	if (!tss->timeWindow) {
		status = ztsProcessTimes(ifltab, tss, 1);
		//  A return of 1 indicates irregular interval and is required.  NO_TIME_WINDOW
		if (status != 1) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, zdssErrorCodes.NO_TIME_WINDOW,
									0, 0, zdssErrorSeverity.WARNING, tss->pathname, "");
		}
	}
	timeGranularity = tss->timeGranularitySeconds;
	if (timeGranularity == 0) timeGranularity = MINUTE_GRANULARITY;

	//  We'll be changing the pathname to have correct date and interval part, so make a copy
	stringCopy(path, sizeof(path), zgetInternalPath(tss), strlen(zgetInternalPath(tss)));


	//  Figure out what kind of data we are storing
	//  floats or doubles?
	if (tss->profileDepthsNumber == 0) {
		if (tss->floatValues) {
			//  Floats
			values = (int *)tss->floatValues;
			valueSize = 1;
			valueElementSize = 1;
			dataType = DATA_TYPE_ITS;
		}
		else if (tss->doubleValues) {
			//  Doubles
			values = (int *)tss->doubleValues;
			valueSize = 2;
			valueElementSize = 2;
			dataType = DATA_TYPE_ITD;
		}
		else {
			//  No data!
			status = zerrorProcessing(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, zdssErrorCodes.NO_DATA_GIVEN,
								    0, 0, zdssErrorSeverity.WARNING, tss->pathname, "");
			zquery("empty", messageString, 2, &i);
			if (!i) return status;
		}
		profileDepths = (int *)0;   //   Dummy, to avoid compiler complaints
		profileDepthsSize = 0;
		internalHeader[INT_HEAD_profileDepthsNumber] = 0;
		profileDepthsNumber = 0;
	}
	else {
		internalHeader[INT_HEAD_profileDepthsNumber] = tss->profileDepthsNumber;
		profileDepthsNumber = tss->profileDepthsNumber;
		if (tss->floatProfileValues) {
			//  Float Profile
			valueElementSize = 1;
			values = (int *)tss->floatProfileValues;
			profileDepths = (int *)tss->floatProfileDepths;
			dataType = DATA_TYPE_ITS_PROFILE;
		}
		else if (tss->doubleProfileValues) {
			//  Doubles
			valueElementSize = 2;
			values = (int *)tss->doubleProfileValues;
			profileDepths = (int *)tss->doubleProfileDepths;
			dataType = DATA_TYPE_ITD_PROFILE;
		}
		else {
			//  No data!
			status = zerrorProcessing(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, zdssErrorCodes.NO_DATA_GIVEN,
								    0, 0, zdssErrorSeverity.WARNING, tss->pathname, "");
			zquery("empty", messageString, 2, &i);
			if (!i) return status;
		}
		valueSize = valueElementSize * tss->profileDepthsNumber;
		profileDepthsSize = valueElementSize * tss->profileDepthsNumber;
	}
	internalHeader[INT_HEAD_valueSize] = valueSize;
	internalHeader[INT_HEAD_valueElementSize] = valueElementSize;
	if (tss->dataType == 0) tss->dataType = dataType;

	//  The time granularity of a individual record is always in seconds, unless
	//  the record block is a century (and would overflow an int); then it is in minutes
	blockSize = tss->timeWindow->blockSize;
	internalHeader[0] = 0;
	julianBlockDate = tss->timeWindow->startBlockJulian;
	startJulian = tss->timeWindow->startJulian;
	startSeconds = tss->timeWindow->startTimeSeconds;
	endJulian = tss->timeWindow->endJulian;

	if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessage(ifltab, " ");
		numberToStore = tss->numberValues;
		if (numberToStore > 10) numberToStore = 10;
		zmessageInt(ifltab, "Times and values for beginning of array, up to ", numberToStore);
		for (i = 0; i < numberToStore; i++) {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "Time and value at ordinate %d; ", (i + 1));
			if (tss->doubleValues) {
				ztsIrregMessage(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, messageString, tss->times[i], tss->timeGranularitySeconds, tss->julianBaseDate,
					(void *)&tss->doubleValues[i], 2, 0);
			}
			else if (tss->floatValues) {
				ztsIrregMessage(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, messageString, tss->times[i], tss->timeGranularitySeconds, tss->julianBaseDate,
					(void *)&tss->floatValues[i], 1, 0);
			}
			else if (tss->doubleProfileValues) {
				ztsIrregMessage(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, messageString, tss->times[i], tss->timeGranularitySeconds, tss->julianBaseDate,
					(void *)&tss->doubleProfileValues[i], 2, 0);
			}
			else if (tss->floatProfileValues) {
				ztsIrregMessage(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, messageString, tss->times[i], tss->timeGranularitySeconds, tss->julianBaseDate,
					(void *)&tss->floatProfileValues[i], 1, 0);
			}
		}
	}


	//  Initial error checking
	//  Verify that the first data time is not before start time, and last data time not after end time
	//compareTimes(int julianFirst, int secondsFirst, int julianBaseFirst, 1,
	//	startJulian, startSeconds, 0, 1);
	
	//  Verify that the times are ascending
	for (i=1; i<tss->numberValues; i++) {
		if (tss->times[i-1] > tss->times[i]) {
//			error out
			status = zerrorProcessing(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, zdssErrorCodes.TIMES_NOT_ASCENDING,
										0, 0, zdssErrorSeverity.WARNING, tss->pathname, "");
			//  Provide the user with a little more information, if he allows
			if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_TERSE)) {
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d;  time %d  is > time %d",
					i, tss->times[i-1], tss->times[i]);
				zmessage2(ifltab, "Ordinate: ", messageString);
				time = tss->times[i-1];
				if (tss->timeGranularitySeconds == MINUTE_GRANULARITY) time *= SECS_IN_1_MINUTE;
				ztsIrregMessage(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, "First value:  ", time, tss->timeGranularitySeconds, tss->julianBaseDate,
					&values[i-1], valueSize, 0);
				time = tss->times[i];
				if (tss->timeGranularitySeconds == MINUTE_GRANULARITY) time *= SECS_IN_1_MINUTE;
				ztsIrregMessage(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, "Next value:   ", time, tss->timeGranularitySeconds, tss->julianBaseDate,
					&values[i], valueSize, 0);
			}
			return status;
		}
	}



	//  Lock the file at this point, so we don't have to flush or
	//  unlock or worry about other users until we are finished.
	//  This way, we can carry buffers around and not have to
	//  re-read them
	status = zlockActive(ifltab, LOCKING_LEVEL_HIGH, LOCKING_LOCK_ON, LOCKING_FLUSH_ON);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_ztsStoreIrreg_ID);
	}

	//  Is this a pattern (D part of "TS-PATTERN")
	//  Get the date from the pathname
	if (tss->boolPattern) {
		//  A time pattern data set
		if (valueElementSize == 1) {
			dataType = DATA_TYPE_ITS_PATTERN;
		}
		else {
			dataType = DATA_TYPE_ITD_PATTERN;
		}
		status = ztsWriteBlock (ifltab, tss, tss->pathname,
								tss->times, 1, tss->numberValues,
								values, valueElementSize,
								tss->quality, tss->qualityElementSize,
								tss->inotes, tss->inoteElementSize,
								tss->cnotes, tss->cnotesLengthTotal,
								profileDepths, tss->profileDepthsNumber,
								internalHeader, 
								tss->userHeader, tss->userHeaderNumber,
								tss->numberValues, tss->numberValues,
								dataType);
		//status = ztsStorePattern(ifltab, tss);
		zlockActive(ifltab, LOCKING_LEVEL_HIGH, LOCKING_LOCK_ON, LOCKING_FLUSH_OFF);
		return status;
	}

	//  Main Loop
	//  Generate a new pathname for each block, changing the D (D part) each time
	//  If we are writing to a record that already exists, read it so that we can
	//  combine the two data sets.
	//-----------------------------------------------------
	currentPosition = 0;
	boolFromStartOfBlock = 0;
	lengthCNotesRemaining = tss->cnotesLengthTotal;
	npos = 0;
	if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		julianToDate(julianBlockDate, 4, blockDate, sizeof(blockDate));
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %s,  julian: %d",
			blockDate, julianBlockDate);
		zmessageDebug(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, "First block date: ", messageString);
		julianToDate(tss->timeWindow->endBlockJulian, 4, blockDate, sizeof(blockDate));
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %s,  julian: %d",
			blockDate, julianBlockDate);
		zmessageDebug(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, "End of time window block date: ", messageString);
	}

	while (julianBlockDate <= tss->timeWindow->endBlockJulian) {
		//  Get the date for this block
		julianToDate(julianBlockDate, 4, blockDate, sizeof(blockDate));

		//  Generate the pathname for this individual record
		zpathnameSetPart (path, sizeof(path), blockDate, 4);
		//  Get the date of the next block
		julianNextBlockDate = ztsIncrementBlock(julianBlockDate, blockSize);
		if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %s,  julian: %d",
				blockDate, julianBlockDate);
			zmessageDebug(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, "Block date: ", messageString);
			julianToDate(julianNextBlockDate, 4, nextBlockDate, sizeof(nextBlockDate));
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %s,  julian: %d",
				nextBlockDate, julianNextBlockDate);
			zmessageDebug(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, "Next Block date: ", messageString);
		}
		//  If the block we are writing to exists, determine if we have to read it.
		//  If the writing block dates encompass the entire block, we won't
		//  read it; we'll just write over the entire block.
		boolReadBlock = 1;
		//  Has to be a replace flag, not a merge flag.
		if (storageFlag == 1) {
			if ((startJulian < julianBlockDate) && (endJulian >= julianNextBlockDate)) {
				boolReadBlock = 0;
				if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
					zmessageDebug(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, "Bypass Read because time window spans entire block", "");
				}
			}
		}
		//  Times for irregular interval data are stored as seconds
		//  from the base date, except for century blocks.
		//  Century blocks are in minutes since seconds would exceed the max int.
		
		//  Calculate how many values we will store for this block
		//  Start from the current position and look for the time at the end of block
		//  timeGranularity is how the data is coming in, where blockSize is how it is stored
		timeBaseToStart = (julianBlockDate - tss->julianBaseDate);
		timeBaseToEnd = (julianNextBlockDate - julianBlockDate);

		if (timeGranularity == SECOND_GRANULARITY) {
			//  Seconds
			timeBaseToStart *= SECS_IN_1_DAY;
			timeBaseToEnd *= SECS_IN_1_DAY;
		}
		else if (timeGranularity == MINUTE_GRANULARITY) {
			timeBaseToStart *= MINS_IN_1_DAY;
			timeBaseToEnd *= MINS_IN_1_DAY;
		}
		else if (timeGranularity == HOUR_GRANULARITY) {
			timeBaseToStart *= HOURS_IN_1_DAY;
			timeBaseToEnd *= HOURS_IN_1_DAY;
		}
		else {   // if (timeGranularity == DAY_GRANULARITY) {
			// timeBaseToStart *= 1;
			// timeBaseToEnd *= 1;
		}

		ipos = currentPosition;
		for (i=currentPosition; i<tss->numberValues; i++) {
			time = tss->times[i] - timeBaseToStart;
			if (time > timeBaseToEnd) {   //  (If time == timeBaseToEnd, that's the end of the block)
				break;
			}
			ipos++;
		}
		if (ipos == tss->numberValues) {
			//  We hit the end of the data before getting to the next block
		}
		numberToStore = ipos - currentPosition;
		if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, "Number to store in this block (record):  ", numberToStore);
		}
		// 
    // If there are no values to store, and the block is not read, then delete the block
		if (numberToStore == 0 && boolReadBlock == 0 && storageFlag == 1) {
			// we need to delete this block/record
			zdelete(ifltab,path);
		}
		else if (numberToStore > 0) {
			itimes = (int *)calloc((size_t)numberToStore, (size_t)4);
			if (!itimes) {
				return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID,
					zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, numberToStore, 0,
					zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for itimes");
			}

			//  Now copy and convert times to seconds, if needed
			icount = 0;
			for (i = currentPosition; i < ipos; i++) {
				//  tss->times[i] and timeBaseToStart are in same time units (e.g., hours)
				//  itimes[icount++] can only be seconds or minutes (usual)
				if (blockSize == 5) {
					ltime = (long long)(tss->times[i] - timeBaseToStart) * (long long)timeGranularity;
					itimes[icount++] = (int)(ltime / SECS_IN_1_MINUTE);	 //  Convert seconds to minutes
				}
				else {
					itimes[icount++] = (tss->times[i] - timeBaseToStart) * timeGranularity;
				}
			}

			if (endJulian >= julianNextBlockDate) {
				boolToEndOfBlock = 1;
			}
			else {
				boolToEndOfBlock = 0;
			}
			if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
				if (blockSize == 5) {
					julian = (itimes[0] / MINS_IN_1_DAY);
					seconds = (itimes[0] - (julian * MINS_IN_1_DAY)) * SECS_IN_1_MINUTE;
					if (seconds == 0) {
						seconds = SECS_IN_1_DAY;
						julian--;
					}
					zmessageDebugInt(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, "First value raw time:  ", itimes[0]);
					zmessageDebugInt(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, "First value Julian offset from block:  ", julian);
					zmessageDebugInt(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, "julianBlockDate:  ", julianBlockDate);
					julian += julianBlockDate;
					ztsDateMessage(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, "First value date and time for block:  ", julian, seconds);
					julian = (itimes[icount - 1] / MINS_IN_1_DAY);
					seconds = (itimes[icount - 1] - (julian * MINS_IN_1_DAY)) * SECS_IN_1_MINUTE;
					if (seconds == 0) {
						seconds = SECS_IN_1_DAY;
						julian--;
					}
					julian += julianBlockDate;
					ztsDateMessage(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, "Last  value date and time for block:  ", julian, seconds);
					zmessageDebugInt(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, "Time granularity in seconds: ", timeGranularity);
					zmessageDebugInt(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, "timeBaseToStart: ", timeBaseToStart);
					zmessageDebugInt(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, "First time (itimes[0]): ", itimes[0]);
				}
				else {
					ztsTimeMessage(ifltab, "Block first value date and time:  ", itimes[0], 1, julianBlockDate);
					ztsTimeMessage(ifltab, "Block last  value date and time:  ", itimes[icount - 1], 1, julianBlockDate);
				}
			}
			//  Copy the data to prepare the block for writing
			ipos = currentPosition * valueSize;
			jpos = currentPosition * tss->qualityElementSize;
			kpos = currentPosition * tss->inoteElementSize;
			npos = tss->cnotesLengthTotal - lengthCNotesRemaining;

			status = ztsIrregStoreBlock(ifltab, tss, path,
				buffer, bufferControl,
				numberToStore, itimes,
				&values[ipos], valueSize, valueElementSize,
				&tss->quality[jpos], tss->qualityElementSize,
				&tss->inotes[kpos], tss->inoteElementSize,
				&tss->cnotes[npos], lengthCNotesRemaining, &lengthCNotesRemaining,
				profileDepths, profileDepthsSize, profileDepthsNumber,
				internalHeader,
				tss->userHeader, tss->userHeaderNumber,
				julianBlockDate, julianNextBlockDate, blockSize,
				boolFromStartOfBlock, boolToEndOfBlock,
				storageFlag, boolReadBlock, dataType);

			if (itimes) {
				free(itimes);
				itimes = 0;
			}

			if (zisError(status)) {
				zlockActive(ifltab, LOCKING_LEVEL_HIGH, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_ztsStoreIrreg_ID);;
			}
		}

		//  Increment block start date
		//  Update the position in the data array for the next write
		currentPosition += numberToStore;
		julianBlockDate = julianNextBlockDate;
		boolFromStartOfBlock = 1;

	}
	//-----------------------------------------------------

	zlockActive(ifltab, LOCKING_LEVEL_HIGH, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);

	if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID, "Exit, Pathname: ", tss->pathname);
	}

	return STATUS_OKAY;
}

