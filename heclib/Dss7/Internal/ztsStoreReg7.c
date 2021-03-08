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
*  Function:	ztsStoreReg7
*
*  Use:			Private (use ztsStore for public access)
*
*  Description:	Write a regular-interval time series data set to a version 7 DSS file.
*
*  Declaration: int ztsStoreReg7(long long *ifltab, zStructTimeSeries *tss, int storageFlag);
*
*  Parameters:	long long *ifltab
*					The file table array, similar to a handle number
*
*				zStructTimeSeries *tss
*					The time series struct that contains all data and information to store.
*					See ztsStore for definition of zStructTimeSeries
*
*				int storageFlag
*					A flag indicating how to handle existing data on disk.  For regular interval data:
*						storageFlag = 0  Always replace data.
*						storageFlag = 1  Only replace missing data.
*						storageFlag = 2  Write regardless, even if all missing data (write a missing record)
*						storageFlag = 3  If a record is all missing, do not write it
*							and delete it from disk if it exists.
*						storageFlag = 4  Do not allow a missing input data to
*							replace a valid data piece.
*
*	Returns:	int status
*					STATUS_OKAY
*					error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*	Remarks:	ztsStore() is the public function.  Do not call this (ztsStoreReg7), as it is
*					internal and arguments are subject to change.
*				This function blocks data sets into the standard single record conventions, and
*				merges/stores data in DSS version 7.
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int ztsStoreReg7(long long *ifltab, zStructTimeSeries *tss, int storageFlag)
{
	int status;
	int *values;
	int valueSize;
	int *profileDepths;
	int profileDepthsSize;
	int julianBlockDate;
	int julianNextBlockDate;
	int currentPosition;
	int blockStartPosition;

	int numberInBlock;
	int ipos;
	int jpos;
	int kpos;
	int npos;
	int lengthCNotes;
	int cnotesSize;

	int zero = 0;
	int i;
	int numberToStore;
	int numberLeft;
	int dataType;

	int blockPositionRelativeToStart;

	int internalHeader[INT_HEAD_SIZE];
	int internalHeaderArraySize  = INT_HEAD_SIZE;
	int valueElementSize;
	int profileDepthsNumber;
	int boolReadBlock;

	char messageString[100];
	char path[MAX_PATHNAME_LENGTH];
	char blockDate[20];



	//  Check for correct DSS Version
	if (zgetVersion(ifltab) != 7) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStoreReg_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
								zgetVersion((void *)ifltab), 0, zdssErrorSeverity.WARNING, tss->pathname, "");
	}


	if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessage(ifltab, " ");
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsStoreReg_ID, "Handle: ", zhandle(ifltab));
		zmessageDebug(ifltab, DSS_FUNCTION_ztsStoreReg_ID, "Pathname: ", tss->pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsStoreReg_ID, "Storage Flag: ", storageFlag);
	}
	//  Cannot store both character and integer notes (only one or the other - occupy same space)
	if ((tss->cnotesLengthTotal > 0) && (tss->inoteElementSize > 0)) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStoreReg_ID, zdssErrorCodes.BOTH_NOTE_KINDS_USED,
								0, 0, zdssErrorSeverity.WARNING, tss->pathname, "");
	}

	//  Initialize the internal header array
	for (i=0; i<internalHeaderArraySize ; i++) {
		internalHeader[i] = 0;
	}


	if (!tss->timeWindow) {
		status = ztsProcessTimes(ifltab, tss, 1);
		//  A return of 0 indicates regular interval and is required.  NO_TIME_WINDOW
		if (status != 0) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStoreReg_ID, zdssErrorCodes.NO_TIME_WINDOW,
									0, 0, zdssErrorSeverity.WARNING, tss->pathname, "");
		}
	}

	//  We'll be changing the pathname to have correct date and interval part, so make a copy
	stringCopy(path, sizeof(path), zgetInternalPath(tss), strlen(zgetInternalPath(tss)));

	////////////////////////////////////
	internalHeader[INT_HEAD_timeOffset] = tss->timeWindow->timeOffsetSeconds;
	//  The time granularity of a individual record is always in seconds, unless
	//  the interval is a year (and would overflow an int) and then in minutes
	internalHeader[0] = 0;
	julianBlockDate = tss->timeWindow->startBlockJulian;
	lengthCNotes = 0;
	npos = 0;

	//  Figure out what kind of data we are storing
	//  floats or doubles?
	if (tss->profileDepthsNumber == 0) {
		if (tss->floatValues) {
			//  Floats
			values = (void *)tss->floatValues;
			valueSize = 1;
			valueElementSize = 1;
			if (tss->boolPattern) {
				dataType = DATA_TYPE_RTS_PATTERN;
			}
			else {
				dataType = DATA_TYPE_RTS;
			}
		}
		else if (tss->doubleValues) {
			//  Doubles
			values = (void *)tss->doubleValues;
			valueSize = 2;
			valueElementSize = 2;
			if (tss->boolPattern) {
				dataType = DATA_TYPE_RTD_PATTERN;
			}
			else {
				dataType = DATA_TYPE_RTD;
			}
		}
		else {
			//  No data!
			status = zerrorProcessing(ifltab, DSS_FUNCTION_ztsStoreReg_ID, zdssErrorCodes.NO_DATA_GIVEN,
								    0, 0, zdssErrorSeverity.WARNING, tss->pathname, "");
			zquery("empty", messageString, 2, &i);
			if (!i) return status;
		}
		profileDepths = (int *)0;   //
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
			values = (void *)tss->floatProfileValues;
			profileDepths = (void *)tss->floatProfileDepths;
			dataType = DATA_TYPE_RTS_PROFILE;
		}
		else if (tss->doubleProfileValues) {
			//  Doubles
			valueElementSize = 2;
			values = (void *)tss->doubleProfileValues;
			profileDepths = (void *)tss->doubleProfileDepths;
			dataType = DATA_TYPE_RTD_PROFILE;
		}
		else {
			//  No data!
			return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStoreReg_ID, zdssErrorCodes.NO_DATA_GIVEN,
								    0, 0, zdssErrorSeverity.WARNING, tss->pathname, "");
		}
		valueSize = valueElementSize * tss->profileDepthsNumber;
		profileDepthsSize = valueElementSize * tss->profileDepthsNumber;
	}
	internalHeader[INT_HEAD_valueSize] = valueSize;
	internalHeader[INT_HEAD_valueElementSize] = valueElementSize;
	if (tss->dataType == 0) tss->dataType = dataType;


	//  Lock the file at this point, so we don't have to flush or
	//  unlock or worry about other users until we are finished.
	//  This way, we can carry buffers around and not have to
	//  re-read them
	status = zlockActive(ifltab, LOCKING_LEVEL_HIGH, LOCKING_LOCK_ON, LOCKING_FLUSH_ON);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_ztsStoreReg_ID);
	}


	//  Is this a pattern (D part of "TS-PATTERN")
	if (tss->boolPattern) {
		//  A time pattern data set
		status = ztsWriteBlock (ifltab, tss, tss->pathname,
								tss->times, 0, tss->numberValues,
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
	while (julianBlockDate <= tss->timeWindow->endBlockJulian) {

		//  Get the date for this block
		julianToDate(julianBlockDate, 4, blockDate, sizeof(blockDate));
		if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %s,  julian: %d",
				blockDate, julianBlockDate);
			zmessageDebug(ifltab, DSS_FUNCTION_ztsStoreReg_ID, "Block date: ", messageString);
		}

		//  Generate the pathname for this individual record
		zpathnameSetPart (path, sizeof(path), blockDate, 4);

		//  Get the date of the next block
		julianNextBlockDate = ztsIncrementBlock(julianBlockDate, tss->timeWindow->blockSize);

		//  Calculate the number of values in this block
		numberInBlock = numberPeriods(tss->timeWindow->intervalSeconds, julianBlockDate-1, 86400, julianNextBlockDate-1, 86400);

		//  If the block we are writing to exists, determine if we have to read it.
		//  If the writing block dates encompass the entire block, we won't
		//  read it; we'll just write over the entire block.  (That is, if it exists)
		boolReadBlock = 1;
		//  FIX ME !!!!!!!!!!!!!!!!!!!
		//  How about times?
		if (((tss->timeWindow->startJulian < julianBlockDate) && (tss->timeWindow->endJulian > julianNextBlockDate)) &&
			((storageFlag == 0) || (storageFlag == 2))) {
			boolReadBlock = 0;
			if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
				zmessageDebug(ifltab, DSS_FUNCTION_ztsStoreReg_ID, "Bypass Read because time window spans entire block", "");
			}
		}


		//  Get the position (number of periods) from the start of the block to the current position in the block.
		//  If this is the start of a write, the value will be from 1 to the size of the block
		//  If we are in the middle of a write, this will be 1.

		//  First get the block position relative to the original start time
		blockPositionRelativeToStart = numberPeriods(tss->timeWindow->intervalSeconds, julianBlockDate-1, 86400, tss->timeWindow->startJulian, tss->timeWindow->startTimeSeconds) -1;


		//  Now determine the position relative to the current position in the data array
		//  (what we have already written)
		blockStartPosition = blockPositionRelativeToStart + currentPosition;
		if (blockStartPosition < 0) {
			//  Error (should be detected way earlier than this!)
			zlockActive(ifltab, LOCKING_LEVEL_HIGH, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
			return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStoreReg_ID, zdssErrorCodes.INVALID_DATE_TIME,
								    blockStartPosition, 0, zdssErrorSeverity.WARNING, tss->pathname, "");
		}

		//  Determine how many values we will write to this block
		//  Get the number if we wrote all (the remainder) of the block
		numberToStore = numberInBlock - blockStartPosition;
		//  Get the number left to write
		numberLeft = tss->numberValues - currentPosition;
		//  Use the least of the the number left in the block or number left to write
		if (numberLeft < numberToStore) {
			numberToStore = numberLeft;
		}

		//  Copy the data to prepare the block for writing
		ipos = currentPosition * valueSize;
		jpos = currentPosition * tss->qualityElementSize;
		kpos = currentPosition * tss->inoteElementSize;
		npos += lengthCNotes;
		cnotesSize = tss->cnotesLengthTotal - npos;

		if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d;  julianBlockDate: %d;  tss->timeWindow->startJulian: %d",
				blockPositionRelativeToStart, julianBlockDate, tss->timeWindow->startJulian);
			zmessageDebug(ifltab, DSS_FUNCTION_ztsStoreReg_ID, "blockPositionRelativeToStart: ", messageString);
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d;  values position: %d;  numberToStore: %d",
				currentPosition, ipos, numberToStore);
			zmessageDebug(ifltab, DSS_FUNCTION_ztsStoreReg_ID, "Current position: ", messageString);
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d;  blockStartPosition: %d;  numberValues: %d;  numberLeft: %d",
				numberInBlock, blockStartPosition, tss->numberValues, numberLeft);
			zmessageDebug(ifltab, DSS_FUNCTION_ztsStoreReg_ID, "numberInBlock: ", messageString);
		}

		assert(ipos >= 0);
		assert(numberToStore >= 0);

		status = ztsRegStoreBlock(ifltab, tss, path, numberToStore,
								&values[ipos], valueSize, valueElementSize,
								&tss->quality[jpos], tss->qualityElementSize,
								&tss->inotes[kpos], tss->inoteElementSize,
								&tss->cnotes[npos], cnotesSize, &lengthCNotes,
								profileDepths, profileDepthsSize, profileDepthsNumber,
								internalHeader, 
								tss->userHeader, tss->userHeaderNumber,
								currentPosition, numberInBlock,
								tss->timeWindow->startJulian, tss->timeWindow->startTimeSeconds,
								tss->timeWindow->intervalSeconds, julianBlockDate, blockStartPosition,
								boolReadBlock, dataType, storageFlag);

		if (zisError(status)) {
			zlockActive(ifltab, LOCKING_LEVEL_HIGH, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_ztsStoreReg_ID);;
		}

		//  Increment block start date
		//  Update the position in the data array for the next write
		currentPosition += numberToStore;
		julianBlockDate = julianNextBlockDate;

	}
	//-----------------------------------------------------

	zlockActive(ifltab, LOCKING_LEVEL_HIGH, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);


	if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztsStoreReg_ID, "Exit, Pathname: ", tss->pathname);
	}

	return STATUS_OKAY;
}

