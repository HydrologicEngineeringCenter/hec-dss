#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "zdssMessages.h"
#include "zdssKeys.h"
#include "zdssVals.h"
#include "hecdssInternal.h"
#include "heclib.h"
#include "zStructTimeSeries.h"
#include "zerrorCodes.h"

/**
*  Function:	ztsRetrieveReg7
*
*  Use:			Private (use ztsRetrieve for public access)
*
*  Description:	Read a regular-interval time series data set from a version 7 DSS file.
*
*  Declaration: int ztsRetrieveReg7(long long *ifltab, zStructTimeSeries *tss,
*									int retrieveFlag, int retrieveDoublesFlag, int boolRetrieveQualityNotes);
*
*  Parameters:	long long *ifltab
*					The file table array, similar to a handle number
*
*				zStructTimeSeries *tss
*					The time series struct that will contain the data read.
*					See ztsRetrieve for definition of zStructTimeSeries
*
*				int retrieveFlag
*					Not used in this call - function preformed in public call (dummy here, but kept for same signature.)
*					In original call:
*					0:  Adhere to time window provided
*					-1: Trim data.  Remove missing values at the beginning and end of data set (not inside)
*
*				int retrieveDoublesFlag
*					A flag indicating if floats or doubles should be returned.  This is independent of
*					what is actually stored on disk!  Values will be converted to the requested type.
*						0:  Return data as stored.  If missing, will return as doubles.
*						1:  Return floats
*						2:  Return doubles
*
*				int boolRetrieveQualityNotes
*					A flag indicating if you want quality and notes read also, if they exist.
*					If you are not going to use them, it is much more efficient not to read them.
*					(If they don't exist, they will not be returned, regardless.)
*						0:  Do not read quality or note arrays
*						1:  Read and return any quality or notes
*
*
*	Returns:	int status
*					STATUS_OKAY
*					error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*	Remarks:	ztsRetrieve() is the public function.
*					Do not call ztsRetrieveReg7, as it is internal and arguments are subject to change.
*					Start and end dates and times are optional.  If they are blank,
*					then the full data set will be returned, unless a date range is
*					given in the pathname ("20July2002 - 13May2010")
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int ztsRetrieveReg7(long long *ifltab, zStructTimeSeries *tss,
					int retrieveFlag, int retrieveDoublesFlag, int boolRetrieveQualityNotes)
{




	//  FIX ME - if startDate is blank, read entire record or time window specified
	// by D part  (e.g., 12Jun2003 - 18Aug 2009)
	//  number then identifies the values array dimension

	int status = 0;
	int intervalSeconds;
	int startJulian, startSeconds;
	int blockSize;
	int endBlockJulian;
	int julianBlockDate;
	int julianNextBlockDate;
	int currentPosition;
	int blockStartPosition;
	int valuesArraySize;
	int profileDepthsNumberRequested;
	int boolProfileRecord;
	int numberInBlock;
	int numberToReadInBlock;
	int positionValues;
	int positionQuality;
	int positionNotes;
	int positionCnotes;
	int sizeCNotesRemaning;

	int numberToRead;
	int numberLeft;
	int boolFound;
	int blockPositionRelativeToStart;
	int maybeIncrementTimes = 0;
	int i;
	int one = 1;

	int internalHeader[INT_HEAD_SIZE];
	int internalHeaderArraySize  = INT_HEAD_SIZE;
	int internalHeaderNumber;
	int lengthCNotes;
	int valueSize;
	int qualityElementSize;
	int inoteElementSize;
	int valuesLengthRequested;
	int dataType;

	int valueElementSizeRead;
	int qualityElementSizeRead;
	int sizeEachNoteRead;
	int totalNumberCnotesRead;
	char *cnotes;

	int multipier;
	int *profileDepths;
	int profileDepthsNumberRead;
	int profileDepthsArraySize;
	int number;
	long long *info;
	long long *fileHeader;

	zStructRecordSize timeSeriesRecordSizes;

	int foundOne=0;

	char path[MAX_PATHNAME_LENGTH];
	char blockDate[20];
	long long nada=0;
	int *values;

	int *buffer = 0;
	long long bufferControl[4];
	bufferControl[BUFF_SIZE] = 0;
	bufferControl[BUFF_STAT] = 0;
	bufferControl[BUFF_ADDRESS] = 0;
	bufferControl[BUFF_INTS_USED] = 0;

	const int WEEKLY_INTERVAL_SECONDS = 604800;


	//  Check for correct DSS Version
	if (zgetVersion(ifltab) != 7) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieveReg_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
								zgetVersion((void *)ifltab), 0, zdssErrorSeverity.WARNING, tss->pathname, "");
	}

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessage(ifltab, " ");
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsRetrieveReg_ID, "Handle: ", zhandle(ifltab));
		zmessageDebug(ifltab, DSS_FUNCTION_ztsRetrieveReg_ID, "Pathname: ", tss->pathname);
	}

	//  If the time window has not been processed (e.g., char dates to julian), do so now
	if (!tss->timeWindow) {
		status = ztsProcessTimes(ifltab, tss, 0);
		if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_USER_DIAG)) {
			ztsMessTimeWindow(ifltab, DSS_FUNCTION_ztsRetrieveReg_ID, tss);
		}
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_ztsRetrieveReg_ID);
		}
	}

	//  Initialize dates and arrays.  These will be modified, so we cannot use originals
	stringCopy(path, sizeof(path), zgetInternalPath(tss), strlen(zgetInternalPath(tss)));
	julianBlockDate = tss->timeWindow->startBlockJulian;
	endBlockJulian = tss->timeWindow->endBlockJulian;
	startJulian = tss->timeWindow->startJulian;
	startSeconds = tss->timeWindow->startTimeSeconds;
	intervalSeconds = tss->timeWindow->intervalSeconds;
	blockSize = tss->timeWindow->blockSize;
	numberToRead = tss->timeWindow->numberValues;
	profileDepths = 0;
	profileDepthsArraySize = 0;
	values = 0;
	cnotes = 0;
	valueSize = 1;

	if ((tss->julianBaseDate == 0) && (tss->timeGranularitySeconds == SECOND_GRANULARITY)) {
		tss->julianBaseDate = tss->timeWindow->startBlockJulian;
	}


	//  bufferControl[BUFF_SIZE] is (max) int*4 size
	//  bufferControl[BUFF_STAT] is write status;  0 - unused, 1 - not dirty (read only), 2 - dirty (needs to write)
	//  bufferControl[BUFF_ADDRESS] is file address (int*8)
	//  bufferControl[BUFF_INTS_USED] is current int size
	//  FIX ME - CHECK FOR REASONABLNESS
	for (i=0; i<4; i++) bufferControl[i] = 0;
	/*
	maxBuffer = (int)fileHeader[zdssFileKeys.kmaxRtsSize] *2;
	if ((int)fileHeader[zdssFileKeys.kmaxRtdSize] > maxBuffer) maxBuffer = (int)fileHeader[zdssFileKeys.kmaxRtdSize];
	if (maxBuffer > 0) {
		buffer = (int *)calloc((size_t)maxBuffer, WORD_SIZE);
		if (buffer) {
			bufferControl[BUFF_SIZE] = maxBuffer;
			bufferControl[BUFF_STAT] = 0;
			bufferControl[BUFF_ADDRESS] = 0;
			bufferControl[BUFF_INTS_USED] = 0;
		}
	}
	*/
	//  Allocate memory on first read.
	//  Since this is regular interval data, we'll have a good chance of
	//  figuring out total space needed from first record.
	//  If it's more (rare), we'll allocate more space when we get there.



	//  Main Loop
	//  Generate a new pathname for each block, changing the D (D part) each time
	//-----------------------------------------------------
	totalNumberCnotesRead = 0;
	currentPosition = 0;
	dataType = 0;
	while (julianBlockDate <= endBlockJulian) {

		//  Get the date for this block
		julianToDate(julianBlockDate, 4, blockDate, sizeof(blockDate));

		//  Generate pathname with this date
		zpathnameSetPart (path, sizeof(path), blockDate, 4);

		if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_USER_DIAG)) {
			ztsDateMessage(ifltab, DSS_FUNCTION_ztsRetrieveReg_ID, "Block date:  ", julianBlockDate, 0);
			zmessageDebug(ifltab, DSS_FUNCTION_ztsRetrieveReg_ID, "Pathname to read: ", path);
		}

		//  Get the date of the next block
		julianNextBlockDate = ztsIncrementBlock(julianBlockDate, blockSize);

		//  Calculate the number of values in this block
		numberInBlock = numberPeriods(intervalSeconds, julianBlockDate-1, 86400, julianNextBlockDate-1, 86400);


		//  Get the position (number of periods) from the start of the block to the current position in the block.
		//  If this is the start of a read, the value will be from 1 to the size of the block
		//  If we are in the middle of a read, this will be 1.

		//  First get the block position relative to the original start time
		blockPositionRelativeToStart = numberPeriods(intervalSeconds, julianBlockDate-1, 86400, startJulian, startSeconds) -1;

		//  Now determine the position relative to the current position in the data array
		//  (what we have already read)
		blockStartPosition = blockPositionRelativeToStart + currentPosition;
		if (blockStartPosition < 0) {
			if (tss->timeWindow->intervalSeconds == WEEKLY_INTERVAL_SECONDS && blockStartPosition == -1) {
				blockStartPosition = 0;
				maybeIncrementTimes = 1;
			}
			else {
				//  Error - should not occur
				return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieveReg_ID, zdssErrorCodes.INVALID_DATE_TIME,
					blockStartPosition, 0, zdssErrorSeverity.WARNING, tss->pathname, "");
			}
		}

		//  Determine how many values we will read from this block
		//  Get the number if we read all (the remainder) of the block
		numberToReadInBlock = numberInBlock - blockStartPosition;
		//  Get the number left to write
		numberLeft = numberToRead - currentPosition;
		//  Use the least of the the number left in the block or number left to read
		if (numberLeft < numberToReadInBlock) {
			numberToReadInBlock = numberLeft;
		}

		if (currentPosition == 0) {
			timeSeriesRecordSizes.pathname = path;
			status = ztsGetSizesInternal(ifltab, tss->timeWindow, &timeSeriesRecordSizes);
			if (zisError(status)) {
				if (buffer) free(buffer);
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_ztsRetrieveReg_ID);
			}
			if (status > 0) {
				boolFound = 1;
			}
			else {
				boolFound = 0;
			}

			if (!foundOne) {
				if ((retrieveDoublesFlag == 0) && boolFound) {
					retrieveDoublesFlag = timeSeriesRecordSizes.tsValueElementSize;
				}
			}

			//  Allocate space to hold all data, if this is the first time into loop
			//  Be sure that we are on 8 byte boundaries for allocating space
			if (isOdd(timeSeriesRecordSizes.values1Number)) timeSeriesRecordSizes.values1Number++;
			if (isOdd(timeSeriesRecordSizes.values2Number)) timeSeriesRecordSizes.values2Number++;
			if (isOdd(timeSeriesRecordSizes.values3Number)) timeSeriesRecordSizes.values3Number++;
			if (isOdd(timeSeriesRecordSizes.internalHeaderNumber)) timeSeriesRecordSizes.internalHeaderNumber++;
			if (isOdd(timeSeriesRecordSizes.header2Number)) timeSeriesRecordSizes.header2Number++;
			if (isOdd(timeSeriesRecordSizes.userHeaderNumber)) timeSeriesRecordSizes.userHeaderNumber++;
		
			dataType = timeSeriesRecordSizes.dataType;
			tss->dataType = dataType;			
			//  Profiles?
			if ((timeSeriesRecordSizes.dataType == DATA_TYPE_RTS_PROFILE) ||
				(timeSeriesRecordSizes.dataType == DATA_TYPE_RTD_PROFILE)) {
				boolProfileRecord = 1;
				multipier = timeSeriesRecordSizes.tsProfileDepthsNumber;
				profileDepthsNumberRequested = timeSeriesRecordSizes.tsProfileDepthsNumber;
				//  Profiles have 2 arrays (depths and values).  Allocate independent (depths) now.
				if (timeSeriesRecordSizes.dataType == DATA_TYPE_RTS_PROFILE) {
					profileDepths = (int *)calloc((size_t)timeSeriesRecordSizes.tsProfileDepthsNumber, FLOAT_SIZE);
					profileDepthsArraySize = (int)timeSeriesRecordSizes.tsProfileDepthsNumber;
				}
				else {
					profileDepths = (int *)calloc((size_t)timeSeriesRecordSizes.tsProfileDepthsNumber, DOUBLE_SIZE);
					profileDepthsArraySize = timeSeriesRecordSizes.tsProfileDepthsNumber * 2;
				}
				if (!profileDepths) {
					return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieveReg_ID,
											zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, profileDepthsArraySize, 0,
											zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating profileDepths array");
				}
			}
			else {
				boolProfileRecord = 0;
				profileDepthsNumberRequested = 0;
				multipier = 1;
			}

			//  Now allocate the main values array
			if (retrieveDoublesFlag != 1) {
				valuesLengthRequested = 2;
				valueSize = 2 * multipier;
				valuesArraySize = numberToRead * valueSize;
				values = (int *)calloc((size_t)valuesArraySize, FLOAT_SIZE);
				if (!values) {
					if (buffer) free(buffer);
					return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieveReg_ID,
											zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, valuesArraySize, 0,
											zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating values array");
				}
				if (boolProfileRecord) {
					tss->doubleProfileValues = (double *)values;
					tss->allocated[zSTRUCT_TS_profileDoubleValues] = 1;
				}
				else {
					tss->doubleValues = (double *)values;
					tss->allocated[zSTRUCT_TS_doubleValues] = 1;
				}
			}
			else {
				valuesLengthRequested = 1;
				valueSize = 1 * multipier;
				valuesArraySize = numberToRead * valueSize;
				values = (int *)calloc((size_t)valuesArraySize, FLOAT_SIZE);
				if (!values) {
					if (buffer) free(buffer);
					return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieveReg_ID,
											zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, valuesArraySize, 0,
											zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating values array");
				}
				if (boolProfileRecord) {
					tss->floatProfileValues = (float *)values;
					tss->allocated[zSTRUCT_TS_profileFloatValues] = 1;
				}
				else {
					tss->floatValues = (float *)values;
					tss->allocated[zSTRUCT_TS_floatValues] = 1;
				}
			}

			//  Has the user asked to have us retrieve quality and/or notes?
			if (boolRetrieveQualityNotes) {
				qualityElementSize = timeSeriesRecordSizes.tsQualityElementSize;
				inoteElementSize = timeSeriesRecordSizes.tsInotesElementSize;

				//  Yes, allocate arrays
				if (qualityElementSize) {
					tss->qualityArraySize = numberToRead * qualityElementSize;
					tss->quality = (int *)calloc((size_t)tss->qualityArraySize, WORD_SIZE);
					if (!tss->quality) {
						if (buffer) free(buffer);
						return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieveReg_ID,
												zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, tss->qualityArraySize, 0,
												zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating quality array");
					}
					tss->allocated[zSTRUCT_TS_quality] = 1;
					tss->qualityElementSize = qualityElementSize;
				}
				if (inoteElementSize) {
					tss->inotesArraySize = numberToRead * inoteElementSize;
					tss->inotes = (int *)calloc((size_t)tss->inotesArraySize, WORD_SIZE);
					if (!tss->inotes) {
						if (buffer) free(buffer);
						return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieveReg_ID,
												zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, tss->inotesArraySize, 0,
												zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating inotes array");
					}
					tss->allocated[zSTRUCT_TS_inotes] = 1;
					tss->inoteElementSize = inoteElementSize;
				}
				if (timeSeriesRecordSizes.tsCnotesLength) {
					tss->cnotes = (char *)calloc((size_t)timeSeriesRecordSizes.tsCnotesLength, CHAR_SIZE);
					if (!tss->cnotes) {
						if (buffer) free(buffer);
						return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieveReg_ID,
							zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, timeSeriesRecordSizes.tsCnotesLength, 0,
							zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating cnotes array");
					}
					tss->allocated[zSTRUCT_TS_cnotes] = 1;
					tss->cnotesSize = timeSeriesRecordSizes.tsCnotesLength;
				}
			}
			else {
				lengthCNotes = 0;
				qualityElementSize = 0;
				inoteElementSize = 0;
			}
		}
		else {
			if (dataType != timeSeriesRecordSizes.dataType) {
				//  Could be an issue - just check for a missing record
				if ((dataType == 0) && !foundOne) {

					//  If not profile, not an issue
					if ((timeSeriesRecordSizes.dataType == DATA_TYPE_RTS_PROFILE) ||
						(timeSeriesRecordSizes.dataType == DATA_TYPE_RTD_PROFILE)) {
						if (tss->allocated[zSTRUCT_TS_floatValues]) {
							free (tss->floatValues);
							tss->floatValues = 0;
							tss->allocated[zSTRUCT_TS_floatValues] = 0;
							values = 0;
						}
						if (tss->allocated[zSTRUCT_TS_doubleValues]) {
							free (tss->doubleValues);
							tss->doubleValues = 0;
							tss->allocated[zSTRUCT_TS_doubleValues] = 0;
							values = 0;
						}
						boolProfileRecord = 1;
						multipier = timeSeriesRecordSizes.tsProfileDepthsNumber;
						profileDepthsNumberRequested = timeSeriesRecordSizes.tsProfileDepthsNumber;
						if (profileDepths) free(profileDepths);
						if (timeSeriesRecordSizes.dataType == DATA_TYPE_RTS_PROFILE) {
							profileDepths = (int *)calloc((size_t)timeSeriesRecordSizes.tsProfileDepthsNumber, FLOAT_SIZE);
							profileDepthsArraySize = (int)timeSeriesRecordSizes.tsProfileDepthsNumber;
						}
						else {
							//  Headers are int by default
							profileDepths = (int *)calloc((size_t)timeSeriesRecordSizes.tsProfileDepthsNumber, DOUBLE_SIZE);
							profileDepthsArraySize = timeSeriesRecordSizes.tsProfileDepthsNumber * 2;
						}
						if (!profileDepths) {
							return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieveReg_ID,
													zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, profileDepthsArraySize, 0,
													zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating profileDepths array");
						}
						//  Free previous malloc and re allocate correct amount of space
						if (retrieveDoublesFlag != 1) {
							valueSize = timeSeriesRecordSizes.tsProfileDepthsNumber * 2;
							valuesArraySize = numberToRead * valueSize;
							if (values) free(values);
							tss->doubleValues = 0;
							values = (int *)calloc((size_t)valuesArraySize, FLOAT_SIZE);
							if (!values) {
								if (buffer) free(buffer);
								return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieveReg_ID,
														zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, valuesArraySize, 0,
														zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating values array");
							}
							tss->doubleProfileValues = (double *)values;
							tss->allocated[zSTRUCT_TS_profileDoubleValues] = 1;
							number = timeSeriesRecordSizes.tsProfileDepthsNumber  * currentPosition;
							zsetMissingDoubleArray((void *)values, number);
						}
						else {
							valueSize = timeSeriesRecordSizes.tsProfileDepthsNumber;
							valuesArraySize = numberToRead * valueSize;
							if (values) free(values);
							values = (int *)calloc((size_t)valuesArraySize, FLOAT_SIZE);
							if (!values) {
								if (buffer) free(buffer);
								return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieveReg_ID,
														zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, valuesArraySize, 0,
														zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating values array");
							}
							tss->floatProfileValues = (float *)values;
							tss->allocated[zSTRUCT_TS_profileFloatValues] = 1;
							number = timeSeriesRecordSizes.tsProfileDepthsNumber  * currentPosition;
							zsetMissingFloatArray((void *)values, number);
						}
					}
					//  else {}  No issue - missing sizes determined value
				}
				else {
					//  dataType == 0 is missing; okay
					if (timeSeriesRecordSizes.dataType != 0) {
						//  Different data type - error out!
						return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieveReg_ID,
												zdssErrorCodes.DIFFERENT_RECORD_TYPE, timeSeriesRecordSizes.dataType,
												0, zdssErrorSeverity.WARNING, tss->pathname, "");
					}
				}
				if (timeSeriesRecordSizes.dataType != 0) {
					dataType = timeSeriesRecordSizes.dataType;
					tss->dataType = dataType;
				}
			}			
		}


		//  Copy the data to prepare the block for reading
		positionValues = currentPosition * (valueSize);
		positionQuality = currentPosition * (qualityElementSize);
		positionNotes = currentPosition * (inoteElementSize);
		positionCnotes = totalNumberCnotesRead;
		sizeCNotesRemaning = tss->cnotesSize - positionCnotes;
		if (timeSeriesRecordSizes.userHeaderNumber > 0) {
			if (tss->userHeader) {
				if (tss->allocated[zSTRUCT_userHeader]) {
					free(tss->userHeader);
					tss->userHeader = 0;
					tss->userHeaderNumber = 0;
					tss->userHeaderSize = 0;
					tss->allocated[zSTRUCT_userHeader] = 0;
				}
			}
			if (!tss->userHeader) {
				tss->userHeaderSize = timeSeriesRecordSizes.userHeaderNumber;
				tss->userHeader = (int *)calloc(tss->userHeaderSize, 4);
				if (!tss->userHeader) {
					return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieveReg_ID,
											zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, (tss->userHeaderSize * 4), 0,
											zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating user header");
				}
				tss->allocated[zSTRUCT_userHeader] = 1;
			}
		}

		//  Read the single record (block) and disaggregate, etc.
		status = ztsRegReadBlock(ifltab, path, boolFound,
			buffer, bufferControl,
			&values[positionValues], (valuesArraySize - positionValues), valuesLengthRequested, &valueElementSizeRead,
			&tss->quality[positionQuality], (tss->qualityArraySize - positionQuality), qualityElementSize, &qualityElementSizeRead,
			&tss->inotes[positionNotes], (tss->inotesArraySize - positionNotes), inoteElementSize, &sizeEachNoteRead,
			&tss->cnotes[positionCnotes], sizeCNotesRemaning, &lengthCNotes,
			profileDepths, profileDepthsNumberRequested,
			profileDepthsArraySize, &profileDepthsNumberRead,
			internalHeader, internalHeaderArraySize , &internalHeaderNumber,
			tss->userHeader, tss->userHeaderSize, &tss->userHeaderNumber,
			numberToReadInBlock, blockStartPosition);

		if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_ztsRetrieveReg_ID, "After reading TS record, status: ", status);
		}

		if (zisError(status)) {
			if (buffer) free(buffer);
			if (profileDepths) free(profileDepths);
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_ztsRetrieveReg_ID);
		}



		if (status == STATUS_RECORD_FOUND) {
			if (!foundOne) {
				if (boolProfileRecord) {
					//  Profile data - do we need to convert?
					if (profileDepthsNumberRead < profileDepthsArraySize) {
						return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieveReg_ID,
													zdssErrorCodes.DIFFERENT_PROFILE_NUMBER, profileDepthsNumberRead,
													(long long)profileDepthsArraySize,
													zdssErrorSeverity.WARNING, tss->pathname, "");
					}
					tss->profileDepthsNumber = internalHeader[INT_HEAD_profileDepthsNumber];
					if (retrieveDoublesFlag != 1) {
						if (timeSeriesRecordSizes.dataType == DATA_TYPE_RTS_PROFILE) {
							tss->doubleProfileDepths = (double *)calloc((size_t)profileDepthsNumberRead*2, FLOAT_SIZE);
							if (!tss->doubleProfileDepths) {
								return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieveReg_ID,
														zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, profileDepthsNumberRead, 0,
														zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating profileDepths array");
							}
							convertDataArray(&profileDepths[0], (void *)tss->doubleProfileDepths, profileDepthsNumberRead, 1, 2);
						}
						else if (timeSeriesRecordSizes.dataType == DATA_TYPE_RTD_PROFILE) {
							tss->doubleProfileDepths = (double *)calloc((size_t)profileDepthsNumberRead, FLOAT_SIZE);
							if (!tss->doubleProfileDepths) {
								return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieveReg_ID,
														zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, profileDepthsNumberRead, 0,
														zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating profileDepths array");
							}
							convertDataArray(&profileDepths[0], (void *)tss->doubleProfileDepths, profileDepthsNumberRead/2, 2, 2);
						}
						tss->allocated[zSTRUCT_TS_profileDoubleDepths] = 1;
					}
					else {
						if (timeSeriesRecordSizes.dataType == DATA_TYPE_RTS_PROFILE) {
							tss->floatProfileDepths = (float *)calloc((size_t)profileDepthsNumberRead, FLOAT_SIZE);
							if (!tss->floatProfileDepths) {
								return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieveReg_ID,
														zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, profileDepthsNumberRead, 0,
														zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating profileDepths array");
							}
							convertDataArray(&profileDepths[0], (void *)tss->floatProfileDepths, profileDepthsNumberRead, 1, 1);
						}
						else if (timeSeriesRecordSizes.dataType == DATA_TYPE_RTD_PROFILE) {
							tss->floatProfileDepths = (float *)calloc((size_t)profileDepthsNumberRead/2, FLOAT_SIZE);
							if (!tss->floatProfileDepths) {
								return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieveReg_ID,
														zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, profileDepthsNumberRead, 0,
														zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating profileDepths array");
							}
							convertDataArray(&profileDepths[0], (void *)tss->floatProfileDepths, profileDepthsNumberRead/2, 2, 1);
						}
						tss->allocated[zSTRUCT_TS_profileFloatDepths] = 1;
					}
					//  If we have one depth set, don't read any more.
					profileDepthsArraySize = 0;
				}

				ztsInternalHeaderUnpack(tss, internalHeader, internalHeaderNumber);

				//  Save info about first record
				tss->dateOfFirstRecFound = julianBlockDate;
				info = (long long *)ifltab[zdssKeys.kinfo];
				tss->lastWrittenTime = info[zdssInfoKeys.kinfoLastWriteTime];
				tss->fileLastWrittenTime = fileHeader[zdssFileKeys.klastWriteTime];
				charLong(&info[zdssInfoKeys.kinfoProgram], tss->programName, 0, zdssVals.numberProgram, 0, 1);
				foundOne = 1;
			}
			totalNumberCnotesRead += lengthCNotes;
		}


		//  Increment block start date
		//  Update the position in the data array for the next write
		currentPosition += numberToReadInBlock;
		julianBlockDate = julianNextBlockDate;


	}
	//-----------------------------------------------------

	if (foundOne) {
		tss->sizeEachValueRead = valueElementSizeRead;
		tss->cnotesLengthTotal = totalNumberCnotesRead;
		tss->numberValues = currentPosition;
		if (boolProfileRecord) {
			tss->numberValues = currentPosition;
		}
		tss->timeIntervalSeconds = tss->timeWindow->intervalSeconds;
		if (maybeIncrementTimes) {
			int firstDataTimeJulian = tss->startJulianDate;
			int sec = tss->startTimeSeconds;
			ztsOffsetAdjustToOffset(tss->timeOffsetSeconds, tss->timeIntervalSeconds, &firstDataTimeJulian, &sec);
			if (firstDataTimeJulian < tss->timeWindow->startBlockJulian) {
				// move the time window forward one interval
				incrementTime(tss->timeIntervalSeconds, 1, tss->startJulianDate, tss->startTimeSeconds, &tss->startJulianDate, &tss->startTimeSeconds);
				incrementTime(tss->timeIntervalSeconds, 1, tss->endJulianDate, tss->endTimeSeconds, &tss->endJulianDate, &tss->endTimeSeconds);
			}
		}
		//  Take care of julian base date, if too large (1,400,000, or MAX_INT / 1440)
		if ((tss->startJulianDate > 1400000) || (tss->endJulianDate > 1400000)) {
			tss->julianBaseDate = tss->startJulianDate;

		}
	}
	
	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_USER_DIAG)) {
		if (foundOne) {
			zmessageDebug(ifltab, DSS_FUNCTION_ztsRetrieveReg_ID, "Data set read: ", tss->pathname);
		}
		else {
			zmessageDebug(ifltab, DSS_FUNCTION_ztsRetrieveReg_ID, "Data set NOT found: ", tss->pathname);
		}
		zmessage(ifltab, " ");  //  Blank line
	}

	if (buffer) free(buffer);
	if (profileDepths) free(profileDepths);

	if (zisError(status)) {
		return status;
	}
	if (foundOne) {
		return STATUS_RECORD_FOUND;
	}
	else {
		return STATUS_RECORD_NOT_FOUND;
	}
}


