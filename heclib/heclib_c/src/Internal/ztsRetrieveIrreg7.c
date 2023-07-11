#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "heclib7.h"
#include "zdssMessages.h"
#include "zdssKeys.h"
#include "zdssVals.h"
#include "hecdssInternal.h"
#include "heclib.h"
#include "zStructTimeSeries.h"
#include "zerrorCodes.h"

/**
*  Function:	ztsRetrieveIrreg7
*
*  Use:			Private (use ztsRetrieve for public access)
*
*  Description:	Read an irregular-interval time series data set from a version 7 DSS file.
*
*  Declaration: int ztsRetrieveIrreg7(long long *ifltab, zStructTimeSeries *tss,
*									  int retrieveFlag, int boolRetrieveDoubles, int boolRetrieveQualityNotes);
*
*  Parameters:	long long *ifltab
*					The file table array, similar to a handle number
*
*				zStructTimeSeries *tss
*					The time series struct that will contain the data read.
*					See ztsRetrieve for definition of zStructTimeSeries
*
*				int retrieveFlag
*					A flag indicating if the value before and/or after the time window should
*					be retrieved also.  This is common when needing an average value, but data is
*					stored as instantaneous
*						0:  Adhere to time window provided
*						1:  Retrieve (one) value previous to start of time window
*						2:  Retrieve (one) value after end of time window
*						3:  Retrieve one value before and one value after time window
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
*					Do not call ztsRetrieveIrreg7, as it is internal and arguments are subject to change.
*					Start and end dates and times are optional.  If they are blank,
*					then the full data set will be returned, unless a date range is
*					given in the pathname {"20July2002 - 13May2010")
*
*					Times in each block are always store in seconds from the D part of the pathname (base date),
*					except for ir-century, where they are stored in minutes from base to avoid int overflow
*					Times in the time array are converted to timeGranularitySeconds, which is for the entire dataset
*					and may be an input from the user.
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/



int ztsRetrieveIrreg7(long long *ifltab, zStructTimeSeries *tss,
					int retrieveFlag, int retrieveDoublesFlag, int boolRetrieveQualityNotes)
{


//  FIX ME - if startDate is blank, read entire record or time window specified
	// by D part  (e.g., 12Jun2003 - 18Aug 2009)
	//  numberValues then identifies the values array dimension


	/////////////////////////////////////////////
	/////////////////   FIX ME
	//  write a function to return the number of values, quality and notes in:
	//  a record
	//  a set of records
	//  with time window (using explicit time window...) slower, but accurate
	//  Do the same for regular and paired data!
	/////////////////////////////////////////////////////////////////////////

	int status;
	int intervalType;
	int blockSize;
	int startBlockJulian;
	int endBlockJulian;
	int julianBlockDate;
	int julianNextBlockDate;
	int currentPosition;
	int maxNumberToRead;
	int numberRead;
	int maxValues;
	int boolProfileRecord;
	int valuesArraySize;
	int profileDepthsNumber;
	int sizeEachElementRequested;
	int timeBaseToStart;
	long long timeGranularity;

	int firstJulian;
	int firstSeconds;
	int lastJulian;
	int lastSeconds;
	int julian;
	int icompare;
	int boolRetrievePrevious;
	int boolRetrieveSubsequent;

	int positionValues;
	int positionQuality;
	int positionNotes;
	int positionCnotes;

	int sizeCNotesRemaning;
	int lengthCNotes;

	int valueSize;
	int qualityElementSize;
	int inoteElementSize;
	int valueElementSizeRead;
	int qualityElementSizeRead;
	int sizeEachNoteRead;
	int totalNumberCnotesRead;

	int multipier;
	int *depthValues;
	int depthValuesNumber;
	int depthValuesArraySize;

	int i;

	int internalHeader[INT_HEAD_SIZE];
	int internalHeaderArraySize  = INT_HEAD_SIZE;
	int internalHeaderNumber;

	zStructRecordSize timeSeriesRecordSizes;

	int foundOne;

	char messageString[100];
	char path[MAX_PATHNAME_LENGTH];

	char blockDate[20];
	long long *fileHeader;
	long long *info;
	long long nada=0;
	int *values;


	//  Do I want this at this level ???
	int buffer[1];
	long long bufferControl[4];
	buffer[0] = 0;
	bufferControl[BUFF_SIZE] = 0;
	bufferControl[BUFF_STAT] = 0;
	bufferControl[BUFF_ADDRESS] = 0;
	bufferControl[BUFF_INTS_USED] = 0;


		//  Check for correct DSS Version
	if (zgetVersion(ifltab) != 7) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieveIrreg_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
								zgetVersion((void *)ifltab), 0, zdssErrorSeverity.WARNING, tss->pathname, "");
	}

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessage(ifltab, " ");
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsRetrieveIrreg_ID, "Handle: ", zhandle(ifltab));
		zmessageDebug(ifltab, DSS_FUNCTION_ztsRetrieveIrreg_ID, "Pathname: ", tss->pathname);
	}

		//  If the time window has not been processed (e.g., char dates to julian), do so now
	if (!tss->timeWindow) {
		intervalType = ztsProcessTimes(ifltab, tss, 0);
		if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_USER_DIAG)) {
			ztsMessTimeWindow(ifltab, DSS_FUNCTION_ztsRetrieveIrreg_ID, tss);
		}
		if ((intervalType != 0) && (intervalType != 1)) {
			return zerrorUpdate(ifltab, intervalType, DSS_FUNCTION_ztsRetrieveIrreg_ID);
		}
	}

	stringCopy(path, sizeof(path), zgetInternalPath(tss), strlen(zgetInternalPath(tss)));
	julianBlockDate = tss->timeWindow->startBlockJulian;
	startBlockJulian = tss->timeWindow->startBlockJulian;
	endBlockJulian = tss->timeWindow->endBlockJulian;
	blockSize = tss->timeWindow->blockSize;
	valueElementSizeRead = 0;
	qualityElementSizeRead = 0;
	sizeEachNoteRead = 0;
	totalNumberCnotesRead = 0;
	lengthCNotes = 0;
	depthValues = 0;


	//  Do we need to retrieve the value previous or subsequent to the time window?
	boolRetrievePrevious = 0;
	boolRetrieveSubsequent = 0;
	if (retrieveFlag > 0) {
		//  Check for the subsequent value, as these require reads and it will be
		//  more efficient to check the previous one right before the block read

		if ((retrieveFlag == 2) || (retrieveFlag == 3)) {
			//  We need to determine if that value is in the last block.
			//  If it is not, then we will use the first value from the block following this
			//  block (in time).  We limit this to one read...
			//  If no value, then no value...
			julianToDate(endBlockJulian, 4, blockDate, sizeof(blockDate));
			zpathnameSetPart (path, sizeof(path), blockDate, 4);
			status = ztsGetFirstLastRecordTimes(ifltab, path,
												&firstJulian, &firstSeconds,
												&lastJulian, &lastSeconds, 1);
			if (zisError(status)) {
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_ztsRetrieveIrreg_ID);
			}
			//  if status != 0, then the record was not found, or
			//  it is not time series.  If it is not found, then
			//  we'll have a tough time get the first last dates
			//  (we only said we'd look at one record past for the subsequent value)
			//  If so, let the remainder of the code deal with it.
			if (status == STATUS_RECORD_FOUND) {
				//  Okay, does the last block have the
				//  subsequent value in it's time window?
				icompare = compareTimes(lastJulian, lastSeconds, 0, 1, tss->timeWindow->endJulian,
										tss->timeWindow->endTimeSeconds, 0, 1);
				if (icompare > 0) {
					//  yes
					//  Don't do anything at this point, retrieveFlag will take care of
				}
				else {
					//  No;  last time in record is same as or before time requested.
					//  We need the record after for the subsequent value
					//  Increment block date
					julian = ztsIncrementBlock(endBlockJulian, blockSize);
					julianToDate(julian, 4, blockDate, sizeof(blockDate));
					zpathnameSetPart (path, sizeof(path), blockDate, 4);
					status = ztsGetFirstLastRecordTimes(ifltab, path,
														&firstJulian, &firstSeconds,
														&lastJulian, &lastSeconds, 1);
					if (zisError(status)) {
						return zerrorUpdate(ifltab, status, DSS_FUNCTION_ztsRetrieveIrreg_ID);
					}
					//  If not, found, let rest of code handle it.
					if (status == STATUS_RECORD_FOUND) {
						//  Yes - adjust the time window to include the first
						//  value of the subsequent block.  Modify flag to reflect this.
						endBlockJulian = julian;
						tss->timeWindow->endJulian = firstJulian;
						tss->timeWindow->endTimeSeconds = firstSeconds;
						if (retrieveFlag == 3) {
							retrieveFlag = 1;
						}
						else {
							retrieveFlag = 0;
						}
					}
				}
			}
		}
		//  Now check for the previous value
		if ((retrieveFlag == 1) || (retrieveFlag == 3)) {
			//  We need to determine if that value is in the first block.
			//  If it is not, then we will use the last value from the previous
			//  block (in time).  We limit this to one previous read...
			//  If no value, then no value...
			julianToDate(startBlockJulian, 4, blockDate, sizeof(blockDate));
			zpathnameSetPart (path, sizeof(path), blockDate, 4);
			status = ztsGetFirstLastRecordTimes(ifltab, path,
												&firstJulian, &firstSeconds,
												&lastJulian, &lastSeconds, 1);
			if (zisError(status)) {
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_ztsRetrieveIrreg_ID);
			}
			//  if status != 0, then the record was not found, or
			//  it is not time series.  If it is not found, then
			//  we'll have a tough time get the first last dates
			//  (we only said we'd look back one record for the previous data)
			//  If so, let the remainder of the code deal with it.
			if (status == STATUS_RECORD_FOUND) {
				//  Okay, does the block we are about to read have the
				//  previous value in it's time window?
				icompare = compareTimes(firstJulian, firstSeconds, 0, 1, tss->timeWindow->startJulian,
										tss->timeWindow->startTimeSeconds, 0, 1);
				if (icompare < 0) {
					//  yes
					boolRetrievePrevious = 1;
				}
				else {
					//  No;  First time in record is same as or after time requested.
					//  We need the record before for the previous value
					//  Decrement block date
					julian = startBlockJulian - 1;
					julian = ztsIrregGetBlockStart(julian, blockSize);
					julianToDate(julian, 4, blockDate, sizeof(blockDate));
					zpathnameSetPart (path, sizeof(path), blockDate, 4);
					status = ztsGetFirstLastRecordTimes(ifltab, path,
														&firstJulian, &firstSeconds,
														&lastJulian, &lastSeconds, 1);
					if (zisError(status)) {
						return zerrorUpdate(ifltab, status, DSS_FUNCTION_ztsRetrieveIrreg_ID);
					}
					//  If not, found, let rest of code handle it.
					if (status == STATUS_RECORD_FOUND) {
						//  Yes - adjust the time window to include the last
						//  value of the previous block.  Modify flag to reflect this.
						startBlockJulian = julian;
						tss->timeWindow->startJulian = lastJulian;
						tss->timeWindow->startTimeSeconds = lastSeconds;
						if (retrieveFlag == 3) {
							retrieveFlag = 2;
						}
						else {
							retrieveFlag = 0;
						}
					}
				}
			}
		}
	}

	tss->timeWindow->startBlockJulian = startBlockJulian;
	tss->timeWindow->endBlockJulian = endBlockJulian;

	//  We need to determine space to allocate to hold data
	//  Since this is irregular, we do not know how much
	//  until we interrogate the header of each record.
	//  For regular interval data, we can make a good guess on
	//  how much space we need, so we don't need to do this.

	//  Determine the array sizes that we need to allocate for the entire data set
	timeSeriesRecordSizes.pathname = path;
	status = ztsGetSizesInternal (ifltab, tss->timeWindow, &timeSeriesRecordSizes);

	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_ztsRetrieveIrreg_ID);
	}
	if (status == STATUS_RECORD_NOT_FOUND) {
		//  Data set does not exist
		if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_GENERAL)) {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, ZREAD_NOT_EXIST, zhandle(ifltab));
			zmessage2(ifltab, messageString, tss->pathname);
		}
		return STATUS_RECORD_NOT_FOUND;
	}

	//  Allocate space to hold all data, if this is the first time into loop
	//  Be sure that we are on 8 byte boundaries for allocating space
	if (isOdd(timeSeriesRecordSizes.values1Number)) timeSeriesRecordSizes.values1Number++;
	if (isOdd(timeSeriesRecordSizes.values2Number)) timeSeriesRecordSizes.values2Number++;
	if (isOdd(timeSeriesRecordSizes.values3Number)) timeSeriesRecordSizes.values3Number++;
	if (isOdd(timeSeriesRecordSizes.internalHeaderNumber)) timeSeriesRecordSizes.internalHeaderNumber++;
	if (isOdd(timeSeriesRecordSizes.header2Number)) timeSeriesRecordSizes.header2Number++;
	if (isOdd(timeSeriesRecordSizes.userHeaderNumber)) timeSeriesRecordSizes.userHeaderNumber++;

	tss->dataType = timeSeriesRecordSizes.dataType;
	if (retrieveDoublesFlag == 0) {
		retrieveDoublesFlag = timeSeriesRecordSizes.tsValueElementSize;
	}

	maxValues = timeSeriesRecordSizes.numberValues;
	tss->times = (int *)calloc((size_t)maxValues, WORD_SIZE);
	if (!tss->times) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieveIrreg_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, (maxValues * 4), 0,
								zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating times array");
	}
	tss->allocated[zSTRUCT_TS_times] = 1;

	if ((timeSeriesRecordSizes.dataType == DATA_TYPE_ITS_PROFILE) ||
		(timeSeriesRecordSizes.dataType == DATA_TYPE_ITD_PROFILE)) {
		boolProfileRecord = 1;
		multipier = timeSeriesRecordSizes.tsProfileDepthsNumber;
		profileDepthsNumber = timeSeriesRecordSizes.tsProfileDepthsNumber;
		if (timeSeriesRecordSizes.dataType == DATA_TYPE_ITS_PROFILE) {
			depthValues = (int *)calloc((size_t)timeSeriesRecordSizes.tsProfileDepthsNumber, FLOAT_SIZE);
			depthValuesArraySize = (int)timeSeriesRecordSizes.tsProfileDepthsNumber;
		}
		else {
			depthValues = (int *)calloc((size_t)timeSeriesRecordSizes.tsProfileDepthsNumber, DOUBLE_SIZE);
			depthValuesArraySize = timeSeriesRecordSizes.tsProfileDepthsNumber * 2;
		}
		if (!depthValues) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieveIrreg_ID,
									zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, (depthValuesArraySize * 4), 0,
									zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating profles array");
		}
	}
	else {
		boolProfileRecord = 0;
		profileDepthsNumber = 0;
		depthValuesArraySize = 0;
		multipier = 1;
	}

	if (retrieveDoublesFlag == 2) {
		sizeEachElementRequested = 2;
		valueSize = 2 * multipier;
		valuesArraySize = maxValues * valueSize;
		values = (int *)calloc((size_t)valuesArraySize, FLOAT_SIZE);
		if (!values) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieveIrreg_ID,
									zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, (valuesArraySize * 4), 0,
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
		sizeEachElementRequested = 1;
		valueSize = 1 * multipier;
		valuesArraySize = maxValues * valueSize;
		values = (int *)calloc((size_t)valuesArraySize, FLOAT_SIZE);
		if (!values) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieveIrreg_ID,
									zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, (valuesArraySize * 4), 0,
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
	if (timeSeriesRecordSizes.userHeaderNumber > 0) {
		if (!tss->userHeader) {
			tss->userHeaderSize = timeSeriesRecordSizes.userHeaderNumber;
			tss->userHeader = (int *)calloc(tss->userHeaderSize, 4);
			if (!tss->userHeader) {
				return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieveIrreg_ID,
										zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, (tss->userHeaderSize * 4), 0,
										zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating user header");
			}
			tss->allocated[zSTRUCT_userHeader] = 1;
		}
	}

	if (boolRetrieveQualityNotes) {
		qualityElementSize = timeSeriesRecordSizes.tsQualityElementSize;
		inoteElementSize = timeSeriesRecordSizes.tsInotesElementSize;
		tss->userHeaderNumber = timeSeriesRecordSizes.userHeaderNumber;

		if (qualityElementSize) {
			tss->qualityArraySize = maxValues * qualityElementSize;
			tss->quality = (int *)calloc((size_t)tss->qualityArraySize, WORD_SIZE);
			if (!tss->quality) {
				return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieveIrreg_ID,
										zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, (tss->qualityArraySize * 4), 0,
										zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating quality array");
			}
			tss->allocated[zSTRUCT_TS_quality] = 1;
			tss->qualityElementSize = qualityElementSize;
		}
		if (inoteElementSize) {
			tss->inotesArraySize = maxValues * inoteElementSize;
			tss->inotes = (int *)calloc((size_t)tss->inotesArraySize, WORD_SIZE);
			if (!tss->inotes) {
				return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieveIrreg_ID,
										zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, (tss->inotesArraySize * 4), 0,
										zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating inotes array");
			}
			tss->allocated[zSTRUCT_TS_inotes] = 1;
			tss->inoteElementSize = inoteElementSize;
		}
		else if (timeSeriesRecordSizes.tsCnotesLength) {
			tss->cnotesSize = timeSeriesRecordSizes.tsCnotesLength;
			tss->cnotes = (char *)calloc((size_t)tss->cnotesSize, CHAR_SIZE);
			if (!tss->cnotes) {
				return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieveIrreg_ID,
										zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, (tss->cnotesSize * 1), 0,
										zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating cnotes array");
			}
			tss->allocated[zSTRUCT_TS_cnotes] = 1;
		}
	}
	else {
		lengthCNotes = 0;
		qualityElementSize = 0;
		inoteElementSize = 0;
	}

	//  Main Loop
	//  Generate a new pathname for each block, changing the D (D part) each time
	//-----------------------------------------------------
	currentPosition = 0;
	foundOne = 0;
	julianBlockDate = startBlockJulian;
	while (julianBlockDate <= endBlockJulian) {

		//  Get the date for this block
		julianToDate(julianBlockDate, 4, blockDate, sizeof(blockDate));

		//  Generate pathname
		zpathnameSetPart (path, sizeof(path), blockDate, 4);

		//  Get the date of the next block
		julianNextBlockDate = ztsIncrementBlock(julianBlockDate, blockSize);

		//  Do we need to read the subsequent value (on the last block)?
		if ((julianBlockDate == endBlockJulian) & (retrieveFlag > 0)) {
			if ((retrieveFlag == 2) || (retrieveFlag == 3)) {
				boolRetrieveSubsequent = 1;
			}
		}

		//  Get the number left to read
		maxNumberToRead = maxValues - currentPosition;
		//  Use the least of the the number left in the block or number left to read
		if (maxNumberToRead <= 0) {
			//  ???
		}

		//  Copy the data to prepare the block for reading
		positionValues = currentPosition * (valueSize);
		positionQuality = currentPosition * (qualityElementSize);
		positionNotes = currentPosition * (inoteElementSize);
		positionCnotes = totalNumberCnotesRead;
		sizeCNotesRemaning = tss->cnotesSize - positionCnotes;

		if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_USER_DIAG)) {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %s,  julian: %d",
				blockDate, julianBlockDate);
			zmessageDebug(ifltab, DSS_FUNCTION_ztsRetrieveIrreg_ID, "Block date: ", messageString);
			zmessageDebug(ifltab, DSS_FUNCTION_ztsRetrieveIrreg_ID, "Block pathname: ", path);
		}

		status = ztsIrregReadBlock(ifltab, path, &nada,
			buffer, bufferControl,
			julianBlockDate, tss->timeWindow,
			&tss->times[currentPosition],
			&values[positionValues], (valuesArraySize - positionValues), sizeEachElementRequested, &valueElementSizeRead,
			&tss->quality[positionQuality], (tss->qualityArraySize - positionQuality), qualityElementSize, &qualityElementSizeRead,
			&tss->inotes[positionNotes], (tss->inotesArraySize - positionNotes), inoteElementSize, &sizeEachNoteRead,
			&tss->cnotes[positionCnotes], sizeCNotesRemaning, &lengthCNotes,
			depthValues, depthValuesArraySize, &depthValuesNumber, profileDepthsNumber,
			internalHeader, internalHeaderArraySize , &internalHeaderNumber,
			tss->userHeader, tss->userHeaderSize, &tss->userHeaderNumber,
			maxNumberToRead, &numberRead,
			boolRetrievePrevious, boolRetrieveSubsequent);

		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_ztsRetrieveIrreg_ID);
		}
		if (status == STATUS_RECORD_FOUND) {

			//  Save info for first record found
			if (!foundOne) {
				ztsInternalHeaderUnpack(tss, internalHeader, internalHeaderNumber);
				if (tss->julianBaseDate < 0) {
					tss->julianBaseDate = julianBlockDate;
				}
				else if ((tss->timeGranularitySeconds == 1) && (tss->julianBaseDate == 0)) {
					tss->julianBaseDate = julianBlockDate;
				}
				tss->sizeEachValueRead = valueElementSizeRead;
				foundOne = 1;
				status = 0;
				if (boolProfileRecord) {
					//  Profile data - do we need to convert?
					if (depthValuesNumber < depthValuesArraySize) {
						//  error out - FIX ME
						return STATUS_NOT_OKAY;
					}
					tss->profileDepthsNumber = internalHeader[INT_HEAD_profileDepthsNumber];
					if (retrieveDoublesFlag == 2) {
						if (timeSeriesRecordSizes.dataType == DATA_TYPE_ITS_PROFILE) {
							tss->doubleProfileDepths = (double *)calloc((size_t)tss->profileDepthsNumber, DOUBLE_SIZE);
							convertDataArray((void *)&depthValues[0], (void *)tss->doubleProfileDepths, tss->profileDepthsNumber, 1, 2);
							free(depthValues);
						}
						else if (timeSeriesRecordSizes.dataType == DATA_TYPE_ITD_PROFILE) {
							tss->doubleProfileDepths = (double *)depthValues;
						}
						tss->allocated[zSTRUCT_TS_profileDoubleDepths] = 1;
					}
					else {
						if (timeSeriesRecordSizes.dataType == DATA_TYPE_ITS_PROFILE) {
							tss->floatProfileDepths = (float *)depthValues;
						}
						else if (timeSeriesRecordSizes.dataType == DATA_TYPE_ITD_PROFILE) {
							tss->floatProfileDepths = (float *)calloc((size_t)tss->profileDepthsNumber, FLOAT_SIZE);
							convertDataArray(&depthValues[0], (void *)tss->floatProfileDepths, tss->profileDepthsNumber, 2, 1);
							free(depthValues);
						}
						tss->allocated[zSTRUCT_TS_profileFloatDepths] = 1;
					}
					depthValues = 0;
					//  If we have one depth set, don't read any more.
					depthValuesArraySize = 0;
				}				
				//  Save info about first record
				tss->dateOfFirstRecFound = julianBlockDate;
				info = (long long *)ifltab[zdssKeys.kinfo];
				tss->lastWrittenTime = info[zdssInfoKeys.kinfoLastWriteTime];
				tss->fileLastWrittenTime = fileHeader[zdssFileKeys.klastWriteTime];
				charLong(&info[zdssInfoKeys.kinfoProgram], tss->programName, 0, zdssVals.numberProgram, 0, 1);
			}
			timeBaseToStart = (julianBlockDate - tss->julianBaseDate);

			if (tss->timeGranularitySeconds == SECOND_GRANULARITY) {
				//  Seconds
				timeBaseToStart *= SECS_IN_1_DAY;
			}
			else if (tss->timeGranularitySeconds == MINUTE_GRANULARITY) {
				timeBaseToStart *= MINS_IN_1_DAY;
			}
			else if (tss->timeGranularitySeconds == HOUR_GRANULARITY) {
				timeBaseToStart *= HOURS_IN_1_DAY;
			}
			else {   // if (tss->timeGranularitySeconds == DAY_GRANULARITY) {
				// timeBaseToStart *= DAYS_IN_1_DAY;
			}

			//  This should have been set by ztsProcessTimes, but we need to make sure not zero
			timeGranularity = (long long)tss->timeGranularitySeconds;
			if (timeGranularity == 0) timeGranularity = SECS_IN_1_MINUTE;

			for (i=0; i<numberRead; i++) {
				if (blockSize == 5) {
					tss->times[i+currentPosition] = (int)(((long long)tss->times[i+currentPosition] * SECS_IN_1_MINUTE) / timeGranularity);
				}
				else {
					tss->times[i+currentPosition] = (int)((long long)tss->times[i+currentPosition] / timeGranularity);
				}
				tss->times[i+currentPosition] += timeBaseToStart;
			}

		}

		//  Make sure this is only used on first call
		boolRetrievePrevious = 0;

		//  Increment block start date
		//  Update the position in the data array for the next write
		currentPosition += numberRead;
		julianBlockDate = julianNextBlockDate;
		totalNumberCnotesRead += lengthCNotes;

		if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_USER_DIAG)) {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d,  Current total: %d, julian Block date: %d",
				numberRead, currentPosition, julianBlockDate);
			zmessageDebug(ifltab, DSS_FUNCTION_ztsRetrieveIrreg_ID, "Number read this block: ", messageString);
		}
	}
	//-----------------------------------------------------

	if (foundOne) {
		tss->numberValues = currentPosition;
		tss->timeWindow->numberValues = currentPosition;
		tss->qualityArraySize = currentPosition * qualityElementSize;
		tss->inotesArraySize = currentPosition * inoteElementSize;
		tss->cnotesLengthTotal = totalNumberCnotesRead;
	}
	else {
	}

	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_USER_DIAG)) {
		if (foundOne) {
			zmessageDebug(ifltab, DSS_FUNCTION_ztsRetrieveIrreg_ID, "Exit, Record Found", "");
			zmessageDebugInt(ifltab, DSS_FUNCTION_ztsRetrieveIrreg_ID, "Number read this block: ", currentPosition);
			zmessageDebugInt(ifltab, DSS_FUNCTION_ztsRetrieveIrreg_ID, "julianBaseDate: ", tss->julianBaseDate);
			zmessageDebugInt(ifltab, DSS_FUNCTION_ztsRetrieveIrreg_ID, "timeGranularitySeconds: ", tss->timeGranularitySeconds);
			if (currentPosition > 0) {
				ztsIrregMessage(ifltab, DSS_FUNCTION_ztsRetrieveIrreg_ID, "First value read: ", tss->times[0], tss->timeGranularitySeconds, tss->julianBaseDate,
					&values[0], valueSize, 1);
				ztsIrregMessage(ifltab, DSS_FUNCTION_ztsRetrieveIrreg_ID, "Last value read:  ", tss->times[currentPosition-1], tss->timeGranularitySeconds, tss->julianBaseDate,
					&values[currentPosition-1], valueSize, 1);
			}
		}
		else {
			zmessageDebug(ifltab, DSS_FUNCTION_ztsRetrieveIrreg_ID, "Exit,  Record NOT Found", "");
		}
		zmessage(ifltab, " ");  //  Blank line
	}

	if (depthValues) {
		free(depthValues);
		depthValues = 0;
	}

	if (zisError(status)) {
		return status;
	}
	if (foundOne) {
		return STATUS_RECORD_FOUND;
	}
	return status;

}

