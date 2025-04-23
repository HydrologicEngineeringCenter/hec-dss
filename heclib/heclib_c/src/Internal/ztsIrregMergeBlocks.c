#include <stdio.h>
#include "hecdssInternal.h"
#include "zdssMessages.h"

/**
*  Function:	ztsIrregMergeBlocks
*
*  Use:			Private
*
*  Description:	Merges two irregular-interval time series blocks together, or replaces part of the
*				one read, in preparation for writing.
*
*  Declaration:
*				void ztsIrregMergeBlocks(long long *ifltab, int storageFlag, int julianStartBlockDate,
*										 int boolFromStartOfBlock, int boolToEndOfBlock,
*										 int *timesToStore, int timeGranularityToStore, int numberToStore, int profileDepthsNumber,
*										 int *timesRead, int timeGranularityRead, int numberRead,
*										 int *valuesToStore, int *valuesRead, int valueElementSize,
*										 int *qualityToStore, int *qualityRead, int qualityElementSize,
*										 int *notesToStore, int *notesRead, int inoteElementSize,
*										 const char *cnotesToStore, int cnotesToStoreLen,
*										 char *cnotesRead, int cnotesReadLen,
*										 int *timesOut, int *valuesOut,
*										 int *quailtyOut, int *notesOut,
*										 char *cnotesOut, int cnotesOutSize, int *cnotesOutLen,
*										 int *totalToStore);
*
*  Parameters:	long long *ifltab
*					The file table array, similar to a handle number
*
*				int storageFlag
*					A flag indicating if the block to store is to replace the block read, or merge
*					the two together:
*						0:  Merge data sets together
*						1:  Replace data set (preferred
*
*				int julianStartBlockDate
*					The julian base date of both time windows, only used for showing real date during debugging.
*
*				int boolFromStartOfBlock
*					A boolean flag indicating that the data to be stored encompass the start of the block.
*					For example, if the data starts mid-block, this is set to 0 (false).
*					If the data started before the block or on the block boundary, this is set to 1 (true).
*					This flag is used to know if data from the start is to be kept or not.
*
*				int boolToEndOfBlock
*					A boolean flag indicating that the data to be stored encompass the end of the block.
*					For example, if the data ends mid-block, this is set to 0 (false).
*					If the data ends after this the block or on the block boundary, this is set to 1 (true).
*					This flag is used to know if data at the end of the block is to be kept or not.
*
*				int *timesToStore
*					An integer array containing times corresponding to the valuesToStore array.  Times are in
*					seconds from the start of the block (julianStartBlockDate), except for IR-CENTURY,
*					where times are in minutes to prevent overflow.  This must be size numberToStore.
*
*				int timeGranularityToStore
*					The number of seconds each unit in timesToStore represents,
*					MINUTE_GRANULARITY (60) or SECOND_GRANULARITY (1)
*
*				int numberToStore
*					The number of values to store for this block.  If quality or notes are
*					to be stored also, they must have the same number to store (along with timesToStore).
*
*				int profileDepthsNumber
*					The number of values for each profile (one profile for each time.)
*
*				int *timesRead
*					An integer array containing times read.  This must be size numberRead.
*
*				int timeGranularityRead
*					The number of seconds each unit in timesRead represents,
*					MINUTE_GRANULARITY (60) or SECOND_GRANULARITY (1)
*
*				int numberRead
*					The number of values read for this block.
*
*				int *valuesToStore
*					The values array to store.  This will either replace or be merged with the values read array.
*
*				int *valuesRead
*					The values array read.
*
*				int valueElementSize
*					The number of words each value element takes; generally 1 for floats, 2 for doubles.
*					At this point, the element size must be the same (or converted to) for the array to store and read.
*
*				int *qualityToStore (Optional)
*					The quality array to store, if quality is to be stored (otherwise a dummy array).
*
*				int *qualityRead (Optional)
*					The quality array read, if quality is read (otherwise a dummy array).
*
*				int qualityElementSize
*					The number of words for each quality element, either 0 (zero) if no quality is
*					to be stored, or greater than 0.  Must be the same for both read and to store.
*
*				int *notesToStore (Optional)
*					If integer notes array, if integer notes are to be stored.  If character notes are to be stored,
*					this must be a dummy array, as only character or integer notes (or neither) can be stored, not both.
*
*				int *notesRead (Optional)
*					The note array read, if notes are read (otherwise a dummy array).
*
*				int inoteElementSize
*					The number of words for each inote element, either 0 (zero) if no notes are
*					to be stored, or greater than 0.  Must be the same for both read and to store.
*
*				int inoteElementSize
*					The number of words for each integer note element, either 0 (zero) if integer notes are not
*					to be stored, or greater than 0.  Typically 1 or 2, but should handle any reasonable number.
*					Must be zero if character notes are stored.
*
*				const char *cnotesToStore (Optional)
*					A character array containing a null terminated string for each value, if character notes
*					are to be stored.  Each character note element is identified by a null termination, one per value
*					May not be used in combination with integer notes.
*
*				int cnotesToStoreLen
*					The length (number of bytes) to store for the cnotesToStore array.
*					If character notes are not to be stored, this should be set to zero.
*
*				const char *cnotesRead (Optional)
*					The character array of string note read.  Each character note element is identified by a null termination, one per value
*
*				int cnotesReadLen
*					The length (number of bytes) of cnotesRead.
*
*				int *timesOut (output)
*					The time array of the merged or replaced block, to write.
*
*				int *valuesOut (output)
*					The values array of the merged or replaced block, to write.
*
*				int *quailtyOut (output)
*					The quality array of the merged or replaced block, to write.
*
*				int *notesOut (output)
*					The integer notes array of the merged or replaced block, to write.
*
*				int *cnotesOut (output)
*					The character notes string of the merged or replaced block, to write.
*
*				int cnotesOutSize
*					The size of the cnotesOut array passed in.
*
*				int *cnotesOutLen (output)
*					The length (number of bytes) in cnotesOut to write.
*
*				int *totalToStore (output)
*					The number of values (and times) to store.
*
*
*
*	Returns:	void.
*
*	Remarks:	All times start from the same base and are in the same units (minutes or seconds)
*					That is:
*						if (timeWindow.blockSize < 5)  {
*							blockGranularity = SECOND_GRANULARITY;
*						}
*						else {
*							blockGranularity = MINUTE_GRANULARITY;
*						}
*					Times are all from the Julian block start date
*
*****************************  Note!  **************************
*	For now, the time granularity must be the same for data read as data to be stored!
*****************************************************************
*
*	Called by:	ztsIrregStoreBlock (only)
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


void ztsIrregMergeBlocks(long long *ifltab, int storageFlag, 
						zStructTimeSeries *tss, int julianStartBlockDate,
						int boolFromStartOfBlock, int boolToEndOfBlock,
						int *timesToStore, int timeGranularityToStore, int numberToStore, int profileDepthsNumber,
						int *timesRead, int timeGranularityRead, int numberRead,
						int *valuesToStore, int *valuesRead, int valueElementSize,
						int *qualityToStore, int *qualityRead, int qualityElementSize,
						int *notesToStore, int *notesRead, int inoteElementSize,
						const char *cnotesToStore, int cnotesToStoreLen,
						char *cnotesRead, int cnotesReadLen,
						int *timesOut, int *valuesOut,
						int *quailtyOut, int *notesOut,
						char *cnotesOut, int cnotesOutSize, int *cnotesOutLen,
						int *totalToStore)
{

	int icountRead;
	int icountStore;
	int icountTotal;

	int combinedTotal;
	int jcountIn;
	int jcountOut;
	int icnotesPosOut;
	int icnotesPosToStore;
	int icnotesPosRead;
	int ispos;
	int i;
	int numberLeft;
	float f;
	char cdummy[1];

	char messageString[200];

	icountRead = 0;
	icountStore = 0;
	icountTotal = 0;
	icnotesPosOut = 0;
	icnotesPosToStore = 0;
	icnotesPosRead = 0;
	int icompare;
	combinedTotal = numberToStore + numberRead;


	if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		ztsTimeMessage(ifltab, "Read Start date and time:  ", timesRead[0], timeGranularityRead, julianStartBlockDate);
		ztsTimeMessage(ifltab, "Read End date and time:    ", timesRead[numberRead-1], timeGranularityRead, julianStartBlockDate);
		if (numberToStore > 0) {
			ztsTimeMessage(ifltab, "Store Start date and time: ", timesToStore[0], timeGranularityToStore, julianStartBlockDate);
			ztsTimeMessage(ifltab, "Store End date and time:   ", timesToStore[numberToStore-1], timeGranularityToStore, julianStartBlockDate);
		}
		printf("tss->timeWindow->startTimeSeconds  %d\n ", tss->timeWindow->startTimeSeconds);
		printf("tss->timeWindow->startJulian  %d\n ", tss->timeWindow->startJulian);
		ztsTimeMessage(ifltab, "Time Window Start date and time:  ", tss->timeWindow->startTimeSeconds, 1, tss->timeWindow->startJulian);
		ztsTimeMessage(ifltab, "Time Window   End date and time:  ", tss->timeWindow->endTimeSeconds, 1, tss->timeWindow->endJulian);
	}

	if (timeGranularityRead != timeGranularityToStore) {

	}

	////////////////////////////////  tss->timeWindow->startJulian, tss->timeWindow->startTimeSeconds
	//////////////////  WRITE TEST CODE TO TEST THE FIRST 2 CASES!!!!!!

	//  Merge data sets
	if (storageFlag == 0) {
		for (i=0; i<combinedTotal; i++) {
			if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
					"Merge; Count: %d, read: %d, toStore %d, total: %d",
					i, icountRead, icountStore, icountTotal);
				zmessageDebug(ifltab, DSS_FUNCTION_ztsIrregMergeBlocks_ID, messageString, "");
			}
			//  Are we all done?
			if ((icountRead == numberRead) && (icountStore == numberToStore)) {
				if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
					zmessageDebug(ifltab, DSS_FUNCTION_ztsIrregMergeBlocks_ID, "Counts all same, break", "");
				}
				break;
			}
			if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
					"timeToStore %d, timesRead %d",timesToStore[icountStore], timesRead[icountRead]);
				zmessageDebug(ifltab, DSS_FUNCTION_ztsIrregMergeBlocks_ID, messageString, "");
			}
			//  This code requires the compiler to correctly evaluate (icountRead >= numberRead) before (timesToStore[icountStore] < timesRead[icountRead])
			if ((icountStore < numberToStore) && ((icountRead >= numberRead) || (timesToStore[icountStore] < timesRead[icountRead]))) {
				if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
					convertDataType((void *)&valuesToStore[icountStore], (void *)&f, valueElementSize, 1);
					_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
					" %d, timeToStore %d, value %f",
					i, timesToStore[icountStore], f);
					zmessageDebug(ifltab, DSS_FUNCTION_ztsIrregMergeBlocks_ID, "Copy store data ", messageString);
				}
				//  Copy "toStore"
				timesOut[icountTotal] = timesToStore[icountStore];
				if (valueElementSize > 0) {
					if (profileDepthsNumber == 0) {
						jcountIn  = icountStore * valueElementSize;
						jcountOut = icountTotal * valueElementSize;
						convertDataType(&valuesToStore[jcountIn], &valuesOut[jcountOut], valueElementSize, valueElementSize);
					}
					else {
						jcountIn  = icountStore * valueElementSize * profileDepthsNumber;
						jcountOut = icountTotal * valueElementSize * profileDepthsNumber;
						convertDataArray(&valuesToStore[jcountIn], &valuesOut[jcountOut], profileDepthsNumber,
							valueElementSize, valueElementSize);
					}
				}
				if (qualityElementSize > 0) {
					jcountIn  = icountStore * qualityElementSize;
					jcountOut = icountTotal * qualityElementSize;
					convertDataType(&qualityToStore[jcountIn], &quailtyOut[jcountOut], qualityElementSize, qualityElementSize);
				}
				if (inoteElementSize > 0) {
					jcountIn  = icountStore * inoteElementSize;
					jcountOut = icountTotal * inoteElementSize;
					convertDataType(&notesToStore[jcountIn], &notesOut[jcountOut], inoteElementSize, inoteElementSize);
				}
				if (cnotesOutSize > 0) {
					if (cnotesToStoreLen == 0) {
						cnotesOut[icnotesPosOut++] = '\0';
					}
					else {
						ispos = copyLines(&cnotesOut[icnotesPosOut], (size_t)(cnotesOutSize - icnotesPosOut),
							&cnotesToStore[icnotesPosToStore], (size_t)(cnotesToStoreLen - icnotesPosToStore), 1);
						icnotesPosOut += ispos;
						icnotesPosToStore += ispos;
					}
				}
				icountTotal++;
				icountStore++;
			}
			else if ((icountRead < numberRead) && ((icountStore >= numberToStore) || (timesToStore[icountStore] > timesRead[icountRead]))) {
				//  Copy "read"
				if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
					convertDataType((void *)&valuesRead[icountRead], (void *)&f, valueElementSize, 1);
					_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
					" %d, timeToStore %d, value %f",
					i, timesRead[icountRead], f);
					zmessageDebug(ifltab, DSS_FUNCTION_ztsIrregMergeBlocks_ID, "Copy read data ", messageString);
				}
				timesOut[icountTotal] = timesRead[icountRead];
				if (valueElementSize > 0) {
					if (profileDepthsNumber == 0) {
						jcountIn  = icountRead * valueElementSize;
						jcountOut = icountTotal * valueElementSize;
						convertDataType(&valuesRead[jcountIn], &valuesOut[jcountOut], valueElementSize, valueElementSize);
					}
					else {
						jcountIn  = icountRead * valueElementSize * profileDepthsNumber;
						jcountOut = icountTotal * valueElementSize * profileDepthsNumber;
						convertDataArray(&valuesRead[jcountIn], &valuesOut[jcountOut], profileDepthsNumber,
							valueElementSize, valueElementSize);
					}
				}
				if (qualityElementSize > 0) {
					jcountIn = icountRead * qualityElementSize;
					jcountOut = icountTotal * qualityElementSize;
					convertDataType(&qualityRead[jcountIn], &quailtyOut[jcountOut], qualityElementSize, qualityElementSize);
				}
				if (inoteElementSize > 0) {
					jcountIn = icountRead * inoteElementSize;
					jcountOut = icountTotal * inoteElementSize;
					convertDataType(&notesRead[jcountIn], &notesOut[jcountOut], inoteElementSize, inoteElementSize);
				}
				if (cnotesOutSize > 0) {
					if (cnotesReadLen == 0) {
						cnotesOut[icnotesPosOut++] = '\0';
					}
					else {
						ispos = copyLines(&cnotesOut[icnotesPosOut], (size_t)(cnotesOutSize - icnotesPosOut),
							&cnotesRead[icnotesPosRead], (size_t)(cnotesReadLen - icnotesPosRead), 1);
						icnotesPosOut += ispos;
						icnotesPosRead += ispos;
					}
				}
				icountTotal++;
				icountRead++;
			}
			else if (timesToStore[icountStore] == timesRead[icountRead]) {
				//  Copy Over
				if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
					jcountIn  = icountStore * valueElementSize;
					convertDataType((void *)&valuesToStore[jcountIn], (void *)&f, valueElementSize, 1);
					_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
					" %d, timeToStore %d, value %f",
					i, timesToStore[icountStore], f);
					zmessageDebug(ifltab, DSS_FUNCTION_ztsIrregMergeBlocks_ID, "Same, Copy store data ", messageString);
				}
				timesOut[icountTotal] = timesToStore[icountStore];
				if (valueElementSize > 0) {
					if (profileDepthsNumber == 0) {
						jcountIn  = icountStore * valueElementSize;
						jcountOut = icountTotal * valueElementSize;
						convertDataType(&valuesToStore[jcountIn], &valuesOut[jcountOut], valueElementSize, valueElementSize);
					}
					else {
						jcountIn  = icountStore * valueElementSize * profileDepthsNumber;
						jcountOut = icountTotal * valueElementSize * profileDepthsNumber;
						convertDataArray(&valuesToStore[jcountIn], &valuesOut[jcountOut], profileDepthsNumber,
							valueElementSize, valueElementSize);
					}
				}
				if (qualityElementSize > 0) {
					jcountIn  = icountStore * qualityElementSize;
					jcountOut = icountTotal * qualityElementSize;
					convertDataType(&qualityToStore[jcountIn], &quailtyOut[jcountOut], qualityElementSize, qualityElementSize);
				}
				if (inoteElementSize > 0) {
					jcountIn  = icountStore * inoteElementSize;
					jcountOut = icountTotal * inoteElementSize;
					convertDataType(&notesToStore[jcountIn], &notesOut[jcountOut], inoteElementSize, inoteElementSize);
				}
				if (cnotesOutSize > 0) {
					if (cnotesToStoreLen == 0) {
						cnotesOut[icnotesPosOut++] = '\0';
					}
					else {
						ispos = copyLines(&cnotesOut[icnotesPosOut], (size_t)(cnotesOutSize - icnotesPosOut),
							&cnotesToStore[icnotesPosToStore], (size_t)(cnotesToStoreLen - icnotesPosToStore), 1);
						icnotesPosOut += ispos;
						icnotesPosToStore += ispos;
					}
					//  Count the characters in the read string and increment
					if (cnotesReadLen > 0) {
						ispos = copyLines(cdummy, (size_t)0, &cnotesRead[icnotesPosRead], (size_t)(cnotesReadLen - icnotesPosRead), 1);
						icnotesPosRead += ispos;
					}
				}
				icountTotal++;
				icountStore++;
				icountRead++;
			}
		}
	}
	else {
		//  Replace a portion (or the entire) block
		if (numberToStore > 0) {
			for (i=0; i<combinedTotal; i++) {
				if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
					_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
						"Replace; Count: %d, read: %d, toStore %d, total: %d, timeToStore %d, timesRead %d",
						i, icountRead, icountStore, icountTotal, timesToStore[icountStore], timesRead[icountRead]);
					zmessageDebug(ifltab, DSS_FUNCTION_ztsIrregMergeBlocks_ID, messageString, "");
				}

				//  Copy the times before the replacement
				//  Are we before the start of the time window?  (if after, bypass this value)
				//   is (timesRead[icountRead] < tss->timeWindow->startJulian)?
				icompare = compareTimes(julianStartBlockDate, timesRead[icountRead], 0, timeGranularityRead,
					tss->timeWindow->startJulian, tss->timeWindow->startTimeSeconds, 0, 1);
				if ((icountRead < numberRead) && (icompare < 0) && (timesRead[icountRead] < timesToStore[0]) && !boolFromStartOfBlock) {
					//  Copy "read"
					if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
						convertDataType((void *)&valuesRead[icountRead], (void *)&f, valueElementSize, 1);
						_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
						" %d, timeToStore %d, value %f",
						i, timesRead[icountRead], f);
						zmessageDebug(ifltab, DSS_FUNCTION_ztsIrregMergeBlocks_ID, "Copy read data ", messageString);
					}
					timesOut[icountTotal] = timesRead[icountRead];
					if (valueElementSize > 0) {
						if (profileDepthsNumber == 0) {
							jcountIn  = icountRead * valueElementSize;
							jcountOut = icountTotal * valueElementSize;
							convertDataType(&valuesRead[jcountIn], &valuesOut[jcountOut], valueElementSize, valueElementSize);
						}
						else {
							jcountIn  = icountRead * valueElementSize * profileDepthsNumber;
							jcountOut = icountTotal * valueElementSize * profileDepthsNumber;
							convertDataArray(&valuesRead[jcountIn], &valuesOut[jcountOut], profileDepthsNumber,
								valueElementSize, valueElementSize);
						}
					}
					if (qualityElementSize > 0) {
						jcountIn  = icountRead * qualityElementSize;
						jcountOut = icountTotal * qualityElementSize;
						convertDataType(&qualityRead[jcountIn], &quailtyOut[jcountOut], qualityElementSize, qualityElementSize);
					}
					if (inoteElementSize > 0) {
						jcountIn  = icountRead * inoteElementSize;
						jcountOut = icountTotal * inoteElementSize;
						convertDataType(&notesRead[jcountIn], &notesOut[jcountOut], inoteElementSize, inoteElementSize);
					}
					if (cnotesOutSize > 0) {
						if (cnotesReadLen == 0) {
							cnotesOut[icnotesPosOut++] = '\0';
						}
						else {
							ispos = copyLines(&cnotesOut[icnotesPosOut], (size_t)(cnotesOutSize - icnotesPosOut),
								&cnotesRead[icnotesPosRead], (size_t)(cnotesReadLen - icnotesPosRead), 1);
							icnotesPosOut += ispos;
							icnotesPosRead += ispos;
						}
					}
					icountTotal++;
					icountRead++;
				}
				else {
					break;
				}
			}
		}

		//  Now we have the start of the part to replace, copy the block to store
		combinedTotal -= icountTotal;
		for (i=0; i<combinedTotal; i++) {
			if (icountStore < numberToStore) {
				if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
					convertDataType((void *)&valuesToStore[icountStore], (void *)&f, valueElementSize, 1);
					_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
					" %d, timeToStore %d, value %f",
					i, timesToStore[icountStore], f);
					zmessageDebug(ifltab, DSS_FUNCTION_ztsIrregMergeBlocks_ID, "Copy store data ", messageString);
				}
				//  Copy "toStore"
				timesOut[icountTotal] = timesToStore[icountStore];
				if (valueElementSize > 0) {
					if (profileDepthsNumber == 0) {
						jcountIn  = icountStore * valueElementSize;
						jcountOut = icountTotal * valueElementSize;
						convertDataType(&valuesToStore[jcountIn], &valuesOut[jcountOut], valueElementSize, valueElementSize);
					}
					else {
						jcountIn  = icountStore * valueElementSize * profileDepthsNumber;
						jcountOut = icountTotal * valueElementSize * profileDepthsNumber;
						convertDataArray(&valuesToStore[jcountIn], &valuesOut[jcountOut], profileDepthsNumber,
							valueElementSize, valueElementSize);
					}
				}
				if (qualityElementSize > 0) {
					jcountIn  = icountStore * qualityElementSize;
					jcountOut = icountTotal * qualityElementSize;
					convertDataType(&qualityToStore[jcountIn], &quailtyOut[jcountOut], qualityElementSize, qualityElementSize);
				}
				if (inoteElementSize > 0) {
					jcountIn  = icountStore * inoteElementSize;
					jcountOut = icountTotal * inoteElementSize;
					convertDataType(&notesToStore[jcountIn], &notesOut[jcountOut], inoteElementSize, inoteElementSize);
				}
				if (cnotesOutSize > 0) {
					if (cnotesToStoreLen == 0) {
						cnotesOut[icnotesPosOut++] = '\0';
					}
					else {
						ispos = copyLines(&cnotesOut[icnotesPosOut], (size_t)(cnotesOutSize - icnotesPosOut),
							&cnotesToStore[icnotesPosToStore], (size_t)(cnotesToStoreLen - icnotesPosToStore), 1);
						icnotesPosOut += ispos;
						icnotesPosToStore += ispos;
					}
					//  Count the characters in the read string and increment
					if (cnotesReadLen > 0) {
						ispos = copyLines(cdummy, (size_t)0, &cnotesRead[icnotesPosRead], (size_t)(cnotesReadLen - icnotesPosRead), 1);
						icnotesPosRead += ispos;
					}
				}
				icountTotal++;
				icountStore++;
			}
			else {
				break;
			}
		}

		// Copy the remaining read data, if any
		if (!boolToEndOfBlock) {
			numberLeft = numberToStore + numberRead - icountTotal;
			icnotesPosRead = 0;
			for (i=0; i<numberLeft; i++) {
				if (icountTotal >= combinedTotal) {
					break;
				}
				if (icountRead < numberRead) {
					if ((numberToStore > 0) && (timesToStore[numberToStore-1] >= timesRead[icountRead])) {
						icountRead++;
						if (cnotesReadLen > 0) {
							ispos = copyLines(cdummy, (size_t)0, &cnotesRead[icnotesPosRead], (size_t)(cnotesReadLen - icnotesPosRead), 1);
							icnotesPosRead += ispos;
						}
/////////////////   FIX ME - NEED TO MOVE THE CHARS COUNT FORWARD TOO!!!
					}
					else {
						if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
							convertDataType((void *)&valuesRead[icountRead], (void *)&f, valueElementSize, 1);
							_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
							" %d, timeToStore %d, value %f",
							i, timesRead[icountRead], f);
							zmessageDebug(ifltab, DSS_FUNCTION_ztsIrregMergeBlocks_ID, "Copy remaining read data  ", messageString);
						}
						timesOut[icountTotal] = timesRead[icountRead];
						if (valueElementSize > 0) {
							if (profileDepthsNumber == 0) {
								jcountIn  = icountRead * valueElementSize;
								jcountOut = icountTotal * valueElementSize;
								convertDataType(&valuesRead[jcountIn], &valuesOut[jcountOut], valueElementSize, valueElementSize);
							}
							else {
								jcountIn  = icountRead * valueElementSize * profileDepthsNumber;
								jcountOut = icountTotal * valueElementSize * profileDepthsNumber;
								convertDataArray(&valuesRead[jcountIn], &valuesOut[jcountOut], profileDepthsNumber,
									valueElementSize, valueElementSize);
							}
						}
						if (qualityElementSize > 0) {
							jcountIn = icountRead * qualityElementSize;
							jcountOut = icountTotal * qualityElementSize;
							convertDataType(&qualityRead[jcountIn], &quailtyOut[jcountOut], qualityElementSize, qualityElementSize);
						}
						if (inoteElementSize > 0) {
							jcountIn = icountRead * inoteElementSize;
							jcountOut = icountTotal * inoteElementSize;
							convertDataType(&notesRead[jcountIn], &notesOut[jcountOut], inoteElementSize, inoteElementSize);
						}
						if (cnotesOutSize > 0) {
							if (cnotesReadLen == 0) {
								cnotesOut[icnotesPosOut++] = '\0';
							}
							else {
								ispos = copyLines(&cnotesOut[icnotesPosOut], (size_t)(cnotesOutSize - icnotesPosOut),
									&cnotesRead[icnotesPosRead], (size_t)(cnotesReadLen - icnotesPosRead), 1);
								icnotesPosOut += ispos;
								icnotesPosRead += ispos;
							}
						}
						icountRead++;
						icountTotal++;
					}
				}
				else {
					break;
				}
			}
		}
	}
	if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		ztsTimeMessage(ifltab, "Final Start date and time: ", timesOut[0], timeGranularityToStore, julianStartBlockDate);
		ztsTimeMessage(ifltab, "Final End date and time:   ", timesOut[icountTotal-1], timeGranularityToStore, julianStartBlockDate);
	}
	*totalToStore = icountTotal;
	*cnotesOutLen = icnotesPosOut;

}

