#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssVals.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "zStructTsTimeWindow.h"
#include "zerrorCodes.h"

/**
*  Function:	ztsIrregStoreBlock
*
*  Use:			Private
*
*  Description:	Prepare a single record of irregular-interval time series data to be written to disk
*					This includes merging with the existing record, if present, and computing
*					positions within arrays.  Data is written with ztsWriteBlock.
*
*  Declaration:
*				int ztsIrregStoreBlock(long long *ifltab, const char *pathname,
*									   int *buffer, long long bufferControl[4],
*									   int numberToStore, int *timesToStore,
*									   int *valuesToStore, int valueSize, int valueElementSize,
*									   int *qualityToStore, int qualityElementSize,
*									   int *notesToStore, int inoteElementSize,
*									   const char *cnotes, int lengthCNotes, int *lengthCNotesRemaining,
*									   int *profileDepths, int profileDepthsSize, int profileDepthsNumber,
*									   int *internalHeader, int internalHeaderNumber,
*									   int *userHeader, int userHeaderNumber,
*									   int julianStartBlockDate, int julianEndBlockDate, int blockSize,
*									   int boolFromStartOfBlock, int boolToEndOfBlock,
*									   int storageFlag, int boolReadBlock, int dataType);
*
*  Parameters:	long long *ifltab
*					The file table array, similar to a handle number
*
*				const char* pathname
*					The pathname of the record to store.  The pathname must be exact, but is case insensitive
*					(i.e., "Apart" is the same as "APART")
*
*				int *buffer (optional)
*					An integer*4 array that is used for buffering.
*                   It is usually passed between reading or writing functions to minimize physical reads or writes.
*					Generally, this array is malloced space that is as least as large as the record and info area to
*					be written or read. For example buffer = malloc((end Data - beginning info) * 8), assuming values are int*8 addresses
*					The actual int size of the array needs to be passed in bufferControl[BUFF_SIZE] (e.g., (end Data - beginning info) * 2)
*
*				long long bufferControl[4]
*					An int*8 array dimensioned to 4 to hold pointers/information used in buffering
*                   and is the control array for the buffer, which follows this argument
*					This array should be zeroed out before using, except for the first value.
*					The first element has to be set to the size of buffer in int*4 words.
*						bufferControl[BUFF_SIZE] is (max) int*4 size
*						bufferControl[BUFF_STAT] is write status;  0 - unused, 1 - not dirty (read only), 2 - dirty (needs to write)
*						bufferControl[BUFF_ADDRESS] is file address (int*8)
*						bufferControl[BUFF_INTS_USED] is current int number used
*
*				int numberToStore
*					The number of values to store for this block.  If quality or notes are
*					to be stored also, they must have the same number to store (along with timesToStore).
*
*				int *timesToStore
*					An integer array containing times corresponding to the values array.  Times are in
*					seconds from the start of the block (julianStartBlockDate), except for IR-CENTURY,
*					where times are in minutes to prevent overflow.  This must be size numberToStore.
*
*				int *valuesToStore
*					The values array.  Generally, values are floats or doubles.
*					This array must contain at least numberToStore number.
*
*				int valueSize
*					The number of words each value takes; generally 1 for floats, 2 for doubles.
*					However, if profiles are being stored, this is the number of words for the
*					individual profile, e.g., for 10 double values in the profile, this would be 10 * 2 = 20.
*
*				int valueElementSize
*					The number of words each value element takes; generally 1 for floats, 2 for doubles.
*					Usually valueElementSize == valueSize.  However, if profiles are being stored,
*					this is the value size for each profile element, e.g., 2 for doubles.
*
*				int *qualityToStore (Optional)
*					The quality array, if quality is to be stored (otherwise a dummy array).
*
*				int qualityElementSize
*					The number of words for each quality element, either 0 (zero) if no quality is
*					to be stored, or greater than 0.  Typically 1 or 2, but should handle any reasonable number.
*
*				int *notesToStore (Optional)
*					If integer notes array, if integer notes are to be stored.  If character notes are to be stored,
*					this must be a dummy array, as only character or integer notes (or neither) can be stored, not both.
*
*				int inoteElementSize
*					The number of words for each integer note element, either 0 (zero) if integer notes are not
*					to be stored, or greater than 0.  Typically 1 or 2, but should handle any reasonable number.
*					Must be zero if character notes are stored.
*
*				const char *cnotes (Optional)
*					A character array containing a null terminated string for each value, if character notes
*					are to be stored.  Each character note element is identified by a null termination, one per value
*					May not be used in combination with integer notes.
*
*				int lengthCNotes
*					The size (dimension) of the cnote array.  This prevents over-running this array.
*					If character notes are not to be stored, this should be set to zero.
*
*				int *lengthCNotesRemaining (output)
*					The length of the cnote array used.  This provides the position for the next block write.
*
*				int *profileDepths (Optional)
*					If time series profiles are being stored, this is the "depths array".  Each full record has
*					only one depths array, which must be the same for all values.  For example, this might
*					be 0., 5., 10., 15., 20., 25.  Use missing flags for periods where a value is not used.
*					If no measurement is made at 25 for a time period, set that value to the missing data flag.
*					Depths are either floats or doubles, as defined by lengthEachValue.  Depths and values
*					must be the same type (float or double).
*
*				int profileDepthsSize
*					The number of words in the profileDepths array to store.  For the example above, if stored as doubles, this would be 12.
*					If profiles are not stored, set this to zero.
*
*				int profileDepthsNumber
*					The number of values in the profileDepths array.  For the example above, if stored as doubles, this would be 6.
*					For non-profile stores, this is set to zero.
*
*				int *internalHeader
*					The internal header.
*
*				int internalHeaderNumber
*					The number of int 4 words to write from the internal header array.
*
*				int *userHeader
*					The user header.
*
*				int userHeaderNumber
*					The number of int 4 words to store in the user header array.
*
*				int julianStartBlockDate
*					The julian date of the start of the block.  For example, if the data was monthly, this would be
*					day 1 (e.g., March 1) and not the date associated with the first value (not March 31).
*
*				int julianEndBlockDate
*					The julian date of the end of the block.
*
*				int blockSize
*					A flag indicating the size of the block, as computed by ztsGetStandardInterval.
*					This will range from 1 for a daily block, to 5 for a century block.
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
*				int storageFlag
*					The position of the first value in the block, compared to the start of the block.
*
*				int boolReadBlock
*					A boolean flag indicating if the (exiting) block needs to be read (merged) first.
*					For example, if you are re-writing the complete block, you do not need to read the
*					block first.  Set to 1, if the block needs to be read first.
*
*				int dataType
*					The data type associated with this record.  See header file zdssVals.h for a list of data types.
*
*
*
*	Returns:	int status
*					STATUS_OKAY
*					error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*	Remarks:	Regardless of the user's time granularity flag, times are always stored
*					in seconds from the block start date for block sizes of one decade or smaller
*					and in minutes from block start date for century blocks
*
*					If only values are stored (most frequent case), then the code will save the position
*					of the first value and last value (and not store leading or trailing missing)
*					If quality or notes (either inotes or cnotes) are stored, then the full block
*					will be stored with leading and trailing missing, although it will be compressed.
*
*					Example for daily data
*					First case:
*					May 2, 2000:    1234.5
*
*					Second case: Time	value		cnotes
*					Jan 1, 2000:	-901.0		'\0'
*					....
*					May 1, 2000:	-901.0		'\0'
*					May 2, 2000:	1234.5		'Gage was one foot below flood.'
*					May 3, 2000:	-901.0		'\0'
*					....
*					Dec 31, 2000:	-901.0		'\0'
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/



int ztsIrregStoreBlock(long long *ifltab, zStructTimeSeries *tss, const char *pathname,
					   int *buffer, long long bufferControl[4],
					   int numberToStore, int *timesToStore,
					   int *valuesToStore, int valueSize, int valueElementSize,
					   int *qualityToStore, int qualityElementSize,
					   int *notesToStore, int inoteElementSize,
					   char *cnotes, int lengthCNotes, int *lengthCNotesRemaining,
					   int *profileDepths, int profileDepthsSize, int profileDepthsNumber,
					   int *internalHeader, 
					   int *userHeader, int userHeaderNumber,
					   int julianStartBlockDate, int julianEndBlockDate, int blockSize,
					   int boolFromStartOfBlock, int boolToEndOfBlock,
					   int storageFlag, int boolReadBlock, int dataType)
{
	int numberToStoreProvided;
	int julianFirstValue;
	int julianLastValue;
	int secondsFirstValue;
	int secondsLastValue;
	int days;
	int allMissing=0;
	int status;
	int zero = 0;
	int one = 1;
	int numberRead;
	int i;
	int icpos;
	int ispos;
	float fval;
	int span;
	int averageInterval;
	int logcialNumberData;

	char messageString[80];

	int internalHeaderRead[INT_HEAD_SIZE];
	int sizeInternalHeaderRead = INT_HEAD_SIZE;
	int lengthInternalHeaderRead;
	int recordFound;
	int logicalNumberValues;

	int internalHeaderReadNumber;
	int uhNumber;

	int *timesRead;
	int *valuesRead;
	int *qualityRead;
	int *notesRead;
	char *cnotesRead;
	int cnotesReadLen;
	char *cnotesToStore;
	char *cnotesStore;
	int cnotesToStoreLen;
	int cnotesToStoreSize;
	int cnotesToStoreNumber;
	int cnotesReadSize;

	int *times;
	int *values;
	int *quality;
	int *notes;

	int *timesOut;
	int *valuesOut;
	int *qualityOut;
	int *notesOut;

	int combinedTotal;
	int totalToStore;
	int valuesReadLength;
	int qualityReadLength=0;
	int inotesReadLength=0;
	ztsTimeWindow timeWindow;

	int *profileDepthsRead = 0;
	int profileDepthsArraySizeRead = 0;
	int profileDepthsNumberRead;
	int sizeEachValueRead;
	int qualityElementSizeRead;
	int sizeEachNoteRead;

	int timeArrayUnits;

	long long *info;




	if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztsIrregStoreBlock_ID, "Enter, Pathname: ", pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsIrregStoreBlock_ID, "Number to store: ", numberToStore);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d;  Read block flag: %d",
			storageFlag, boolReadBlock);
		zmessageDebug(ifltab, DSS_FUNCTION_ztsIrregStoreBlock_ID, "Storage flag: ", messageString);
		valuesReadLength = numberToStore * valueSize;
		qualityReadLength = numberToStore * qualityElementSize;
		inotesReadLength = numberToStore * inoteElementSize;
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
			"%d, Number ints quality flags: %d, Number ints  notes: %d,  length C notes: %d",
			valuesReadLength, qualityReadLength, inotesReadLength, *lengthCNotesRemaining);
		zmessageDebug(ifltab, DSS_FUNCTION_ztsIrregStoreBlock_ID, "Number ints in data array: ", messageString);
	}


	//  Set new space memory pointers to 0
	timesRead = 0;
	valuesRead = 0;
	numberRead = 0;
	qualityRead = 0;
	notesRead = 0;
	cnotesRead = 0;
	cnotesReadLen = 0;
	cnotesStore = 0;
	cnotesToStore = 0;
	cnotesToStoreLen = 0;
	cnotesToStoreSize = 0;
	cnotesToStoreNumber = 0;
	numberToStoreProvided = numberToStore;

	timesOut = 0;
	valuesOut = 0;
	qualityOut = 0;
	notesOut = 0;
	status = 0;

	//  Set default arrays to arrays passed in, in case we don't merge blocks
	times = timesToStore;
	values = valuesToStore;
	quality = qualityToStore;
	notes = notesToStore;
	totalToStore = numberToStore;



	if (lengthCNotes > 0) {
		//  Figure out how much we will be writing for C notes.
		//  (Not copying chars now, just getting len)
		cnotesToStoreNumber = copyLines(cnotesToStore, (size_t)0, cnotes, (size_t)lengthCNotes, numberToStore);
		cnotesToStore = cnotes;
	}

	//  Do we need to see if this block already exists, so that we have to merge the data sets?
	if (boolReadBlock) {
		//  Because we might read the internal header right after the info block,
		//  read info manually, using a buffer area to read the internal header at the same time
		recordFound = zreadInfo (ifltab, pathname, 0);
		if (zisError(recordFound)) {
			return recordFound;
		}
		if (recordFound == STATUS_RECORD_NOT_FOUND) {
			boolReadBlock = 0;
		}
		else {
			//  Get the info block
			//  Keep location of info
			ifltab[zdssKeys.kinfoSize] = zdssVals.infoSize + numberLongsInBytes((int)ifltab[zdssKeys.klenLastPath]);;
			ifltab[zdssKeys.kiftPathHash] = ifltab[zdssKeys.kpathnameHash];
			ifltab[zdssKeys.kinfoAddress] = ifltab[zdssKeys.kaddInfoLastPath];
			info = (long long *)ifltab[zdssKeys.kinfo];

			//  Now read internal header array
			lengthInternalHeaderRead = (int)info[zdssInfoKeys.kinfoInternalHeadNumber];
			if (lengthInternalHeaderRead > sizeInternalHeaderRead) lengthInternalHeaderRead = sizeInternalHeaderRead;
			status = zget(ifltab, info[zdssInfoKeys.kinfoInternalHeadAddress],
				internalHeaderRead, lengthInternalHeaderRead, 1);
			if (zisError(status)) {
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_zbinUpdate_ID);
			}

			logicalNumberValues = (int)info[zdssInfoKeys.kinfoLogicalNumber];
			if (logicalNumberValues < 1) {
				//  Very rare (not storing data, but quality or notes)
				boolReadBlock = 0;
			}

			//  FOR EXPANSIONS, compute full length base on number in block from start of block
			//  to end data, compared to end of block (i.e., like average interval)

			if (boolReadBlock) {
				//  Also, don't read or expand block if we are replacing the same (lengths)
				//  Need to compare missing values that are in data set and block, and quality and note lengths

				//  Read in the existing block and merge the data
				//  First, get space for arrays
				timesRead = (int *)calloc((size_t)logicalNumberValues, WORD_SIZE);
				if (!timesRead) {
					return zerrorProcessing(ifltab, DSS_FUNCTION_ztsIrregStoreBlock_ID,
											zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, logicalNumberValues, 0,
											zdssErrorSeverity.MEMORY_ERROR, pathname, "Allocating timesRead array");
				}

				if (valueSize > 0) {
					valuesReadLength = logicalNumberValues * valueSize;
					valuesRead = (int *)calloc((size_t)valuesReadLength, WORD_SIZE);
					if (!valuesRead) {
						return zerrorProcessing(ifltab, DSS_FUNCTION_ztsIrregStoreBlock_ID,
												zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, valuesReadLength, 0,
												zdssErrorSeverity.MEMORY_ERROR, pathname, "Allocating values read array");
					}
				}
				else {
					valuesRead = 0;
				}
				if (qualityElementSize > 0) {
					qualityReadLength = logicalNumberValues * qualityElementSize;
					qualityRead = (int *)calloc((size_t)qualityReadLength, WORD_SIZE);
					if (!qualityRead) {
						return zerrorProcessing(ifltab, DSS_FUNCTION_ztsIrregStoreBlock_ID,
												zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, qualityReadLength, 0,
												zdssErrorSeverity.MEMORY_ERROR, pathname, "Allocating quality read array");
					}
				}
				else {
					qualityRead = 0;
				}
				if (inoteElementSize > 0) {
					inotesReadLength = logicalNumberValues * inoteElementSize;
					notesRead = (int *)calloc((size_t)inotesReadLength, WORD_SIZE);
					if (!notesRead) {
						return zerrorProcessing(ifltab, DSS_FUNCTION_ztsIrregStoreBlock_ID,
												zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, inotesReadLength, 0,
												zdssErrorSeverity.MEMORY_ERROR, pathname, "Allocating notesRead array");
					}
				}
				else {
					notesRead = 0;
				}
				if (profileDepthsNumber > 0) {
					//  FIX me - do I need to read this????
					//profileDepthsRead, profileDepthsArraySizeRead
					profileDepthsArraySizeRead = 0;
				}

				//  If we are storing character notes, so we need to allocate enough space
				//  to both read in cnotes on disk and to add our notes on.
				//  Get the amount of space used on disk
				if (internalHeaderRead[INT_HEAD_cnotesLength] > 0) {
					cnotesReadSize = internalHeaderRead[INT_HEAD_cnotesLength];
					cnotesRead = (char *)calloc((size_t)cnotesReadSize, CHAR_SIZE);
					if (!cnotesRead) {
						return zerrorProcessing(ifltab, DSS_FUNCTION_ztsIrregStoreBlock_ID,
												zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, cnotesReadSize, 0,
												zdssErrorSeverity.MEMORY_ERROR, pathname, "Allocating cnotesRead array");
					}
				}
				else {
					cnotesReadSize = 0;
					cnotesRead = 0;
				}

				timeWindow.startJulian = julianStartBlockDate;
				timeWindow.startTimeSeconds = 0;
				timeWindow.endJulian = julianEndBlockDate;
				timeWindow.endTimeSeconds = 0;
				timeWindow.blockSize = tss->timeWindow->blockSize;
				///////  FIX ME - time granularity read???  what if different????
				status = ztsIrregReadBlock(ifltab, pathname, info,
					buffer, bufferControl,
					julianStartBlockDate, &timeWindow, timesRead,
					valuesRead, valuesReadLength, valueElementSize, &sizeEachValueRead,
					qualityRead, qualityReadLength, qualityElementSize, &qualityElementSizeRead,
					notesRead, inotesReadLength, inoteElementSize, &sizeEachNoteRead,
					cnotesRead, cnotesReadSize, &cnotesReadLen,
					profileDepthsRead, profileDepthsArraySizeRead, &profileDepthsNumberRead, profileDepthsNumber,
					internalHeaderRead, lengthInternalHeaderRead, &internalHeaderReadNumber,
					userHeader, 0, &uhNumber,
					logicalNumberValues, &numberRead,
					0, 0);

				if (status != STATUS_RECORD_FOUND) {
					//  Free any space malloced
					if (timesRead) free(timesRead);
					if (valuesRead) free(valuesRead);
					if (qualityRead) free(qualityRead);
					if (notesRead) free(notesRead);
					if (cnotesRead) free(cnotesRead);
					if (cnotesStore) free(cnotesStore);
					return status;
				}

				//  Now that we've read the data, merge the two sets together
				//  First, allocate space for the combined data sets
				//  The maximum space needed will be the combined total of
				//  that read and that to be stored (e.g., the block to
				//  be stored is totally after the block read.)
				combinedTotal = numberToStore + numberRead;
				timesOut = (int *)calloc((size_t)combinedTotal, WORD_SIZE);
				if (!timesOut) {
					return zerrorProcessing(ifltab, DSS_FUNCTION_ztsIrregStoreBlock_ID,
											zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, combinedTotal, 0,
											zdssErrorSeverity.MEMORY_ERROR, pathname, "Allocating timesOut array");
				}
				if (valueSize > 0) {
					valuesReadLength = combinedTotal * valueSize;
					valuesOut = (int *)calloc((size_t)valuesReadLength, WORD_SIZE);
					if (!valuesOut) {
						return zerrorProcessing(ifltab, DSS_FUNCTION_ztsIrregStoreBlock_ID,
												zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, valuesReadLength, 0,
												zdssErrorSeverity.MEMORY_ERROR, pathname, "Allocating valuesOut array");
					}
				}
				else {
					valuesOut = 0;
				}
				if (qualityElementSize > 0) {
					qualityReadLength = combinedTotal * qualityElementSize;
					qualityOut = (int *)calloc((size_t)qualityReadLength, WORD_SIZE);
					if (!qualityOut) {
						return zerrorProcessing(ifltab, DSS_FUNCTION_ztsIrregStoreBlock_ID,
												zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, qualityReadLength, 0,
												zdssErrorSeverity.MEMORY_ERROR, pathname, "Allocating qualityOut array");
					}
				}
				else {
					qualityOut = 0;
				}
				if (inoteElementSize > 0) {
					inotesReadLength = combinedTotal * inoteElementSize;
					notesOut = (int *)calloc((size_t)inotesReadLength, WORD_SIZE);
					if (!notesOut) {
						return zerrorProcessing(ifltab, DSS_FUNCTION_ztsIrregStoreBlock_ID,
												zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, inotesReadLength, 0,
												zdssErrorSeverity.MEMORY_ERROR, pathname, "Allocating notesOut array");
					}
				}
				else {
					notesOut = 0;
				}
				if ((cnotesToStoreNumber > 0) || (cnotesReadLen > 0)) {
					cnotesToStoreSize = cnotesToStoreNumber + cnotesReadLen;
					//  Account for empty (null terminated) lines for the set not read
					if (cnotesToStoreNumber == 0) {
						cnotesToStoreSize += numberToStore;
					}
					if (cnotesReadLen == 0) {
						cnotesToStoreSize += numberRead;
					}
					cnotesStore = (char *)calloc((size_t)cnotesToStoreSize, CHAR_SIZE);
					if (!cnotesStore) {
						return zerrorProcessing(ifltab, DSS_FUNCTION_ztsIrregStoreBlock_ID,
												zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, cnotesToStoreSize, 0,
												zdssErrorSeverity.MEMORY_ERROR, pathname, "Allocating cnotesStore array");
					}
					cnotesToStore = cnotesStore;
				}
				else {
					cnotesToStoreSize = 0;
				}

				if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
					_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d, number read: %d",
						numberToStore, numberRead);
					zmessageDebug(ifltab, DSS_FUNCTION_ztsIrregStoreBlock_ID, "Calling ztsIrregMergeBlocks, number to store: ", messageString);
				}

				//  The time array is always in seconds from the julian start block date, 
				//  except for century blocks, which is in minutes to avoid int overflow
				//  (Note - this is separate from the time granularity, which is the user's input on the time accuracy)
				//  i.e., timeArrayUnits refers to the stored times in the individual record
				//  and timeGranularitySeconds refers to the user set granularity for the time array.
				if (blockSize < 5) {
					//  Times are in seconds
					timeArrayUnits = 1;
				}
				else {
					//  Times are in minutes
					timeArrayUnits = 60;
				}
				//  Merge the read and to be stored blocks
				//  the next function does not return a status
				ztsIrregMergeBlocks(ifltab, storageFlag, tss, julianStartBlockDate,
									boolFromStartOfBlock, boolToEndOfBlock,
									timesToStore, timeArrayUnits, numberToStore, profileDepthsNumber,
									timesRead, timeArrayUnits, numberRead,
									valuesToStore, valuesRead, valueElementSize,
									qualityToStore, qualityRead, qualityElementSize,
									notesToStore, notesRead, inoteElementSize,
									cnotes, cnotesToStoreNumber,
									cnotesRead, cnotesReadLen,
									timesOut, valuesOut,
									qualityOut, notesOut,
									cnotesToStore, cnotesToStoreSize, &cnotesToStoreLen,
									&totalToStore);
				if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
					zmessageDebugInt(ifltab, DSS_FUNCTION_ztsIrregStoreBlock_ID, "After ztsIrregMergeBlocks, Total to store: ", totalToStore);
					if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
						for (i=0; i<totalToStore; i++) {
							convertDataType((void *)&valuesOut[i], (void *)&fval, 1, 1);
							_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d, %d, %f", i, timesOut[i], fval);
							zmessage(ifltab, messageString);
						}
					}
				}

				//  Set arrays to those generated by merge block
				times = timesOut;
				values = valuesOut;
				quality = qualityOut;
				notes = notesOut;
			}
		}
	}


	if (totalToStore > 0) {

		//  Do we need to write this block (is it all missing?), or
		//  more so, do we need to delete the block on disk?
		//  Note - If quality flags or notes are used, we will always
		//  write the record, regardless if the data itself is all missing.
		//if (allMissing && !writeRegardless) {
		//}

		//  Now store the block in the DSS file
		// --------------   Main Write ------------------
		if (blockSize < 5) {
			//  Times are in seconds
			days = times[0]/86400;
			secondsFirstValue = times[0] - (days * 86400);
			julianFirstValue = julianStartBlockDate + days;
			days = times[totalToStore-1]/86400;
			secondsLastValue = times[totalToStore-1] - (days * 86400);
			julianLastValue = julianStartBlockDate + days;
		}
		else {
			//  Times are in minutes
			days = times[0]/1440;
			secondsFirstValue = (times[0] - (days * 1440)) * 60;
			julianFirstValue = julianStartBlockDate + days;
			days = times[totalToStore-1]/1440;
			secondsLastValue = (times[totalToStore-1] - (days * 1440)) * 60;
			julianLastValue = julianStartBlockDate + days;
		}
		if (secondsFirstValue < 1) {
			julianFirstValue--;
			secondsFirstValue += 86400;
		}
		if (secondsLastValue < 1) {
			julianLastValue--;
			secondsLastValue += 86400;
		}
		ifltab[zdssKeys.kdataFirstDate] = i4toi8(julianFirstValue, secondsFirstValue);
		ifltab[zdssKeys.kdataLastDate]  = i4toi8(julianLastValue, secondsLastValue);
		if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			ztsDateMessage(ifltab, DSS_FUNCTION_ztsIrregStoreBlock_ID, "First value date and time for block:  ", julianFirstValue, secondsFirstValue);
			zmessageDebugLong(ifltab, DSS_FUNCTION_ztsIrregStoreBlock_ID, "Combined long (ifltab[zdssKeys.kdataFirstDate]): ", ifltab[zdssKeys.kdataFirstDate]);
			ztsDateMessage(ifltab, DSS_FUNCTION_ztsIrregStoreBlock_ID, "Last  value date and time for block:  ", julianLastValue, secondsLastValue);
			zmessageDebugLong(ifltab, DSS_FUNCTION_ztsIrregStoreBlock_ID, "Combined long (ifltab[zdssKeys.kdataLastDate]): ", ifltab[zdssKeys.kdataLastDate]);
		}

		//  Compute "logicalNumberValues", the number of data if this block was full
		//  using the current number of values / time for values
		//  First, get the number of days in this block
		days = julianEndBlockDate - julianStartBlockDate + 1;
		//  Now the average interval between values
		if (days < 370) {
			//  In seconds
			span = ((julianLastValue - julianFirstValue + 1) * 86400) + (secondsLastValue - secondsFirstValue);
			averageInterval = span/totalToStore;    //  in seconds  (both span and averageInterval > 0)
			if (averageInterval > 0) {
				logcialNumberData = (days * 86400) / averageInterval;
			}
			else {
				logcialNumberData = 1;
			}
		}
		else {
			//  In minutes, for larger blocks (so that we don't over run int)
			span = ((julianLastValue - julianFirstValue + 1) * 1440) + ((secondsLastValue - secondsFirstValue)/60);
			averageInterval = span/totalToStore;
			if (averageInterval > 0) {
				logcialNumberData = (days * 1440) / averageInterval;
			}
			else {
				logcialNumberData = 1;
			}
		}

		//  Do we have cnotes to store and did not read the block?
		if ((cnotesToStoreNumber > 0) && (!boolReadBlock)) {
			cnotesToStoreLen = cnotesToStoreNumber;
			cnotesStore = (char *)calloc((size_t)cnotesToStoreLen, CHAR_SIZE);
			if (!cnotesStore) {
				return zerrorProcessing(ifltab, DSS_FUNCTION_ztsIrregStoreBlock_ID,
										zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, cnotesToStoreLen, 0,
										zdssErrorSeverity.MEMORY_ERROR, pathname, "Allocating cnotesStore array");
			}
			cnotesToStore = cnotesStore;
			icpos = 0;
			//  Copy the notes for the data
			ispos = copyLines(&cnotesToStore[icpos], (size_t)(cnotesToStoreLen - icpos), cnotes, (size_t)*lengthCNotesRemaining, numberToStoreProvided);
			icpos += ispos;
			*lengthCNotesRemaining -= ispos;
			cnotesToStoreLen = icpos;
		}
		else if (cnotesToStoreNumber > 0) {
			*lengthCNotesRemaining -= cnotesToStoreNumber;
		}

		zStructRecordBasics* rb = zstructRecordBasicsNew(pathname);
		status = zgetRecordBasics(ifltab, rb);

		int recordType = rb->recordType;
		zstructFree(rb);

		if (status == STATUS_RECORD_FOUND && recordType == DATA_TYPE_ITD
			&& dataType == DATA_TYPE_ITS
			&& tss->floatValues
			&& tss->doubleValues == NULL) {
			// Support writing floats into a double record (calling ztsStore recursively) 
			tss->doubleValues = calloc(totalToStore, sizeof(double));
			if (tss->doubleValues) {

				printf("\n'%s'\n", pathname);
				printf("floats:\n");
				for (float* fp = values; fp - values < totalToStore; ++fp) {
					printf("[%d]%2.f\n", (int)(fp - values), *fp);
				}

				convertDataArray((void*)values, (void*)tss->doubleValues, totalToStore, 1, 2);

				printf("doubles:\n");
				for (double* fp = tss->doubleValues; fp - tss->doubleValues < totalToStore; ++fp) {
					printf("[%d]%2.f\n", (int)(fp - tss->doubleValues), *fp);
				}

				float* pinned_float = tss->floatValues;
				tss->floatValues = NULL;

				status = ztsStore(ifltab, tss, storageFlag);
				tss->floatValues = pinned_float;
				free(tss->doubleValues);
				tss->doubleValues = NULL;
			}
			else
			{
				if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
					zmessageDebug(ifltab, DSS_FUNCTION_ztsRegStoreBlock_ID, "Memory Error storing ", pathname);
				}
			}
		}
		else {
			status = ztsWriteBlock(ifltab, tss, pathname,
				times, 1, totalToStore,
				values, valueSize,
				quality, qualityElementSize,
				notes, inoteElementSize,
				cnotesToStore, cnotesToStoreLen,
				profileDepths, profileDepthsSize,
				internalHeader,
				userHeader, userHeaderNumber,
				0, logcialNumberData,
				dataType);
		}
	}
	else {
		// All missing
		// Delete record
		if (recordFound == STATUS_RECORD_FOUND) {
			status = zdeleteInternal(ifltab, pathname, 0);
		}
	}

	//  Free any space malloced
	if (timesRead) {
		free(timesRead);
		timesRead = 0;
	}
	if (valuesRead) {
		free(valuesRead);
		valuesRead = 0;
	}
	if (qualityRead) {
		free(qualityRead);
		qualityRead = 0;
	}
	if (notesRead) {
		free(notesRead);
		notesRead = 0;
	}
	if (timesOut) {
		free(timesOut);
		timesOut = 0;
	}
	if (valuesOut) {
		free(valuesOut);
		valuesOut = 0;
	}
	if (qualityOut) {
		free(qualityOut);
		qualityOut = 0;
	}
	if (notesOut) {
		free(notesOut);
		notesOut = 0;
	}
	if (cnotesRead) {
		free(cnotesRead);
		cnotesRead = 0;
	}
	if (cnotesStore) {
		free(cnotesStore);
		cnotesStore = 0;
	}
	if (profileDepthsRead) {
		free(profileDepthsRead);
		profileDepthsRead = 0;
	}

	//  If an error, we'll let the calling function worry about it.
	return status;

}

