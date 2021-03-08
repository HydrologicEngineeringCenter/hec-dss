#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "heclib.h"
#include "zdssKeys.h"
#include "zdssVals.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "zStructTsTimeWindow.h"
#include "zerrorCodes.h"



/**
*  Function:	ztsIrregReadBlock
*
*  Use:			Private
*
*  Description:	Reads either a full irregular-interval block or portion of a block, depending on the time windowk.
*
*  Declaration:
*				int ztsIrregReadBlock(long long *ifltab, const char *pathname, long long *info,
*									  int *buffer, long long bufferControl[4],
*									  int julianStartBlockDate, ztsTimeWindow *timeWindow, int *timeArray,
*									  int *values, int valuesArraySize, int valuesSizeRequested,  int *valueSizeRead,
*									  int *quality, int qualityArraySize, int qualitySizeRequested, int *qualityElementSizeRead,
*									  int *notes, int notesArraySize, int inotesSizeRequested, int *inoteElementSizeRead,
*									  char *cnotes, int sizeCNotesRemaning, int *totalNumberCnotesRead,
*									  int *profileDepths, int profileDepthsArraySize, int *profileDepthsNumberRequested, int profileDepthsSize,
*									  int *internalHeader, int internalHeaderArraySize , int *internalHeaderNumber,
*									  int *userHeader, int userHeaderArraySize, int *userHeaderNumber,
*									  int maxNumberToRead, int *numberRead,
*									  int boolRetrievePrevious, int boolRetrieveSucceeding);
*
*  Parameters:	long long *ifltab
*					The file table array, similar to a handle number
*
*				const char* pathname
*					The pathname of the record to read.  The pathname must be exact, but is case insensitive
*					(i.e., "Apart" is the same as "APART")
*
*				long long *info
*					If the record was just checked or the info block read, then this is the info block read,
*					which means that it does not need to be re-read again.
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
*				int julianStartBlockDate
*					The julian date of the start of the block.  For example, if the data was monthly, this would be
*					day 1 (e.g., March 1) and not the date associated with the first value (not March 31).
*
*				ztsTimeWindow *timeWindow
*					A pointer to a time window struct that identifies the start and end dates/times
*					of the dataset.  This is computed by the calling function
*
*				int *timeArray
*					A pointer to an integer array dimensioned to maxNumberToRead.  This will be returned
*					with the dates/times of each value in seconds past julianStartBlockDate, or minutes past
*					julianStartBlockDate if a century block.
*
*				int *values
*					The array to read the values data into.  Generally, values are floats or doubles.
*					This array must be dimensioned to hold all values.
*
*				int valuesArraySize
*					The size (in ints) of array values.
*
*				int valuesSizeRequested
*					The requested element size of each value in words; generally 1 for floats, 2 for doubles.
*
*				int *valueSizeRead
*					Returns the size of each value read.  Informational only.
*
*				int *quality
*					The array to read quality into.
*
*				int qualityArraySize
*					The size (in ints) of array quality.
*
*				int qualitySizeRequested
*					The requested element size of each quality item, in words.
*
*				int *qualityElementSizeRead
*					Returns the size of each quality element read.  Informational only.
*
*				int *inotes
*					The array to read integer notes into.
*
*				int inotesArraySize
*					The size (in ints) of array notes.
*
*				int inotesSizeRequested
*					The requested element size of each inote, in words.
*
*				int *inoteElementSizeRead
*					Returns the size of each inote element read.  Informational only.
*
*				char *cnotes
*					A character array to read character notes into.
*					Each character note string is identified by a null termination, one per value
*					May not be used in combination with integer notes.
*
*				int sizeCNotesRemaning
*					The size (dimension) of the cnote array (left to fill).
*
*				int *totalNumberCnotesRead (output)
*					The number of bytes read and placed into the cnote array.
*
*				int *profileDepths
*					If time series profiles are being stored, this is the "depths array".  Each full record has
*					only one depths array, which must be the same for all values.  For example, this might
*					be 0., 5., 10., 15., 20., 25.  Use missing flags for periods where a value is not used.
*					If no measurement is made at 25 for a time period, set that value to the missing data flag.
*					Depths are either floats or doubles, as defined by lengthEachValue.  Depths and values
*					must be the same type (float or double).
*
*				int profileDepthsArraySize
*					The size of the profileDepths array in int words.
*
*				int profileDepthsSize
*					The number of profile depths requested.  This is used only when there are none and
*					that number needs to be filled in with missing.
*
*				int *profileDepthsNumberRead
*					The number of profile depths read.  For the example above, this would be 6.
*
*				int *internalHeader
*					The array to read the internal header into
*
*				int internalHeaderArraySize
*					The size of the internal header, in int 4 words.
*
*				int *internalHeaderNumber
*					The number of int words read for the internal header
*
*				int *userHeader
*					The array to read the user header into
*
*				int userHeaderArraySize
*					The size of the user header, in int 4 words.
*
*				int userHeaderNumber
*					The number of int 4 words read into the user header array.
*
*				int maxNumberToRead
*					The maximum number of values to read for this block, which is the dimensions of the
*					values, itimes, arrays.
*
*				int *numberRead
*					Returns the actual number read (e.g., the number in the *itimes array.)
*
*				int boolRetrievePrevious
*					A boolean flag that when set to true (one), indicates the previous value
*					from the start time (in the times struct) in this block should be read and included.
*
*				int boolRetrieveSucceeding
*					A boolean flag that when set to true (one), indicates the value following the last one
*					from the end time (in the times struct) in this block should be read and included.
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
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/



int ztsIrregReadBlock(long long *ifltab, const char *pathname, long long *info,
					  int *buffer, long long bufferControl[4],
					  int julianStartBlockDate, ztsTimeWindow *timeWindow, int *timeArray,
					  int *values, int valuesArraySize, int valuesSizeRequested,  int *valueSizeRead,
					  int *quality, int qualityArraySize, int qualitySizeRequested, int *qualityElementSizeRead,
					  int *notes, int notesArraySize, int inotesSizeRequested, int *inoteElementSizeRead,
					  char *cnotes, int sizeCNotesRemaning, int *totalNumberCnotesRead,
					  int *profileDepths, int profileDepthsArraySize, int *profileDepthsNumberRequested, int profileDepthsSize,
					  int *internalHeader, int internalHeaderArraySize , int *internalHeaderNumber,
					  int *userHeader, int userHeaderArraySize, int *userHeaderNumber,
					  int maxNumberToRead, int *numberRead,
					  int boolRetrievePrevious, int boolRetrieveSucceeding)
{

	int julianFirstValue = 0;
	int minsFirstValue = 0;
	int julianLastValue = 0;
	int minsLastValue = 0;

	int numberStored;
	int numberExpanded;
	int number;
	int size;

	int *timesThisRecord;
	int *valuesRead;
	int *qualityRead;
	int *notesRead;
	char *cnotesRead;

	int blockGranularity;

	int ipos;
	int jpos;
	int kpos;
	int iposData;
	int status;
	int lastValueStatus;
	int boolInRange;
	int i;
	int j;
	int k;
	int cnotesLength;
	int start;
	int end;
	int iposNotes;
	int len;
	int endPos;
	int icount;
	int sizeCNotesRead;
	int julian;
	int itime;

	char messageString[80];
	int lenData;
	int lenQuality;
	int lenNotes;
	zStructTransfer* ztransfer;

	int version;
	int temp;
	long long nada=0;



	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztsIrregReadBlock_ID, "Enter, Pathname: ", pathname);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d;  Max Number to read: %d",
			zhandle(ifltab), maxNumberToRead);
		zmessageDebug(ifltab, DSS_FUNCTION_ztsIrregReadBlock_ID, "Handle: ", messageString);
	}


	//  Was the info block just read?
	if (info[0] == DSS_INFO_FLAG) {
		status = STATUS_RECORD_FOUND;
	}
	else {
		status = zreadInfo (ifltab, pathname, 0);
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_ztsIrregReadBlock_ID);
		}

		if (status == STATUS_RECORD_FOUND) {
			info = (long long *)ifltab[zdssKeys.kinfo];
		}
		else {
			info = &nada;
		}
	}

	valuesRead = 0;
	qualityRead = 0;
	notesRead = 0;
	cnotesRead = 0;
	ztransfer = 0;

	// existing record

	if (status == STATUS_RECORD_FOUND) {
		ztransfer = zstructTransferNew(pathname, 1);
		if (!ztransfer) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_ztsIrregReadBlock_ID,
									zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
									zdssErrorSeverity.MEMORY_ERROR, pathname, "Allocating ztransfer struct");
		}
		ztransfer->info = info;
		ztransfer->values2 = profileDepths;
		ztransfer->values2Mode = profileDepthsArraySize;
		ztransfer->internalHeader = internalHeader;
		ztransfer->internalHeaderMode = internalHeaderArraySize;
		ztransfer->userHeader = userHeader;
		ztransfer->userHeaderMode = userHeaderArraySize;
		ztransfer->header2Mode = 1;

		status = zreadInternal(ifltab, ztransfer, bufferControl, buffer, 1);
		if (zisError(status)) {
			zstructFree(ztransfer);
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_ztsIrregReadBlock_ID);
		}
		
		if ((ztransfer->dataType < DATA_TYPE_ITS) || (ztransfer->dataType >= DATA_TYPE_PD) ||
			(ztransfer->internalHeaderNumber < 5)) {
			status = zerrorProcessing(ifltab, DSS_FUNCTION_ztsIrregReadBlock_ID,
				zdssErrorCodes.WRONG_RECORD_TYPE, DATA_TYPE_ITS,
				(long long)ztransfer->dataType, zdssErrorSeverity.WARNING, ztransfer->pathname, "");
			zstructFree(ztransfer);
			return status;
		}

		if (getEndian()) {
			zswitchInts(ztransfer->internalHeader, INT_HEAD_cnotesLength + 1);
			if (ztransfer->header2Number > 0) {
				zswitchInts(ztransfer->header2, ztransfer->header2Number);
			}
		}

		*internalHeaderNumber = ztransfer->internalHeaderNumber;
		*inoteElementSizeRead = internalHeader[INT_HEAD_inotesElementSize];
		*profileDepthsNumberRequested = ztransfer->values2Number;
		*userHeaderNumber = ztransfer->userHeaderNumber;
		if (*userHeaderNumber > 0) {
			if (userHeaderArraySize == 1) {
				//  space allocated in zread
				userHeader = ztransfer->userHeader;
				ztransfer->allocated[zSTRUCT_userHeader] = 0;
			}
		}

		numberStored = (int)info[zdssInfoKeys.kinfoNumberData];
		if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d, Quality Compression: %d, Notes Compression: %d",
				internalHeader[INT_HEAD_valuesCompressionFlag], internalHeader[INT_HEAD_qualityCompressionFlag], internalHeader[INT_HEAD_inotesCompressionFlag]);
			zmessageDebug(ifltab, DSS_FUNCTION_ztsIrregReadBlock_ID, "Values Compression: ", messageString);
		}

		//  Allocate space for disaggreated values, quailty and notes
		if (valuesSizeRequested > 0) {
			if (profileDepthsSize == 0) {
				size = valuesSizeRequested * maxNumberToRead;
			}
			else {
				size = valuesSizeRequested * maxNumberToRead * profileDepthsSize;
			}
			valuesRead = (int *)calloc((size_t)size, WORD_SIZE);
			if (!valuesRead) {
				zstructFree(ztransfer);
				return zerrorProcessing(ifltab, DSS_FUNCTION_ztsIrregReadBlock_ID,
										zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, size, 0,
										zdssErrorSeverity.MEMORY_ERROR, pathname, "Allocating valuesRead array");
			}
		}
		if (qualitySizeRequested > 0) {
			size = qualitySizeRequested * maxNumberToRead;
			qualityRead = (int *)calloc((size_t)size, WORD_SIZE);
			if (!qualityRead) {
				zstructFree(ztransfer);
				return zerrorProcessing(ifltab, DSS_FUNCTION_ztsIrregReadBlock_ID,
										zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, size, 0,
										zdssErrorSeverity.MEMORY_ERROR, pathname, "Allocating qualityRead array");
			}
		}
		if (inotesSizeRequested > 0) {
			size = inotesSizeRequested * maxNumberToRead;
			notesRead = (int *)calloc((size_t)size, WORD_SIZE);
			if (!notesRead) {
				zstructFree(ztransfer);
				return zerrorProcessing(ifltab, DSS_FUNCTION_ztsIrregReadBlock_ID,
										zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, size, 0,
										zdssErrorSeverity.MEMORY_ERROR, pathname, "Allocating notesRead array");
			}
		}

		if (sizeCNotesRemaning > 0) {
			cnotesLength = internalHeader[INT_HEAD_cnotesLength];
			if (cnotesLength > 0) {
				cnotesRead = (char *)calloc((size_t)cnotesLength, CHAR_SIZE);
				if (!cnotesRead) {
					zstructFree(ztransfer);
					return zerrorProcessing(ifltab, DSS_FUNCTION_ztsIrregReadBlock_ID,
											zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, size, 0,
											zdssErrorSeverity.MEMORY_ERROR, pathname, "Allocating cnotesRead array");
				}
			}
			else {
				*totalNumberCnotesRead = 0;
			}
		}
		else {
			cnotesLength = 0;
		}
	}


	if (status != STATUS_RECORD_FOUND) {
			//  Record does not exist
		if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_USER_DIAG)) {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, ZREAD_NOT_EXIST, zhandle(ifltab));
			zmessage2(ifltab, messageString, pathname);
		}
	}
	else {
		if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_GENERAL)) {
			i8toi4(info[zdssInfoKeys.kinfoTypeVersion], &temp, &version);
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, ZREAD_MESS, zhandle(ifltab), version);
			zmessage2(ifltab, messageString, pathname);
			if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%lld, Path bin: %lld, Info block: %lld, Data Array: %lld",
				ifltab[zdssKeys.kaddTableHash], ifltab[zdssKeys.kpathBinAddress], ifltab[zdssKeys.kaddInfoLastPath], info[zdssInfoKeys.kinfoValues1Address]);
				zmessageDebug(ifltab, DSS_FUNCTION_ztsIrregReadBlock_ID, "Addresses;  table: ", messageString);
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d, Size internal header: %d, Size compression header: %d",
					ztransfer->values1Number, ztransfer->internalHeaderNumber, ztransfer->header2Number);
				zmessageDebug(ifltab, DSS_FUNCTION_ztsIrregReadBlock_ID, "Size data array: ", messageString);
			}
		}
	}


	//  Now disagrate the array using instructions in the internal header
	if (status == STATUS_RECORD_FOUND) {
		*valueSizeRead = internalHeader[INT_HEAD_valueSize];
		*qualityElementSizeRead = internalHeader[INT_HEAD_qualityElementSize];
		*inoteElementSizeRead = internalHeader[INT_HEAD_inotesElementSize];
		iposData = numberStored;  //  remove the times from the disaggreation
		
		status = ztsDisaggregate (ifltab, maxNumberToRead, numberStored, numberStored,
			&numberExpanded, 0, //////  FIX ME   blockStartPosition,
			0, 0,
			ztransfer->values1, ztransfer->values1Number,
			ztransfer->header2, internalHeader,
			valuesRead, valuesArraySize, valuesSizeRequested,
			qualityRead, qualityArraySize, qualitySizeRequested,
			notesRead, notesArraySize, inotesSizeRequested,
			cnotesRead, cnotesLength, &sizeCNotesRead);

		if (zisError(status)) {
			zstructFree(ztransfer);
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_ztsIrregReadBlock_ID);
		}

		//  Time array is at the beginning of values
		timesThisRecord = ztransfer->values1;


		if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1) || zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d, Number stored: %d, Number Expanded: %d",
				maxNumberToRead, numberStored, numberExpanded);
			zmessageDebug(ifltab, DSS_FUNCTION_ztsIrregReadBlock_ID, "Number to read: ", messageString);
			lenData = numberStored * internalHeader[INT_HEAD_valueSize];
			lenQuality = numberStored * internalHeader[INT_HEAD_qualityElementSize];
			lenNotes = numberStored * internalHeader[INT_HEAD_inotesElementSize];
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d, Number each flag: %d, Number each note: %d",
				internalHeader[INT_HEAD_valueSize], internalHeader[INT_HEAD_qualityElementSize], internalHeader[INT_HEAD_inotesElementSize]);
			zmessageDebug(ifltab, DSS_FUNCTION_ztsIrregReadBlock_ID, "Length each value: ", messageString);
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d, Number quality flags: %d, Number notes: %d",
				lenData, lenQuality, lenNotes);
			zmessageDebug(ifltab, DSS_FUNCTION_ztsIrregReadBlock_ID, "Number values array: ", messageString);
		}

		//   times are stored in seconds from block start date, except for the century block
		//  (regardless of the user's time granularity setting)
		if (timeWindow->blockSize < 5)  {
			blockGranularity = SECOND_GRANULARITY;
		}
		else {
			blockGranularity = MINUTE_GRANULARITY;
		}


		//  Now compare times against time window and copy times and data
		//  Disaggregate gave us a full block.  If our time window starts
		//  or ends within this block, we need to return that portion
		ipos = 0;
		lastValueStatus = -1;
		if (boolRetrievePrevious) {
			number = numberExpanded + 1;
		}
		else {
			number = numberExpanded;
		}
		i = -1;
		start = numberExpanded + 1;
		end = -1;
		for (k=0; k<number; k++) {
			i++;
			if (i == numberExpanded)
				break;
			boolInRange = isTimeInRange(timesThisRecord[i], &lastValueStatus, blockGranularity, julianStartBlockDate, timeWindow);
			if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_INTERNAL_DIAG_2) || zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
				zmessageDebugInt(ifltab, DSS_FUNCTION_ztsIrregReadBlock_ID, "boolInRange for time: ", boolInRange);
				zmessageDebugInt(ifltab, DSS_FUNCTION_ztsIrregReadBlock_ID, "Iteration: ", i);
				julian = julianStartBlockDate;
				itime = timesThisRecord[i];
				cleanTime(&julian, &itime, blockGranularity);
				ztsDateMessage(ifltab, DSS_FUNCTION_ztsIrregReadBlock_ID, "Time for this value: ", julian, itime);
			}
			if (boolRetrievePrevious && boolInRange) {
				if (i > 0) i--;
				boolRetrievePrevious = 0;
			}
			if (boolRetrieveSucceeding && !boolInRange && (ipos > 0)) {
				boolInRange = 1;
				boolRetrieveSucceeding = 0;
			}
			if (boolInRange) {
				if (i < start) start = i;
				if (i > end) end = i;
			}
			if (boolInRange) {
				timeArray[ipos] = timesThisRecord[i];
				if (valuesSizeRequested > 0) {
					if (profileDepthsSize == 0) {
						jpos = ipos * valuesSizeRequested;
						kpos = i * valuesSizeRequested;
						for (j=0; j<valuesSizeRequested; j++) {
							values[jpos+j] = valuesRead[kpos+j];
						}
					}
					else {
						jpos = ipos * valuesSizeRequested * profileDepthsSize;
						kpos = i * valuesSizeRequested * profileDepthsSize;
						for (j=0; j<valuesSizeRequested * profileDepthsSize; j++) {
							values[jpos+j] = valuesRead[kpos+j];
						}
					}
				}
				if (qualitySizeRequested > 0) {
					jpos = ipos * qualitySizeRequested;
					kpos = i * qualitySizeRequested;
					for (j=0; j<qualitySizeRequested; j++) {
						quality[jpos+j] = qualityRead[kpos+j];
					}
				}
				if (inotesSizeRequested > 0) {
					jpos = ipos * inotesSizeRequested;
					kpos = i * inotesSizeRequested;
					for (j=0; j<inotesSizeRequested; j++) {
						notes[jpos+j] = notesRead[kpos+j];
					}
				}
				ipos++;
			}
			else {
				if (lastValueStatus > 0) {
					break;
				}
			}
		}
		*numberRead = ipos;
		if (end > -1) {
			iposNotes = 0;
			//  We need to find the location of the first note requested
			if (cnotesLength > 0) {
				if (start > 0) {
					for (i = 0; i < start; i++) {
						len = strnlen_hec(&cnotesRead[iposNotes], (size_t)cnotesLength);
						iposNotes += len + 1;
						cnotesLength -= len + 1;
						if (cnotesLength <= 0) {
							break;
						}
					}
				}
			}
			//  Now, the last position
			if (cnotesLength > 0) {
				endPos = iposNotes;
				for (i=start; i<=end; i++) {
					len = strnlen_hec(&cnotesRead[endPos], (size_t)cnotesLength);
					endPos += len + 1;
					cnotesLength -= len + 1;
					if (cnotesLength <= 0) {
						break;
					}
				}

				//  We have the start and end location of the cnotes requested
				//  Copy into the cnote array
				len = endPos - iposNotes;
				if (len > sizeCNotesRemaning) {
					endPos -= (len - sizeCNotesRemaning);
				}
				icount = 0;
				for (i=iposNotes; i<endPos; i++) {
					cnotes[icount++] = cnotesRead[i];
				}
				*totalNumberCnotesRead = endPos - iposNotes;
			}
		}
	}
	else {
		//  Record not found
		*numberRead = 0;
		*totalNumberCnotesRead = 0;
	}

	if (valuesRead){
		free(valuesRead);
		valuesRead = 0;
	}
	if (qualityRead){
		free(qualityRead);
		qualityRead = 0;
	}
	if (notesRead){
		free(notesRead);
		notesRead = 0;
	}
	if (cnotesRead){
		free(cnotesRead);
		cnotesRead = 0;
	}
	if (ztransfer){
		zstructFree(ztransfer);
		ztransfer = 0;
	}

	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsIrregReadBlock_ID, "Exit, number read: ", *numberRead);
	}

	return status;

}

