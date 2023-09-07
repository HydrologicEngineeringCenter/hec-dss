#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "zdssKeys.h"
#include "zdssVals.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "heclib.h"
#include "zerrorCodes.h"

/**
*  Function:	ztsRegReadBlock
*
*  Use:			Private
*
*  Description:	Read, uncompress and disaggregate a single time series record.  Returns a fully expanded block.
*					For example, if this was daily data and there was only one value on Jan 30,
*					it would return 29 missing, the jan 30 value, then 365-30 missing.
*					Accommodates non-existing records (returns all missing).
*
*  Declaration:
*				int ztsRegReadBlock(long long *ifltab, const char *pathname, int boolExists,
*									int *buffer, long long bufferControl[4],
*									int *values, int valuesArraySize, int valuesSizeRequested, int *valueElementSizeRead,
*									int *quality, int qualityArraySize, int qualitySizeRequested, int *qualityElementSize,
*									int *notes, int notesArraySize, int notesSizeRequested, int *inoteElementSize,
*									char *cnotes, int sizeCNotesRemaning, int *numberCnotes,
*									int *profileDepths, int profileDepthsNumberRequested,
*									int profileDepthsArraySize, int *profileDepthsNumberRead,
*									int *internalHeader, int internalHeaderArraySize, int *internalHeaderNumber,
*									int *userHeader, int userHeaderArraySize, int *userHeaderNumber,
*									int numberToRead, int blockStartPosition);
*
*  Parameters:	long long *ifltab
*					The file table array, similar to a handle number
*
*				const char* pathname
*					The pathname of the record to read.  The pathname must be exact, but is case insensitive
*					(i.e., "Apart" is the same as "APART")
*
*				int boolExists
*					A logical flag indicating if the record exists or not.  If it does not, the
*					arrays are filled with missing flags.  If it does, the record is read and disaggregated.
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
*				int *valueElementSizeRead
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
*				int *qualityElementSize
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
*				int *inoteElementSize
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
*				int *numberCnotes (output)
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
*				int profileDepthsNumberRequested
*					The number of profile depths requested.  This is used only when there are none and
*					that number needs to be filled in with missing.
*
*				int profileDepthsArraySize
*					The size of the profileDepths array in int words.
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
*				int numberToRead
*					The number of values to read for this block.
*
*				int blockStartPosition
*					The position of the first value in the block to start reading at
*
*
*
*
*	Returns:	int status
*					STATUS_OKAY
*					error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*	Remarks:	Arrays are passed, instead of using a struct, so that locations in the arrays
*			         can be specified by calling function (easier).  e.g., &values[ipos]
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int ztsRegReadBlock(long long *ifltab, const char *pathname, int boolExists,
					int *buffer, long long bufferControl[4],
					int *values, int valuesArraySize, int valuesSizeRequested, int *valueElementSizeRead,
					int *quality, int qualityArraySize, int qualitySizeRequested, int *qualityElementSize,
					int *notes, int notesArraySize, int notesSizeRequested, int *inoteElementSize,
					char *cnotes, int sizeCNotesRemaning, int *numberCnotes,
					int *profileDepths, int profileDepthsNumberRequested,
					int profileDepthsArraySize, int *profileDepthsNumberRead,
					int *internalHeader, int internalHeaderArraySize, int *internalHeaderNumber,
					int *userHeader, int userHeaderArraySize, int *userHeaderNumber,
					int numberToRead, int blockStartPosition)
{
	int numberExpanded;
	int numberStored;
	int lenData;
	int lenQuality;
	int lenNotes;
	int positionRelativeFirstValid;
	int positionRelativeLastValid;
	int iposValues;
	int iposQuality;
	int iposNotes;
	int status;
	int i;
	int version;
	int temp;
	float missing;
	long long *info;

	char messageString[80];
	zStructTransfer* ztransfer;

	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztsRegReadBlock_ID, "Enter, Pathname: ", pathname);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d;  Record Exists: %d,  Number to read: %d",
			zhandle(ifltab), boolExists, numberToRead);
		zmessageDebug(ifltab, DSS_FUNCTION_ztsRegReadBlock_ID, "Handle: ", messageString);
	}

	//  If the record exists, read it and disaggregate
	//  If not, fill in data with missing flags	
	ztransfer = zstructTransferNew(pathname, 1);
	if (!ztransfer) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRegReadBlock_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
								zdssErrorSeverity.MEMORY_ERROR, pathname, "Allocating ztransfer struct");
	}
		
	status = zreadInternal(ifltab, ztransfer, bufferControl, buffer, 1);

	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztsRegReadBlock_ID, "Read record, Pathname: ", ztransfer->pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsRegReadBlock_ID, "Status: ", status);
	}
	if (zisError(status)) {
		zstructFree(ztransfer);
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_ztsRegReadBlock_ID);
	}

	if (status == STATUS_OKAY) {
		boolExists = 1;
		info = (long long *)ifltab[zdssKeys.kinfo];

		if ((ztransfer->dataType < DATA_TYPE_RTS) || (ztransfer->dataType >= DATA_TYPE_ITS) ||
			(ztransfer->internalHeaderNumber < 5)) {
			int rval = zerrorProcessing(ifltab, DSS_FUNCTION_ztsRegReadBlock_ID,
				zdssErrorCodes.INVALID_RECORD_HEADER, ztransfer->dataType,
				0, zdssErrorSeverity.WARNING, ztransfer->pathname, "");
			zstructFree(ztransfer);
			return rval;
		}
		if (bigEndian()) {
			zswitchInts(ztransfer->internalHeader, INT_HEAD_cnotesLength + 1);
			if (ztransfer->header2Number > 0) {
				zswitchInts(ztransfer->header2, ztransfer->header2Number);
			}
		}
		*internalHeaderNumber = ztransfer->internalHeaderNumber;
		if (*internalHeaderNumber > internalHeaderArraySize) *internalHeaderNumber = internalHeaderArraySize;
		convertIntArray(ztransfer->internalHeader, internalHeader, *internalHeaderNumber, 1, 1);
		*valueElementSizeRead = internalHeader[INT_HEAD_valueElementSize];
		*qualityElementSize = internalHeader[INT_HEAD_qualityElementSize];
		*inoteElementSize = internalHeader[INT_HEAD_inotesElementSize];
		if ((ztransfer->values2Number > 0) && (profileDepthsArraySize > 0)) {
			if (profileDepthsArraySize == 1) {
				//  space allocated in zread
				*profileDepthsNumberRead = ztransfer->values2Number;
				profileDepths = ztransfer->values2;
				ztransfer->allocated[zSTRUCT_TRANS_values2] = 0;
				ztransfer->values2 = 0;
			}
			else {
				if (profileDepthsArraySize > ztransfer->values2Number) profileDepthsArraySize = ztransfer->values2Number;
				*profileDepthsNumberRead = profileDepthsArraySize;
				convertIntArray(ztransfer->values2, profileDepths, profileDepthsArraySize, 1, 1);
			}
		}
		else {
			*profileDepthsNumberRead = 0;
		}		
		if ((ztransfer->userHeaderNumber > 0) && (userHeaderArraySize > 0) && ztransfer->userHeader && userHeader) {
			if (profileDepthsArraySize == 1) {
				//  space allocated in zread
				*userHeaderNumber = ztransfer->userHeaderNumber;
				userHeader = ztransfer->userHeader;
				ztransfer->allocated[zSTRUCT_userHeader] = 0;
				ztransfer->userHeader = 0;
			}
			else {
				if (userHeaderArraySize > ztransfer->userHeaderNumber) userHeaderArraySize = ztransfer->userHeaderNumber;
				*userHeaderNumber = userHeaderArraySize;
				convertIntArray(ztransfer->userHeader, userHeader, userHeaderArraySize, 1, 1);
			}
		}
		else {
			*userHeaderNumber = 0;
		}
	}
	else {
		boolExists = 0;
		if (ztransfer)
			zstructFree(ztransfer);
		ztransfer = 0;
		status = STATUS_RECORD_NOT_FOUND;
	}


	if (boolExists) {
		if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_GENERAL)) {
			i8toi4(info[zdssInfoKeys.kinfoTypeVersion], &temp, &version);
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, ZREAD_MESS, zhandle(ifltab), version);
			zmessage2(ifltab, messageString, pathname);
			if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%lld, Path bin: %lld, Info block: %lld, Data Array: %lld",
				ifltab[zdssKeys.kaddTableHash], ifltab[zdssKeys.kpathBinAddress], ifltab[zdssKeys.kaddInfoLastPath], info[zdssInfoKeys.kinfoValues1Address]);
				zmessageDebug(ifltab, DSS_FUNCTION_ztsRegReadBlock_ID, "Addresses;  table: ", messageString);

				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d, Length internal header: %d, Length compression header: %d",
					ztransfer->values1Number, ztransfer->internalHeaderNumber, ztransfer->header2Number);
				zmessageDebug(ifltab, DSS_FUNCTION_ztsRegReadBlock_ID, "Length data array: ", messageString);
			}
		}

		//  Now disaggregate the array using instructions in the internal header
		positionRelativeFirstValid = blockStartPosition - internalHeader[INT_HEAD_blockStartPosition];
		positionRelativeLastValid = numberToRead + positionRelativeFirstValid -
			(internalHeader[INT_HEAD_blockEndPosition] - internalHeader[INT_HEAD_blockStartPosition] + 1);
		numberStored = (int)info[zdssInfoKeys.kinfoNumberData];
		if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d, Quality Compression: %d, Notes Compression: %d",
				internalHeader[INT_HEAD_valuesCompressionFlag], internalHeader[INT_HEAD_qualityCompressionFlag], internalHeader[INT_HEAD_inotesCompressionFlag]);
			zmessageDebug(ifltab, DSS_FUNCTION_ztsRegReadBlock_ID, "Values Compression: ", messageString);
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d, position relative to last valid value: %d",
				positionRelativeFirstValid, positionRelativeLastValid);
			zmessageDebug(ifltab, DSS_FUNCTION_ztsRegReadBlock_ID, "Position relative to first valid value: ", messageString);
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d, Number Stored: %d, Block Start Position: %d",
				numberToRead, numberStored, blockStartPosition);
			zmessageDebug(ifltab, DSS_FUNCTION_ztsRegReadBlock_ID, "Number to read: ", messageString);
		}

		int ztsDisaggregateVersion;
		zquery("disa", "", 0, &ztsDisaggregateVersion);
		if (ztsDisaggregateVersion < 0) {
			status = ztsDisaggregateDep(ifltab, numberToRead, numberStored, &numberExpanded,
				blockStartPosition, positionRelativeFirstValid, positionRelativeLastValid,
				ztransfer->values1,
				ztransfer->header2, ztransfer->internalHeader,
				values, valuesArraySize, valuesSizeRequested,
				quality, qualityArraySize, qualitySizeRequested,
				notes, notesArraySize, notesSizeRequested,
				cnotes, sizeCNotesRemaning, numberCnotes);
		}
		else {
			status = ztsDisaggregate(ifltab, numberToRead, numberStored, 0, &numberExpanded,
				blockStartPosition, positionRelativeFirstValid, positionRelativeLastValid,
				ztransfer->values1, ztransfer->values1Number,
				ztransfer->header2, ztransfer->internalHeader,
				values, valuesArraySize, valuesSizeRequested,
				quality, qualityArraySize, qualitySizeRequested,
				notes, notesArraySize, notesSizeRequested,
				cnotes, sizeCNotesRemaning, numberCnotes);
		}
		if (zisError(status)) {
			zstructFree(ztransfer);
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_ztsRegReadBlock_ID);
		}

		if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d, Number stored: %d, Number Expanded: %d",
				numberToRead, numberStored, numberExpanded);
			zmessageDebug(ifltab, DSS_FUNCTION_ztsRegReadBlock_ID, "Number to read: ", messageString);
			lenData = numberToRead * (*valueElementSizeRead);
			lenQuality = numberToRead * (*qualityElementSize);
			lenNotes = numberToRead * (*inoteElementSize);
			zmessageDebug(ifltab, DSS_FUNCTION_ztsRegReadBlock_ID, "Uncompressed: ", "");
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d, Length each flag: %d, length each note: %d",
				(*valueElementSizeRead), (*qualityElementSize), (*inoteElementSize));
			zmessageDebug(ifltab, DSS_FUNCTION_ztsRegReadBlock_ID, "Length each value: ", messageString);
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d, Length quality flags: %d, length notes: %d",
				lenData, lenQuality, lenNotes);
			zmessageDebug(ifltab, DSS_FUNCTION_ztsRegReadBlock_ID, "Length values array: ", messageString);
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d, Number C notes read: %d",
				sizeCNotesRemaning, *numberCnotes);
			zmessageDebug(ifltab, DSS_FUNCTION_ztsRegReadBlock_ID, "Size C notes remaining: ", messageString);
		}
	}
	else {
		//  Record does not exist
		//  Set all the elements to missing
		iposValues = 0;
		iposQuality = 0;
		iposNotes = 0;
		*valueElementSizeRead = 0;
		*qualityElementSize = 0;
		*inoteElementSize = 0;

		for (i=0; i<numberToRead; i++) {
			//  To set missing, we both have to have it defined and want it.
			if (valuesSizeRequested) {
				if (profileDepthsNumberRequested == 0) {
					zsetMissing(&values[iposValues], valuesSizeRequested);
					iposValues += valuesSizeRequested;
				}
				else {
					missing = zmissingFlag();
					fillArray((void *)&missing, 1, &values[iposValues], valuesSizeRequested, profileDepthsNumberRequested);
					iposValues += valuesSizeRequested * profileDepthsNumberRequested;
				}
			}
			//  Undefined in this case is just 0.
			if (qualitySizeRequested) {
				zsetUndefined(&quality[iposQuality], qualitySizeRequested);
				iposQuality += qualitySizeRequested;
			}
			if (notesSizeRequested) {
				zsetUndefined(&notes[iposNotes], notesSizeRequested);
				iposNotes += notesSizeRequested;
			}
			if ((sizeCNotesRemaning > 0) && (iposNotes < sizeCNotesRemaning)) {
				cnotes[iposNotes++] = '\0';
			}
		}
		if (cnotes) {
			*numberCnotes = iposNotes;
		}
		else {
			*numberCnotes = 0;
		}
	}

	if (ztransfer) {
		zstructFree(ztransfer);
	}
	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztsRegReadBlock_ID, "Exit ", "");
	}
	return status;

}

