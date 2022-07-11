#include <stdio.h>
#include <stdlib.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "zerrorCodes.h"


/**
*  Function:	ztsWriteBlock
*
*  Use:			Private
*
*  Description:	Write a single record of time series data (both regular and irregular interval)
*
*  Declaration:
*				 int ztsWriteBlock (long long *ifltab, zStructTimeSeries *tss, const char* pathname,
*									int *timeArray, int boolUseTimes, int numberValues,
*									int *values, int lengthEachValue,
*									int *quality, int qualityElementSize,
*									int *notes, int inoteElementSize,
*									const char *cnotes, int lengthCNotes,
*									int *profileDepths, int profileDepthsNumber,
*									int *internalHeader, int internalHeaderNumber,
*									int *userHeader, int userHeaderNumber,
*									int totalAllocatedSize, int logicalNumberValues,
*									int dataType);
*
*  Parameters:	long long *ifltab
*					The file table array, similar to a handle number
*
*				const char* pathname
*					The pathname of the record to read.  The pathname must be exact, but is case insensitive
*					(i.e., "Apart" is the same as "APART")
*
*				int *timeArray  (optional)
*					The time array, in minutes or seconds from base date.  The time array
*					is usually used only with irregular-interval data
*
*				int boolUseTimes
*					Set to 1 (one) if the time array is to be stored, zero otherwise
*
*				int numberValues
*					The number of values to store for this block.  If quality or notes are
*					to be stored also, they must have the same number to store.
*
*				int *values
*					A pointer to the values array.  Generally, are floats or doubles.
*					This array must contain at least numberValues number.
*
*				int lengthEachValue
*					The number of words each value element takes; 1 for floats, 2 for doubles (only)
*
*				int *quality (Optional)
*					The quality array, if quality is to be stored.
*
*				int qualityElementSize
*					The number of words for each quality element, either 0 (zero) if no quality is
*					to be stored, or greater than 0.  Typically 1 or 2, but should handle any reasonable number.
*
*				int *notes (Optional)
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
*					The length of the cnote array.  Only numberValues null terminated strings will be stored.
*					If character notes are not to be stored, this should be set to zero.
*
*				int *profileDepths (Optional)
*					If time series profiles are being stored, this is the "depths array".  Each full record has
*					only one depths array, which must be the same for all values.  Use missing flags for periods
*					where a depth is not used.  For example, this might be 0., 5., 10., 15., 20., 25.  If no measurement
*					is made at 25 for a time period, set that value to the missing data flag.
*					Depths are either floats or doubles, as defined by lengthEachValue.  Depths and values
*					must be the same type (float or double).
*
*				int profileDepthsNumber
*					The number of depths in the profileDepths array to store.  For the example above, this would be 6.
*					This number should be the same for all records for this location.  If profiles are not stored, set this to zero.
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
*					The number of int 4 words to write from the user header array.
*
*				int totalAllocatedSize (Optional)
*					If additional space is to be allocated for expanding records, then this is the total amount of space
*					to reserve for this record.  If the default is to be used, set this to zero.
*
*				int logicalNumberValues
*					The number of uncompressed values that the user sees.  For example, if a month of
*					daily profile data is to be stored, this would be 365 to represent the full year, even though
*					only a month is stored.
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
*	Remarks:	This function is used for both regular and irregular interval data.  Its purpose is to pack and write
*					arrays.
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int ztsWriteBlock (long long *ifltab, zStructTimeSeries *tss, const char* pathname,
				 int *timeArray, int boolUseTimes, int numberValues,
				 int *values, int lengthEachValue,
				 int *quality, int qualityElementSize,
				 int *notes, int inoteElementSize,
				 const char *cnotes, int lengthCNotes,
				 int *profileDepths, int profileDepthsNumber,
				 int *internalHeader,
				 int *userHeader, int userHeaderNumber,
				 int totalAllocatedSize, int logicalNumberValues,
				 int dataType)
{

	char messageString[50];
	int i;
	int istat;
	int *dataOut;
	int *header2;
	int numberOut;
	int lenHeader2;
	int internalHeaderNumber;
	int *internalHeaderTemp;
	int numberIn;
	int totalExpandedSize;
	long long bufferControl[4] = { 0, 0, 0, 0 };
	int *buffer = 0;

	zStructTransfer* ztransfer;

	if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsWriteBlock_ID, "Enter,  handle: ", zhandle(ifltab));
		zmessageDebug (ifltab, DSS_FUNCTION_ztsWriteBlock_ID, "Pathname: ",pathname);
	}
	//  Zero pointers to array addresses
	dataOut = 0;
	header2 = 0;
	bufferControl[0] = 0;
	internalHeader[INT_HEAD_timeGranularity] = tss->timeGranularitySeconds;
	internalHeader[INT_HEAD_precision] = tss->precision;
	internalHeader[INT_HEAD_timeOffset] = tss->timeOffsetSeconds;

	//  We never compress time array
	ztsAggregate(ifltab, numberValues, internalHeader,
				timeArray, boolUseTimes,
				values, lengthEachValue,
				quality, qualityElementSize,
				notes, inoteElementSize,
				cnotes, lengthCNotes,
				&dataOut, &numberOut,
				&header2, &lenHeader2);


	if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d, Quality Compression: %d, Notes Compression: %d",
			internalHeader[INT_HEAD_valuesCompressionFlag], internalHeader[INT_HEAD_qualityCompressionFlag], internalHeader[INT_HEAD_inotesCompressionFlag]);
		zmessageDebug(ifltab, DSS_FUNCTION_ztsWriteBlock_ID, "Values Compression: ", messageString);
		numberIn = numberValues * (lengthEachValue + qualityElementSize + inoteElementSize);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d,  Number Out: %d, Length Compression: %d",
			numberIn, numberOut, lenHeader2);
		zmessageDebug(ifltab, DSS_FUNCTION_ztsWriteBlock_ID,  "Number in: ", messageString);
	}

	totalExpandedSize = logicalNumberValues * (lengthEachValue + qualityElementSize + inoteElementSize);
	if (boolUseTimes) {
		//  Add space for the time array
		totalExpandedSize += logicalNumberValues;
	}
	ztransfer = zstructTransferNew(pathname, 0);
	if (!ztransfer) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsWriteBlock_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
								zdssErrorSeverity.MEMORY_ERROR, pathname, "Allocating ztransfer struct");
	}

	internalHeaderTemp = 0;
	internalHeaderNumber = ztsInternalHeaderPack(tss, internalHeader);

	if (bigEndian()) {
		if (lengthEachValue == 1) {
			if (profileDepthsNumber > 0) {
				zswitchInts(profileDepths, profileDepthsNumber);
			}
		}
	}


	if (bigEndian()) {
		//  Don't switch ints on an array still in use
		internalHeaderTemp = calloc(internalHeaderNumber, 4);
		for (i = 0; i < internalHeaderNumber; i++) {
			internalHeaderTemp[i] = internalHeader[i];
		}
		zswitchInts(internalHeaderTemp, INT_HEAD_cnotesLength);
		ztransfer->internalHeader = internalHeaderTemp;
		ztransfer->internalHeaderNumber = internalHeaderNumber;
	}
	else {
		ztransfer->internalHeader = internalHeader;
		ztransfer->internalHeaderNumber = internalHeaderNumber;
	}
	ztransfer->header2 = header2;
	ztransfer->header2Number = lenHeader2;
	ztransfer->userHeader = userHeader;
	ztransfer->userHeaderNumber = userHeaderNumber;
	ztransfer->values1 = dataOut;
	ztransfer->values1Number = numberOut;
	ztransfer->values2 = profileDepths;
	ztransfer->values2Number = profileDepthsNumber;

	ztransfer->numberValues = numberValues;
	ztransfer->logicalNumberValues = logicalNumberValues;
	ztransfer->totalAllocatedSize = totalAllocatedSize;
	ztransfer->totalExpandedSize = totalExpandedSize;
	ztransfer->dataType = dataType;

	istat = zwriteInternal(ifltab, ztransfer, 0, bufferControl, buffer, 0);

	if (dataOut) {
		free(dataOut);
		dataOut = 0;
	}
	if (header2) {
		free(header2);
		header2 = 0;
	}
	if (internalHeaderTemp) {
		free(internalHeaderTemp);
		internalHeaderTemp = 0;
	}

	zstructFree(ztransfer);

	return istat;
}



