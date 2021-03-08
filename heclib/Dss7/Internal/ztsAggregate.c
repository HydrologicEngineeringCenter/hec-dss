#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "heclib7.h"
#include "hecdssInternal.h"
#include "zerrorCodes.h"


/**
*  Function:	ztsAggregate
*
*  Use:			Private
*
*  Description:	Aggregate and compress time series data
*
*  Declaration:
*				 int ztsAggregate(long long *ifltab, int numberIn, int *internalHeader,
*								  int *timeArray, int boolUseTimes,
*								  int *values, int valueElementSize,
*								  int *quality, int qualityElementSize,
*								  int *notes, int inoteElementSize,
*								  const char *cnotes, int cnotesLength,
*								  int **arrayOut, int *arrayOutNumber,
*								  int **header2, int *header2Number);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.

*				int numberIn
*					The number of values, which is the same as number in the time array, quality, etc.
*
*				int *internalHeader
*					The internal header.  This will contain compression information for the data.
*
*				int *timeArray
*					If the data is irregular-interval time-series data, then this array must contain
*					time times for each value, in minutes or seconds from the base date.  It must contain
*					numberIn times.  If regular-interval data, this array is ignored.
*
*				int boolUseTimes
*					Set to 1 (one) if the time array is to be stored, zero otherwise.
*
*				int *values
*					The primary data values array.  Generally, are floats or doubles.
*					This array must contain numberIn number.
*
*				int valueElementSize
*					The number of words in each value element; 1 for floats, 2 for doubles (only)
*
*				int *quality
*					The quality array, if quality is to be stored.
*
*				int qualityElementSize
*					The number of words for each quality element, either 0 (zero) if no quality is
*					to be stored, or greater than 0.  Typically 1 or 2, but should handle any reasonable number.
*
*				int *notes
*					If integer notes array, if integer notes are to be stored.  If character notes are to be stored,
*					this must be a dummy array, as only character or integer notes (or neither) can be stored, not both.
*
*				int inoteElementSize
*					The number of words for each integer note element, either 0 (zero) if integer notes are not
*					to be stored, or greater than 0.  Typically 1 or 2, but should handle any reasonable number.
*					Must be zero if character notes are stored.
*
*				const char *cnotes
*					A character array containing a null terminated string for each value, if character notes
*					are to be stored.  Each character note element is identified by a null termination, one per value
*					May not be used in combination with integer notes.
*
*				int cnotesLength
*					The length of the cnote array.  Only numberValues null terminated strings will be stored.
*					If character notes are not to be stored, this should be set to zero.
*
*				int **arrayOut (Output)
*					A pointer to the (compressed) array to store in DSS.  This array is calloced;
*					free() must be called for this after use.
*
*				int *arrayOutNumber (Output)
*					A pointer to the number of values in arrayOut.
*
*				int **header2 (Output)
*					A pointer to the compression header to store.  This will be an array
*					of bits (one bit per value), if compression is used.
*					This array is calloced; free() must be called for this after use.
*
*				int *header2Number (Output)
*					The number of words in the compression header (the int number to store).
*
*
*
*
*	Returns:	int compressionFlag
*					0 - No compression.
*					1 - standard repeat compression
*					2 - all the same value (e.g., 0.0 or missing)
*
*	Remarks:	Compress a data set based on removing sequentially repeated values.
*                 Data set may contain missing flags and may have leading and trailing
*				  missing flags, although convention is to remove those first.
*				  If compression is not reasonable (few or no repeating numbers),
*				  then the data is not compressed and boolean false is returned.
*				  The function has a special flag for all the same values.
*				  This function is used for both regular and irregular interval data.
*
*				The following arrays are aggregated into one array in the following order
*					1.  times (if supplied)
*					2.  data values
*					3.  quality (if supplied)
*					4.  notes (if supplied)
*
*				Each of the arrays must start/end on 8-byte (64 bit) boundaries for binary
*					compatibility across machines.  If they are not, a zero value, which will 
*					be ignored, will be inserted so that they are on 64 bit boundaries.
*					Except, for double data values, which was implemented before this requirement
*					was realized.
*
*					Ints (and floats) have to be swapped on big endian machines.  Doubles and chars are not
*
*	Note:		  This is a more complex function.  Care should be used with any modifications.
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

//  Depreciated versions for pre versions 7-HA
int ztsAggregateDep(long long *ifltab, int numberIn, int *internalHeader,
	int *timeArray, int boolUseTimes,
	int *values, int valueElementSize,
	int *quality, int qualityElementSize,
	int *notes, int inoteElementSize,
	const char *cnotes, int cnotesLength,
	int **arrayOut, int *arrayOutNumber,
	int **header2, int *header2Number);



int ztsAggregate(long long *ifltab, int numberIn, int *internalHeader,
				  int *timeArray, int boolUseTimes,
				  int *values, int valueElementSize,
				  int *quality, int qualityElementSize,
				  int *notes, int inoteElementSize,
				  const char *cnotes, int cnotesLength,
				  int **arrayOut, int *arrayOutNumber,
				  int **header2, int *header2Number)
{
	int valueRepeatNumber;
	int qualityRepeatNumber;
	int inotesRepeatNumber;
	int compressionHeaderNumber;
	int compressValues;
	int compressQuality;
	int compressNotes;
	int valuesNumber;
	int qualityNumber;
	int inotesNumber;
	int header2Count;
	int dataCompressedOutCount;
	int number;
	int compressionFlag;

	float ratio;
	int i;
	int j;
	int k;
	int n;
	int ipos;
	int iposComp;
	int same;
	int istart;

	int *dataCompressedOut;
	int *compressHeader;
	int dataCompressedOutNumber;

	int timesNumber;
	int valuesUncompressedNumber;
	int qualityUncompressedNumber;
	int inotesUncompressedNumber;
/*
	Internal Header Definition

	int internalHeader[INT_HEAD_SIZE];

	*  Time series internal header:
	*			INT_HEAD_precision				1
	*           INT_HEAD_timeOffset				2
	*           INT_HEAD_profileDepthsNumber	3
	*           INT_HEAD_blockStartPosition		4
	*           INT_HEAD_blockEndPosition		5
	*           INT_HEAD_valuesNumber			6		//  Size of values area in data array (big number)
	*           INT_HEAD_valueSize				7		//  Length of each value  (double = 2, profile = 12)
	*           INT_HEAD_valueElementSize		8		//  Length of each element  (double = 2, profile = 2)
	*           INT_HEAD_valuesCompressionFlag	9
	*           INT_HEAD_qualityNumber			10
	*           INT_HEAD_qualityElementSize		11
	*           INT_HEAD_qualityCompressionFlag 12
	*           INT_HEAD_inotesNumber			13
	*           INT_HEAD_inotesElementSize		14
	*           INT_HEAD_inotesCompressionFlag	15
	*           INT_HEAD_cnotesLength			16
	*           INT_HEAD_units					17

	Compression flags:
		0 = No compression
		1 = Compression based on repeat values (with repeat bit saved in header)
		2 = All repeats

*/

	if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsAggregate_ID, "Enter, numberIn: ", numberIn);
	}

	//  Before 7-HA?
	if (zinquire(ifltab, "fver") < 77265) {
		return ztsAggregateDep(ifltab, numberIn, internalHeader,
			timeArray, boolUseTimes,
			values, valueElementSize,
			quality, qualityElementSize,
			notes, inoteElementSize,
			cnotes, cnotesLength,
			arrayOut, arrayOutNumber,
			header2, header2Number);
	}

	//  Note: everything is in single ints, not doubles or otherwise
	//  The compression header array contains the following:
	//  header2[0] = compression information for data array, if compressed
	//  header2[((numberIn -1) / 32) + 1] = compression information for quality array, if compressed
	//  If values is not compressed, quality compression information starts in [0]



	//  Fill in the default header
	valuesUncompressedNumber = numberIn * valueElementSize;
	qualityUncompressedNumber = numberIn * qualityElementSize;
	inotesUncompressedNumber = numberIn * inoteElementSize;

	if (boolUseTimes) {
		timesNumber = numberIn;
	}
	else {
		timesNumber = 0;
	}
	if ((cnotesLength > 0) && (inoteElementSize == 0)){
		inotesUncompressedNumber = (cnotesLength -1) / 4 + 1;
	}

	internalHeader[INT_HEAD_valueSize] = valueElementSize;
	internalHeader[INT_HEAD_valuesCompressionFlag] = 0;
	
	internalHeader[INT_HEAD_qualityElementSize] = qualityElementSize;
	internalHeader[INT_HEAD_qualityCompressionFlag] = 0;
	
	internalHeader[INT_HEAD_inotesElementSize] = inoteElementSize;
	internalHeader[INT_HEAD_inotesCompressionFlag] = 0;
	internalHeader[INT_HEAD_cnotesLength] = cnotesLength;
	dataCompressedOut = 0;
	compressHeader = 0;

	//  Set default length
	dataCompressedOutCount = timesNumber + valuesUncompressedNumber + qualityUncompressedNumber + inotesUncompressedNumber;
	if (dataCompressedOutCount <= 0) {
		if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_ztsAggregate_ID, "Exit ztsAggregate Loc 1: dataCompressedOutCount: ", dataCompressedOutCount);
		}
		return 0;
	}
	//  Add 4 in case we are not going to be on 8 byte boundaries
	dataCompressedOutCount += 4;

	compressValues = 0;
	compressQuality = 0;
	compressNotes = 0;

	//  Don't bother compressing if number is small
	//  However, we must fill in the header and order
	//  all the data sets into a single array...
	//  Don't compress profiles either

	if ((numberIn > 12) && (internalHeader[INT_HEAD_profileDepthsNumber] == 0)) {

		//  Determine if we want to compress data, and if so, using what method
		//  Count the number of repeat values
		valueRepeatNumber = 0;
		for (i = 1; i<numberIn; i++) {
			same = 1;
			for (j = 0; j<valueElementSize; j++) {
				k = (i * valueElementSize) + j;
				n = ((i - 1) * valueElementSize) + j;
				if (values[k] != values[n]) {
					same = 0;
					break;
				}
			}
			if (same) {
				valueRepeatNumber++;
			}
		}

		//  Count the number of quality flag repeats
		qualityRepeatNumber = 0;
		if (qualityElementSize > 0) {
			for (i = 1; i<numberIn; i++) {
				same = 1;
				for (j = 0; j<qualityElementSize; j++) {
					k = (i * qualityElementSize) + j;
					n = ((i - 1) * qualityElementSize) + j;
					if (quality[k] != quality[n]) {
						same = 0;
						break;
					}
				}
				if (same) {
					qualityRepeatNumber++;
				}
			}
		}

		//  Count the number of note repeats
		inotesRepeatNumber = 0;
		if (inoteElementSize > 0) {
			for (i = 1; i<numberIn; i++) {
				same = 1;
				for (j = 0; j<inoteElementSize; j++) {
					k = (i * inoteElementSize) + j;
					n = ((i - 1) * inoteElementSize) + j;
					if (notes[k] != notes[n]) {
						same = 0;
						break;
					}
				}
				if (same) {
					inotesRepeatNumber++;
				}
			}
		}

		//  Is it really advantageous to compress this data set?
		//  valueRepeatNumber has to be > 5% to be useful
		ratio = (float)valueRepeatNumber / (float)numberIn;
		if (ratio > 0.05) {
			compressValues = 1;
		}
		if (qualityElementSize) {
			ratio = (float)qualityRepeatNumber / (float)numberIn;
			if (ratio > 0.10) {
				compressQuality = 1;
			}
		}
		if (inoteElementSize) {
			ratio = (float)inotesRepeatNumber / (float)numberIn;
			if (ratio > 0.10) {
				compressNotes = 1;
			}
		}
	}

	//  If we are not going to compress anything, aggragate data and return
	if (!compressValues && !compressQuality && !compressNotes) {
		//  Combine all data sets into one array
		dataCompressedOut = (int*)calloc((size_t)dataCompressedOutCount, WORD_SIZE);
		if (!dataCompressedOut) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStoreReg_ID,
									zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, dataCompressedOutCount, 0,
									zdssErrorSeverity.MEMORY_ERROR, "", "Data output array");
		}
		ipos = 0;
		if (timesNumber) {
			for (i=0; i<timesNumber; i++) {
				assert(ipos < dataCompressedOutCount);
				dataCompressedOut[ipos++] = timeArray[i];
			}
		}
		if (valueElementSize) {
			number = numberIn * valueElementSize;
			internalHeader[INT_HEAD_valuesNumber] = number;
			for (i=0; i<number; i++) {
				assert(ipos < dataCompressedOutCount);
				dataCompressedOut[ipos++] = values[i];
			}
		}
		else {
			internalHeader[INT_HEAD_valuesNumber] = 0;
		}
		//  Make sure we stay on 8-byte boundaries
		//  We don't need to do this between the times and values array,
		//  as this is taken care of in disaggregate
		if (isOdd(ipos)) {
			dataCompressedOut[ipos++] = 0;
		}
		if (getEndian()) {
			//  For Solaris (big endian), we need to swap for binary compatibility with little endian
			//  This will swap both values and times, not quality or notes
			zswitchDoubles(dataCompressedOut, ipos, valueElementSize,  1, timesNumber);
		}

		if (qualityElementSize) {
			istart = ipos;
			number = numberIn * qualityElementSize;
			internalHeader[INT_HEAD_qualityNumber] = number;
			for (i = 0; i < number; i++) {
				assert(ipos < dataCompressedOutCount);
				dataCompressedOut[ipos++] = quality[i];
			}
			if (isOdd(ipos)) {
				dataCompressedOut[ipos++] = 0;
				number++;
			}
			if (getEndian()) {
				zswitchInts((void *)&dataCompressedOut[istart], number);
			}
		}
		else {
			internalHeader[INT_HEAD_qualityNumber] = 0;
		}

		if (inoteElementSize) {
			istart = ipos;
			number = numberIn * inoteElementSize;
			internalHeader[INT_HEAD_inotesNumber] = number;
			for (i=0; i<number; i++) {
				assert(ipos < dataCompressedOutCount);
				dataCompressedOut[ipos++] = notes[i];
			}
			if (isOdd(ipos)) {
				dataCompressedOut[ipos++] = 0;
				number++;
			}
			if (getEndian()) {
				zswitchInts((void *)&dataCompressedOut[istart], number);
			}
		}
		else {
			internalHeader[INT_HEAD_inotesNumber] = 0;
		}
	    if ((cnotesLength > 0) && (inotesUncompressedNumber > 0)) {
			internalHeader[INT_HEAD_inotesNumber] = inotesUncompressedNumber;
			charLong((void *)cnotes, (void *)&dataCompressedOut[ipos], cnotesLength, (inotesUncompressedNumber * 4), 1, -1);
			//charInt((void *)cnotes, &dataCompressedOut[ipos], cnotesLength, (inotesUncompressedNumber * 4), 1, isOdd(ipos));
			//  (We don't swap characters)
			ipos += numberIntsInBytes(cnotesLength);
			assert(ipos <= dataCompressedOutCount);
			if (isOdd(ipos)) {
				ipos++;
				//dataCompressedOut[ipos++] = 0;
			}
		}
		*arrayOut = dataCompressedOut;
		*header2 = 0;
		*arrayOutNumber = ipos;
		*header2Number = 0;
		if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_ztsAggregate_ID, "Exit ztsAggregate Loc 1: No compression needed. arrayOutNumber: ", ipos);
		}
		return 0;
	}


	//  We are going to compress the data.  Check if all the same (special compression)

	//  If all the same value, set the compression status flag to "2"
	compressionFlag = 0;
	if ((valueRepeatNumber+1) == numberIn) {
		//  No quality or notes?
		if ((qualityElementSize == 0) && (inoteElementSize == 0) && (cnotesLength == 0)) {
			//  Most common case, just do this and return
			compressionFlag = 2;
			dataCompressedOutCount = timesNumber + valueElementSize;
			internalHeader[INT_HEAD_valueSize] = valueElementSize;
			internalHeader[INT_HEAD_valuesCompressionFlag] = 2;
			internalHeader[INT_HEAD_valuesNumber] = valueElementSize;
			internalHeader[INT_HEAD_qualityNumber] = 0;
			internalHeader[INT_HEAD_inotesNumber] = 0;
			//  Add an additional word to calloc in case we need to swap on big endian
			dataCompressedOut = (int*)calloc((size_t)(dataCompressedOutCount + 1), WORD_SIZE);
			if (!dataCompressedOut) {
				return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStoreReg_ID,
										zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, dataCompressedOutCount, 0,
										zdssErrorSeverity.MEMORY_ERROR, "", "Data output array");
			}
			ipos = 0;
			if (timesNumber) {
				for (i=0; i<timesNumber; i++) {
					assert(ipos < dataCompressedOutCount);
					dataCompressedOut[ipos++] = timeArray[i];
				}
			}
			for (i=0; i<valueElementSize; i++) {
				dataCompressedOut[ipos++] = values[i];
			}
			// Make sure we stay on 8 - byte boundaries
			if (isOdd(ipos)) {
				dataCompressedOut[ipos++] = 0;
			}
			if (getEndian()) {
				//  For Solaris (big endian), we need to swap for binary compatibility with little endian
				zswitchDoubles(dataCompressedOut, ipos, valueElementSize,  1, timesNumber);
			}
			*arrayOut = dataCompressedOut;
			*arrayOutNumber = ipos;
			*header2Number = 0;
			*header2 = 0;
			if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
				zmessageDebugInt(ifltab, DSS_FUNCTION_ztsAggregate_ID, "Exit ztsAggregate early: all repeats. arrayOutNumber: ", ipos);
				zmessageDebugInt(ifltab, DSS_FUNCTION_ztsAggregate_ID, "CompressionFlag: ", compressionFlag);
				zmessageDebugInt(ifltab, DSS_FUNCTION_ztsAggregate_ID, "First value as int in array out: ", dataCompressedOut[0]);
			}
			return compressionFlag;
		}
	}



	//  At this point we will be doing a full compression
	//  Note: everything is in single ints, not doubles or otherwise
	//  The compression header array contains the following:
	//  header2[0] = compression information for data array, if compressed
	//  header2[((numberIn -1) / 32) + 1] = compression information for quality array, if compressed
	//  If values is not compressed, quality compression information starts in [0]


	//  Calculate the length of the data array and compression header
	compressionHeaderNumber = ((numberIn -1) / 32) + 1;
	header2Count = (compressValues + compressQuality + compressNotes) * compressionHeaderNumber;
	compressHeader = (int*)calloc((size_t)header2Count, WORD_SIZE);
	if (!compressHeader) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStoreReg_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, header2Count, 0,
								zdssErrorSeverity.MEMORY_ERROR, "", "compression array");
	}
	if (compressValues) {
		if ((valueRepeatNumber+1) == numberIn) {
			valuesNumber = valueElementSize;
			internalHeader[INT_HEAD_valuesCompressionFlag] = 2;
		}
		else {
			valuesNumber = (numberIn - valueRepeatNumber) * valueElementSize;
			internalHeader[INT_HEAD_valuesCompressionFlag] = 1;
		}
	}
	else {
		valuesNumber = valuesUncompressedNumber;
	}
	internalHeader[INT_HEAD_valuesNumber] = valuesNumber;

	if (compressQuality) {
		if ((qualityRepeatNumber+1) == numberIn) {
			qualityNumber = qualityElementSize;
			internalHeader[INT_HEAD_qualityCompressionFlag] = 2;
		}
		else {
			qualityNumber = (numberIn - qualityRepeatNumber) * qualityElementSize;
			internalHeader[INT_HEAD_qualityCompressionFlag] = 1;
		}
	}
	else {
		qualityNumber = qualityUncompressedNumber;
	}
	internalHeader[INT_HEAD_qualityNumber] = qualityNumber;

	if (compressNotes) {
		if ((inotesRepeatNumber+1) == numberIn) {
			inotesNumber = inoteElementSize;
			internalHeader[INT_HEAD_inotesCompressionFlag] = 2;
		}
		else {
			inotesNumber = (numberIn - inotesRepeatNumber) * inoteElementSize;
			internalHeader[INT_HEAD_inotesCompressionFlag] = 1;
		}		
	}
	else {
		inotesNumber = inotesUncompressedNumber;
	}
	internalHeader[INT_HEAD_inotesNumber] = inotesNumber;

	dataCompressedOutNumber = timesNumber + valuesNumber + qualityNumber + inotesNumber;
	//  Add 4 in case we are not going to be on 8 byte boundaries
	dataCompressedOutNumber += 4;

	//  Allocate space for them
	dataCompressedOut = (int*)calloc((size_t)dataCompressedOutNumber, WORD_SIZE);
	if (!dataCompressedOut) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStoreReg_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, dataCompressedOutNumber, 0,
								zdssErrorSeverity.MEMORY_ERROR, "", "output array");
	}

	ipos = 0;
	if (timesNumber) {
		for (i=0; i<timesNumber; i++) {
			assert(ipos < dataCompressedOutCount);
			dataCompressedOut[ipos++] = timeArray[i];
		}
	}
	iposComp = 0;
	if (compressValues)  {
		if (internalHeader[INT_HEAD_valuesCompressionFlag] == 1) {
			/////    FIX ME - These need to be ipos + length used in repeat compress!!!!!!
			assert(ipos < dataCompressedOutCount);
			assert(iposComp < header2Count);
			ztsRegRepeatCompress(values, numberIn, valueElementSize, &dataCompressedOut[ipos], dataCompressedOutCount, 
				&compressHeader[iposComp], header2Count);
			ipos += (numberIn - valueRepeatNumber) * valueElementSize;
			iposComp += compressionHeaderNumber;
		}
		else {
			//  All the same
			for (i=0; i<valueElementSize; i++) {
				assert(ipos < dataCompressedOutCount);
				dataCompressedOut[ipos++] = values[i];
			}
		}
	}
	else {
		for (i=0; i<valuesUncompressedNumber; i++) {
			assert(ipos < dataCompressedOutCount);
			dataCompressedOut[ipos++] = values[i];
		}
	}
	//  Make sure we stay on 8-byte boundaries
	if (isOdd(ipos)) {
		dataCompressedOut[ipos++] = 0;
	}
	if (getEndian()) {
		//  For Solaris (big endian), we need to swap for binary compatibility with little endian
		//  This will swap both values and times, not quality or notes
		zswitchDoubles(dataCompressedOut, ipos, valueElementSize, 1, timesNumber);
	}
	istart = ipos;
	if (compressQuality)  {
		if (internalHeader[INT_HEAD_qualityCompressionFlag] == 1) {
			//  FIX here, as above
			assert(ipos < dataCompressedOutCount);
			assert(iposComp < header2Count);
			ztsRegRepeatCompress(quality, numberIn, qualityElementSize, &dataCompressedOut[ipos], dataCompressedOutCount, 
				&compressHeader[iposComp], header2Count);
			ipos += (numberIn - qualityRepeatNumber) * qualityElementSize;
			iposComp += compressionHeaderNumber;
		}
		else {
			//  All the same
			for (i=0; i<qualityElementSize; i++) {
				assert(ipos < dataCompressedOutCount);
				dataCompressedOut[ipos++] = quality[i];
			}
		}
	}
	else if (qualityUncompressedNumber > 0) {		
		for (i=0; i<qualityUncompressedNumber; i++) {
			assert(ipos < dataCompressedOutCount);
			dataCompressedOut[ipos++] = quality[i];
		}
	}
	if (qualityUncompressedNumber > 0) {
		if (isOdd(ipos)) {
			dataCompressedOut[ipos++] = 0;
		}
		if (isOdd(istart)) {
			istart++;
		}
		if (getEndian()) {
			number = ipos - istart;
			zswitchInts((void *)&dataCompressedOut[istart], number);
		}
	}
	istart = ipos;
	if (compressNotes)  {
		if (internalHeader[INT_HEAD_inotesCompressionFlag] == 1) {
			//  fix, as above
			assert(ipos < dataCompressedOutCount);
			assert(iposComp < header2Count);
			ztsRegRepeatCompress(notes, numberIn, inoteElementSize, &dataCompressedOut[ipos], dataCompressedOutCount, 
				&compressHeader[iposComp], header2Count);
			ipos += (numberIn - inotesRepeatNumber) * inoteElementSize;
		}
		else {
			//  All the same
			for (i=0; i<inoteElementSize; i++) {
				assert(ipos < dataCompressedOutCount);
				dataCompressedOut[ipos++] = notes[i];
			}
		}
	}
	else if ((inotesUncompressedNumber > 0) && (inoteElementSize > 0)) {
		for (i = 0; i<inotesUncompressedNumber; i++) {
			assert(ipos < dataCompressedOutCount);
			dataCompressedOut[ipos++] = notes[i];
		}
	}
	if (inotesUncompressedNumber > 0) {
		if (isOdd(ipos)) {
			dataCompressedOut[ipos++] = 0;
		}
		if (isOdd(istart)) {
			istart++;
		}
		if (getEndian()) {
			number = ipos - istart;
			zswitchInts((void *)&dataCompressedOut[istart], number);
		}
	}
	if ((cnotesLength > 0) && (inoteElementSize == 0)) {
		//  We have a character string to store
		charLong((void *)cnotes, (void *)&dataCompressedOut[ipos], cnotesLength, (inotesUncompressedNumber * 4), 1, -1);
		//charInt((void *)cnotes, &dataCompressedOut[ipos], cnotesLength, (inotesUncompressedNumber * 4), 1, 1, isOdd(ipos));
		//  (We don't swap characters)
		ipos += numberIntsInBytes(cnotesLength);
		assert(ipos <= dataCompressedOutCount);
		if (isOdd(ipos)) {
			ipos++;
			//dataCompressedOut[ipos++] = 0;
		}
		assert(ipos <= dataCompressedOutCount);
	}
	

	//  Done.  Return arrays and lengths.
	*arrayOut = dataCompressedOut;
	*arrayOutNumber = ipos;
	if (getEndian()) {
		zswitchInts(compressHeader, header2Count);
	}
	*header2 = compressHeader;
	*header2Number = header2Count;

	if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsAggregate_ID, "Exit, arrayOutNumber: ", *arrayOutNumber);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsAggregate_ID, "CompressionFlag: ", compressionFlag);
	}

	return 1;
}

//  Depreciated versions for pre versions 7-HA
int ztsAggregateDep(long long *ifltab, int numberIn, int *internalHeader,
	int *timeArray, int boolUseTimes,
	int *values, int valueElementSize,
	int *quality, int qualityElementSize,
	int *notes, int inoteElementSize,
	const char *cnotes, int cnotesLength,
	int **arrayOut, int *arrayOutNumber,
	int **header2, int *header2Number)
{
	int valueRepeatNumber;
	int qualityRepeatNumber;
	int inotesRepeatNumber;
	int compressionHeaderNumber;
	int compressValues;
	int compressQuality;
	int compressNotes;
	int valuesNumber;
	int qualityNumber;
	int inotesNumber;
	int header2Count;
	int dataCompressedOutCount;
	int number;
	int compressionFlag;

	float ratio;
	int i;
	int j;
	int k;
	int n;
	int ipos;
	int iposComp;
	int same;

	int *dataCompressedOut;
	int *compressHeader;
	int dataCompressedOutNumber;

	int timesNumber;
	int valuesUncompressedNumber;
	int qualityUncompressedNumber;
	int inotesUncompressedNumber;
	/*
	Internal Header Definition

	int internalHeader[INT_HEAD_SIZE];

	*  Time series internal header:
	*			INT_HEAD_precision				1
	*           INT_HEAD_timeOffset				2
	*           INT_HEAD_profileDepthsNumber	3
	*           INT_HEAD_blockStartPosition		4
	*           INT_HEAD_blockEndPosition		5
	*           INT_HEAD_valuesNumber			6		//  Size of values area in data array (big number)
	*           INT_HEAD_valueSize				7		//  Length of each value  (double = 2, profile = 12)
	*           INT_HEAD_valueElementSize		8		//  Length of each element  (double = 2, profile = 2)
	*           INT_HEAD_valuesCompressionFlag	9
	*           INT_HEAD_qualityNumber			10
	*           INT_HEAD_qualityElementSize		11
	*           INT_HEAD_qualityCompressionFlag 12
	*           INT_HEAD_inotesNumber			13
	*           INT_HEAD_inotesElementSize		14
	*           INT_HEAD_inotesCompressionFlag	15
	*           INT_HEAD_cnotesLength			16
	*           INT_HEAD_units					17
	*/

	//  Note: everything is in single ints, not doubles or otherwise
	//  The compression header array contains the following:
	//  header2[0] = compression information for data array, if compressed
	//  header2[((numberIn -1) / 32) + 1] = compression information for quality array, if compressed
	//  If values is not compressed, quality compression information starts in [0]



	//  Fill in the default header
	valuesUncompressedNumber = numberIn * valueElementSize;
	qualityUncompressedNumber = numberIn * qualityElementSize;
	inotesUncompressedNumber = numberIn * inoteElementSize;

	if (boolUseTimes) {
		timesNumber = numberIn;
	}
	else {
		timesNumber = 0;
	}
	if ((cnotesLength > 0) && (inoteElementSize == 0)) {
		inotesUncompressedNumber = (cnotesLength - 1) / 4 + 1;
	}
	internalHeader[INT_HEAD_valuesNumber] = valuesUncompressedNumber;

	internalHeader[INT_HEAD_valueSize] = valueElementSize;
	internalHeader[INT_HEAD_valuesCompressionFlag] = 0;
	internalHeader[INT_HEAD_qualityNumber] = qualityUncompressedNumber;
	internalHeader[INT_HEAD_qualityElementSize] = qualityElementSize;
	internalHeader[INT_HEAD_qualityCompressionFlag] = 0;
	internalHeader[INT_HEAD_inotesNumber] = inotesUncompressedNumber;
	internalHeader[INT_HEAD_inotesElementSize] = inoteElementSize;
	internalHeader[INT_HEAD_inotesCompressionFlag] = 0;
	internalHeader[INT_HEAD_cnotesLength] = cnotesLength;
	dataCompressedOut = 0;
	compressHeader = 0;

	//  Set default length
	dataCompressedOutCount = timesNumber + valuesUncompressedNumber + qualityUncompressedNumber + inotesUncompressedNumber;
	if (dataCompressedOutCount <= 0) {
		return 0;
	}

	//  Don't bother compressing if number is small
	//  However, we must fill in the header and order
	//  all the data sets into a single array...
	//  Don't compress profiles either
	if ((numberIn < 12) || (internalHeader[INT_HEAD_profileDepthsNumber] > 0)) {
		//  Combine all data sets into one array
		dataCompressedOut = (int*)calloc((size_t)dataCompressedOutCount, WORD_SIZE);
		if (!dataCompressedOut) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStoreReg_ID,
				zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, dataCompressedOutCount, 0,
				zdssErrorSeverity.MEMORY_ERROR, "", "Data output array");
		}
		ipos = 0;
		if (timesNumber) {
			for (i = 0; i<timesNumber; i++) {
				assert(ipos < dataCompressedOutCount);
				dataCompressedOut[ipos++] = timeArray[i];
			}
		}
		if (valueElementSize) {
			number = numberIn * valueElementSize;
			for (i = 0; i<number; i++) {
				assert(ipos < dataCompressedOutCount);
				dataCompressedOut[ipos++] = values[i];
			}
		}
		if (qualityElementSize) {
			number = numberIn * qualityElementSize;
			for (i = 0; i<number; i++) {
				assert(ipos < dataCompressedOutCount);
				dataCompressedOut[ipos++] = quality[i];
			}
		}
		if (inoteElementSize) {
			number = numberIn * inoteElementSize;
			for (i = 0; i<number; i++) {
				assert(ipos < dataCompressedOutCount);
				dataCompressedOut[ipos++] = notes[i];
			}
		}
		else if ((cnotesLength > 0) && (inotesUncompressedNumber > 0)) {
			charLong((void *)cnotes, (void *)&dataCompressedOut[ipos], cnotesLength, (inotesUncompressedNumber * 4), 1, -1);
			//charInt((void *)cnotes, &dataCompressedOut[ipos], cnotesLength, (inotesUncompressedNumber * 4), 1, isOdd(ipos));
			if (getEndian()) {
				zswitchInts((void *)&dataCompressedOut[ipos], inotesUncompressedNumber);
			}
			ipos += inotesUncompressedNumber;
			assert(ipos <= dataCompressedOutCount);
		}
		*arrayOut = dataCompressedOut;
		*header2 = 0;
		*arrayOutNumber = dataCompressedOutCount;
		*header2Number = 0;
		return 0;
	}


	//  Count the number of repeat values
	valueRepeatNumber = 0;
	for (i = 1; i<numberIn; i++) {
		same = 1;
		for (j = 0; j<valueElementSize; j++) {
			k = (i * valueElementSize) + j;
			n = ((i - 1) * valueElementSize) + j;
			if (values[k] != values[n]) {
				same = 0;
				break;
			}
		}
		if (same) {
			valueRepeatNumber++;
		}
	}

	//  Count the number of quality flag repeats
	qualityRepeatNumber = 0;
	if (qualityElementSize > 0) {
		for (i = 1; i<numberIn; i++) {
			same = 1;
			for (j = 0; j<qualityElementSize; j++) {
				k = (i * qualityElementSize) + j;
				n = ((i - 1) * qualityElementSize) + j;
				if (quality[k] != quality[n]) {
					same = 0;
					break;
				}
			}
			if (same) {
				qualityRepeatNumber++;
			}
		}
	}

	//  Count the number of note repeats
	inotesRepeatNumber = 0;
	if (inoteElementSize > 0) {
		for (i = 1; i<numberIn; i++) {
			same = 1;
			for (j = 0; j<inoteElementSize; j++) {
				k = (i * inoteElementSize) + j;
				n = ((i - 1) * inoteElementSize) + j;
				if (notes[k] != notes[n]) {
					same = 0;
					break;
				}
			}
			if (same) {
				inotesRepeatNumber++;
			}
		}
	}

	//  If all the same value, set the compression status flag to "2"
	compressionFlag = 0;
	if ((valueRepeatNumber + 1) == numberIn) {
		//  No quality or notes?
		if ((qualityElementSize == 0) && (inoteElementSize == 0) && (cnotesLength == 0)) {
			//  Most common case, just do this and return
			compressionFlag = 2;
			dataCompressedOutCount = timesNumber + valueElementSize;
			internalHeader[INT_HEAD_valueSize] = valueElementSize;
			internalHeader[INT_HEAD_valuesCompressionFlag] = 2;
			dataCompressedOut = (int*)calloc((size_t)dataCompressedOutCount, WORD_SIZE);
			if (!dataCompressedOut) {
				return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStoreReg_ID,
					zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, dataCompressedOutCount, 0,
					zdssErrorSeverity.MEMORY_ERROR, "", "Data output array");
			}
			ipos = 0;
			if (timesNumber) {
				for (i = 0; i<timesNumber; i++) {
					assert(ipos < dataCompressedOutCount);
					dataCompressedOut[ipos++] = timeArray[i];
				}
			}
			for (i = 0; i<valueElementSize; i++) {
				dataCompressedOut[ipos++] = values[i];
			}
			*arrayOut = dataCompressedOut;
			*arrayOutNumber = dataCompressedOutCount;
			*header2Number = 0;
			*header2 = 0;
			return compressionFlag;
		}
	}

	compressValues = 0;
	compressQuality = 0;
	compressNotes = 0;

	//  Is it really advantageous to compress this data set?
	//  valueRepeatNumber has to be > 5% to be useful
	ratio = (float)valueRepeatNumber / (float)numberIn;
	if (ratio > 0.05) {
		compressValues = 1;
	}

	if (qualityElementSize) {
		ratio = (float)qualityRepeatNumber / (float)numberIn;
		if (ratio > 0.10) {
			compressQuality = 1;
		}
	}

	if (inoteElementSize) {
		ratio = (float)inotesRepeatNumber / (float)numberIn;
		if (ratio > 0.10) {
			compressNotes = 1;
		}
	}

	//  If we are not going to compress anything, aggragate data and return
	if (!compressValues && !compressQuality && !compressNotes) {
		dataCompressedOut = (int*)calloc((size_t)dataCompressedOutCount, WORD_SIZE);
		if (!dataCompressedOut) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStoreReg_ID,
				zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, dataCompressedOutCount, 0,
				zdssErrorSeverity.MEMORY_ERROR, "", "Data output array");
		}
		ipos = 0;
		for (i = 0; i<timesNumber; i++) {
			assert(ipos < dataCompressedOutCount);
			dataCompressedOut[ipos++] = timeArray[i];
		}
		for (i = 0; i<valuesUncompressedNumber; i++) {
			assert(ipos < dataCompressedOutCount);
			dataCompressedOut[ipos++] = values[i];
		}
		for (i = 0; i<qualityUncompressedNumber; i++) {
			assert(ipos < dataCompressedOutCount);
			dataCompressedOut[ipos++] = quality[i];
		}

		if ((cnotesLength > 0) && (inoteElementSize == 0)) {
			charLong((void *)cnotes, (void *)&dataCompressedOut[ipos], cnotesLength, (inotesUncompressedNumber * 4), 1, -1);
			//charInt((void *)cnotes, &dataCompressedOut[ipos], cnotesLength, (inotesUncompressedNumber * 4), 1, isOdd(ipos));
			if (getEndian()) {
				zswitchInts((void *)&dataCompressedOut[ipos], inotesUncompressedNumber);
			}
			ipos += inotesUncompressedNumber;
			assert(ipos <= dataCompressedOutCount);
		}
		else {
			for (i = 0; i<inotesUncompressedNumber; i++) {
				assert(ipos < dataCompressedOutCount);
				dataCompressedOut[ipos++] = notes[i];
			}
		}
		*arrayOut = dataCompressedOut;
		*arrayOutNumber = dataCompressedOutCount;
		*header2Number = 0;
		*header2 = 0;
		return compressionFlag;
	}

	//  At this point we will be compressing
	//  Note: everything is in single ints, not doubles or otherwise
	//  The compression header array contains the following:
	//  header2[0] = compression information for data array, if compressed
	//  header2[((numberIn -1) / 32) + 1] = compression information for quality array, if compressed
	//  If values is not compressed, quality compression information starts in [0]


	//  Calculate the length of the data array and compression header
	compressionHeaderNumber = ((numberIn - 1) / 32) + 1;
	header2Count = (compressValues + compressQuality + compressNotes) * compressionHeaderNumber;
	compressHeader = (int*)calloc((size_t)header2Count, WORD_SIZE);
	if (!compressHeader) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStoreReg_ID,
			zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, header2Count, 0,
			zdssErrorSeverity.MEMORY_ERROR, "", "compression array");
	}
	if (compressValues) {
		if ((valueRepeatNumber + 1) == numberIn) {
			valuesNumber = valueElementSize;
			internalHeader[INT_HEAD_valuesCompressionFlag] = 2;
		}
		else {
			valuesNumber = (numberIn - valueRepeatNumber) * valueElementSize;
			internalHeader[INT_HEAD_valuesCompressionFlag] = 1;
		}
	}
	else {
		valuesNumber = valuesUncompressedNumber;
	}
	internalHeader[INT_HEAD_valuesNumber] = valuesNumber;

	if (compressQuality) {
		if ((qualityRepeatNumber + 1) == numberIn) {
			qualityNumber = qualityElementSize;
			internalHeader[INT_HEAD_qualityCompressionFlag] = 2;
		}
		else {
			qualityNumber = (numberIn - qualityRepeatNumber) * qualityElementSize;
			internalHeader[INT_HEAD_qualityCompressionFlag] = 1;
		}
		internalHeader[INT_HEAD_qualityNumber] = qualityNumber;
	}
	else {
		qualityNumber = qualityUncompressedNumber;
	}

	if (compressNotes) {
		if ((inotesRepeatNumber + 1) == numberIn) {
			inotesNumber = inoteElementSize;
			internalHeader[INT_HEAD_inotesCompressionFlag] = 2;
		}
		else {
			inotesNumber = (numberIn - inotesRepeatNumber) * inoteElementSize;
			internalHeader[INT_HEAD_inotesCompressionFlag] = 1;
		}
		internalHeader[INT_HEAD_inotesNumber] = inotesNumber;
	}
	else {
		inotesNumber = inotesUncompressedNumber;
	}

	dataCompressedOutNumber = timesNumber + valuesNumber + qualityNumber + inotesNumber;

	//  Allocate space for them
	dataCompressedOut = (int*)calloc((size_t)dataCompressedOutNumber, WORD_SIZE);
	if (!dataCompressedOut) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStoreReg_ID,
			zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, dataCompressedOutNumber, 0,
			zdssErrorSeverity.MEMORY_ERROR, "", "output array");
	}

	ipos = 0;
	if (timesNumber) {
		for (i = 0; i<timesNumber; i++) {
			assert(ipos < dataCompressedOutCount);
			dataCompressedOut[ipos++] = timeArray[i];
		}
	}
	iposComp = 0;
	if (compressValues) {
		if (internalHeader[INT_HEAD_valuesCompressionFlag] == 1) {
			/////    FIX ME - These need to be ipos + length used in repeat compress!!!!!!
			assert(ipos < dataCompressedOutCount);
			assert(iposComp < header2Count);
			ztsRegRepeatCompress(values, numberIn, valueElementSize, &dataCompressedOut[ipos], dataCompressedOutCount, 
				&compressHeader[iposComp], header2Count);
			ipos += (numberIn - valueRepeatNumber) * valueElementSize;
			iposComp += compressionHeaderNumber;
		}
		else {
			//  All the same
			for (i = 0; i<valueElementSize; i++) {
				assert(ipos < dataCompressedOutCount);
				dataCompressedOut[ipos++] = values[i];
			}
		}
	}
	else {
		for (i = 0; i<valuesUncompressedNumber; i++) {
			assert(ipos < dataCompressedOutCount);
			dataCompressedOut[ipos++] = values[i];
		}
	}
	if (compressQuality) {
		if (internalHeader[INT_HEAD_qualityCompressionFlag] == 1) {
			//  FIX here, as above
			assert(ipos < dataCompressedOutCount);
			assert(iposComp < header2Count);
			ztsRegRepeatCompress(quality, numberIn, qualityElementSize, &dataCompressedOut[ipos], dataCompressedOutCount, 
				&compressHeader[iposComp], header2Count);
			ipos += (numberIn - qualityRepeatNumber) * qualityElementSize;
			iposComp += compressionHeaderNumber;
		}
		else {
			//  All the same
			for (i = 0; i<qualityElementSize; i++) {
				assert(ipos < dataCompressedOutCount);
				dataCompressedOut[ipos++] = quality[i];
			}
		}
	}
	else {
		for (i = 0; i<qualityUncompressedNumber; i++) {
			assert(ipos < dataCompressedOutCount);
			dataCompressedOut[ipos++] = quality[i];
		}
	}
	if (compressNotes) {
		if (internalHeader[INT_HEAD_inotesCompressionFlag] == 1) {
			//  fix, as above
			assert(ipos < dataCompressedOutCount);
			assert(iposComp < header2Count);
			ztsRegRepeatCompress(notes, numberIn, inoteElementSize, &dataCompressedOut[ipos], dataCompressedOutCount,
				&compressHeader[iposComp], header2Count);
		}
		else {
			//  All the same
			for (i = 0; i<inoteElementSize; i++) {
				assert(ipos < dataCompressedOutCount);
				dataCompressedOut[ipos++] = notes[i];
			}
		}
	}
	else if ((cnotesLength > 0) && (inoteElementSize == 0)) {
		//  We have a character string to store
		//charLong((void *)cnotes, (void *)&dataCompressedOut[ipos], cnotesLength, (inotesUncompressedNumber * 4), 1, -1);
		charInt((void *)cnotes, &dataCompressedOut[ipos], cnotesLength, (inotesUncompressedNumber * 4), 1, 1, isOdd(ipos));
		if (getEndian()) {
			zswitchInts((void *)&dataCompressedOut[ipos], inotesUncompressedNumber);
		}
		ipos += inotesUncompressedNumber;
		assert(ipos <= dataCompressedOutCount);
	}
	else {
		//  Either no notes, or no compression of notes
		for (i = 0; i<inotesUncompressedNumber; i++) {
			assert(ipos < dataCompressedOutCount);
			dataCompressedOut[ipos++] = notes[i];
		}
	}

	//  Done.  Return arrays and lengths.
	*arrayOut = dataCompressedOut;
	*arrayOutNumber = dataCompressedOutNumber;
	if (getEndian()) {
		zswitchInts(compressHeader, header2Count);
	}
	*header2 = compressHeader;
	*header2Number = header2Count;
	return 1;
}
