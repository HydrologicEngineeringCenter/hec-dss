#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "heclib.h"
#include "hecdss7.h"

/**
*  Function:	ztsDisaggregate
*
*  Use:			Private
*
*  Description:	Disggregate and uncompress time series data
*
*  Declaration:
*				 void ztsDisaggregate(long long *ifltab, int numberToRead, int numberStored,
*									  int *numberExpanded, int blockStartPosition,
* 									  int positionRelativeFirstValid, int positionRelativeLastValid,
*									  int *dataIn, int *header2, int *internalHeader,
*									  int *values, int valuesArraySize, int valuesSizeRequested,
*									  int *quality, int qualityArraySize, int qualitySizeRequested,
*									  int *inotes, int inotesArraySize, int inotesSizeRequested,
*									  char *cnotes, int cnotesSize, int *cnotesLength);
*
*  Parameters:
*				int numberIn
*					The number of values, which is the same as number in the time array, quaity, etc.
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
*				int valuesArraySize
*					The size (in ints) of array values.
*
*				int valueElementSize
*					The number of words in each value element; 1 for floats, 2 for doubles (only)
*
*				int *quality
*					The quality array, if quality is to be stored.
*
*				int qualityArraySize
*					The size (in ints) of array quality.
*
*				int qualityElementSize
*					The number of words for each quality element, either 0 (zero) if no quality is
*					to be stored, or greater than 0.  Typically 1 or 2, but should handle any reasonable number.
*
*				int *inotes
*					If integer notes array, if integer notes are to be stored.  If character notes are to be stored,
*					this must be a dummy array, as only character or integer notes (or neither) can be stored, not both.
*
*				int inotesArraySize
*					The size (in ints) of array notes.
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
*					< 0 - error
*
*	Remarks:	This is for uncompressing or disaggregating  a single block (record)
*					It includes filling in missing data before and/or after data actually
*					stored in the block, as well as uncompressing and disaggregating the
*					data stored within the block
*
*
*	Note:		  This is a more complex function.  Care should be used with any modifications.
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int ztsDisaggregateDep(long long *ifltab, int numberToRead, int numberStored,
	int *numberExpanded, int blockStartPosition,
	int positionRelativeFirstValid, int positionRelativeLastValid,
	int *dataIn, int *header2, int *internalHeader,
	int *values, int valuesArraySize, int valuesSizeRequested,
	int *quality, int qualityArraySize, int qualitySizeRequested,
	int *inotes, int inotesArraySize, int inotesSizeRequested,
	char *cnotes, int cnotesSize, int *cnotesLength);


int ztsDisaggregate(long long *ifltab, int numberToRead, int numberStored,
	int dataInPosition, int *numberExpanded, int blockStartPosition,
	int positionRelativeFirstValid, int positionRelativeLastValid,
	int *dataIn, int dataInNumber,
	int *header2, int *internalHeader,
	int *values, int valuesArraySize, int valuesSizeRequested,
	int *quality, int qualityArraySize, int qualitySizeRequested,
	int *inotes, int inotesArraySize, int inotesSizeRequested,
	char *cnotes, int cnotesSize, int *cnotesLength)
{
	int position;
	int i, j;
	int number;
	int icount;
	int valuesPosition;
	int qualityPosition;
	int inotesPosition;
	int dataPosition;
	int cnotesPosition;
	int carrayPosition;

	int compressionHeaderPosition;
	int numberStoredToRead;
	int numberNonValidBefore;
	int startElement;
	int endElement;
	int startArray;
	int endArray;
	int valuesElementSize;
	int numberRemaining;
	int profileDepthsNumber;
	float missing;

	int valuesLength;
	int valuesCompressionFlag;
	int valuesSizeRead;
	int qualityNumber;
	int qualityCompressionFlag;
	int qualityElementSize;
	int inotesNumber;
	int inotesCompressionFlag;
	int inotesElementSize;
	int cnotesReadLength;
	int numberValues;
	char *carray;
	int status;

	/*
		Internal Header Definition

		int internalHeader[INT_HEAD_SIZE];

		pos	Use
		0   timeGranularitySeconds
		1   precision
		2   timeOffsetSeconds
		3   profileDepthsNumber
		4   blockStartPosition
		5   blockEndPosition
		6   valuesNumber
		7   valuesSize
		8	valueElementSize
		9   valuesCompressionFlag
		10  qualityNumber
		11  qualityElementSize
		12  qualityCompressionFlag
		13  inotesNumber
		14  inotesElementSize
		15  inotesCompressionFlag
		16  cnotesLength
		17+ units, type, timezone separated by lf

		Compression flags:
			0 = No compression
			1 = Compression based on repeat values (with repeat bit saved in header)
			2 = All repeats


		startArray and endArray refer to array locations, for example
			double values(100), startArray = 20, endArray = 30
		startElement and endElement refer to element locations,
		accounting for the length of each value.  For example
			double values(100), startElement = 20 * 2 = 40, endElement = 30 * 2 = 60

		Note: everything is in single ints, not doubles or otherwise
		The compression header array contains the following:
		header2[0] = compression information for data array, if compressed
		header2[((numberIn -1) / 32) + 1] = compression information for quality array, if compressed
		If values is not compressed, quality compression information starts in [0]

	*/

	//  Before 7-HA?

	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, "Enter ztsDisaggregate numberToRead: ", numberToRead);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, "numberStored: ", numberStored);
	}

	if (zinquire(ifltab, "fver") < 77265) {
		return ztsDisaggregateDep(ifltab, numberToRead, numberStored,
			numberExpanded, blockStartPosition,
			positionRelativeFirstValid, positionRelativeLastValid,
			&dataIn[dataInPosition], header2, internalHeader,
			values, valuesArraySize, valuesSizeRequested,
			quality, qualityArraySize, qualitySizeRequested,
			inotes, inotesArraySize, inotesSizeRequested,
			cnotes, cnotesSize, cnotesLength);
	}
	

	if (!dataIn) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, zdssErrorCodes.NULL_ARRAY,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "dataIn array is null");
	}
	if (!values) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, zdssErrorCodes.NULL_ARRAY,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "values array is null");
	}
	if (!internalHeader) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, zdssErrorCodes.NULL_ARRAY,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "internalHeader array is null");
	}
	if ((qualitySizeRequested > 0) && !quality) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, zdssErrorCodes.NULL_ARRAY,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "quality array is null");
	}
	if ((inotesSizeRequested > 0) && !inotes) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, zdssErrorCodes.NULL_ARRAY,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "inotes array is null");
	}
	if ((cnotesSize > 0) && !cnotes) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, zdssErrorCodes.NULL_ARRAY,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "cnotes array is null");
	}

	//  Put information from the header into variable names that are earlier to work with
	valuesLength = internalHeader[INT_HEAD_valuesNumber];				//  Size of values area in data array (big number)
	valuesSizeRead = internalHeader[INT_HEAD_valueSize];				//  Length of each value  (double = 2, profile = 12)
	valuesElementSize = internalHeader[INT_HEAD_valueElementSize];		//  Length of each element  (double = 2, profile = 2)
	profileDepthsNumber = internalHeader[INT_HEAD_profileDepthsNumber];
	valuesCompressionFlag = internalHeader[INT_HEAD_valuesCompressionFlag];
	qualityNumber = internalHeader[INT_HEAD_qualityNumber];
	qualityElementSize = internalHeader[INT_HEAD_qualityElementSize];
	qualityCompressionFlag = internalHeader[INT_HEAD_qualityCompressionFlag];
	inotesNumber = internalHeader[INT_HEAD_inotesNumber];
	inotesElementSize = internalHeader[INT_HEAD_inotesElementSize];
	inotesCompressionFlag = internalHeader[INT_HEAD_inotesCompressionFlag];
	cnotesReadLength = internalHeader[INT_HEAD_cnotesLength];

	status = 0;
	valuesPosition = 0;
	qualityPosition = 0;
	inotesPosition = 0;
	*cnotesLength = 0;
	cnotesPosition = 0;
	carrayPosition = 0;	
	numberValues = valuesLength / valuesSizeRead;



	//  If a type of data was requested, but not read from the record,
	//  fill that data with an empty set.
	if ((valuesSizeRequested > 0) && (valuesLength == 0)) {
		//  Values requested, but none read
		//  Fill in with missing flag
		assert((numberToRead * valuesSizeRequested) <= valuesArraySize);
		if (valuesSizeRequested == 1) {
			zsetMissingFloatArray((float*)values, numberToRead);
		}
		else {
			zsetMissingDoubleArray((double *)values, numberToRead);
		}
		valuesSizeRequested = 0;
	}
	if ((qualitySizeRequested > 0) && (qualityNumber == 0)) {
		//  quality requested, but none read
		//  Fill in with zeros
		assert((numberToRead * qualitySizeRequested) <= qualityArraySize);
		i = 0;

		fillArray(&i, 1, quality, qualitySizeRequested, numberToRead);

		//  Set size requested to zero to bypass code below.
		qualitySizeRequested = 0;
		//fillArray(int *valueIn, int valueInElementLength, int *arrayOut,  int arrayOutElementLength, int numberOut)
	}
	if ((inotesSizeRequested > 0) && (inotesNumber == 0)) {
		//  notes requested, but none read
		//  Fill in with zeros
		assert((numberToRead * inotesSizeRequested) <= inotesArraySize);
		i = 0;
		fillArray(&i, 1, inotes, inotesSizeRequested, numberToRead);
		//  Set size requested to zero to bypass code below.
		inotesSizeRequested = 0;
	}
	if ((cnotesSize > 0) && (cnotesReadLength == 0)) {
		//  character notes requested, but none read
		//  Fill in with zeros
		//  number = min(cnotesSize, numberToRead);
		number = numberToRead;
		if (cnotesSize < number) number = cnotesSize;
		zeroFill(cnotes, numberToRead);
		//  Set size requested to zero to bypass code below.
		cnotesSize = 0;
		*cnotesLength = number;
	}


	//  Now process data that we have read
	
	//  Set any missing values before data in record begins
	if (positionRelativeFirstValid < 0) {
		numberNonValidBefore = abs(positionRelativeFirstValid);
		if (numberNonValidBefore > numberToRead) {
			numberNonValidBefore = numberToRead;
		}
		for (i=0; i<numberNonValidBefore; i++) {
			//  To set missing, we both have to have it defined and want it.
			if (valuesSizeRequested) {
				assert(valuesPosition < valuesArraySize);
				if (profileDepthsNumber == 0) {
					zsetMissing(&values[valuesPosition], valuesSizeRequested);
					valuesPosition += valuesSizeRequested;
				}
				else {
					missing = (float)zmissingFlag();
					fillArray((void *)&missing, 1, &values[valuesPosition], valuesSizeRequested, profileDepthsNumber);
					valuesPosition += valuesSizeRequested * profileDepthsNumber;
				}
			}
			//  Undefined in this case is just 0.
			if (qualitySizeRequested) {
				assert(qualityPosition < qualityArraySize);
				zsetUndefined(&quality[qualityPosition], qualitySizeRequested);
				qualityPosition += qualitySizeRequested;
			}
			if (inotesSizeRequested) {
				assert(inotesPosition < inotesArraySize);
				zsetUndefined(&inotes[inotesPosition], inotesSizeRequested);
				inotesPosition += inotesSizeRequested;
			}
			if ((cnotesSize > 0) && (cnotesPosition < cnotesSize)) {
				cnotes[cnotesPosition++] = '\0';
			}
		}

		if (numberNonValidBefore >= numberToRead) {
			//   FIX ME - BE SURE THIS IS RIGHT!!!
			// check me!!!
			//if ((qualitySizeRequested == 0) && (inotesSizeRequested == 0) && (cnotesSize == 0)) {
				*numberExpanded = numberToRead;
				if (cnotesSize > 0) {
					*cnotesLength = cnotesPosition;
				}
				return 0;
			//}
		}
	}

	//  At this point we know that we need data later in the record...
	//  however, it may be beyond the valid data and only missing.
	numberStoredToRead = numberStored - positionRelativeFirstValid;

	//  Figure out startArray and how many values to get
	if (positionRelativeFirstValid > 0) {
		startArray = positionRelativeFirstValid;
		endArray = startArray + numberStoredToRead;
	}
	else {
		startArray = 0;
		endArray = numberStored;
	}
	//  Do we only need a portion of the record?
	if (positionRelativeLastValid < 0) {
	//  Yes, adjust down the number we are going to copy
		endArray += positionRelativeLastValid;
	}

	if ((endArray - startArray) > numberToRead) {
		endArray = startArray + numberToRead;
	}
	*numberExpanded = endArray - startArray;

	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, "numberToRead: ", numberToRead);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, "numberStored: ", numberStored);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, "dataInNumber: ", dataInNumber);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, "*numberExpanded: ", *numberExpanded);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, "blockStartPosition: ", blockStartPosition);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, "positionRelativeFirstValid: ", positionRelativeFirstValid);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, "numberStoredToRead: ", numberStoredToRead);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, "startArray: ", startArray);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, "endArray: ", endArray);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, "numberValues: ", numberValues);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, "valuesLength: ", valuesLength);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, "dataInPosition: ", dataInPosition);
	}


	//  Are we past the last valid data? 
	//  Do we have data to uncompress (most of the time, yes)
	if (*numberExpanded > 0) {

		//  Now either copy or uncompress the values
		if (numberStoredToRead > 0) {
			if (valuesSizeRequested > 0) {
				if (valuesSizeRead > 0) {
					if (bigEndian()) {
						number = valuesLength + dataInPosition;
						if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
							zmessageDebugInt(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, "switchDoubles, number to switch: ", number);
							zmessageDebugInt(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, "switchDoubles, Length of dataIn: ", dataInNumber);
						}
						assert(number <= dataInNumber);
						zswitchDoubles(dataIn, number, valuesSizeRead,  0, dataInPosition);
					}
					//  Is data compressed?
					if (valuesCompressionFlag == 0) {
						//  No, just copy it	
						//  Now get the int *4 length to copy
						assert(valuesPosition < valuesArraySize);
						if (valuesSizeRequested == valuesSizeRead) {
							//  Straight copy
							startElement = startArray * valuesSizeRead;
							endElement = endArray * valuesSizeRead;
							for (i = startElement; i < endElement; i++) {
								values[valuesPosition++] = dataIn[i + dataInPosition];
							}						
						}
						else {
							//  Need to convert the data type
							if (profileDepthsNumber == 0) {
								for (i = startArray; i < endArray; i++) {
									convertDataType(&dataIn[(i*valuesSizeRead) + dataInPosition], &values[valuesPosition], valuesElementSize, valuesSizeRequested);
									valuesPosition += valuesSizeRequested;
								}
							}
							else {
								for (i = startArray; i < endArray; i++) {
									convertDataArray(&dataIn[(i*valuesSizeRead) + dataInPosition], &values[valuesPosition], profileDepthsNumber, valuesElementSize, valuesSizeRequested);
									valuesPosition += valuesSizeRequested * profileDepthsNumber;
								}
							}
						}
					}
					else if (valuesCompressionFlag == 1) {
						//  Regular compression - uncompress this array
						if (!header2) {
							return zerrorProcessing(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, zdssErrorCodes.NULL_ARRAY,
								0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "compression header array is null");
						}
						ztsRegRepeatUncompress(startArray, endArray,
							valuesSizeRead, valuesSizeRequested, 1, &header2[0],
							&dataIn[dataInPosition], &values[valuesPosition], (valuesArraySize - valuesPosition));
					}
					else if (valuesCompressionFlag == 2) {
						//  Same value for all data points (e..g., 0.0, 0.0, 0.0) (think precip)
						if (valuesSizeRequested == valuesSizeRead) {
							//  Straight copy
							for (i = startArray; i < endArray; i++) {
								for (j = 0; j < valuesSizeRead; j++) {
									assert(valuesPosition < valuesArraySize);
									values[valuesPosition++] = dataIn[j + dataInPosition];
								}
							}
						}
						else {
							//  Need to convert the data type
							for (i = startArray; i < endArray; i++) {
								assert(valuesPosition < valuesArraySize);
								convertDataType(&dataIn[dataInPosition], &values[valuesPosition], valuesSizeRead, valuesSizeRequested);
								valuesPosition += valuesSizeRequested;
							}
						}
					}
				}
				else {
					//  Requesting values, but no values stored
					if (profileDepthsNumber == 0) {
						for (i = startArray; i < endArray; i++) {
							assert(valuesPosition < valuesArraySize);
							zsetMissing(&values[valuesPosition], valuesSizeRequested);
							valuesPosition += valuesSizeRequested;
						}
					}
					else {
						for (i = startArray; i < endArray; i++) {
							assert(valuesPosition < valuesArraySize);
							missing = (float)zmissingFlag();
							fillArray((void *)&missing, 1, &values[valuesPosition], valuesSizeRequested, profileDepthsNumber);
							valuesPosition += valuesSizeRequested * profileDepthsNumber;
						}
					}
				}
			}

			dataInPosition += valuesLength;
			if (isOdd(dataInPosition)) {
				dataInPosition++;
			}

			//  Uncompress quality flags
			if (qualitySizeRequested > 0) {
				if (qualityElementSize > 0) {
					if (bigEndian()) {
						zswitchInts((void *)&dataIn[dataInPosition], qualityNumber);
					}
					//  Are the quality compressed?
					if (qualityCompressionFlag == 0) {
						//  No, just copy it
						if (qualitySizeRequested == qualityElementSize) {
							startElement = (startArray * qualityElementSize) + dataInPosition;
							endElement = (endArray * qualityElementSize) + dataInPosition;
							for (i = startElement; i < endElement; i++) {
								assert(qualityPosition < qualityArraySize);
								quality[qualityPosition++] = dataIn[i];
							}
						}
						else {
							//  Need to convert the data length
							for (i = startArray; i < endArray; i++) {
								assert(qualityPosition < qualityArraySize);
								dataPosition = (i * qualityElementSize) + dataInPosition;
								convertDataLength(&dataIn[dataPosition], &quality[qualityPosition], qualityElementSize, qualitySizeRequested);
								qualityPosition += qualitySizeRequested;
							}
						}
					}
					else if (qualityCompressionFlag == 1) {
						//  Regular compression - uncompress this array
						//  Get the position of the quality compression info
						//  Start with the position of the repeat compression info

						//  Was the values area compressed?  If so, jump past that
						if (valuesCompressionFlag == 1) {
							compressionHeaderPosition = ((numberStored - 1) / 32) + 1;
						}
						else {
							compressionHeaderPosition = 0;
						}
						ztsRegRepeatUncompress(startArray, endArray,
							qualityElementSize, qualitySizeRequested, 0, &header2[compressionHeaderPosition],
							&dataIn[dataInPosition], &quality[qualityPosition], (qualityArraySize - qualityPosition));
					}
					else if (qualityCompressionFlag == 2) {
						//  Same value for all data points (e..g., 0.0)
						if (qualitySizeRequested == qualityElementSize) {
							//  Straight copy
							for (i = startArray; i < endArray; i++) {
								for (j = 0; j < qualityElementSize; j++) {
									assert(qualityPosition < qualityArraySize);
									quality[qualityPosition++] = dataIn[j + dataInPosition];
								}
							}
						}
						else {
							//  Need to convert the data length
							for (i = startArray; i < endArray; i++) {
								dataPosition = dataInPosition;
								assert(qualityPosition < qualityArraySize);
								convertDataLength(&dataIn[dataPosition], &quality[qualityPosition], qualityElementSize, qualitySizeRequested);
								qualityPosition += qualitySizeRequested;
							}
						}
					}
				}
				else {
					//  Requesting quality, but no quality stored
					for (i = startArray; i < endArray; i++) {
						assert(qualityPosition < qualityArraySize);
						zsetUndefined(&quality[qualityPosition], qualitySizeRequested);
						qualityPosition += qualitySizeRequested;
					}
				}
			}

			dataInPosition += qualityNumber;
			//  Keep set on 8-byte boundaries (64 bit words)
			if (isOdd(dataInPosition)) {
				dataInPosition++;
			}

			//  Uncompress Notes
			if (inotesSizeRequested > 0) {
				if (inotesElementSize > 0) {
					if (bigEndian()) {
						zswitchInts((void *)&dataIn[dataInPosition], inotesNumber);
					}
					//  Are the inotes compressed?
					if (inotesCompressionFlag == 0) {
						//  No, just copy it
						if (inotesSizeRequested == inotesElementSize) {
							//  Add the length from the values and quality
							startElement = (startArray * inotesElementSize) + dataInPosition;
							endElement = (endArray * inotesElementSize) + dataInPosition;
							for (i = startElement; i < endElement; i++) {
								assert(inotesPosition < inotesArraySize);
								inotes[inotesPosition++] = dataIn[i];
							}
						}
						else {
							//  Need to convert the data length
							for (i = startArray; i < endArray; i++) {
								dataPosition = (i * inotesElementSize) + dataInPosition;
								assert(inotesPosition < inotesArraySize);
								convertDataLength(&dataIn[dataPosition], &inotes[inotesPosition], inotesElementSize, inotesSizeRequested);
								inotesPosition += inotesSizeRequested;
							}
						}
					}
					else if (inotesCompressionFlag == 1) {
						//  Get the position of the quality compression info
						compressionHeaderPosition = 0;
						//  Was the values area compressed?  If not, jump past that
						if (valuesCompressionFlag == 1) {
							compressionHeaderPosition += ((numberStored - 1) / 32) + 1;
						}
						//  Was the quality area compressed?  If not, jump past that
						if (qualityCompressionFlag == 1) {
							compressionHeaderPosition += ((numberStored - 1) / 32) + 1;
						}
						dataPosition = dataInPosition;
						ztsRegRepeatUncompress(startArray, endArray,
							inotesElementSize, inotesSizeRequested, 0, &header2[compressionHeaderPosition],
							&dataIn[dataPosition], &inotes[inotesPosition], (inotesArraySize - inotesPosition));
					}
					else if (inotesCompressionFlag == 2) {
						//  Same value for all data points (e..g., 0.0)
						if (inotesSizeRequested == inotesElementSize) {
							//  Straight copy
							dataPosition = dataInPosition;
							for (i = startArray; i < endArray; i++) {
								for (j = 0; j < inotesElementSize; j++) {
									assert(inotesPosition < inotesArraySize);
									inotes[inotesPosition++] = dataIn[j + dataPosition];
								}
							}
						}
						else {
							//  Need to convert the data length
							for (i = startArray; i < endArray; i++) {
								dataPosition = dataInPosition;
								assert(inotesPosition < inotesArraySize);
								convertDataLength(&dataIn[dataPosition], &inotes[inotesPosition], inotesElementSize, inotesSizeRequested);
								inotesPosition += inotesSizeRequested;
							}
						}
					}
				}
				else {
					//  Requesting inotes, but no inotes stored
					for (i = startArray; i < endArray; i++) {
						assert(inotesPosition < inotesArraySize);
						zsetUndefined(&inotes[inotesPosition], inotesSizeRequested);
						inotesPosition += inotesSizeRequested;
					}
				}
			}

			if ((cnotesReadLength > 0) && (cnotesSize > 0)) {
				//  Character string inotes are different in that we
				//  do no compression and there are no missing values
				//  Thus if no inotes with no information, a full block
				//  of '\0' is stored.    cnotes[cnotesPosition++]
				//  Add the length from the values and quality
				startElement = dataInPosition;
				if (bigEndian()) {
					number = numberIntsInBytes(cnotesReadLength);
					carray = (char *)calloc(number + 1, 4);
					charLong((void *)&dataIn[startElement], (void *)carray, cnotesReadLength, ((number + 1) * 4), 0, -1);
				}
				else { 
					carray = (char *)&dataIn[startElement];
				}
				
				carrayPosition = 0;
				if (startArray > 0) {
					//  Count through the first startArray lines
					carrayPosition = copyLines(cnotes, (size_t)0, carray, (size_t)cnotesReadLength, startArray);
				}
				numberRemaining = endArray - startArray;
				//endElement  = inotesNumber + dataInPosition;
				icount = copyLines(&cnotes[cnotesPosition], (size_t)(cnotesSize - cnotesPosition),
					&carray[carrayPosition], (size_t)(cnotesReadLength - carrayPosition), numberRemaining);
				cnotesPosition += icount;
				carrayPosition += icount;
				if (bigEndian()) {
					free(carray);
				}
			}
		}
	}

	//  Set any missing values after data in record ends
	if (positionRelativeLastValid > 0) {
		//  Figure out our position in the values array.
		//  (quality and inotes will be the same)
		position = numberStored - positionRelativeFirstValid;
		//  Is all the data to be read after the valid data
		if (position < 0) {
			//  Yes, just set the number requested to missing then
			positionRelativeLastValid += position;  // Should be the same as numberToRead
			position = 0;
		}
		//  Position gives us the logical location, but we need it relative to 4 byte words
		if (valuesSizeRequested) {
			if (profileDepthsNumber == 0) {
				valuesPosition = position * valuesSizeRequested;
				for (i=0; i<positionRelativeLastValid; i++) {
					zsetMissing(&values[valuesPosition], valuesSizeRequested);
					valuesPosition += valuesSizeRequested;
				}
			}
			else {
				valuesPosition = position * valuesSizeRequested * profileDepthsNumber;
				for (i=0; i<positionRelativeLastValid; i++) {
					missing = (float)zmissingFlag();
					fillArray((void *)&missing, 1, &values[valuesPosition], valuesSizeRequested, profileDepthsNumber);
					valuesPosition += valuesSizeRequested * profileDepthsNumber;
				}
			}
		}
		//  Less frequent cases
		if (qualitySizeRequested || inotesSizeRequested || cnotesSize) {
			qualityPosition = position * qualitySizeRequested;
			inotesPosition = position * inotesSizeRequested;
			for (i=0; i<positionRelativeLastValid; i++) {
				//  Undefined in this case is just 0.
				if (qualitySizeRequested) {
					zsetUndefined(&quality[qualityPosition], qualitySizeRequested);
					qualityPosition += qualitySizeRequested;
				}
				if (inotesSizeRequested) {
					zsetUndefined(&inotes[inotesPosition], inotesSizeRequested);
					inotesPosition += inotesSizeRequested;
				}
				if ((cnotesSize > 0) && (cnotesPosition < cnotesSize)) {
					cnotes[cnotesPosition++] = '\0';
				}
			}
		}
	}
	if (cnotesSize > 0) {
		*cnotesLength = cnotesPosition;
	}
	//  All done!
	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, "Exit ztsDisaggregate status: ", status);
	}
	return status;
}

int ztsDisaggregateDep(long long *ifltab, int numberToRead, int numberStored,
	int *numberExpanded, int blockStartPosition,
	int positionRelativeFirstValid, int positionRelativeLastValid,
	int *dataIn, int *header2, int *internalHeader,
	int *values, int valuesArraySize, int valuesSizeRequested,
	int *quality, int qualityArraySize, int qualitySizeRequested,
	int *inotes, int inotesArraySize, int inotesSizeRequested,
	char *cnotes, int cnotesSize, int *cnotesLength)
{
	int position;
	int i, j;
	int number;
	int icount;
	int valuesPosition;
	int qualityPosition;
	int inotesPosition;
	int dataPosition;
	int cnotesPosition;
	int carrayPosition;
	int compressionHeaderPosition;
	int numberStoredToRead;
	int numberNonValidBefore;
	int startElement;
	int endElement;
	int startArray;
	int endArray;
	int valuesElementSize;
	int numberRemaining;
	int profileDepthsNumber;
	float missing;

	int valuesNumber;
	int valuesCompressionFlag;
	int valuesSizeRead;
	int qualityNumber;
	int qualityCompressionFlag;
	int qualityElementSize;
	int inotesNumber;
	int inotesCompressionFlag;
	int inotesElementSize;
	int cnotesReadLength;
	char *carray;

	int status;


	/*
	Internal Header Definition

	int internalHeader[INT_HEAD_SIZE];

		pos	Use
		0   timeGranularitySeconds
		1   precision
		2   timeOffsetSeconds
		3   profileDepthsNumber
		4   blockStartPosition
		5   blockEndPosition
		6   valuesNumber
		7   valuesSize
		8	valueElementSize
		9   valuesCompressionFlag
		10  qualityNumber
		11  qualityElementSize
		12  qualityCompressionFlag
		13  inotesNumber
		14  inotesElementSize
		15  inotesCompressionFlag
		16  cnotesLength
		17+ units, type, timezone separated by lf


	startArray and endArray refer to array locations, for example
	double values(100), startArray = 20, endArray = 30
	startElement and endElement refer to element locations,
	accounting for the length of each value.  For example
	double values(100), startElement = 20 * 2 = 40, endElement = 30 * 2 = 60

	Note: everything is in single ints, not doubles or otherwise
	The compression header array contains the following:
	header2[0] = compression information for data array, if compressed
	header2[((numberIn -1) / 32) + 1] = compression information for quality array, if compressed
	If values is not compressed, quality compression information starts in [0]

	*/

	if (!dataIn) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, zdssErrorCodes.NULL_ARRAY,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "dataIn array is null");
	}
	if (!values) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, zdssErrorCodes.NULL_ARRAY,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "values array is null");
	}
	if (!internalHeader) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, zdssErrorCodes.NULL_ARRAY,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "internalHeader array is null");
	}
	if ((qualitySizeRequested > 0) && !quality) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, zdssErrorCodes.NULL_ARRAY,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "quality array is null");
	}
	if ((inotesSizeRequested > 0) && !inotes) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, zdssErrorCodes.NULL_ARRAY,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "inotes array is null");
	}
	if ((cnotesSize > 0) && !cnotes) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, zdssErrorCodes.NULL_ARRAY,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "cnotes array is null");
	}

	//  Put information from the header into variable names that are earlier to work with
	valuesNumber = internalHeader[INT_HEAD_valuesNumber];				//  Size of values area in data array (big number)
	valuesSizeRead = internalHeader[INT_HEAD_valueSize];				//  Length of each value  (double = 2, profile = 12)
	valuesElementSize = internalHeader[INT_HEAD_valueElementSize];		//  Length of each element  (double = 2, profile = 2)
	profileDepthsNumber = internalHeader[INT_HEAD_profileDepthsNumber];
	valuesCompressionFlag = internalHeader[INT_HEAD_valuesCompressionFlag];
	qualityNumber = internalHeader[INT_HEAD_qualityNumber];
	qualityElementSize = internalHeader[INT_HEAD_qualityElementSize];
	qualityCompressionFlag = internalHeader[INT_HEAD_qualityCompressionFlag];
	inotesNumber = internalHeader[INT_HEAD_inotesNumber];
	inotesElementSize = internalHeader[INT_HEAD_inotesElementSize];
	inotesCompressionFlag = internalHeader[INT_HEAD_inotesCompressionFlag];
	cnotesReadLength = internalHeader[INT_HEAD_cnotesLength];

	status = 0;
	valuesPosition = 0;
	qualityPosition = 0;
	inotesPosition = 0;
	*cnotesLength = 0;
	cnotesPosition = 0;
	carrayPosition = 0;


	//  If a type of data was requested, but not read from the record,
	//  fill that data with an empty set.
	if ((valuesSizeRequested > 0) && (valuesNumber == 0)) {
		//  Values requested, but none read
		//  Fill in with missing flag
		if (valuesSizeRequested == 1) {
			zsetMissingFloatArray((float*)values, numberToRead);
		}
		else {
			zsetMissingDoubleArray((double *)values, numberToRead);
		}
		valuesSizeRequested = 0;
	}
	if ((qualitySizeRequested > 0) && (qualityNumber == 0)) {
		//  quality requested, but none read
		//  Fill in with zeros
		///  FIX ME - DON'T WE HAVE A ZERO ARRAY??
		i = 0;

		fillArray(&i, 1, quality, qualitySizeRequested, numberToRead);

		//  Set size requested to zero to bypass code below.
		qualitySizeRequested = 0;
		//fillArray(int *valueIn, int valueInElementLength, int *arrayOut,  int arrayOutElementLength, int numberOut)
	}
	if ((inotesSizeRequested > 0) && (inotesNumber == 0)) {
		//  notes requested, but none read
		//  Fill in with zeros
		///  FIX ME - DON'T WE HAVE A ZERO ARRAY??
		i = 0;
		fillArray(&i, 1, inotes, inotesSizeRequested, numberToRead);
		//  Set size requested to zero to bypass code below.
		inotesSizeRequested = 0;
	}
	if ((cnotesSize > 0) && (cnotesReadLength == 0)) {
		//  character notes requested, but none read
		//  Fill in with zeros
		//  number = min(cnotesSize, numberToRead);
		number = numberToRead;
		if (cnotesSize < number) number = cnotesSize;
		zeroFill(cnotes, numberToRead);
		//  Set size requested to zero to bypass code below.
		cnotesSize = 0;
		*cnotesLength = number;
	}


	//  Now process data that we have read

	//  Set any missing values before data in record begins
	if (positionRelativeFirstValid < 0) {
		numberNonValidBefore = abs(positionRelativeFirstValid);
		if (numberNonValidBefore > numberToRead) {
			numberNonValidBefore = numberToRead;
		}
		for (i = 0; i<numberNonValidBefore; i++) {
			//  To set missing, we both have to have it defined and want it.
			if (valuesSizeRequested) {
				if (profileDepthsNumber == 0) {
					zsetMissing(&values[valuesPosition], valuesSizeRequested);
					valuesPosition += valuesSizeRequested;
				}
				else {
					missing = (float)zmissingFlag();
					fillArray((void *)&missing, 1, &values[valuesPosition], valuesSizeRequested, profileDepthsNumber);
					valuesPosition += valuesSizeRequested * profileDepthsNumber;
				}
			}
			//  Undefined in this case is just 0.
			if (qualitySizeRequested) {
				zsetUndefined(&quality[qualityPosition], qualitySizeRequested);
				qualityPosition += qualitySizeRequested;
			}
			if (inotesSizeRequested) {
				zsetUndefined(&inotes[inotesPosition], inotesSizeRequested);
				inotesPosition += inotesSizeRequested;
			}
			if ((cnotesSize > 0) && (cnotesPosition < cnotesSize)) {
				cnotes[cnotesPosition++] = '\0';
			}
		}

		if (numberNonValidBefore >= numberToRead) {
			//   FIX ME - BE SURE THIS IS RIGHT!!!
			// check me!!!
			//if ((qualitySizeRequested == 0) && (inotesSizeRequested == 0) && (cnotesSize == 0)) {
			*numberExpanded = numberToRead;
			if (cnotesSize > 0) {
				*cnotesLength = cnotesPosition;
			}
			return 0;
			//}
		}
	}

	//  At this point we know that we need data later in the record...
	//  however, it may be beyond the valid data and only missing.
	numberStoredToRead = numberStored - positionRelativeFirstValid;

	//  Figure out startArray and how many values to get
	if (positionRelativeFirstValid > 0) {
		startArray = positionRelativeFirstValid;
		endArray = startArray + numberStoredToRead;
	}
	else {
		startArray = 0;
		endArray = numberStored;
	}
	//  Do we only need a portion of the record?
	if (positionRelativeLastValid < 0) {
		//  Yes, adjust down the number we are going to copy
		endArray += positionRelativeLastValid;
	}

	if ((endArray - startArray) > numberToRead) {
		endArray = startArray + numberToRead;
	}
	*numberExpanded = endArray - startArray;

	//  Are we past the last valid data?
	//  Do we have data to uncompress (most of the time, yes)
	if (*numberExpanded > 0) {

		//  Now either copy or uncompress the values
		if (numberStoredToRead > 0) {
			if (valuesSizeRequested > 0) {
				if (valuesSizeRead > 0) {
					//  Is data compressed?
					if (valuesCompressionFlag == 0) {
						//  No, just copy it
						//  Now get the int *4 length to copy
						if (valuesSizeRequested == valuesSizeRead) {
							//  Straight copy
							startElement = startArray * valuesSizeRead;
							endElement = endArray * valuesSizeRead;
							for (i = startElement; i<endElement; i++) {
								values[valuesPosition++] = dataIn[i];
							}
						}
						else {
							//  Need to convert the data type
							if (profileDepthsNumber == 0) {
								for (i = startArray; i<endArray; i++) {
									convertDataType(&dataIn[i*valuesSizeRead], &values[valuesPosition], valuesElementSize, valuesSizeRequested);
									valuesPosition += valuesSizeRequested;
								}
							}
							else {
								for (i = startArray; i<endArray; i++) {
									convertDataArray(&dataIn[i*valuesSizeRead], &values[valuesPosition], profileDepthsNumber, valuesElementSize, valuesSizeRequested);
									valuesPosition += valuesSizeRequested * profileDepthsNumber;
								}
							}
						}
					}
					else if (valuesCompressionFlag == 1) {
						//  Regular compression - uncompress this array
						if (!header2) {
							return zerrorProcessing(ifltab, DSS_FUNCTION_ztsDisaggregate_ID, zdssErrorCodes.NULL_ARRAY,
								0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "compression header array is null");
						}
						ztsRegRepeatUncompress(startArray, endArray,
							valuesSizeRead, valuesSizeRequested, 1, &header2[0],
							&dataIn[0], &values[valuesPosition], (valuesArraySize - valuesPosition));
					}
					else if (valuesCompressionFlag == 2) {
						//  Same value for all data points (e..g., 0.0)
						if (valuesSizeRequested == valuesSizeRead) {
							//  Straight copy
							for (i = startArray; i<endArray; i++) {
								for (j = 0; j<valuesSizeRead; j++) {
									values[valuesPosition++] = dataIn[j];
								}
							}
						}
						else {
							//  Need to convert the data type
							for (i = startArray; i<endArray; i++) {
								convertDataType(&dataIn[0], &values[valuesPosition], valuesSizeRead, valuesSizeRequested);
								valuesPosition += valuesSizeRequested;
							}
						}
					}
				}
				else {
					//  Requesting values, but no values stored
					if (profileDepthsNumber == 0) {
						for (i = startArray; i<endArray; i++) {
							zsetMissing(&values[valuesPosition], valuesSizeRequested);
							valuesPosition += valuesSizeRequested;
						}
					}
					else {
						for (i = startArray; i<endArray; i++) {
							missing = (float)zmissingFlag();
							fillArray((void *)&missing, 1, &values[valuesPosition], valuesSizeRequested, profileDepthsNumber);
							valuesPosition += valuesSizeRequested * profileDepthsNumber;
						}
					}
				}
			}

			//  Uncompress quality flags
			if (qualitySizeRequested > 0) {
				if (qualityElementSize > 0) {
					//  Are the quality compressed?
					if (qualityCompressionFlag == 0) {
						//  No, just copy it
						if (qualitySizeRequested == qualityElementSize) {
							startElement = (startArray * qualityElementSize) + valuesNumber;
							endElement = (endArray * qualityElementSize) + valuesNumber;
							for (i = startElement; i<endElement; i++) {
								quality[qualityPosition++] = dataIn[i];
							}
						}
						else {
							//  Need to convert the data length
							for (i = startArray; i<endArray; i++) {
								dataPosition = (i * qualityElementSize) + valuesNumber;
								convertDataLength(&dataIn[dataPosition], &quality[qualityPosition], qualityElementSize, qualitySizeRequested);
								qualityPosition += qualitySizeRequested;
							}
						}
					}
					else if (qualityCompressionFlag == 1) {
						//  Regular compression - uncompress this array
						//  Get the position of the quality compression info
						//  Start with the position of the repeat compression info

						//  Was the values area compressed?  If so, jump past that
						if (valuesCompressionFlag == 1) {
							compressionHeaderPosition = ((numberStored - 1) / 32) + 1;
						}
						else {
							compressionHeaderPosition = 0;
						}
						ztsRegRepeatUncompress(startArray, endArray,
							qualityElementSize, qualitySizeRequested, 0, &header2[compressionHeaderPosition],
							&dataIn[valuesNumber], &quality[qualityPosition], (qualityArraySize - qualityPosition));
					}
					else if (qualityCompressionFlag == 2) {
						//  Same value for all data points (e..g., 0.0)
						if (qualitySizeRequested == qualityElementSize) {
							//  Straight copy
							for (i = startArray; i<endArray; i++) {
								for (j = 0; j<qualityElementSize; j++) {
									quality[qualityPosition++] = dataIn[j + valuesNumber];
								}
							}
						}
						else {
							//  Need to convert the data length
							for (i = startArray; i<endArray; i++) {
								dataPosition = valuesNumber;
								convertDataLength(&dataIn[dataPosition], &quality[qualityPosition], qualityElementSize, qualitySizeRequested);
								qualityPosition += qualitySizeRequested;
							}
						}
					}
				}
				else {
					//  Requesting quality, but no quality stored
					for (i = startArray; i<endArray; i++) {
						zsetUndefined(&quality[qualityPosition], qualitySizeRequested);
						qualityPosition += qualitySizeRequested;
					}
				}
			}

			//  Uncompress Notes
			if (inotesSizeRequested > 0) {
				if (inotesElementSize > 0) {
					//  Are the inotes compressed?
					if (inotesCompressionFlag == 0) {
						//  No, just copy it
						if (inotesSizeRequested == inotesElementSize) {
							//  Add the length from the values and quality
							startElement = (startArray * inotesElementSize) + valuesNumber + qualityNumber;
							endElement = (endArray * inotesElementSize) + valuesNumber + qualityNumber;
							for (i = startElement; i<endElement; i++) {
								inotes[inotesPosition++] = dataIn[i];
							}
						}
						else {
							//  Need to convert the data length
							for (i = startArray; i<endArray; i++) {
								dataPosition = (i * inotesElementSize) + valuesNumber + qualityNumber;
								convertDataLength(&dataIn[dataPosition], &inotes[inotesPosition], inotesElementSize, inotesSizeRequested);
								inotesPosition += inotesSizeRequested;
							}
						}
					}
					else if (inotesCompressionFlag == 1) {
						//  Get the position of the quality compression info
						compressionHeaderPosition = 0;
						//  Was the values area compressed?  If not, jump past that
						if (valuesCompressionFlag == 1) {
							compressionHeaderPosition += ((numberStored - 1) / 32) + 1;
						}
						//  Was the quality area compressed?  If not, jump past that
						if (qualityCompressionFlag == 1) {
							compressionHeaderPosition += ((numberStored - 1) / 32) + 1;
						}
						dataPosition = valuesNumber + qualityNumber;
						ztsRegRepeatUncompress(startArray, endArray,
							inotesElementSize, inotesSizeRequested, 0, &header2[compressionHeaderPosition],
							&dataIn[dataPosition], &inotes[inotesPosition], (inotesArraySize - inotesPosition));
					}
					else if (inotesCompressionFlag == 2) {
						//  Same value for all data points (e..g., 0.0)
						if (inotesSizeRequested == inotesElementSize) {
							//  Straight copy
							dataPosition = valuesNumber + qualityNumber;
							for (i = startArray; i<endArray; i++) {
								for (j = 0; j<inotesElementSize; j++) {
									inotes[inotesPosition++] = dataIn[j + dataPosition];
								}
							}
						}
						else {
							//  Need to convert the data length
							for (i = startArray; i<endArray; i++) {
								dataPosition = valuesNumber + qualityNumber;
								convertDataLength(&dataIn[dataPosition], &inotes[inotesPosition], inotesElementSize, inotesSizeRequested);
								inotesPosition += inotesSizeRequested;
							}
						}
					}
				}
				else {
					//  Requesting inotes, but no inotes stored
					for (i = startArray; i<endArray; i++) {
						zsetUndefined(&inotes[inotesPosition], inotesSizeRequested);
						inotesPosition += inotesSizeRequested;
					}
				}
			}
		}
		if ((cnotesReadLength > 0) && (cnotesSize > 0)) {
			//  Character string inotes are different in that we
			//  do no compression and there are no missing values
			//  Thus if no inotes with no information, a full block
			//  of '\0' is stored.    cnotes[cnotesPosition++]
			//  Add the length from the values and quality
			startElement = valuesNumber + qualityNumber;
			if (bigEndian()) {
				number = numberIntsInBytes(cnotesReadLength);
				zswitchInts((void *)&dataIn[startElement], number);
				carray = (char *)calloc(number + 1, 4);
				charLong((void *)&dataIn[startElement], (void *)carray, cnotesReadLength, ((number + 1) * 4), 0, 0);
			}
			else {
				carray = (char *)&dataIn[startElement];
			}
			carrayPosition = 0;
			if (startArray > 0) {
				//  Count through the first startArray lines
				carrayPosition = copyLines(cnotes, (size_t)0, carray, (size_t)cnotesReadLength, startArray);
			}
			numberRemaining = endArray - startArray;
			//endElement  = inotesNumber + valuesNumber + qualityNumber;
			icount = copyLines(&cnotes[cnotesPosition], (size_t)(cnotesSize - cnotesPosition),
				&carray[carrayPosition], (size_t)(cnotesReadLength - carrayPosition), numberRemaining);
			cnotesPosition += icount;
			carrayPosition += icount;
			if (bigEndian()) {
				free(carray);
			}
		}
	}

	//  Set any missing values after data in record ends
	if (positionRelativeLastValid > 0) {
		//  Figure out our position in the values array.
		//  (quality and inotes will be the same)
		position = numberStored - positionRelativeFirstValid;
		//  Is all the data to be read after the valid data
		if (position < 0) {
			//  Yes, just set the number requested to missing then
			positionRelativeLastValid += position;  // Should be the same as numberToRead
			position = 0;
		}
		//  Position gives us the logical location, but we need it relative to 4 byte words
		if (valuesSizeRequested) {
			if (profileDepthsNumber == 0) {
				valuesPosition = position * valuesSizeRequested;
				for (i = 0; i<positionRelativeLastValid; i++) {
					zsetMissing(&values[valuesPosition], valuesSizeRequested);
					valuesPosition += valuesSizeRequested;
				}
			}
			else {
				valuesPosition = position * valuesSizeRequested * profileDepthsNumber;
				for (i = 0; i<positionRelativeLastValid; i++) {
					missing = (float)zmissingFlag();
					fillArray((void *)&missing, 1, &values[valuesPosition], valuesSizeRequested, profileDepthsNumber);
					valuesPosition += valuesSizeRequested * profileDepthsNumber;
				}
			}
		}
		//  Less frequent cases
		if (qualitySizeRequested || inotesSizeRequested || cnotesSize) {
			qualityPosition = position * qualitySizeRequested;
			inotesPosition = position * inotesSizeRequested;
			for (i = 0; i<positionRelativeLastValid; i++) {
				//  Undefined in this case is just 0.
				if (qualitySizeRequested) {
					zsetUndefined(&quality[qualityPosition], qualitySizeRequested);
					qualityPosition += qualitySizeRequested;
				}
				if (inotesSizeRequested) {
					zsetUndefined(&inotes[inotesPosition], inotesSizeRequested);
					inotesPosition += inotesSizeRequested;
				}
				if ((cnotesSize > 0) && (cnotesPosition < cnotesSize)) {
					cnotes[cnotesPosition++] = '\0';
				}
			}
		}
	}
	if (cnotesSize > 0) {
		*cnotesLength = cnotesPosition;
	}
	//  All done!
	return status;
}

