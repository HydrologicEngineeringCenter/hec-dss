#include "hecdssInternal.h"


/**
*  Function:	ztsRegRepeatUncompress
*
*  Use:			Private
*
*  Description:	Uncompress a regular interval time series array that has been compressed using repeats.
*
*  Declaration:
*				void ztsRegRepeatUncompress(int start, int end, int valueElementSize,
*											int sizeRequested, int boolIsAValue,
*											int *header2, int *dataIn, int *dataOut, int dataOutSize);
*
*  Parameters:
*				int start
*					The starting position in the array dataIn to start uncompressing.
*					If we do not need the first part of the array (e.g., we only want the second
*					half of a monthly block), then this tells us where we have to uncompress to
*					before we start using
*
*				int end
*					The ending position in the array dataIn to stop uncompressing.
*
*				int valueElementSize
*					The size of each value in array dataIn, where 1 is float, 2 is double.
*
*				int sizeRequested
*					The size of the value requested by the user, where 1 is float, 2 is double.
*					This routine will convert data sizes from what was read to what was wanted.
*
*				int boolIsAValue
*					A boolean set to 1 if this part of the array is the values section.
*					If the item is a value, then a float would have to be converted to a
*					double, not just copied.  If this is set false, words are directly copied.
*
*				int *header2
*					The compression header.  Each bit in the header indicates a repeat value.
*					If the bit is on (1), the logical value for that position is repeat - the
*					same as the previous value.  If it is off (0), then the dataIn array
*					contains a new value for that position.
*
*				int *dataIn
*					The data array read from DSS.  This is what is being uncompressed.
*
*				int *dataOut
*					The array the uncompressed data is stored in.
*
*				int dataOutSize
*					The array size (ints) for dataOut.
*
*
*	Returns:	None
*
*	Remarks:	The array consists of the values portion (floats/doubles) and
*					quality and notes (all may be compressed).  This function is
*					called for each part (values, quality,...) of the array
*					Each value has a bit in the compression header, with
*					0 meaning this is a new, non-repeated value, 1 means it is a repeat.
*
*	See Also:	ztsRegRepeatCompress()
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

void ztsRegRepeatUncompress(int start, int end, int valueElementSize,
						   int sizeRequested, int boolIsAValue,
						   int *header2, int *dataIn, int *dataOut, int dataOutSize)
{
	int i, j, n;
	int ipos;
	int word;
	int position;
	int icount;
	int bit;



	//  Get the first value
	icount = 0;
	if (start == 0) {
		//  It's at the beginning of the array
		if (valueElementSize == sizeRequested) {
			for (j=0; j<valueElementSize; j++) {
				dataOut[j] = dataIn[icount++];
			}
		}
		else if (boolIsAValue) {
			convertDataType(dataIn, dataOut, valueElementSize, sizeRequested);
			icount = valueElementSize;
		}
		else {
			convertDataLength(dataIn, dataOut, valueElementSize, sizeRequested);
			icount = valueElementSize;
		}
	}
	else {
		//  We need to uncompress all the values before to find the first value (ugh!)
		//  Now go forward until we reach start (not start -1)
		for (i=0; i<start+1; i++) {
			word = i / 32;
			position = i - (word * 32);
			//  Is the repeat bit set for this value?
			bit = header2[word] & (1 << position);
			if (!bit) {
				if (valueElementSize == sizeRequested) {
					for (j=0; j<valueElementSize; j++) {
						dataOut[j] = dataIn[icount++];
					}
				}
				else if (boolIsAValue) {
					convertDataType(&dataIn[icount], &dataOut[0], valueElementSize, sizeRequested);
					icount += valueElementSize;
				}
				else {
					convertDataLength(&dataIn[icount], &dataOut[0], valueElementSize, sizeRequested);
					icount += valueElementSize;
				}
			}
			//  Don't care about repeats - just looking for start value
		}
	}

	//  Now we have the value at our starting location
	//  Only one value?
	if (start == end) {
		return;
	}

	//  Uncompress the remaining
	//  Set ipos to second value
	ipos = sizeRequested;
	for (i=(start+1); i<end; i++) {
		assert(ipos < dataOutSize);
		word = i / 32;
		position = i - (word * 32);
		//  Is the repeat bit set for this value?
		bit = header2[word] & (1 << position);
		if (bit) {
			//  Yes, copy all the words from the previous value
			for (j=0; j<sizeRequested; j++) {
				n = ipos - sizeRequested;
				dataOut[ipos] = dataOut[n];
				ipos++;
			}
		}
		else {
			//  No, use the value from dataIn
			if (valueElementSize == sizeRequested) {
				for (j=0; j<valueElementSize; j++) {
					dataOut[ipos++] = dataIn[icount++];
				}
			}
			else {
				if (boolIsAValue) {
					convertDataType(&dataIn[icount], &dataOut[ipos], valueElementSize, sizeRequested);
				}
				else {
					convertDataLength(&dataIn[icount], &dataOut[ipos], valueElementSize, sizeRequested);
				}
				icount += valueElementSize;
				ipos += sizeRequested;
			}
		}
	}
}


