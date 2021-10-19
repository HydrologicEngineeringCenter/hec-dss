#include "hecdssInternal.h"


/**
*  Function:	ztsRegRepeatCompress
*
*  Use:			Private
*
*  Description:	Compress a regular interval time series array using repeats.
*
*  Declaration:
*				void ztsRegRepeatCompress(int *dataIn, int numberIn, int sizeEachData,
*										  int *dataCompressedOut, int dataCompressedOutSize, 
										  int *header2, int header2Size);
*
*  Parameters:
*				int *dataIn
*					The data array read to be compressed and stored.
*
*				int numberIn
*					The number of data in dataIn to compress.
*
*				int sizeEachData
*					The size of each data item in array dataIn, in int 4 words.
*
*				int *dataCompressedOut
*					The compressed array to be stored to DSS.
*
*				int dataCompressedOutSize
*					The size (in ints) of dataCompressedOut.
*
*				int *header2
*					The compression header.  Each bit in the header indicates a repeat value.
*					If the bit is on (1), the logical value for that position is repeat - the
*					same as the previous value.  If it is off (0), then the dataIn array
*					contains a new value for that position.
*
*				int header2Size
*					The size (in ints) of header2.
*
*
*	Returns:	Length of compressed data (dataCompressedOut) (number of ints compressed.)
*
*	Remarks:	This function does no conversion, unlike its counterpart ztsRegRepeatUncompress.
*					Thus, it is a simpler implementation, and just compares current value to
*					the previous value to determine if they are the same and then marks as
*					a repeating value or not.
*					Each value has a bit in the compression header, with
*					0 meaning this is a new, non-repeated value, 1 means it is a repeat.
*
*	See Also:	ztsRegRepeatUncompress()
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int ztsRegRepeatCompress(int *dataIn, int numberIn, int sizeEachData, 
	int *dataCompressedOut, int dataCompressedOutSize, int *header2, int header2Size)
{
	int i, j, k, n;
	int icount;
	int jcount;
	int word;
	int position;
	int same;  //  boolean


	//  Copy the first value
	jcount = 0;
	for (j=0; j<sizeEachData; j++) {
		dataCompressedOut[j] = dataIn[j];
		jcount++;
	}
	icount = 1;	
	for (i=1; i<numberIn; i++) {
		//  Determine if this is a repeat value
		same = 1;
		for (j=0; j<sizeEachData; j++) {
			k = (i * sizeEachData) + j;
			n = ((i - 1) * sizeEachData) + j;
			if (dataIn[k] != dataIn[n]) {
				same = 0;
				break;
			}
		}
		if (same) {
			//  Yes, set the bit on for this location in the array
			word = i / 32;
			position = i - (word * 32);
			assert(word < header2Size);
			header2[word] |= 1 << position;
		}
		else {
			//  No, copy the data value.  The bit will be off because of header2 calloc
			for (j=0; j<sizeEachData; j++) {
				k = (i * sizeEachData) + j;
				n = (icount * sizeEachData) + j;
				assert(n < dataCompressedOutSize);
				dataCompressedOut[n] = dataIn[k];
				jcount++;
			}
			icount++;
		}
	}
	return jcount;
}

