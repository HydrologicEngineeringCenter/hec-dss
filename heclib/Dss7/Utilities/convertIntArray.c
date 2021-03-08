#include "hecdssInternal.h"

/**
*  Function:	convertIntArray
*
*  Use:			Private
*
*  Description:	 Converts ints so that dataIn may == dataOut
*
*  Declaration: void convertIntArray(int *dataIn, int *dataOut, int number, int dataInElementLength, int dataOutElementLength);
*
*  Parameters:	void *dataIn
*					The array to convert from.
*
*				void *dataOut (output)
*					The array to convert too.
*
*				int number
*					The number of values to convert
*
*				int dataInElementLength
*					The word length for dataIn.
*
*				int dataOutElementLength
*					The word length for dataOut.
*
*  Returns:		None.
*
*  Remarks:		This is usually used where the note length requested is different from what was read.
*				Can convert in place - same array output as input, so does not overwrite.
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


void convertIntArray(int *dataIn, int *dataOut, int number, int dataInElementLength, int dataOutElementLength)
{
	int i;
	int iposIn;
	int iposOut;

	if (dataInElementLength == dataOutElementLength) {
		for (i=0; i<number; i++) {
			iposIn = i * dataInElementLength;
			iposOut = i * dataOutElementLength;
			convertDataLength(&dataIn[iposIn], &dataOut[iposOut], dataInElementLength, dataOutElementLength);
		}
	}
	else if (dataInElementLength > dataOutElementLength) {
		//  longer ints to shorter ints
		for (i=0; i<number; i++) {
			iposIn = i * dataInElementLength;
			iposOut = i * dataOutElementLength;
			convertDataLength(&dataIn[iposIn], &dataOut[iposOut], dataInElementLength, dataOutElementLength);
		}
	}
	else {
		//  shorter ints to longer ints
		//  Start at the end so that we don't write over ourselves...
		for (i=(number-1); i>=0; i--) {
			iposIn = i * dataInElementLength;
			iposOut = i * dataOutElementLength;
			convertDataLength(&dataIn[iposIn], &dataOut[iposOut], dataInElementLength, dataOutElementLength);
		}
	}
}



