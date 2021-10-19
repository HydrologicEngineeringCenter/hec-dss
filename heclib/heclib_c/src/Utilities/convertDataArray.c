#include <stdio.h>


#include "hecdssInternal.h"


/**
*  Function:	convertDataArray
*
*  Use:			Semi public
*
*  Description:	 Converts an array of doubles to floats or an array of floats to doubles
*
*  Declaration: void convertDataArray(int *dataIn, int *dataOut, int number, int dataInElementLength, int dataOutElementLength);
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
*					The word length for dataIn.  1 is for int or float, 2 is for double
*
*				int dataOutElementLength
*					The word length for dataOut.  1 is for int or float, 2 is for double
*
*  Returns:		None.
*
*  Remarks:		Make sure *dataOut is dimensioned correctly; no length checking is done.
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


void convertDataArray(int *dataIn, int *dataOut, int number, int dataInElementLength, int dataOutElementLength)
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
		//  doubles to floats
		for (i=0; i<number; i++) {
			iposIn = i * dataInElementLength;
			iposOut = i * dataOutElementLength;
			convertDataType(&dataIn[iposIn], &dataOut[iposOut], dataInElementLength, dataOutElementLength);
		}
	}
	else {
		//  floats to doubles
		//  Start at the end so that we don't write over ourselves...
		for (i=(number-1); i>=0; i--) {
			iposIn = i * dataInElementLength;
			iposOut = i * dataOutElementLength;
			convertDataType(&dataIn[iposIn], &dataOut[iposOut], dataInElementLength, dataOutElementLength);
		}
	}
}

