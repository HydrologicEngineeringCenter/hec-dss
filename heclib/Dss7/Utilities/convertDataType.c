#include <stdio.h>


//  #include "hecdssInternal.h"  Don't include!

/**
*  Function:	convertDataType
*
*  Use:			Semi public
*
*  Description:	 Converts a double to a float or a float to a double or copies value (int, float or double)
*
*  Declaration: void convertDataType(void *dataIn, void *dataOut, int dataInElementLength, int dataOutElementLength);
*
*  Parameters:	void *dataIn
*					A (address to a) value or array to convert from.
*
*				void *dataOut (output)
*					A (address to a) value or array to convert too.
*
*				int dataInElementLength
*					The word length for dataIn.  1 is for int or float, 2 is for double
*
*				int dataOutElementLength
*					The word length for dataOut.  1 is for int or float, 2 is for double
*
*  Returns:		None.
*
*  Remarks:		To convert from float to double, dataInElementLength = 1 and dataOutElementLength = 2.
*				To copy an array of the same length, make dataInElementLength = dataOutElementLength = number of words to copy
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


//  Don't include in header, as int obvisouly is not a float or double
void floatToDouble(int *dataIn, int *dataOut);
void doubleToFloat(int *dataIn, int *dataOut);

void convertDataType(int *dataIn, int *dataOut, int dataInElementLength, int dataOutElementLength)
{
	int i;

	if (dataInElementLength == dataOutElementLength) {
		for (i=0; i<dataOutElementLength; i++) {
			dataOut[i] = dataIn[i];
		}
	}
	else if ((dataInElementLength == 1) && (dataOutElementLength == 2)) {
		floatToDouble(dataIn, dataOut);
	}
	else if ((dataInElementLength == 2) && (dataOutElementLength == 1)) {
		doubleToFloat(dataIn, dataOut);
	}
	else {
		//  This is undefined
	}
}

