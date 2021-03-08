#include <stdio.h>

#include "hecdssInternal.h"

/**
*  Function:	convertDataLength
*
*  Use:			Private
*
*  Description:	Convert an int from one length to another by either truncating or zero filling end.
*
*  Declaration: void convertDataLength(int *dataIn, int *dataOut, int dataInElementLength, int dataOutElementLength);
*
*  Parameters:	void *dataIn
*					The array to convert from.
*
*				void *dataOut (output)
*					The array to convert too.
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
*				Make sure *dataOut is dimensioned correctly; no length checking is done.
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

void convertDataLength(int *dataIn, int *dataOut, int dataInElementLength, int dataOutElementLength)
{
	int i;
	if (dataInElementLength == dataOutElementLength) {
		//  Direct copy
		for (i=0; i<dataOutElementLength; i++) {
			dataOut[i] = dataIn[i];
		}
	}
	else if (dataInElementLength > dataOutElementLength) {
		//  Truncate to output length
		for (i=0; i<dataOutElementLength; i++) {
			dataOut[i] = dataIn[i];
		}
	}
	else {  //  dataOutElementLength > dataInElementLength
		for (i=0; i<dataInElementLength; i++) {
			dataOut[i] = dataIn[i];
		}
		//  Zero fill remaining length
		for (i=dataInElementLength; i<dataOutElementLength; i++) {
			dataOut[i] = 0;
		}
	}
}

