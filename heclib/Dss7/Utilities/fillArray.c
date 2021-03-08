#include "hecdssInternal.h"

/**
*  Function:	fillArray
*
*  Use:			Private
*
*  Description:	 Copies a (single) value to all elements in an array.  Can be either int, float or double.
*
*  Declaration: void fillArray(int *valueIn, int valueInElementLength, int *arrayOut,  int arrayOutElementLength, int numberOut);
*
*  Parameters:	void *valueIn
*					The value to fill the array with.
*
*				int valueInElementLength
*					The word length of valueIn; 1 = int or float, 2 = double
*
*				void *arrayOut (output)
*					The array to fill.
*
*				int arrayOutElementLength
*					The word length of each element in arrayOut.  Usually same as valueInElementLength.
*
*				int numberOut
*					The number of values in the array to fill
*
*  Returns:		None.
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


void fillArray(int *valueIn, int valueInElementLength, int *arrayOut,  int arrayOutElementLength, int numberOut)
{
	int i;
	int iposOut;

	for (i=0; i<numberOut; i++) {
		iposOut = i * arrayOutElementLength;
		convertDataType(valueIn, &arrayOut[iposOut], valueInElementLength, arrayOutElementLength);
	}
}


