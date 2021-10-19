#include <stdio.h>

//  Convert an array of floats into doubles, in place
//  This works from the end of the array so that the same array
//  is used without writing over any values
//
//  Data types are mixed, so we avoid header declarition to avoid
//  a compiler error
//
//  BE SURE your array is dimension to hold all double values!
//
//
void floatToDouble(int *dataIn, int *dataOut);

void floatsToDoubles(int *values, int numberValues)
{
	int i;
	int idoublePos;

	for (i=(numberValues-1); i>=0; i--) {
		idoublePos = i * 2;
		floatToDouble(&values[i], &values[idoublePos]);
	}
}

