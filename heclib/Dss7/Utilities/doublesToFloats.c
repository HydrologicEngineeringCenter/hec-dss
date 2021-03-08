#include <stdio.h>

//  Convert an array of doubles into floats, in place
//  This works from the end of the array so that the same array
//  is used without writing over any values
//
//  Data types are mixed, so we avoid header declaration to avoid
//  a compiler error
//
//  BE SURE your array is dimension to hold all double values!
//
//

void doubleToFloat(int *doubleValue, int *floatValue);

void doublesToFloats(int *values, int numberValues)
{
	int i;
	int idoublePos;

	//  (Assembly code copy should prevent first value from being mangled)
	for (i=0; i<numberValues; i++) {
		idoublePos = i * 2;
		doubleToFloat(&values[idoublePos], &values[i]);
	}
}

