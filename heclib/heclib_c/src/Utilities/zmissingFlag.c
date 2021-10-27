//#include <float.h>
#include "heclib.h"
#include "hecdssInternal.h"

/**
*  Function:	zmissingFlag functions
*
*  Use:			Public
*
*  Description:	 A set of small utility function to provide the missing flag and test for the missing flag
*
*  Declarations:	float zmissingFlag();					//  Returns float missing flag
*					float zmissingFlagFloat();				//  Returns float missing flag
*					double zmissingFlagDouble();			//  Returns double missing flag
*					int zisMissingFloat(float value);		//  Tests float value for missing, returns 1 if missing, 0 if not
*					int zisMissingDouble(double value);		//  Tests double value for missing, returns 1 if missing, 0 if not
*					void zsetMissingFloat(float *value);	//  Sets float value to missing
*					void zsetMissingDouble(double *value);	//  Sets double value to missing
*					void zsetMissing(int *value, int lengthValue);		//  Sets value to missing, length 1 for float, 2 for double
*					void zsetMissingFloatArray(float *values, int numberValues);	//  Sets a float array to missing
*					void zsetMissingDoubleArray(double *values, int numberValues);	//  Sets a double array to missing
*					void zsetUndefined(int *data, int dataLength);		//  Sets non "values" to zero (undefined) - mainly notes and quality
*
*
*	Remarks:		Missing flag is defined as negative max float: A negative of the maximum float
*					value for 32 bits;  -FLT_MAX
*					Note:  This is the same for both floats and doubles (a missing double is the
*					same, not the max double!)
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/



float zmissingFlag()
{
	// return (float)-901.0;
	return UNDEFINED_FLOAT;
}

float zmissingFlagFloat()
{
	//return (float)-901.0;
	return UNDEFINED_FLOAT;
}

double zmissingFlagDouble()
{
	//return (double)-901.0;
	return UNDEFINED_DOUBLE;
}

float zmissingfloat_()
{
	//return (float)-901.0;
	return UNDEFINED_FLOAT;
}

double zmissingdouble_()
{
	//return (double)-901.0;
	return UNDEFINED_DOUBLE;
}

int lismissingf_(float *value) {
	if (*value == -901.0) return 1;
	if (*value == -902.0) return 1;
	if (*value == UNDEFINED_FLOAT) return 1;
	return 0;
}

int lismissingd_(double *value) {
	if (*value == -901.0) return 1;
	if (*value == -902.0) return 1;
	if (*value == UNDEFINED_DOUBLE) return 1;
	return 0;
}


void zsetMissingFloat(float *value)
{
	*value = UNDEFINED_FLOAT;
}

void zsetMissingDouble(double *value)
{
	*value = UNDEFINED_DOUBLE;
}




int zisMissing(void *value, int lengthValue)
{
	float *f;
	double *d;
	if (lengthValue == 1) {
		f = (float *)value;
		return zisMissingFloat(*f);
	}
	else if (lengthValue == 2) {
		d = (double *)value;
		return zisMissingDouble(*d);
	}
	else {
		return 0;
	}
}

int zisMissingFloat(float value)
{
	if (value == zmissingFlagFloat()) {
		return 1;
	}
	else if (value == -901.0) {
		return 1;
	}
	else if (value == -902.0) {
		return 1;
	}
	else {
		return 0;
	}
}

int zisMissingDouble(double value)
{
	if (value == zmissingFlagDouble()) {
		return 1;
	}
	else if (value == -901.0) {
		return 1;
	}
	else if (value == -902.0) {
		return 1;
	}
	else {
		return 0;
	}
}

void zsetMissing(int *value, int lengthValue)
{
	if (lengthValue == 1) {
		zsetMissingFloat((float *)value);
	}
	else {
		zsetMissingDouble((double *)value);
	}
}

void zsetMissingFloatArray(float *values, int numberValues)
{
	int i;
	for (i=0; i<numberValues; i++) {
		zsetMissingFloat(&values[i]);
	}
}
void zsetMissingDoubleArray(double *values, int numberValues)
{
	int i;
	for (i=0; i<numberValues; i++) {
		zsetMissingDouble(&values[i]);
	}
}

//  This is for quality and notes, not data!
//  Here undefined is just zero.
void zsetUndefined(int *data, int dataLength)
{
	int i;
	for (i=0; i<dataLength; i++) {
		data[i] = 0;
	}
}

