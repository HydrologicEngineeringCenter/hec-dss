//#include <float.h>
#include "heclib.h"
#include "hecdssInternal.h"


/*

	Missing flag is defined as negative max float: A negative of the maximum float
	value for 32 bits;  -FLT_MAX
	Note:  This is the same for both floats and doubles (a missing double is the
	same, not the max double!)

	float zmissingFlag():  Returns the missing flag for HEC-DSS version 7.

	zsetMissingFloat, zsetMissingDouble:  Tiny utility functions to set a value to missing
	These functions are needed because the value passed in may come from an int
	but really be either a single or double (i.e., they might be called with an int array)

	zisMissing, zisMissingDouble:  Utilities for testing if a value is missing or not.
	Returns true (1) if value is missing, false (0) if not missing.

*/

//  FORTRAN Compatible functions


/*
	For FORTRAN, it is easiest to use zsetmissing
	But, if you insist:
		real zmissingflag
		external zmissingflag
		...
		x = zsetmissingfloat()

*/
float zmissingflag_()
{
	return zmissingFlag();
}


void zsetmissingfloat_(float *value)
{
	*value = zmissingFlag();
}

void zsetmissingdouble_(double *value)
{
	*value = zmissingFlagDouble();
}

int zismissingfloat_(float *value)
{
	if (*value == zmissingFlag()) {
		return 1;
	}
	else {
		return 0;
	}
}

int zismissingdouble_(double *value)
{
	if (*value == zmissingFlagDouble()) {
		return 1;
	}
	else {
		return 0;
	}
}

