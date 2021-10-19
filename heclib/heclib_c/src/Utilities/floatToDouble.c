#include <stdio.h>


/**
*  Function:	floatToDouble
*
*  Use:			Private
*
*  Description:	 A tiny convienence routine to convert a float to a double.  This is used for in place conversion.
*
*  Declaration: void floatToDouble(float *f, double *d);
*
*  Parameters:	float *f
*					The float value to convert from.
*
*				double *d (output)
*					The double value to convert too.
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

void floatToDouble(float *f, double *d)
{
	d[0] = (double)f[0];
}

