#include <stdio.h>

#include "hecdssInternal.h"
/**
*  Function:	doubleToFloat
*
*  Use:			Private
*
*  Description:	 A tiny convenience routine to convert a double to a float.  This is used for in place conversion.
*
*  Declaration: void doubleToFloat(double *d, float *f);
*
*  Parameters:	double *d
*					The double value to convert from.
*
*				float *f (output)
*					The float value to convert too.
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

void doubleToFloat(double *d, float *f)
{
	f[0] = (float)d[0];
}

