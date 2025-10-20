#include "heclib7.h"
#include "hecdssFort.h"
#include "hecdssInternal.h"
#include "zdssKeys.h"

/**
*  Function:	zhandle
*
*  Use:			Semi-private
*
*  Description:	Returns the handle number of the associated file.  For both version 6 and 7
*
*  Declaration: int zhandle(long long *ifltab);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*
*	Returns:	int handle
*					The handle of the file, or -1 if not opened.
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int zhandle(long long *ifltab)
{
	if (zgetVersion(ifltab) == 7) {
		return (int)ifltab[zdssKeys.khandle];
	}
	return 0; 
	
}



