#include <stdlib.h>

#include "zdssKeys.h"
#include "hecdss7.h"

/**
*  Function:	zwhatChanged
*
*  Use:			Public
*
*  Description:	Gets the pathnames for records that have changed since a previous point
*
*  Declaration: int zwhatChanged(long long *ifltab, zStructCatalog *catStructChanged);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.*  Parameters:	const char *pathname1:  The first pathname to compare
*
*				zStructCatalog *catStructChanged:
*					An empty catalog struct that will contain the changed pathnames, etc.   Be sure to free
*					it after you are done.
*
*
*  Returns:		Number of records that have changed and a fill out catStructChanged.
*
*  Remarks:		DSS Version 7 only
*
*  See Also:	zwhatChangedCompare(), an extended version of this.
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zwhatChanged(long long *ifltab, zStructCatalog *catStructChanged)
{
	int status;

	zStructCatalog *catStruct;

	catStruct = (zStructCatalog *)ifltab[zdssKeys.kcatStruct];
	if (!catStruct) {
		return 0;
	}

	status = zwhatChangedCompare(ifltab, catStruct, catStructChanged, catStruct->pathWithWildChars, catStruct->boolGetCRCvalues);

	return status;
}

