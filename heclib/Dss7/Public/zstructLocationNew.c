#include <stdlib.h>
#include <string.h>

#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "heclib.h"


/**
*  Function:	zstructLocationNew
*
*  Use:			Public
*
*  Description:	Creates a new location struct
*
*  Declaration: zStructLocation* zstructLocationNew(const char* pathname);
*
*  Parameters:	const char* pathname
*					The pathname for this location or dataset.
*					Does NOT need the location C part, as that
*					will be provided automatically by the
*					retrieve or store functions
*
*
*	Returns:	zStructLocation*
*					An address to the location struct created.
*
*   Note:		ALWAYS call function zstructFree(struct) when finished using.
*
*	See:		zStructLocation.h for contents
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

zStructLocation* zstructLocationNew(const char* pathname)
{
	zStructLocation *locationStruct;
	int i;
	int len;

	len = sizeof(zStructLocation);
	locationStruct = (zStructLocation*)calloc((size_t)len, BYTE_SIZE);

	for (i=0; i<zSTRUCT_length; i++) {
		locationStruct->allocated[i] = 0;
	}

	if (pathname) {
		locationStruct->pathname = mallocAndCopyPath(pathname);
		locationStruct->allocated[zSTRUCT_pathname] = 1;
	}

	locationStruct->timeZoneName = 0;
	locationStruct->supplemental = 0;

	//  Location struct
	locationStruct->structType = DATA_TYPE_LOCATION;

	return locationStruct;
}

