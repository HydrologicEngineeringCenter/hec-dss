#include <stdlib.h>
#include <string.h>

#include "hecdss7.h"

/**
*  Function:	zstructRecordSizeNew
*
*  Use:			Semi-Public
*
*  Description:	Creates a new record size struct so internal functions can calculate the amount of space to allocate
*
*  Declaration: zStructRecordSize* zstructRecordSizeNew(const char* pathname);
*
*  Parameters:	const char* pathname
*					The pathname of the record to query.
*
*
*	Returns:	zStructRecordSize*
*					An address to the paired struct created ready to be used with zgetRecordSize().
*
*   Note:		ALWAYS call function zstructFree(struct) when finished using.
*
*	Remarks:	You must not reuse this struct.  Make a new one for every record.
*
*	See Also:	zgetRecordSize()
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


zStructRecordSize* zstructRecordSizeNew(const char* pathname)
{
	zStructRecordSize* zrecSize;
	int len;
	int i;

	len = sizeof(zStructRecordSize);
	zrecSize = (zStructRecordSize*)calloc((size_t)len, BYTE_SIZE);

	//  Record Size struct
	zrecSize->structType = STRUCT_TYPE_RECORD_SIZES;

	for (i=0; i<zSTRUCT_length; i++) {
		zrecSize->allocated[i] = 0;
	}

	if (pathname) {
		zrecSize->pathname = mallocAndCopyPath(pathname);
		zrecSize->allocated[zSTRUCT_pathname] = 1;
	}

	return zrecSize;
}

