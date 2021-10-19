#include <stdlib.h>
#include <string.h>

#include "hecdss7.h"

/**
*  Function:	zstructRecordBasicsNew
*
*  Use:			Semi-Public
*
*  Description:	Creates a new struct about basic information for a record
*
*  Declaration: zStructRecordBasics* zstructRecordBasicsNew(const char* pathname);
*
*  Parameters:	const char* pathname
*					The pathname of the record to query.
*
*
*	Returns:	zStructRecordBasics*
*					An address to the struct created ready to be used with zgetRecordBasics().
*
*   Note:		ALWAYS call function zstructFree(struct) when finished using.
*
*	Remarks:	You must not reuse this struct.  Make a new one for every record.
*
*	See Also:	zgetRecordBasics()
*
*
*	Author:			Bill Charley
*	Date:			2017
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


zStructRecordBasics* zstructRecordBasicsNew(const char* pathname)
{
	zStructRecordBasics* zrecBasics;
	int len;
	int i;

	len = sizeof(zStructRecordBasics);
	zrecBasics = (zStructRecordBasics*)calloc((size_t)len, BYTE_SIZE);

	//  Record Size struct
	zrecBasics->structType = STRUCT_TYPE_RECORD_BASICS;

	for (i=0; i<zSTRUCT_length; i++) {
		zrecBasics->allocated[i] = 0;
	}

	if (pathname) {
		zrecBasics->pathname = mallocAndCopyPath(pathname);
		zrecBasics->allocated[zSTRUCT_pathname] = 1;
	}

	return zrecBasics;
}

