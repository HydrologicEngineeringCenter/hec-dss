#include <stdlib.h>
#include <string.h>

#include "heclib.h"

/**
*  Function:	zstructArrayNew
*
*  Use:			Public
*
*  Description:	Creates a new array struct for storing and retrieving array data.
*
*  Declaration: zStructArray* zstructArrayNew(const char* pathname);
*
*  Parameters:	const char* pathname
*					The pathname of the record to store or retrieve.
*
*
*	Returns:	zStructArray*
*					An address to the struct for use with zarrayRetrieve() or zarrayStore().
*
*   Note:		ALWAYS call function zstructFree(struct) when finished using.
*
*	Remarks:	You must not reuse this struct.  Make a new one for every dataset.
*
*	See:		zarrayStore for use and definition
*
*	See Also:	zarrayStore()
*				zarrayRetrieve()
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

zStructArray* zstructArrayNew(const char* pathname)
{
	zStructArray *arrayStruct;
	int i;
	int len;

	len = sizeof(zStructArray);
	arrayStruct = (zStructArray*)calloc((size_t)len, BYTE_SIZE);

	for (i=0; i<zSTRUCT_length; i++) {
		arrayStruct->allocated[i] = 0;
	}

	arrayStruct->pathname = mallocAndCopyPath(pathname);
	arrayStruct->allocated[zSTRUCT_pathname] = 1;

	arrayStruct->structType = DATA_TYPE_ARRAY;

	return arrayStruct;
}

