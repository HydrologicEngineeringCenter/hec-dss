#include <stdlib.h>
#include <string.h>

#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "heclib.h"

/**
*  Function:	zstructTextNew
*
*  Use:			Public
*
*  Description:	Creates a new text struct for storing and retrieving text data.
*
*  Declaration: zStructText* zstructTextNew(const char* pathname);
*
*  Parameters:	const char* pathname
*					The pathname of the record to sotre or retrieve.
*
*
*	Returns:	zStructText*
*					An address to the struct for use with ztextRetrieve() or ztextStore().
*
*   Note:		ALWAYS call function zstructFree(struct) when finished using.
*
*	Remarks:	You must not reuse this struct.  Make a new one for every dataset.
*
*	See:		ztextStore for use and definition
*
*	See Also:	zstructTextStringNew()
*				ztextStore()
*				ztextRetrieve()
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

zStructText* zstructTextNew(const char* pathname)
{
	zStructText *textStruct;
	int i;
	int len;

	len = sizeof(zStructText);
	textStruct = (zStructText*)calloc((size_t)len, BYTE_SIZE);

	for (i=0; i<zSTRUCT_length; i++) {
		textStruct->allocated[i] = 0;
	}

	textStruct->pathname = mallocAndCopyPath(pathname);
	textStruct->allocated[zSTRUCT_pathname] = 1;

	textStruct->structType = DATA_TYPE_TEXT;

	return textStruct;
}

