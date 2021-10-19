#include <stdlib.h>
#include <string.h>

#include "hecdss7.h"

/**
*  Function:	zstructCatalogNew
*
*  Use:			Public
*
*  Description:	Creates a new catalog struct to hold a list of pathnames in a DSS file
*
*  Declaration: zStructCatalog* zstructCatalogNew();
*
*  Parameters:	None
*
*
*
*	Returns:	zStructCatalog*
*					An address to the catalog struct created.
*
*   Note:		ALWAYS call function zstructFree(struct) when finished using.
*
*	Remarks:	This is the standard struct used by the various DSS catalog functions.
*
*
*
*	Author:			Bill Charley
*	Date:			2015
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


zStructCatalog* zstructCatalogNew()
{
	zStructCatalog *catStruct;
	int i;
	int len;

	len = sizeof(zStructCatalog);
	catStruct = (zStructCatalog*)calloc((size_t)len, BYTE_SIZE);
	if (!catStruct) {
		return catStruct;
	}

	for (i=0; i<zSTRUCT_length; i++) {
		catStruct->allocated[i] = 0;
	}

	catStruct->boolIncludeDates = 0;
	catStruct->boolSorted = 0;
	catStruct->statusWanted = 0;
	catStruct->typeWantedStart = 0;
	catStruct->typeWantedEnd = 0;

	catStruct->structType = STRUCT_TYPE_CATALOG;


	return catStruct;
}

