#include <stdlib.h>
#include <string.h>

#include "heclib.h"

/**
*  Function:	zstructSpatialTinNew
*
*  Use:			Public
*
*  Description:	Creates a new spatial TIN struct
*
*  Declaration: zStructSpatialTin* zstructSpatialTinNew(const char* pathname);
*
*  Parameters:	const char* pathname
*					The pathname for this struct.  Must be a valid pathname.
*					A copy of the pathname is used in struct.
*
*
*
*	Returns:	zStructSpatialTin*
*					An address to the struct created.
*
*   Note:		ALWAYS call function zstructFree(struct) when finished using.
*
*	See:		zspatialTinStore and zspatialTinRetrieve for more information
*
*
*
*	Author:			Tom Evans
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


zStructSpatialTin* zstructSpatialTinNew(const char* pathname)
{
	zStructSpatialTin *tinStruct;
	int i;
	int len;

	len = sizeof(zStructSpatialTin);
	tinStruct = (zStructSpatialTin*)calloc((size_t)len, BYTE_SIZE);
	if (!tinStruct) {
		return tinStruct;
	}

	for (i=0; i<zSTRUCT_length; i++) {
		tinStruct->allocated[i] = 0;
	}

	if (pathname) {
		tinStruct->pathname = mallocAndCopyPath(pathname);
		tinStruct->allocated[zSTRUCT_pathname] = 1;
	}

	tinStruct->connectTableLen = 6;

	tinStruct->structType = DATA_TYPE_SPATIAL_TIN;

	return tinStruct;
}

