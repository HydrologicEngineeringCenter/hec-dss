#include <stdlib.h>
#include <string.h>

#include "hecdss7.h"

/**
*  Function:	zstructPdNew
*
*  Use:			Public
*
*  Description:	Creates a new paired data struct for retrieving a paired data set.
*
*  Declaration: zStructPairedData* zstructPdNew(const char* pathname);
*
*  Parameters:	const char* pathname
*					The pathname of the record to retrieve.  The pathname must be exact, but is case insensitive
*					(i.e., "Apart" is the same as "APART"), and have a correct C (independnet-dependent name) part.
*					For example, "/Stage-Damage/", where stage is the independent parameter and Damage is the dependent.
*
*
*	Returns:	zStructPairedData*
*					An address to the paired struct created ready to be retrieved with zpdRetrieve().
*
*   Note:		ALWAYS call function zstructFree(struct) when finished using.
*
*	Remarks:	You must not reuse this struct.  Make a new one for every dataset.
*
*	See:		zpdRetriev for use and definition of zStructPairedData
*
*	See Also:	zstructPdNewDoubles()
*				zstructPDsNewFloats()
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

zStructPairedData* zstructPdNew(const char* pathname)
{
	zStructPairedData *pds;
	int i;
	int len;

	len = sizeof(zStructPairedData);
	pds = (zStructPairedData*)calloc((size_t)len, BYTE_SIZE);

	for (i=0; i<zSTRUCT_length; i++) {
		pds->allocated[i] = 0;
	}

	if (pathname) {
		pds->pathname = mallocAndCopyPath(pathname);
		pds->allocated[zSTRUCT_pathname] = 1;
	}

	pds->xprecision = -1;
	pds->yprecision = -1;
	pds->boolIndependentIsXaxis = 1;  //  Default (1 or 2 valid)

	//  Paired Data type
	pds->structType = DATA_TYPE_PD;

	pds->locationStruct = zstructLocationNew(pathname);
	pds->allocated[zSTRUCT_PD_locationStruct] = 1;

	return pds;
}

