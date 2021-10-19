#include "heclib.h"


/**
*  Function:	zcollectionCat
*
*  Use:			Public
*
*  Description:	Convenience function to retrieve a pathname collection set (all paths in a collection
*
*  Declaration: int zcollectionCat(long long *ifltab, const char *seedPathname, zStructCatalog *catStruct)
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				const char *seedPathname
*					A (valid) collection pathname for the set desired.  Wild character are not supported here.
*					Collections are identified by an F part of /C:000000|REST OF FPART/
*					Where 00000 is generally a sequence number, for example
*					/YUBA/SMARTSVILLE/FLOW/01JAN1997/1HOUR/C:000042|OPERATION A/
*
*				zStructCatalog *catStruct
*					A struct created by function zstructCatalogNew().  This struct will hold a list of all
*					the collection pathnames returned by this call.  See zStructCatalog.h for definition.
*					Be sure to call function zstructFree(struct) when finished.
*
*
*
*	Returns:	int numberCataloged
*					Number of collection pathnames for this set (a positive number)
*					or, if a negative number, then errorCode for error
*					errorCode may be decoded by function zerrorDecode()
*
*
*	Remarks:	For both DSS Version 6 and DSS Version 7 files.
*					Calls the appropriate functions for each version.
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zcollectionCat(long long *ifltab, const char *seedPathname, zStructCatalog *catStruct)
{
	int numberPaths;

	numberPaths = zcatInternalSort (ifltab, seedPathname, catStruct, 0, 0, 1);

	return numberPaths;
}



