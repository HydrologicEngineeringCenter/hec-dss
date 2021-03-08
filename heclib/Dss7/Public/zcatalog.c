#include "heclib.h"


/**
*  Function:	zcatalog
*
*  Use:			Public
*
*  Description:	Primary function to retrieve a catalog (list of pathnames) in the DSS file
*
*  Declaration: int zcatalog(long long *ifltab, const char *pathWithWild, zStructCatalog *catStruct, int boolSorted);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				const char *pathWithWildChars
*					Either null (for ignore) or a String that represents a pathname with wild characters represented
*					by a star (*) to match any string in the pathname part.  Wild characters can only be at the beginning or end of a part,
*					not inside of a string.  An example is a C part with "*Flow*" , which
*					will match all pathnames that have "Flow" anywhere in the C part, such as "Flow", "Inflow", "Outflow-Reg", etc.
*					A part such as "Flow*Reg" is not legal.  A null (//) will only match a null, where only a star (*) will match all.
*
*				zStructCatalog *catStruct
*					A struct created by function zstructCatalogNew().  This struct will hold a list of all
*					pathnames returned by this call.  See zStructCatalog.h for definition.
*					Be sure to call function zstructFree(struct) when finished.
*
*				int sortCollectionFlag
*					A flag indicating if the pathname list should be sorted or sorted for collections.
*					Sorting takes considerable more resources, so only use if you need to.
*						0:  Do not sort (fastest)
*						1:  Normal sort
*						2:  Collection sort (slowest)
*
*
*
*	Returns:	int numberCataloged
*					Number of pathnames in the file (a positive number)
*					or, if a negative number, then errorCode for error
*					errorCode may be decoded by function zerrorDecode()
*
*
*	Remarks:	For both DSS Version 6 and DSS Version 7 files.
*					Calls the appropriate functions for each version.
*
*	See Also:	zstructCatalogNew(), zcatalogToFile(), zcatalogFile()
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zcatalog(long long *ifltab, const char *pathWithWild, zStructCatalog *catStruct, int sortCollectionFlag)
{
	int numberPaths;
	int boolCollection;

	if (!catStruct) {
		return -1;
	}

	if (sortCollectionFlag) {
		if (sortCollectionFlag == 1) {
			boolCollection = 0;
		}
		else {
			boolCollection = 1;
		}
		numberPaths = zcatInternalSort (ifltab, pathWithWild, catStruct, 0, 0, boolCollection);
	}
	else {
		numberPaths = zcatalogInternal (ifltab, pathWithWild, catStruct, 0, 0, 0, 0, 0);
	}

	return numberPaths;
}



