#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#include "hecdss7.h"

/**
*  Function:	ztssrch7_
*
*  Use:			Private - Interface for Fortan (depricated)
*
*  Description:	Search for time series pathnames that are all the same except the "D" (date) part.
*
*  Declaration: void ztssrch7_ (long long *ifltab, const char *pathname, int *fortranUnit, char *pathFound, int *numberFound,
*				 			   size_t lenPathname, size_t pathFoundLen);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				const char *pathname
*					The pathname to search for, excampt for the date part.
*
*				int *fortranUnit
*					Unit of fortran file to write all matching paths to.
*					For returning only first path found, set this to 0.
*
*				char *pathFound
*					Returns with the first pathname found, if fortranUnit == 0
*
*				int *numberFound
*					The number of pathanmes found
*
*
*  Returns:		int number
*					The number of pathnames in the list
*
*
*
*	Author:			Bill Charley
*	Date:			2015
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

//  Search for pathnames that are all the same except the "D" (date) part.
//  This will provide all the paths for a dataset.


void ztssrch7_ (long long *ifltab, const char *pathname, int *fortranUnit, char *pathFound, int *numberFound,
			   size_t lenPathname, size_t pathFoundLen)
{
	int number;
	zStructCatalog *tsPaths;
	char cpath[MAX_PATHNAME_SIZE];

	//  Do not use malloc here (path is expanded in set part
	copyAndTrim(cpath, MAX_PATHNAME_SIZE, pathname, lenPathname);
	zpathnameSetPart (cpath, MAX_PATHNAME_SIZE, "*", 4);

	if (*fortranUnit == 0) {
		tsPaths = zstructCatalogNew();
		tsPaths->typeWantedStart = 100;
		tsPaths->typeWantedEnd = 199;
		number = zcatalogInternal (ifltab, cpath, tsPaths, 0, 0, 1, 0, 0);
		if (number > 0) {
			stringCToFort(pathFound, pathFoundLen,  tsPaths->pathnameList[0]);
		}
		zstructFree(tsPaths);
	}
	else {
		number = zcatInternalSort(ifltab, cpath, (zStructCatalog *)0, 0, *fortranUnit, 0);
	}

	*numberFound = number;
}

