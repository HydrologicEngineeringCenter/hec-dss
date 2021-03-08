#include <ctype.h>

#include "hecdss7.h"

/**
*  Function:	zpathnameCompareCollection
*
*  Use:			Public
*
*  Description:	compare two pathnames, except for the collection part.  Must be collection pathnames!
*
*  Declaration: int zpathnameCompare(const char *pathname1, const char *pathname2, int pathnameLength);
*
*  Parameters:	const char *pathname1:  The first pathname to compare
*
*				const char *pathname2:  The second pathname to compare
*
*				int pathnameLength:  The length of each pathname (Note:  both pathnames have
*					to be the same length, so check that first.
*
*  Returns:		int (boolean) same:		0 (zero) is not the same collection
*										1 (One) if the same collection
*
*  Remarks:		Case insensitive, may be either upper or lower, results will be the same
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zpathnameCompareCollection(const char *pathname1, const char *pathname2, size_t pathnameLength)
{
	int i;
	int i1;
	int i2;
	int numberSlashes;
	int boolCollectionPart;
	int boolIsaCollection;


	numberSlashes = 0;
	boolCollectionPart = 0;
	boolIsaCollection = 0;
	for(i=0; i<(int)pathnameLength; i++) {
		i1 = toupper(pathname1[i]);
		i2 = toupper(pathname2[i]);
		if (!boolCollectionPart) {
			if (i1 != i2) {
				return 0;
			}
		}
		else {
			if (pathname1[i] == '|') {
				boolIsaCollection = 1;
				boolCollectionPart = 0;
			}
		}
		if (pathname1[i] == '/') {
			numberSlashes++;
			if (numberSlashes == 6) {
				if (pathname1[i+2] == ':') {
					boolCollectionPart = 1;
				}
			}
		}
	}
	//  Made it
	return boolIsaCollection;
}

