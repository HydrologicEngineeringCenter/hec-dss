#include "zdssKeys.h"
#include "hecdssInternal.h"


//  Gets the addresses of pathnames in the sorted catalog and places in array sortOrder
//
/**
*  Function:	zgetCatalogSortAddresses
*
*  Use:			Private
*
*  Description:	Reads the list of addresses to sorted pathnames (if it exists) in order to presort the catalog.
*
*  Declaration: int zgetCatalogSortAddresses(long long* ifltab, long long* sortAddresses, int sortAddressesLen);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				long long* sortAddresses
*					An array of the pathname bin addresses of the full sorted list of pathnames
*
*				int sortAddressesLen
*					The size of array sortAddresses.
*
*
*	See also:	zsetCatalogSortAddresses() for more documentation
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*/
int zgetCatalogSortAddresses(long long* ifltab, long long* sortAddresses, int sortAddressesLen)
{
	int istat;
	long long *fileHeader;

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	istat = zpermRead(ifltab);
	if (istat != 0) {
		return istat;
	}

	if ((int)fileHeader[zdssFileKeys.kcatSortNumber] > sortAddressesLen) {
		return -2;
	}

	istat = zget(ifltab, (fileHeader[zdssFileKeys.kcatSortAddress] + 1), (int *)sortAddresses,
		(int)fileHeader[zdssFileKeys.kcatSortNumber], 2);

	return 0;

}



