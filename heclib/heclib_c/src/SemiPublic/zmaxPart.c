#include "zdssKeys.h"
#include "heclib.h"



/**
*  Function:	zmaxPart
*
*  Use:			Semi-Public
*
*  Description:	Gets the maximum lengths for all parts. Used by HEC-DSSVue for catalog operations
*
*  Declaration: void zmaxPart(long long *ifltab, int *maxParts);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				int *maxParts
*					An 6 element integer array returned with the corresponding max length for each part.
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*/
//
void zmaxPart7(long long *ifltab, int *maxParts)
{
	int istat;
	long long *fileHeader;

	istat = zpermRead(ifltab);
	//  Not sure what to do if istat != 0

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];

	maxParts[0] = (int)fileHeader[zdssFileKeys.kmaxA];
	maxParts[1] = (int)fileHeader[zdssFileKeys.kmaxB];
	maxParts[2] = (int)fileHeader[zdssFileKeys.kmaxC];
	maxParts[3] = (int)fileHeader[zdssFileKeys.kmaxD];
	maxParts[4] = (int)fileHeader[zdssFileKeys.kmaxE];
	maxParts[5] = (int)fileHeader[zdssFileKeys.kmaxF];

}

void zmaxPart(long long *ifltab, int *maxParts)
{
	if (zgetVersion(ifltab) == 7) {
		zmaxPart7 (ifltab, maxParts);
	}
	else {
		zmaxpart6_ (ifltab, maxParts);
	}
}

void zmaxpart_(long long *ifltab, int *maxParts)
{
	zmaxPart(ifltab, maxParts);
}

