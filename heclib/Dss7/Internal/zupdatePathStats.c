#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssVals.h"
#include "hecdssInternal.h"



/**
*  Function:	zupdatePathStats
*
*  Use:			Private (Internal)
*
*  Description:	On a new write, updates the maximum pathname part lengths for the file, as well as hash statistics
*
*  Declaration: void zupdatePathStats(long long *ifltab, const char *pathname, size_t pathnameLen);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				const char *pathname
*					The pathname for the write
*
*				size_t pathnameLen: The number of characters in the pathname (excluding any terminating \0)
*
*
*	Returns:	Nothing
*
*
*
*
*  Called By:	zwriteNew only
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

void zupdatePathStats(long long *ifltab, const char *pathname, size_t pathnameLen)
{
	int lengths[6];
	long long *fileHeader;

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	zpathnamePartLengths (pathname, pathnameLen, lengths, 6);
	if ((long long)pathnameLen > fileHeader[zdssFileKeys.kmaxPath])
		fileHeader[zdssFileKeys.kmaxPath] = ifltab[zdssKeys.klenLastPath];
	if (lengths[0] > fileHeader[zdssFileKeys.kmaxA]) fileHeader[zdssFileKeys.kmaxA] = lengths[0];
	if (lengths[1] > fileHeader[zdssFileKeys.kmaxB]) fileHeader[zdssFileKeys.kmaxB] = lengths[1];
	if (lengths[2] > fileHeader[zdssFileKeys.kmaxC]) fileHeader[zdssFileKeys.kmaxC] = lengths[2];
	if (lengths[3] > fileHeader[zdssFileKeys.kmaxD]) fileHeader[zdssFileKeys.kmaxD] = lengths[3];
	if (lengths[4] > fileHeader[zdssFileKeys.kmaxE]) fileHeader[zdssFileKeys.kmaxE] = lengths[4];
	if (lengths[5] > fileHeader[zdssFileKeys.kmaxF]) fileHeader[zdssFileKeys.kmaxF] = lengths[5];

	if ((ifltab[zdssKeys.kpathsThisHash] + 1) > fileHeader[zdssFileKeys.kmaxPathsOneHash]) {
		fileHeader[zdssFileKeys.kmaxPathsOneHash] = ifltab[zdssKeys.kpathsThisHash] + 1;
		fileHeader[zdssFileKeys.kmaxPathsHashCode] = ifltab[zdssKeys.ktableHash];
	}

}

