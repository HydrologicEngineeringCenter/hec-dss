
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include "zdssKeys.h"
#include "zdssVals.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"

/**
*  Function:	znewFileSize
*
*  Use:			Private (Internal)
*
*  Description:	Determines / sets the hash table size and pathname bin size based on max number paths expected
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				int maxExpectedPathnames (Optional)
*					Sets the size of the internal tables to be optimum with then number of pathnames specified
*					The DSS file will work with any number of pathnames, this just optimizes speed and space.
*					To set to the default, set to 0 (zero).
*
*				int hashSize;  Optional - Recommend set to zero.
*					The size of the hash table (number of hash entries).
*					Optimal size will be:
*						maxExpectedPathnames = hashSize * binSize;
*					Generally, hashSize is about 20,000 for 1,000,000 pathnames.
*					Setting this parameter too large will waste (a significant amount of) space.
*					Too small will cause an increase in execution time.
*					(Note:  Too large will also incur increased execution time.  Optimum will be the least execution time.)
*					The minimum is one, the maximum is 100000.
*
*				int binSize;  Optional - Recommend set to zero.
*					The size of the pathname bin, in int*8 words.
*					Each pathname takes 6 + ((pathname Length-1)/8) + 1 int*8 words.
*					For an average pathname length of 60 that's 6 + 8 = 14 words.
*					A bin of 200 would hold about 200/14 = 14 pathnames.
*					If the table hash size is 8192, that means a bin for each table hash, or 8192 bins:  8192 * 14 = 114,688 pathnames.
*					If the hash codes were perfectly distributed, then the perfect number of paths would be 114,688 in the file.
*					The minimum is 30, the maximum is 400.
*
*
*	Caution:		If you set hashSize and binSize, be careful!  These have a huge impact on file size and performance.
*
*   Notes:			The "perfect" file uses all hashes, and
*					there is one bin per hash and that bin
*					is full, but has not overflowed.
*
*					The intent is that when accessing a record, the table hash is read,
*					then the bin block is read and that block contains the path for that
*					record (2 reads to get the record's address).
*
*					For example, suppose the average pathname length is 60 characters
*					Each pathname takes 6 + ((pathLen-1)/8) + 1 in a bin
*					So for a length of 60 that's 6 + 8 = 14 words
*					A bin of 200 would hold about 200/14 = 14 pathnames
*					If the table hash size is 8192, that means
*					a bin for each table hash, or 8192 bins
*					8192 * 14 = 114,688 pathnames
*					If the hash codes were perfectly distributed,
*					then the perfect number of paths would be 114,688
*
*					A collection set uses the same table hash; thus there
*					will be a lot of pathnames in a bin that has collection
*					So collections should have large bins and a smaller table hash
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


void znewFileSize(long long *ifltab, int maxExpectedPaths, int hashSize, int binSize)
{
	long long *fileHeader;
	char messageString[50];


	//  Check for initialization
	 if (zdssVals.integrityKey != DSS_INTEGRITY_KEY) {
		zinit();
	 }

	 fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];

	 fileHeader[zdssFileKeys.kbinsPerBlock] = 32;

	 if (((hashSize > 0) && (binSize > 0)) && ((hashSize < 100001) && (binSize <= 400))) {
		 //  Make sure we have reasonable values
		 //  hash table could go up to 100,000, bin up to 2,000 before issues arose
		fileHeader[zdssFileKeys.kmaxHash] = hashSize;
		fileHeader[zdssFileKeys.kbinSize] = binSize;
		return;
	 }

	 if (maxExpectedPaths <= 0) {
		 //  The default size
		 fileHeader[zdssFileKeys.kmaxHash] = 8192;
		 fileHeader[zdssFileKeys.kbinSize] = 200;
	 }
	 else if (maxExpectedPaths <= 10000) {
		 fileHeader[zdssFileKeys.kmaxHash] = 8192;
		 fileHeader[zdssFileKeys.kbinSize] = 200;
	 }
	 else if (maxExpectedPaths <= 50000) {
		 fileHeader[zdssFileKeys.kmaxHash] = 8192;
		 fileHeader[zdssFileKeys.kbinSize] = 150;
	 }
	 else if (maxExpectedPaths <= 100000) {
		 fileHeader[zdssFileKeys.kmaxHash] = 8192;
		 fileHeader[zdssFileKeys.kbinSize] = 200;
	 }
	 else if (maxExpectedPaths <= 500000) {
		 fileHeader[zdssFileKeys.kmaxHash] = 16348;
		 fileHeader[zdssFileKeys.kbinSize] = 200;
	 }
	 else if (maxExpectedPaths <= 1000000) {
		 fileHeader[zdssFileKeys.kmaxHash] = 32768;
		 fileHeader[zdssFileKeys.kbinSize] = 300;
	 }
	 else {
		 fileHeader[zdssFileKeys.kmaxHash] = 64000;
		 fileHeader[zdssFileKeys.kbinSize] = 300;
	 }
	 if (zdssVals.newCollectionFile) {
		fileHeader[zdssFileKeys.kbinSize] = 400;
		zdssVals.newCollectionFile = 0;
	 }

	 if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
			"Max hash: %d,  Bin size: %d",
			(int)fileHeader[zdssFileKeys.kmaxHash], (int)fileHeader[zdssFileKeys.kbinSize]);
		zmessageDebug(ifltab, DSS_FUNCTION_znewFileSize_ID, messageString, "");
	 }
}

