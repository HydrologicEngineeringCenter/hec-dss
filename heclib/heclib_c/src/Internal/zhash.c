#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>

#include "zdssKeys.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"


/**
*  Function:	zhash
*
*  Use:			Private (Internal)
*
*  Description:	Determines both the table hash and the bin hash of a pathname, case insensitive.
*
*  Declaration: int zhash (long long *ifltab, const char *pathname, int pathnameLength, int *tableHash, long long *pathnameHash);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				const char *pathname
*					The pathname to determine the hash codes for
*
*				int pathnameLength
*					The length of the pathname
*
*				int *tableHash (output)
*					A hash code that corresponds to a location in the file hash table.
*					tableHash is returned so that  0 <= *tableHash < maxHash
*
*				long long *pathnameHash (output)
*					a almost unique hash code that distinguishes the pathname from other
*					pathnames with the same table hash.  This will always be a very large, almost unique
*					almost unique number.  pathnameHash will not be zero.
*					pathnameHash is returned with any value  (-max Int) < *pathnameHash < max Int
*
*
*	Returns:	int booleanIsCollection
*					1 if is a collections path, otherwise 0.
*
*	Note:			The table hash is a non-unique hash code that is used in the primary hash table
*					of the DSS file.  The value maxHash is the table size (int 64 array) and should be
*					a power of 2.  The table hash is usually pretty evenly distributed between 0 and maxHahs.
*					Important - this value is used as a lookup, so maxHash should not be too large (or small).
*					maxHash is the key parameter at optimizing the DSS file.
*
*
*
*  Called By:	zcheck only
*
*	Author:			Originally written by Art Pabst, 1979
*					Updated and converted to c by Bill Charley, 2010
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int zhash (long long *ifltab, const char *pathname, int pathnameLength, int *tableHash, long long *pathnameHash)
{
	char messageString[90];
	char path[MAX_PATHNAME_LENGTH+10];
	int i;
	double fraction;
	int ibit;
	int nt;
	int ineed;
	int ihave;
	int imove;
	int it;
	int itp;
	int i2;
	int maxHash;
	int collectionsPosition;
	int ich;
	int ibar;
	long long pathHash;
	long long *fileHeader;

	static int detuneMessage = 0;
	int slashCount = 0;
	int ibyte = 0;


	ibar = 0;
	//  Change to upper case and remove any invalid characters
	for (i=0; i<pathnameLength; i++) {
		ich = pathname[i];
		if (ich < 32) {
			ich = '?';
		}
		else {
			ich = toupper(ich);
		}
		path[i] = ich;
		if (ich == '|') ibar = i;
	}
	//  zero out the 8 bytes following path
	for (i=pathnameLength; i<(pathnameLength+8); i++) {
		path[i] = '\0';
	}

	if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zhash_ID, "Enter; Pathname: ", pathname);
	}

	//  Compute the pathname bin hash first
	//  This an almost unique bin hash.
	//  code swiped from Java hash
	pathHash = 0;
    for (i=0; i<pathnameLength; i++) {
		pathHash = (31 * pathHash) + (long long)path[i];
		/*
		if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
			messageString[0] = path[i];
			messageString[1] = '\0';
			zmessageDebug(ifltab, DSS_FUNCTION_zhash_ID, "char: ", messageString);
			zmessageDebugLong(ifltab, DSS_FUNCTION_zhash_ID, "hash: ", pathHash);
		}
		*/
	}
	//  Never allow a pathname bin hash to be zero
	if (pathHash == 0) {
		pathHash = 1;
	}
	*pathnameHash = pathHash;


	//  Compute the table hash
	//  Before we do so, see if this is a collection
	//  All pathnames from the same collection will use the same table hash
	if (ibar > 8) {
		//  zcollectionsPath will convert the collection number into "XXXXXX"
		collectionsPosition = zcollectionsPath (path, (size_t)pathnameLength);
	}
	else {
		collectionsPosition = 0;
	}


	//  compute the exponent of maxHash
	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	maxHash = (int)fileHeader[zdssFileKeys.kmaxHash];
	fraction = frexp((double)maxHash, &ibit);
	if (fraction == 0.50) {
		ibit--;
	}

	//  Find the number of characters that include full number groups
	//  This rounds up to full number of 8 bit char to process when
	//  you treat them in IBIT chunks
	nt = (((pathnameLength * 8) - 1) / ibit) + 1;
	nt = (((nt * ibit) -1) / 8) + 1;

	it = 0;
	i2 = 0;
	ineed = ibit;
	ihave = 0;

	while (1) {
		//  Get a group of bits
		if (ihave > 0) {
			//  imove = min(ineed, ihave);
			imove = ineed;
			if (ihave < imove) imove = ihave;
			//  left shift
			i2 = i2 << imove;
			ineed -= imove;
			ihave -= imove;
			if (ineed <= 0) {
				itp = i2 / 256;
				i2 %= 256;
				//Use Exclusive OR to form value
				it ^= itp;
				ineed = ibit;
			}
		}

		//  Refill a character
		if (ihave <= 0) {
			++ibyte;
			if (ibyte > nt) {
				break;
			}
			//  Use the collection path, if it is one.
			i2 += path[ibyte-1];
			ihave = 8;
		}
	}
	//  Compute final table hash
	*tableHash = it % maxHash;


	////////////////////////////////////////////////////////
	/////  DE-TUNE For testing only!!!!!
	if (fileHeader[zdssFileKeys.kdetune]) {
		*tableHash = 3;
		*pathnameHash = 1234;
		if (detuneMessage < 100) {
			zmessage(ifltab, "***  WARNING...  hash hardwired for testing only!!!  Do Not Use this library!!!! ****");
			zmessage2(ifltab, "** Pathname: ", pathname);
			detuneMessage++;
		}
	}
	////////////////////////////////////////////////////////

	if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zhash_ID, "Pathname: ", pathname);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
			"%d,  Maximum Hash: %d,  Bin Hash: %lld", *tableHash, maxHash, *pathnameHash);
		zmessageDebug(ifltab, DSS_FUNCTION_zhash_ID, "Table Hash: ", messageString);
	}

	return (collectionsPosition)?1:0;
}

