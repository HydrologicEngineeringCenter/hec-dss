#include <string.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zStructRecordAddresses.h"
#include "hecdssInternal.h"
#include "zdssMessages.h"

/**
*  Function:	zgetRecordAddresses
*
*  Use:			Semi-Public
*
*  Description:	Function to get for a single record.  Valid for DSS-7 only
*
*  Declaration: int zgetRecordAddresses (long long *ifltab, const char *pathname, zStructRecordAddresses *recordInfo)
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				zStructRecordAddresses *recordInfo
*					A struct will contain addresses for a single record.
*
*
*	Remarks:	This is used internally and for debugging.
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*
*/

int zgetRecordAddresses (long long *ifltab, const char *pathname, zStructRecordAddresses *recordInfo)
{
	int foundOne;
	int internalHeaderArraySize  = INT_HEAD_SIZE;
	long long *info;
	long long *fileHeader;
	int numberChars;
	int pathnameSize;

	//  Initialize all to zero, so we can add for multiple records
	//  (or just return if pathname not found)

	if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_other_ID, " Enter zgetRecordAddresses, Pathname: ", pathname);
	}

	foundOne = 0;

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	//  If the record has just been accessed (usually the case), we don't need to re-read it.
	info = (long long *)ifltab[zdssKeys.kinfo];
	//  Double check that this is the correct path
	if (!zpathnameCompare(pathname, &ifltab[zdssKeys.kpathAddressInBin], strlen(pathname))) {
		//  Not loaded into memory.   Read info area
		ifltab[zdssKeys.kpathnameHash] = 0;
		foundOne = zreadInfo(ifltab, pathname, 0);
		if (!foundOne) {
			return foundOne;
		}
		if (foundOne < 0) {
			//   Fix me - if not found?
			//  error out
			return foundOne;
		}
	}

	recordInfo->tableHash = ifltab[zdssKeys.ktableHash];
	recordInfo->tableHash = ifltab[zdssKeys.kpathnameHash];
	recordInfo->binSize = (int)fileHeader[zdssFileKeys.kbinSize];
	i8toi4(ifltab[zdssKeys.kbinPathLen], &numberChars, &pathnameSize);
	recordInfo->infoLength = (int)(zdssVals.infoSize + pathnameSize);
	recordInfo->totalAllocatedSize = (int)info[zdssInfoKeys.kinfoAllocatedSize];

	recordInfo->hashAddress = ifltab[zdssKeys.kaddTableHash];
	recordInfo->binAddress  = ifltab[zdssKeys.kpathBinAddress];
	recordInfo->infoAddress = ifltab[zdssKeys.kaddInfoLastPath];

	recordInfo->internalHeaderAddress = info[zdssInfoKeys.kinfoInternalHeadAddress];
	recordInfo->header2Address        = info[zdssInfoKeys.kinfoHeader2Address];
	recordInfo->userHeaderAddress     = info[zdssInfoKeys.kinfoUserHeadAddress];
	recordInfo->values1Address        = info[zdssInfoKeys.kinfoValues1Address];
	recordInfo->values2Address        = info[zdssInfoKeys.kinfoValues2Address];
	recordInfo->values3Address        = info[zdssInfoKeys.kinfoValues3Address];

	return foundOne;
}


