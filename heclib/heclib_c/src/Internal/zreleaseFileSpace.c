#ifdef _MSC_VER
#include <io.h>
#else

#endif

#include <stdio.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"

//  Temporarily not used

//  To be completed later
//  (A pretty complex function)
//
//  zrelease space puts unused (deleted) space back into the pool
//  and keeps the pool organized
//  When space is released, it checks for space adjacent to it in the pool
//  and will add it to that space to make a contiguous segment, if present
//  It also updates the max space available and removes the smallest
//  space, if the table becomes full.

void zreleaseFileSpace(long long *ifltab, long long address, int amountLongs)
{
	long long *reclaimArray;
	int minMem;
	int iarrayNumber;
	int i;
	int loc;
	int ipos;
	int boolArrayUpdated;
	long long *fileHeader;


	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_internalUtility_ID, "Enter zreleaseFileSpace;  Handle: ",  zhandle(ifltab));
		zmessageDebugLong(ifltab, DSS_FUNCTION_internalUtility_ID, "Release space at address: ",  address);
		zmessageDebugInt(ifltab, DSS_FUNCTION_internalUtility_ID, "Number longs released: ",  amountLongs);
	}

	boolArrayUpdated = 0;
	iarrayNumber = 0;
	if (amountLongs >= fileHeader[zdssFileKeys.kreclaimMin]) {
		while ((reclaimArray = zreclaimRead(ifltab, &iarrayNumber, 1)) > 0) {		
			minMem = (int)reclaimArray[0];
			loc = 0;
			ipos = -2;
			//  Walk down reclaimed space array, looking for smallest space
			//  that is equal or larger than what we need
			for (i=0; i<(int)fileHeader[zdssFileKeys.kreclaimNumber]; i++) {
				ipos += 2;
				if (reclaimArray[ipos] == 0) {
					reclaimArray[ipos] = amountLongs;
					fileHeader[zdssFileKeys.kreclaimTotal] += amountLongs;
					reclaimArray[ipos+1] = address;
					minMem = 0;
					boolArrayUpdated = 1;
					break;
				}
				else {
					if ((reclaimArray[ipos] > 0) && (reclaimArray[ipos] < minMem)) {
						minMem = (int)reclaimArray[ipos];
						loc = ipos;
					}
				}
			}
			if ((minMem > 0) && (amountLongs > minMem)){
				reclaimArray[ipos] = amountLongs;
				fileHeader[zdssFileKeys.kreclaimTotal] += amountLongs;
				reclaimArray[ipos+1] = address;
				boolArrayUpdated = 1;
				break;
			}
			if (boolArrayUpdated) {
				break;
			}
			iarrayNumber++;
		}
		
		//  save reclaim Array
		if (boolArrayUpdated) {
			zreclaimWrite(ifltab);
			if (amountLongs > fileHeader[zdssFileKeys.kreclaimMaxAvailable]) {
				fileHeader[zdssFileKeys.kreclaimMaxAvailable] = amountLongs;
			}
		}
	}

	fileHeader[zdssFileKeys.kdead] += amountLongs;

}
