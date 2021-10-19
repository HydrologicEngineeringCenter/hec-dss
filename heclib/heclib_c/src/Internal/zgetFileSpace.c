
#include <stdio.h>

#include "zdssKeys.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"

/**
*  Function:	zgetFileSpace
*
*  Use:			Private (Internal)
*
*  Description:	Whenever new file space is needed (the file is lengthened), this function is called
*					to get it or add to the end of the file.  Space might include reclaimed space.
*					For example, if a new write request for space (at end of file), this will create it
*					and return the address to that space.
*
*  Declaration: long long zgetFileSpace(long long *ifltab, int sizeNeeded, int boolReclaimedOK, int *atEOF);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				int sizeNeeded
*					The total space needed, in int*8 words.
*
*				int boolReclaimedOK
*					A boolean flag indicating if it is okay to use reclaimed space or not (0 = do not use, 1 = okay to use.)
*					Generally, reclaimed space is not used for address tables and similar.
*
*				int *atEOF
*					A boolean return parameter indicating if the space is at the end of the file (by extending the file size.)
*					If so, generally an EOF flag is written after the space, indicating the end of file.
*
*	Returns:	long long address
*					The address of the space to use.
*
*	Remarks:		No error or error code is returned.  If reclaimed space is not available, then space from the end of
*					the file is given.  If the file cannot be extended, or written to, or some other error, this routine
*					will not report it.  It will show up during a write operation (not here!)
*
*
*
*	Author:			Bill Charley
*	Date:			2013
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

long long zgetFileSpace(long long *ifltab, int sizeNeeded, int boolReclaimedOK, int *atEOF)
{
	char messageString[80];
	long long address;
	long long add;
	long long *reclaimArray;
	long long *fileHeader;
	int iarrayNumber;
	int sizeHave;
	int i;
	int loc;
	int ipos;
	int diff;


	address = 0;
	//  If we don't have any reclaimed space available, just get it from EOF
	if (ifltab[zdssKeys.kreclaimLevel] <= 1) {
		boolReclaimedOK = 0;
	}

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	sizeHave = 0;
	if ((boolReclaimedOK) && (fileHeader[zdssFileKeys.kreclaimSize] > 0) &&
	  (sizeNeeded >= fileHeader[zdssFileKeys.kreclaimMin]) &&
	  (sizeNeeded <= fileHeader[zdssFileKeys.kreclaimMaxAvailable])) {
		iarrayNumber = 0;
		while (reclaimArray = zreclaimRead(ifltab, &iarrayNumber, 0)) {
			//  Walk down reclaimed space array, looking for smallest space
			//  that is equal or larger than what we need
			//  We want the smallest space so that larger chunks can be used
			//  with larger needs.
			for (i=0; i<(int)fileHeader[zdssFileKeys.kreclaimNumber]; i++) {
				ipos = numberIntsInLongs(i);
				if (reclaimArray[ipos] >= sizeNeeded) {
					if ((sizeHave == 0) || (reclaimArray[ipos] < sizeHave)) {
						sizeHave = (int)reclaimArray[ipos];
						loc = i;
						if (sizeHave == sizeNeeded) {
							break;
						}
					}
				}
			}
			if (sizeHave > 0) {
				ipos = numberIntsInLongs(loc);
				address = reclaimArray[ipos+1];
				diff = sizeHave - sizeNeeded;
				if (diff > fileHeader[zdssFileKeys.kreclaimMin]) {
					//  Reduce the space available
					reclaimArray[ipos] = diff;
					//  Move the available address to after the number reclaimed.
					reclaimArray[ipos+1] = reclaimArray[ipos+1] + sizeNeeded;
				}
				else {
					reclaimArray[ipos] = 0;
					reclaimArray[ipos+1] = 0;
				}
				fileHeader[zdssFileKeys.kdead] -= sizeNeeded;
				fileHeader[zdssFileKeys.kreclaimTotal] -= sizeNeeded;
				if (fileHeader[zdssFileKeys.kreclaimTotal] < 0) fileHeader[zdssFileKeys.kreclaimTotal] = 0;
				fileHeader[zdssFileKeys.kreclaimedSpace]++;
				//  save reclaim Array
				zreclaimWrite(ifltab);
				break;
			}
			else {
				iarrayNumber++;
			}
		}
	}


	//  Either EOF requested, or reclaimed space of the size required not available.
	//  Just get from EOF
	if (address == 0) {
		//  Use the EOF address and say the file size is that much larger.
		address = fileHeader[zdssFileKeys.kfileSize];
		fileHeader[zdssFileKeys.kfileSize] += sizeNeeded;
		*atEOF = 1;
	}

	if (zmessageLevel(ifltab, MESS_METHOD_GET_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%lld, size: %d",address, sizeNeeded);
		 zmessageDebug(ifltab, DSS_FUNCTION_zgetFileSpace_ID, "Address                   ", messageString);
		 if (*atEOF == 1) {
			 add = fileHeader[zdssFileKeys.kfileSize] - sizeNeeded;
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%lld", add);
			zmessageDebug(ifltab, DSS_FUNCTION_zgetFileSpace_ID, "Space from EOF at address ", messageString);
		 }
		 else {
			 diff = sizeHave - sizeNeeded;
			 _snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d, Sized needed: %d, difference: %d",
				 sizeHave, sizeNeeded, diff);
			zmessageDebug(ifltab, DSS_FUNCTION_zgetFileSpace_ID, "Reclaimed space, sized available", messageString);

		 }
	}

	return address;
}

