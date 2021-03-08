
#include <stdio.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"


long long* zreclaimRead (long long *ifltab, int *iarrayNumber, int boolRelease)
{
	char messageString[60];
	long long *reclaimArray;
	long long address;
	int zero[2];
	long long *fileHeader;
	int inumber;
	int iaddLoc;
	int size;
	int atEOF;
	int i;
	int istat;


	if (ifltab[zdssKeys.kreclaimLevel] == RECLAIM_NONE) {
		return (long long*)0;
	}

	zero[0] = 0;
	zero[1] = 0;

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];

	if (ifltab[zdssKeys.kreclaim] == 0) {
		istat = zmemoryGet(ifltab, zdssKeys.kreclaim, (int)fileHeader[zdssFileKeys.kreclaimSize], "Reclaim array");
		if (istat != STATUS_OKAY) {
			zerrorUpdate(ifltab, istat, 0);
			return (long long *)0;
		}
	}
	reclaimArray = (long long *)ifltab[zdssKeys.kreclaim];

	inumber = *iarrayNumber;
	if (boolRelease) {
		if ((inumber == 0) || (inumber == (int)fileHeader[zdssFileKeys.kreclaimSegNumber])){
			//  Read in the current array with space
			istat = zget(ifltab, fileHeader[zdssFileKeys.kreclaimSegAvailableAdd], (int *)reclaimArray, (int)fileHeader[zdssFileKeys.kreclaimSize], 2);
			if (istat != 0) {
				return (long long *)0;
			}
			(*iarrayNumber) = (int)fileHeader[zdssFileKeys.kreclaimSegNumber];
		}
		else if (inumber < (int)fileHeader[zdssFileKeys.kreclaimMaxSegment]) {
			//  Now we have to search for that array
			address = fileHeader[zdssFileKeys.kreclaimTableAddress];
			iaddLoc = (int)fileHeader[zdssFileKeys.kreclaimSize] -1;
			for (i=0; i<=inumber; i++) {
				istat = zget(ifltab, address, (int *)reclaimArray, (int)fileHeader[zdssFileKeys.kreclaimSize], 2);
				if (istat != 0) {
					return (long long *)0;
				}
				if (i < inumber) {
					//Don't get the address for the last array
					address = reclaimArray[iaddLoc];
				}
				if (address == 0) {
					//  No more reclaim arrays; create the next one
					if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, 10)) {
			//			zmessage(ifltab, "     Creating reclaimed space table.");
					}
					size = (int)fileHeader[zdssFileKeys.kreclaimSize];
					address = zgetFileSpace(ifltab, size, 0, &atEOF);
					reclaimArray[iaddLoc] = address;
					zreclaimWrite (ifltab);
					fileHeader[zdssFileKeys.kreclaimSegmentsUsed]++;
					istat = zput(ifltab, address, zero, -size, 2);
					fileHeader[zdssFileKeys.kreclaimSegAvailableAdd] = address;
				}
			}
			fileHeader[zdssFileKeys.kreclaimSegAvailableAdd] = address;
			fileHeader[zdssFileKeys.kreclaimSegNumber] = inumber;
		}
		else {
			reclaimArray = 0;
		}
	}
	else {
		if (inumber == 0) {
			istat = zget(ifltab, fileHeader[zdssFileKeys.kreclaimTableAddress], (int *)reclaimArray, (int)fileHeader[zdssFileKeys.kreclaimSize], 2);
			if (istat != 0) {
				return (long long *)0;
			}
			fileHeader[zdssFileKeys.kreclaimSegAvailableAdd] = fileHeader[zdssFileKeys.kreclaimTableAddress];
		}
		else { // if (inumber < fileHeader[zdssFileKeys.kreclaimSegmentsUsed]) {
			//  Now we have to search for that array
			address = fileHeader[zdssFileKeys.kreclaimTableAddress];
			iaddLoc = (int)fileHeader[zdssFileKeys.kreclaimSize] -1;
			for (i=0; i<=inumber; i++) {
				istat = zget(ifltab, address, (int *)reclaimArray, (int)fileHeader[zdssFileKeys.kreclaimSize], 2);
				if (istat != 0) {
					return (long long *)0;
				}
				if (i < inumber) {
					//Don't get the address for the last array
					address = reclaimArray[iaddLoc];
				}
				if (address == 0) {
					reclaimArray = 0;
					break;
				}
			}
			fileHeader[zdssFileKeys.kreclaimSegAvailableAdd] = address;
			fileHeader[zdssFileKeys.kreclaimSegNumber] = inumber;
		}
	}


	if (zmessageLevel(ifltab, MESS_METHOD_PUT_ID, 10)) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %lld", fileHeader[zdssFileKeys.kreclaimTableAddress]);
	//	zmessage2D(ifltab, "zreclaimRead:  Read reclaim array from disk at address: ", messageString);
	}
	return reclaimArray;
}

