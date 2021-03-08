
#include <stdio.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"


int zreclaimWrite (long long *ifltab)
{
	char messageString[60];
	long long *reclaimArray;
	long long *fileHeader;
	int istat;

	if (ifltab[zdssKeys.kreclaimLevel] == RECLAIM_NONE) {
		return 0;
	}

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	if (ifltab[zdssKeys.kreclaim] == 0) {
		istat = zmemoryGet(ifltab, zdssKeys.kreclaim, (int)fileHeader[zdssFileKeys.kreclaimSize], "Reclaim array");
		if (istat != STATUS_OKAY) {
			return zerrorUpdate(ifltab, istat, 0);
		}
	}
	reclaimArray = (long long *)ifltab[zdssKeys.kreclaim];
	istat = zput(ifltab, fileHeader[zdssFileKeys.kreclaimSegAvailableAdd], (int *)reclaimArray, (int)fileHeader[zdssFileKeys.kreclaimSize], 2);
	if (istat != 0) {
		return istat;
	}

	if (zmessageLevel(ifltab, MESS_METHOD_PUT_ID, 10)) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %lld", fileHeader[zdssFileKeys.kreclaimTableAddress]);
//		zmessage2D(ifltab, "zreclaimRead:  Wrote reclaim array to disk at address: ", messageString);
	}
	return 0;
}

