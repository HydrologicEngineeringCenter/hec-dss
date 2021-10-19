
#include <stdio.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "zdssLocking.h"

/**
*  Function:	zreclaimSet
*
*  Use:			Private (Internal)
*
*  Description:	Changes the reclamation level for the file, usually temporarily
*
*  Declaration: int zreclaimSet(long long *ifltab, int reclaimLevel);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				int reclaimLevel
*					The reclamation level:
*						RECLAIM_NONE		1	//  Don't use space reclamation
*						RECLAIM_EXCESS		2	//  Reclaim space left over from extending records, etc.  (can recover records)
*						RECLAIM_ALL			3	//  Reclaim all unused space, including deleted records (cannot recover)
*
*
*	Returns:	int status
*					Write status
*
*
*
*	Author:			Bill Charley
*	Date:			2013
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zreclaimSet(long long *ifltab, int reclaimLevel)
{
	long long boolWritingNow;
	long long *fileHeader;
	int zero;
	int size;
	int status;


	//  Lock file, if we need to
	//  And read the file header
	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	boolWritingNow = ifltab[zdssKeys.kwritingNow];
	status =  zlockActive(ifltab, LOCKING_LEVEL_LOW, LOCKING_LOCK_ON, LOCKING_FLUSH_ON);
	if (zisError(status)) {
		//return zerrorUpdate(ifltab, status, functionID);
		return status;
	}

	if (reclaimLevel == RECLAIM_NONE) {
		if (fileHeader[zdssFileKeys.kreclaimTotal] > 0) {
			//  zero out total and size available
			fileHeader[zdssFileKeys.kreclaimTotal] = 0;
			fileHeader[zdssFileKeys.kreclaimSegNumber] = 0;
			fileHeader[zdssFileKeys.kreclaimSegmentsUsed] = 0;
			ifltab[zdssKeys.kreclaimLevel] = RECLAIM_NONE;
			fileHeader[zdssFileKeys.kreclaimSegAvailableAdd] = fileHeader[zdssFileKeys.kreclaimTableAddress];
			//  zero out the reclamation table
			size = (int)fileHeader[zdssFileKeys.kreclaimSize];
			status = zput(ifltab, fileHeader[zdssFileKeys.kreclaimTableAddress], &zero, -size, 2);
			if (zisError(status)) {
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_zpermCreate_ID);
			}
		}
	}
	else {
		//  If we are not clearing the reclaim area, we just set
		//  the level to the new one.
		ifltab[zdssKeys.kreclaimLevel] = reclaimLevel;
	}

	//  Unlock
	zlockActive(ifltab, LOCKING_LEVEL_LOW, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
	if (zisError(status)) {
		//return zerrorUpdate(ifltab, status, functionID);
	}
	return status;
}

