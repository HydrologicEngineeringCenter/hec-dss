#include "heclib7.h"
#include "hecdssInternal.h"
#include "zdssKeys.h"
#include "zerrorCodes.h"
#include "zdssLocking.h"

/**
*  Function:	zerrorSave
*
*  Use:			Private (Internal)
*
*  Description:	Part of the error processing functions, this saves severe errors in the perm section of the file and
*					stores the error on the disk, but does not save the perm section (as it may be damaged)
*
*  Called By:	zerrorProcessing
*
*  Declaration: void zerrorSave(long long *ifltab, int errorNumber);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				int errorNumber
*					The basic DSS error number to be saved.  This does not include calling functions or other info,
*					but is a standard error in zdssErrorCodes.
*
*
*	Returns:	None
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


void zerrorSave(long long *ifltab, int errorNumber)
{
	long long address;
	long long *fileHeader;
	int status;


	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	if (errorNumber == zdssErrorCodes.INVALID_ADDRESS) {
		//  Increment error counter in perm section
		fileHeader[zdssFileKeys.kerrorAddress]++;
		//  Save to disk, unless a goofy number
		if ((fileHeader[zdssFileKeys.kerrorAddress] > 0) && (fileHeader[zdssFileKeys.kerrorAddress] < 1000)) {
			//  Don't flush or check for error, in case things are really messed up
			if ((ifltab[zdssKeys.kopenStatus] == OPEN_STAT_WRITE) && (ifltab[zdssKeys.kfileWritten] > 0)) {
				address = zdssFileKeys.kerrorAddress - zdssKeys.kfileHeader;
				status = zlockActive(ifltab, LOCKING_LEVEL_LOW, LOCKING_LOCK_ON, LOCKING_FLUSH_ON);
				if (zisError(status)) return;
				zput(ifltab, address, (int *)&fileHeader[zdssFileKeys.kerrorAddress], 2, 2);
				zlockActive(ifltab, LOCKING_LEVEL_LOW, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
			}
		}
	}
	else if (errorNumber == zdssErrorCodes.KEY_CORRUPT) {
		//  Increment error counter in perm section
		fileHeader[zdssFileKeys.kerrorMemory]++;
		//  Save to disk, unless a goofy number
		if ((fileHeader[zdssFileKeys.kerrorMemory] > 0) && (fileHeader[zdssFileKeys.kerrorMemory] < 1000)) {
			//  Don't flush or check for error, in case things are really messed up
			if ((ifltab[zdssKeys.kopenStatus] == OPEN_STAT_WRITE) && (ifltab[zdssKeys.kfileWritten] > 0)) {
				address = zdssFileKeys.kerrorMemory - zdssKeys.kfileHeader;
				status = zlockActive(ifltab, LOCKING_LEVEL_LOW, LOCKING_LOCK_ON, LOCKING_FLUSH_ON);
				if (zisError(status)) return;
				zput(ifltab, address, (int *)&fileHeader[zdssFileKeys.kerrorMemory], 2, 2);
				zlockActive(ifltab, LOCKING_LEVEL_LOW, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
			}
		}
	}
}

