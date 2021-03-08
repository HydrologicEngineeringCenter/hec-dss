#include <stdio.h>
#include <stdlib.h>

#include "zdssKeys.h"
#include "zdssMessages.h"
#include "zdssLocking.h"
#include "zdssVals.h"
#include "hecdssInternal.h"
#include "hecdss7.h"

/**
*  Function:	zfileAccessInfo
*
*  Use:			Semi-Public
*
*  Description:	Returns various information on (multi-user) access mode
*
*
*  Declaration: int zfileAccessInfo(long long *ifltab, int *accessMode, int *numberAccessing,
*									int *pidsArray, int arraySize, int *numberPids)
*
*  Parameters:	long long *ifltab
*					The integer file table array passed among DSS functions.
*
*				int *accessMode
*					Returns the current access mode of the file, as defined in zdssVals.h
*					For example, MULTI_USER_ACCESS (2) or SINGLE_USER_ADVISORY_ACCESS (3)
*
*				int *numberAccessing
*					Returns number of processes accessing this file, both read and write
*					excluding the calling process
*
*				int *pidsArray
*					An integer array that is returned with the process ids for the current
*					process WRITING to the file.
*
*				int *modesArray
*					An integer array of the access modes of each of the processes, in a 1 to 1
*					correspondence (only processes writing to file, none just looking)
*
*				int arraySize
*					The size of the pids and modes array, in int words
*
*				int *numberPids
*					The number of processes writing to the file, and the number of
*					process ids in the pids array.  (only sets numberPids elements)
*
*
*	Returns:	int status
*					STATUS_OKAY for successful operation.
*					errorCode for error.
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zfileAccessInfo(long long *ifltab, int *accessMode, int *numberAccessing, int *numberWriting,
					int *pidsArray, int *modesArray, int arraySize, int *numberPids)
{
	int i;
	int status;
	long long *pids;
	long long id;
	int mode;
	int count;
	long long *fileHeader;


	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	*accessMode = (int)ifltab[zdssKeys.kmultiUserAccess];
	*numberAccessing = zlockPassive(ifltab, LOCKING_LOCK_NUMBER, LOCKING_ACCESS_READ) + 1;  //  The "+ 1" adds us in.
	*numberWriting = zlockPassive(ifltab, LOCKING_LOCK_NUMBER, LOCKING_ACCESS_WRITE);
	if (ifltab[zdssKeys.knumberWrites] > 0) (*numberWriting)++;  //  Adds us in, if we have been writing.

	//  the stored pid integer is a combination of the mode and the process id
	//  pids[] =  (mode * 10000000L) + pid

	if ((fileHeader[zdssFileKeys.klockArraySizes] > 0) && (arraySize > 0)){
		pids = (long long *)calloc((int)fileHeader[zdssFileKeys.klockArraySizes], 8);
		status = zget(ifltab, fileHeader[zdssFileKeys.kpidArrayAddress], (int *)pids, (int)fileHeader[zdssFileKeys.klockArraySizes], 2);
		count = 0;
		for (i=0; i<fileHeader[zdssFileKeys.klockArraySizes]; i++) {
			if (pids[i] > 0) {
				id = pids[i];
				if (id > 10000000) {
					mode = (int)(id / 10000000L);
					id = id - (mode * 10000000L);
				}
				else {
					mode = 0;
				}
				modesArray[count] = mode;
				pidsArray[count++] = (int)id;
				if (count == arraySize) break;
			}
		}
		*numberPids = count;
		free(pids);
	}
	else {
		*numberPids = 0;
		status = 0;
	}

	return status;

}

