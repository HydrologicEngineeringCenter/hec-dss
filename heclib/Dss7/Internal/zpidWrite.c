#include <stdio.h>
#include <stdlib.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "zdssLocking.h"
#include "hecdssInternal.h"

/**
*  Function:	zpidWrite
*
*  Use:			Private
*
*  Description:	On first write to a file, this saves the current process ID for multi-user info
*
*
*  Declaration: void zpidWrite(long long *ifltab)
*
*  Parameters:	long long *ifltab
*					The integer file table array passed among DSS functions.
*
*
*  Note:		Assumes file is locked
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

void zpidWrite(long long *ifltab)
{
	int i;
	int status;
	long long *pids;
	long long *fileHeader;
	int boolLocked;


	if (zgetVersion(ifltab) != 7) return;

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];

	//  Check for old DSS files that use a different locking scheme
	if (fileHeader[zdssFileKeys.klockReadArrayAddress] == 0) {
		return;
	}

	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		 zmessageDebugInt(ifltab, DSS_FUNCTION_zwriteDisk_ID, "Write process ID;  Handle:", zhandle(ifltab));
		 zmessageDebugInt(ifltab, DSS_FUNCTION_zwriteDisk_ID, "Process number:", zdssVals.pid);
	 }

	//  the stored pid integer is a combination of the mode and the process id
	//  pids[] =  (mode * 10000000L) + pid

	boolLocked = (int)ifltab[zdssKeys.klocked];
	if (boolLocked == 0) {
		status = zlockActive(ifltab, LOCKING_LEVEL_LOW, LOCKING_LOCK_ON, LOCKING_FLUSH_ON);
		if (status != 0) {
			//  Cannot do it.
			return;
		}
	}

	if ((ifltab[zdssKeys.kpidMyAddress] == 0) && (fileHeader[zdssFileKeys.kpidArrayAddress] > 0)){
		pids = (long long *)calloc((int)fileHeader[zdssFileKeys.klockArraySizes], 8);
		//  Bypass any errors here and let them be handled elsewhere
		if (pids) {
			//  Check to see if anyone else is accessing file.  If so, just add our pid to the pid array area
			//  zlockPassive with LOCKING_LOCK_TEST returns 1 if anyone else is accessing the file (read or write)
			 if (zlockPassive(ifltab, LOCKING_LOCK_TEST, LOCKING_ACCESS_READ)  > 0) {
				status = zget(ifltab, fileHeader[zdssFileKeys.kpidArrayAddress], (int *)pids, (int)fileHeader[zdssFileKeys.klockArraySizes], 2);
				if (status == 0) {
					for (i=0; i<(int)fileHeader[zdssFileKeys.klockArraySizes]; i++) {
						if (pids[i] == 0) {
							ifltab[zdssKeys.kpidMyAddress] = fileHeader[zdssFileKeys.kpidArrayAddress] + i;
							pids[i] = zdssVals.pid + (ifltab[zdssKeys.kmultiUserAccess] * 10000000L);
							break;
						}
					}
				}
			}
			else {
				//  No one else is accessing file, clear any stale IDs that may have occurred
				//  from a program that did not close the file.
				for (i=0; i<(int)fileHeader[zdssFileKeys.klockArraySizes]; i++) {
					pids[i] = 0;
				}
				ifltab[zdssKeys.kpidMyAddress] = fileHeader[zdssFileKeys.kpidArrayAddress];
				pids[0] = zdssVals.pid + (ifltab[zdssKeys.kmultiUserAccess] * 10000000L);
			}
			//  If successful, store array
			//  (could be more than zdssVals.lockSize and there is no more room, or read may have failed.)
			if (ifltab[zdssKeys.kpidMyAddress] != 0) {
				zput(ifltab, fileHeader[zdssFileKeys.kpidArrayAddress], (int *)pids, (int)fileHeader[zdssFileKeys.klockArraySizes], 2);
			}			
			if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
				 zmessageDebugInt(ifltab, DSS_FUNCTION_zwriteDisk_ID, "zpidWrite;  Saved process id: ", zdssVals.pid);
				 zmessageDebugLong(ifltab, DSS_FUNCTION_zwriteDisk_ID, "zpidWrite;  To address : ", ifltab[zdssKeys.kpidMyAddress]);
				 zmessageDebug(ifltab, DSS_FUNCTION_zwriteDisk_ID, "zpidWrite;  Current PID list in file: ", "");
				 for (i=0; i<fileHeader[zdssFileKeys.klockArraySizes]; i++) {
					 if (pids[i] > 0) {
						 zmessageDebugLong(ifltab, DSS_FUNCTION_zwriteDisk_ID, "zpidWrite; PID: ", pids[i]);
					 }
				 }
			}
			free(pids);
		}
	}

	if (boolLocked == 0) {
		zlockActive(ifltab, LOCKING_LEVEL_LOW, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
	}

}


//  Called when the mode is updated
void zpidUpdate(long long *ifltab)
{
	long long pids[1];
	int boolLocked;
	long long writingNow;
	int status;


	if (zgetVersion(ifltab) != 7) return;

	if (ifltab[zdssKeys.kpidMyAddress] == 0) {
		return;
	}

	//  the stored pid integer is a combination of the mode and the process id
	//  pids[] =  (mode * 10000000L) + pid


	boolLocked = (int)ifltab[zdssKeys.klocked];
	if (boolLocked == 0) {
		status = zlockActive(ifltab, LOCKING_LEVEL_LOW, LOCKING_LOCK_ON, LOCKING_FLUSH_OFF);
		if (status != 0) {
			//  Cannot do it.
			return;
		}
	}

	//  Don't let the writing now flag change for saving the ID.
	writingNow = ifltab[zdssKeys.kwritingNow];
	pids[0] = zdssVals.pid + (ifltab[zdssKeys.kmultiUserAccess] * 10000000L);
	zput(ifltab, ifltab[zdssKeys.kpidMyAddress], (int *)pids, 1, 2);
	ifltab[zdssKeys.kwritingNow] = writingNow;

	if (boolLocked == 0) {
		zlockActive(ifltab, LOCKING_LEVEL_LOW, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
	}
}

