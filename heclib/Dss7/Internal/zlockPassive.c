
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssLocking.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"
#include "zerrorCodes.h"

/**
*  Function:	zlockPassive
*
*  Use:			Private (Internal)
*
*  Description:  Locks, unlocks or tests passive lock words to indicate that this process is
*					reading or writing to the file.  If writing, other processes require
*					multi-user access mode.  If reading, other processes should occasionally sync.
*
*  Declaration:  int zlockPassive(long long *ifltab, int lockFlag, int accessMode)
*
*  Parameters:	long long *ifltab
*					The integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				int lockFlag
*					A flag indicating if the word should be locked, unlocked or tested:
*						0 - Unlock
*						1 - Lock
*						2 - Test to see if another process has this file locked.  (Do not lock.)
*						3 - Test for number processes have this file lock.  (Do not lock.)
*
*				int accessMode
*					A flag indicating if this process is locking for read access or write access
*						0 - Read access
*						1 - Write access
*
*
*	Returns:	int status
*					STATUS_OKAY for successful operation.
*					errorCode for invalid operations or other errors.  See error processing for codes.
*
*
*
*	Remarks:
*
*	  ifltab(kmultiUserAccess) = 0,  Exclusive open / Single user only mode
*        (The file was opened so that no one else can access the file)
*     ifltab(kmultiUserAccess) = 1,  Read only mode, but locks are supported
*        (someone else can lock the file)
*     ifltab(kmultiUserAccess) = 2,  Standard multi-user mode with locks
*     ifltab(kmultiUserAccess) = 3,  Multi-user "advisory".  This locks the
*        file on the first write and only un-locks on close, or if
*        someone else requests access (where the mode goes to 2).
*        This is much faster if we are the only one writing to the file.
*     ifltab(kmultiUserAccess) = 4, Exclusive access with locks for WRITING only
*     ifltab(kmultiUserAccess) = 5, Exclusive access with locks for both reading
*        and writing.  NOT ACTIVE!!!
*     ifltab(kmultiUserAccess) = 6,  Release exclusive access (internal flag)
*
*     ifltab(klocked) = 0, The file is currently not locked
*     ifltab(klocked) = 1, The file is currently locked
*
*     ifltab(KLOCKB) contains the first byte position of the
*        record used in the file for locking
*     The first word indicates the file is locked
*     The second word is a request to lock the file (another
*        program already has the file locked, and need to check
*        this occasionally)
*     The third word, when combined with the first two, indicate
*        that the file is exclusively locked and is unaccessible
*        to other programs until it is closed.  This is used, for
*        example, when a squeeze is going on.  Other programs
*        should generally gracefully exit.
*
*     primary lock;  advisory (request) lock;  exclusive lock
*       word 1         word 2         word 3
*
*     The lockFlag (second) argument  is as follows:
*        0 - Unlock
*        1 - Lock
*        2 - Test to see if another process has this file locked.  (Do not lock.)
*
*	lockMode
*		0 - Read access
*		1 - Write access
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*
**/

 int zlockPassive(long long *ifltab, int lockFlag, int accessMode)
 {
	int handle;
	int i;
	int count;
	int lockArraySize;
	long long lockPosition;
	long long *fileHeader;
	int status;
	char messageString[90];

	//////////////////////////////////////////
/*	if (accessMode == LOCKING_ACCESS_WRITE) {
		return zlockWrite(ifltab, lockFlag);
	}
	else if (accessMode == LOCKING_ACCESS_READ) {
		return zlockRead(ifltab, lockFlag);
	}
*/

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	handle = (int)ifltab[zdssKeys.khandle];
	lockArraySize = (int)fileHeader[zdssFileKeys.klockArraySizes];
	status = STATUS_OKAY;


	if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d;", handle);
		zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "Enter Lock Passive;  Handle ", messageString);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
			"Multi-user Access %d, locked: %d, Request to lock write word: %d",
			(int)ifltab[zdssKeys.kmultiUserAccess], (int)ifltab[zdssKeys.klocked], lockFlag);
		if (accessMode == 0) {
			zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, messageString, ",  Read access mode.");
		}
		else {
			zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, messageString, ",  Write access mode.");
		}
	}

	//  Check for old DSS files that use a different locking scheme
	if (fileHeader[zdssFileKeys.klockReadArrayAddress] == 0) {
		return status;
	}


	if (lockFlag == 0) {
		if (accessMode == 0) {		//  unlock read
			if (ifltab[zdssKeys.klockReadMyAddress] > 0) {
				status = zlockDss(ifltab, handle, 0, ifltab[zdssKeys.klockReadMyAddress], 8);
				if (status != 0) {
					return zerrorProcessing(ifltab, DSS_FUNCTION_zlocking_ID, zdssErrorCodes.CANNOT_LOCK_FILE,
						0, 0, zdssErrorSeverity.INFORMATION,
						"", "In zlockPassive, lock flag 0");
				}
				ifltab[zdssKeys.klockReadMyAddress] = 0;
			}
			else {
				//  This can be okay....
				if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
					zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "Request to unlock read lock, but read lock is not set.", "");
				}
				status = STATUS_OKAY;
			}
		}
		else {   //  if (accessMode == 1) {		//  unlock write
			if (ifltab[zdssKeys.klockWriteMyAddress] > 0) {
				status = zlockDss(ifltab, handle, 0, ifltab[zdssKeys.klockWriteMyAddress], 8);
				if (status != 0) {
					return zerrorProcessing(ifltab, DSS_FUNCTION_zlocking_ID, zdssErrorCodes.CANNOT_LOCK_FILE,
						0, 0, zdssErrorSeverity.INFORMATION,
						"", "In zlockWrite, lock flag 0.");
				}
				ifltab[zdssKeys.klockWriteMyAddress] = 0;
			}
			else {
				//  This can be okay....
				if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
					zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "Request to unlock write lock, but write lock is not set.", "");
				}
				status = STATUS_OKAY;
			}
		}
	}
	else if (lockFlag == 1) {
		if (accessMode == 0) {		//  lock read
			lockPosition = fileHeader[zdssFileKeys.klockReadArrayAddress];
			for (i = 0; i<lockArraySize; i++) {
				status = zlockDss(ifltab, handle, 2, lockPosition, 8);
				if (status == 0) {
					ifltab[zdssKeys.klockReadMyAddress] = lockPosition;
					break;
				}
				lockPosition++;
			}
			if (status != 0) {
				return zerrorProcessing(ifltab, DSS_FUNCTION_zlocking_ID, zdssErrorCodes.CANNOT_LOCK_FILE,
					0, 0, zdssErrorSeverity.INFORMATION,
					"", "In zlockPassive, lock flag 1");
			}
		}
		else {   //  if (accessMode == 1) {		//  lock write
			lockPosition = fileHeader[zdssFileKeys.klockWriteArrayAddress];
			for (i = 0; i < lockArraySize; i++) {
				status = zlockDss(ifltab, handle, 2, lockPosition, 8);
				if (status == 0) {
					ifltab[zdssKeys.klockWriteMyAddress] = lockPosition;
					break;
				}
				lockPosition++;
			}
			if (status != 0) {
				return zerrorProcessing(ifltab, DSS_FUNCTION_zlocking_ID, zdssErrorCodes.CANNOT_LOCK_FILE,
					0, 0, zdssErrorSeverity.INFORMATION,
					"", "In zlockWrite, lock flag 1.");
			}
		}
	}
	else if (lockFlag >= 2) {
		//  First, do a quick pass to see if another process is accessing this file
		//  Note - you cannot have a passive write lock set without a passive read lock
		//  So, usually, you only need to check for read locks
		if (accessMode == 0) {		//  lock read
			lockPosition = fileHeader[zdssFileKeys.klockReadArrayAddress];
			if ((ifltab[zdssKeys.klockReadMyAddress] == lockPosition) || (ifltab[zdssKeys.klockReadMyAddress] == 0)) {
				//  We have the first position in the passive read lock area
				//  Just do a wholesale check for locks past that
				if (ifltab[zdssKeys.klockReadMyAddress] == lockPosition) {
					lockPosition++;
					lockArraySize--;
				}
				if ((lockPosition < 1) || (lockArraySize < 1)) {
					return zerrorProcessing(ifltab, DSS_FUNCTION_zlocking_ID, zdssErrorCodes.CANNOT_LOCK_FILE,
						0, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS,
						"", "In zlockPassive, read lock info is zero (Squeeze this file to fix)");
				}
				status = zlockDss(ifltab, handle, 3, lockPosition, (lockArraySize * 8));
				//  This will be 99.5% of the cases
				if (status == 0) {
					if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
						zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "zlockPassive, no other process read locks found", "");
					}
					return 0;
				}
			}
			//  Another process either has, or has had, a passive lock set.
			//  Walk down the read lock chain until we find it
			lockPosition = fileHeader[zdssFileKeys.klockReadArrayAddress];
			count = 0;
			for (i = 0; i < lockArraySize; i++) {
				//  Don't check my own read lock
				if (lockPosition != ifltab[zdssKeys.klockReadMyAddress]) {
					status = zlockDss(ifltab, handle, 3, lockPosition, 8);
					if (status != 0) {
						if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
							zmessageDebugLong(ifltab, DSS_FUNCTION_zlocking_ID, "Read Locked at address: ", lockPosition);
						}
						if (lockFlag == LOCKING_LOCK_TEST) {
							return 1;
						}
						count++;
					}
				}
				lockPosition++;
			}
			if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
				zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "zlockPassive, no other process read locks found", "");
			}
			status = count;
		}
		else {   //  if (accessMode == 1) {		//  lock write
			//  We generally will not get to this area unles we have
			//  already detected another process is accessing the file
			//  (and we are writing to the file)
			lockPosition = fileHeader[zdssFileKeys.klockWriteArrayAddress];
			if ((ifltab[zdssKeys.klockWriteMyAddress] == lockPosition) || (ifltab[zdssKeys.klockWriteMyAddress] == 0)) {
				//  We have the first position in the passive write lock area
				//  Do a wholesale check for locks past that
				if (ifltab[zdssKeys.klockWriteMyAddress] == lockPosition) {
					lockPosition++;
					lockArraySize--;
				}
				status = zlockDss(ifltab, handle, 3, lockPosition, (lockArraySize * 8));
				//  This will be 99.5% of the cases
				if (status == 0) {
					if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
						zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "zlockPassive, no other process write locks found", "");
					}
					return 0;
				}
			}
			//  Another process either has, or has had, a passive lock set.
			//  Walk down the lock chain until we find it
			lockPosition = fileHeader[zdssFileKeys.klockWriteArrayAddress];
			count = 0;
			for (i = 0; i < lockArraySize; i++) {
				//  Don't check my own read lock
				if (lockPosition != ifltab[zdssKeys.klockWriteMyAddress]) {
					status = zlockDss(ifltab, handle, 3, lockPosition, 8);
					if (status != 0) {
						if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
							zmessageDebugLong(ifltab, DSS_FUNCTION_zlocking_ID, "Write Locked at address: ", lockPosition);
						}
						if (lockFlag == LOCKING_LOCK_TEST) {
							return 1;
						}
						count++;
					}
				}
				lockPosition++;
			}
			if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
				zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "zlockPassive, no other process write locks found", "");
			}
			status = count;
		}
	}

	if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		if (accessMode == 0) {
			if (ifltab[zdssKeys.klockReadMyAddress]) {
				zmessageDebugLong(ifltab, DSS_FUNCTION_zlocking_ID, "File is Read Locked at address", ifltab[zdssKeys.klockReadMyAddress]);
			}
			else {
				zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "File Read Lock is Unlocked.", "");
			}
		}
		else {
			if (ifltab[zdssKeys.klockWriteMyAddress]) {
				zmessageDebugLong(ifltab, DSS_FUNCTION_zlocking_ID, "File is Write Locked at address", ifltab[zdssKeys.klockWriteMyAddress]);
			}
			else {
				zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "File Write Lock is Unlocked.", "");
			}
		}
	}

	return status;
}

