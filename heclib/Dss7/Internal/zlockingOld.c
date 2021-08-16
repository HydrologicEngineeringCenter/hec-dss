#ifdef _MSC_VER
#include <io.h>
#include <windows.h>
#else
#include <unistd.h>
#include <stdint.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include "hecdssInternal.h"
#include "hecdss7.h"
#include "zdssKeys.h"
#include "zdssLocking.h"
#include "zdssMessages.h"
#include "zdssVals.h"

#include "zerrorCodes.h"

/**
*  Function:	zlockingOld
*
*	DREPRECIATED
*	Replaced by zlockActive
*
*  Use:			Private (Internal)
*
*  Description:	Function for locking DSS file for multiple user access so that write collisions do not occur.
*				When any write occurs to a DSS file, this function is first called to lock the file so that
*				no other process can write to it.  All writes in that function take place.  At the end of that
*				process, the file is flushed (either virtually or physically) and then the file is unlocked.
*
*  Declaration: int zlockingOld(long long *ifltab, int level, int lock, int flush)
*
*  Parameters:	long long *ifltab
*					The integer array dimensioned to int*8 [250] that contains file specific information.  
*					This should be considered as a “handle” array and must be passed to any function that accesses that file.
*
*				int level 
*					The level of the calling function.  A high level takes presidence; thus if a level 1 call is
*					made to unlock a level 3 lock, that (lower) function level is ignored.  Only the same level
*					or a higher level than set, will control.  Levels are from 1 to 3.
*
*				int lock
*					A boolean (1 or 0) to indicate if the file should be locked (lock == 1) or
*					if it should be unlocked (lock == 0)
*
*				int flush
*					A boolean (1 or 0) indicating if the file buffers should be flushed to disk on 
*					a unlock (all writes completed), or the perm area read in on a read.
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
*	  ifLTAB(kmultiUserAccess) = 0,  Exclusive open / Single user only mode
*        (The file was opened so that no one else can access the file)
*     ifLTAB(kmultiUserAccess) = 1,  Read only mode, but locks are supported
*        (someone else can lock the file)
*     ifLTAB(kmultiUserAccess) = 2,  Standard multi-user mode with locks
*     ifLTAB(kmultiUserAccess) = 3,  Multi-user "advisory".  This locks the
*        file on the first write and only un-locks on close, or if
*        someone else requests access (where the mode goes to 2).
*        This is much faster if we are the only one writing to the file.
*     ifLTAB(kmultiUserAccess) = 4, Exclusive access with locks for WRITING only
*     ifLTAB(kmultiUserAccess) = 5, Exclusive access with locks for both reading
*        and writing.  NOT ACTIVE!!!
*     ifLTAB(kmultiUserAccess) = 6,  Release exclusive access (internal flag)
*
*     ifLTAB(KLOCK) = 1, The file is currently not locked
*     ifLTAB(KLOCK) = 2, The file is currently locked
*
*     IFLTAB(KLOCKB) contains the first byte position of the
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
*     The MODE (second) argument for LOCKDSS is as follows:
*        0 - Unlock
*        1 - Lock.  Wait if unavailable.
*        2 - Lock.  Do not wait if unavailable.
*        3 - Test for lock.  (Do not lock.)
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*
**/

 int zlockingOld(long long *ifltab, int level, int lock, int flush)
 {
	int handle;
	int i;
	int j;
	int count;
	long long lockPosition;
	long long klockAddress;
	int status;
	char messageString[90];

	int klockRequested;
	int kwlock;
	
	klockRequested = zdssKeys.kpidMyAddress;
	kwlock = zdssKeys.klockWriteMyAddress;

	klockAddress = 100;

	handle = (int)ifltab[zdssKeys.khandle];
	status = STATUS_OKAY;
	if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {	
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d;", handle);
		zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "Enter     zlockingOld;  Handle ", messageString);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "Multi-user Access %d, locked: %d, level: %d, lock: %d:, flush: %d",
			(int)ifltab[zdssKeys.kmultiUserAccess], (int)ifltab[zdssKeys.klocked], level, lock, flush);
		zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, messageString, "");
	}

	//  If a higher level function owns this lock, then we do nothing,
	//  As that function is controling locks, reads and writes
	//  For example, the time series functions may lock the file
	//  for several records to be written, instead of just one
	//  The copy file function my lock the file for all of the records
	if (level < ifltab[zdssKeys.klockLevel]) {
		if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d;  Higher Level: %d", 
				level, (int)ifltab[zdssKeys.klockLevel]);
			zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "returning; higher level controls.  This level: ", messageString);
		}
		return STATUS_OKAY;
	}

	//  Check that this is a multiple user access file
	if (ifltab[zdssKeys.kmultiUserAccess] > READ_ACCESS) {
		if (lock != 0) {
		//  If the file has already been locked, ignore this request
		//  (this allows a smart program to lock and save buffers before zwrite does)
			if (!ifltab[zdssKeys.klocked]) {
				//  Is this an exclusive lock (no other users allowed access)?
				if (ifltab[zdssKeys.kmultiUserAccess] == EXCLUSIVE_ACCESS) {
					status = zlockDss(ifltab, handle, 2, klockAddress, 24);
					if (status != STATUS_OKAY) {
						return zerrorProcessing(ifltab, DSS_FUNCTION_zlocking_ID, zdssErrorCodes.CANNOT_LOCK_EXCLUSIVE,
										 0, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS, "", "");
					}
				}
				else {
					//  Standard lock
					//  Most processes will use this lock
					status = zlockDss(ifltab, handle, 2, klockAddress, 8);
					if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
						_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d,  status: %d", handle, status);
						zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "attempt no-wait lock primary lock;  Handle:", messageString);
					}
				}
				
				//  If are in the "advisory mode" (IFLTAB(KMULT) = 3), and cannot
				//  access the file because it is in use, attempt a lock on the advisory lock,
				//  the word following the primary lock, and see if the other user
				//  will unlock the file (go to mode 2)
				if (status != STATUS_OKAY) {
					// Do a quick test to see if we have access to the file
					// (Does someone else have the file in exclusive access mode?)
					lockPosition = klockAddress + 2;
					status = zlockDss(ifltab, handle, 3, lockPosition, 8);
					if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
						_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d,  status: %d", handle, status);
						zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "test exclusive lock;  Handle:", messageString);
					}
					if (status != STATUS_OKAY) {
						//  Can not lock the exclusive access word.  Be sure this is not just
						//  a fluke (someone else could be testing lock at the same time!
						//  (This is not common)
						for (i=0; i<5; i++) {
#ifdef _MSC_VER
							Sleep(500);// 500 Milliseconds
#else
							usleep(500 * 1000); // 500 Milliseconds
#endif
							status = zlockDss(ifltab, handle, 3, lockPosition, 8);
							if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
								_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d,  status: %d", handle, status);
								zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID,"additional test exclusive lock word;  Handle:", messageString);
							}
							if (status == STATUS_OKAY) {
								break;
							}
						}
						if (status != STATUS_OKAY) {
							//  No access to file - someone else has exclusive access (uncommon)							
							zlockDss(ifltab, handle, 0, lockPosition, 8);							
							return zerrorProcessing(ifltab, DSS_FUNCTION_zlocking_ID, zdssErrorCodes.CANNOT_LOCK_FILE,
										 0, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS, "", "");
						}
					}
					//  We failed to lock the priamry lock area.... someone else must have it locked.
					if (ifltab[zdssKeys.kmultiUserAccess] == SINGLE_USER_ADVISORY_ACCESS) {
						//  If we are in a mutli-user "advisory" mode, then we need to go into regular mode
						//  Mode 2 will cause standard locking and flushing after every write
						//  This can greatly slow down writes, but is the only way for safe access.
						ifltab[zdssKeys.kmultiUserAccess] = MULTI_USER_ACCESS;
						if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_TERSE))  {
							_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, ENTER_MULTI_USER_MESS, handle);
							zmessage(ifltab, messageString);
						}
					}
					//  Now let that other program that we want to write to the file
					//  We do this by locking the second lock word - the advisory request lock
					lockPosition = klockAddress + 1;
					status = zlockDss(ifltab, handle, 2, lockPosition, 8);
					if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
						_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d,  status: %d", handle, status);
						zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "attempt no-wait lock of advisory lock word;  Handle:", messageString);
					}
					if (status == STATUS_OKAY) {
						//  Remember that we have the advisory request word locked.
						ifltab[klockRequested] = 1;
					}
					//  Now try to re-lock the original area until the other program
					//  releases it, or we time out
					count = 0;
					for (i=0; i<100; i++) {
						status = zlockDss(ifltab, handle, 1, klockAddress, 8);
						if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
							_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d,  status: %d", handle, status);
							zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "attempt wait lock of priamry lock;  Handle:", messageString);
						}
						if (status == STATUS_OKAY) {
							if (count > 0) {
								if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_GENERAL)) {
									_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, GOT_MULTI_USER_MESS, handle);
									zmessage(ifltab, messageString);
								}
							}
							break;
						}
						//Sleep(1000);
						j = i / 2;
						j *= 2;
						if (j == i) {
							if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_GENERAL)) {
								count++;
								_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, WAIT_MULTI_USER_MESS, count, handle);
								zmessage(ifltab, messageString);
							}
						}
					}
					if (status != STATUS_OKAY) {
						//  Hmmm.  Failed lock.  Error out
						return zerrorProcessing(ifltab, DSS_FUNCTION_zlocking_ID, zdssErrorCodes.CANNOT_LOCK_MULTI_USER,
										 0, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS, "", "");
					}
				}
				else {
					//  Successful lock.  Lock the request area just in case
					//  another program begins to access this file
					if (ifltab[zdssKeys.kmultiUserAccess] == MULTI_USER_ACCESS) {
						lockPosition = klockAddress + 1;
						status = zlockDss(ifltab, handle, 2, lockPosition, 8);
						if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
							_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d,  status: %d", handle, status);
							zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "attempt wait lock of advisory lock;  Handle:", messageString);
						}
						if (status == STATUS_OKAY) {
						//  Remember we have that area locked
							ifltab[klockRequested] = 1;
						}
					}
				}
				ifltab[zdssKeys.klocked] = 1;
				ifltab[zdssKeys.klockLevel] = level;
				if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
					_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d;", handle);
					zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "File Locked;  Handle:", messageString);
				}     
				if (flush) {
					//  Read the permanent section of the file
					//	On Multiple users systems, be sure this is a
					//  phyical read (not just a buffer in memory)!!
					zpermRead(ifltab);
				}
			}
		}
		else {
			//  Unlock file

			//  If we are in an exclusive write lock mode, don't unlock file
			if ((ifltab[kwlock] != 1) && (ifltab[zdssKeys.kmultiUserAccess] != EXCLUSIVE_ACCESS)) {
				if (flush) {
					zpermWrite(ifltab);
				}
				//  If we have the request area locked, unlock it
				if (ifltab[klockRequested] != 0) {
					lockPosition = klockAddress + 1;
					zlockDss(ifltab, handle, 0, lockPosition, 8);
					if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
						_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d,  status: %d", handle, status);
						zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "unlock advisory lock;  Handle:", messageString);
					}
					ifltab[klockRequested] = 0;					
				}
				//  check if un-lock request has been issued
				if (ifltab[zdssKeys.kmultiUserAccess] == SINGLE_USER_ADVISORY_ACCESS) {
					lockPosition = klockAddress + 1;
					status = zlockDss(ifltab, handle, 3, lockPosition, 8);
					if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
						_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d,  status: %d", handle, status);
						zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "test advisory lock;  Handle:", messageString);
					}
					if (status != STATUS_OKAY) {						
						ifltab[zdssKeys.kmultiUserAccess] = MULTI_USER_ACCESS;
						if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_TERSE)) {
							_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, ENTER_MULTI_USER_MESS, handle);
							zmessage(ifltab, messageString);
						}
					}
				}
				if (ifltab[zdssKeys.klocked] && (ifltab[zdssKeys.kmultiUserAccess] == MULTI_USER_ACCESS)) {
					//  Flush the file if in multi-user access mode...
					if (flush) {						
						zflushToDisk(ifltab, 1);
					}				
					//  Now unlock the file
					status = zlockDss(ifltab, handle, 0, klockAddress, 8);
					if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
						_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d,  status: %d", handle, status);
						zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "unlock primary lock;  Handle:", messageString);
					}
					ifltab[zdssKeys.klocked] = 0; 
					ifltab[zdssKeys.klockCheckSet] = 0;
					if (status != STATUS_OKAY) {
						//   hmmm...  shouldn't get here
						if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_TERSE)) {
							_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, UNABLE_UNLOCK_MESS, handle);
							zmessage(ifltab, messageString);
						}   
					}
				}
/////////////   FIX me - should't this be 4 - EXCLUSIVE_ACCESS ???
				else if (ifltab[zdssKeys.kmultiUserAccess] == 6) {
					//  Release exclusive access
					if (flush) {
						zpermWrite(ifltab);
						zflushToDisk(ifltab, 1);
					}
					//  Now unlock the file
					status = zlockDss(ifltab, handle, 0, klockAddress, 24);
					ifltab[zdssKeys.klocked] = 0;
					if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
						_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d;", handle);
						zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "File Unlocked;  Handle:", messageString);
					} 
				}
			}
		}
	}
	else {
		if (lock != 0) {
			if (!ifltab[zdssKeys.klocked]) {
				ifltab[zdssKeys.klocked] = 1;
				//  Read the permanent section of the file (Always!)
				//  Don't need to force (not multi-user)
				if (flush) {
					zpermRead(ifltab);
				}
			}
		}
		else {
			if (flush) {
				zpermWrite(ifltab);
			}
			ifltab[zdssKeys.klocked] = 0;		
		}
	}


	//  If we are unlocking the file, then we need to return the
	//  level to 0 to be sure perm and other areas are written to disk
	if (lock == 0) {
		ifltab[zdssKeys.klockLevel] = 0;
	}

	if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d;", handle);
		zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "Exit;  Handle ", messageString);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "Multi-user Access %d, locked: %d, lock: %d",
			(int)ifltab[zdssKeys.kmultiUserAccess], (int)ifltab[zdssKeys.klocked], lock);
		zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, messageString, "");
		if ((int)ifltab[zdssKeys.klocked]) {
			zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "File is Locked.", "");
		}
		else {
			zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "File is Unlocked.", "");
		}
	}
	return status;
}
