
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _MSC_VER
#include <windows.h>
#else
#include <unistd.h>
#endif

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssLocking.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"
#include "zerrorCodes.h"

/**
*  Function:	zlockActive
*
*  Use:			Private (Internal)
*
*  Description:	Primary function for locking DSS file for multiple user access so that write collisions do not occur.
*				When any write occurs to a DSS file, this function is first called to lock the file so that
*				no other process can write to it.  All writes in that function take place.  At the end of that
*				process, the file is flushed (either virtually or physically) and then the file is unlocked.
*
*  Declaration: int zlockActive(long long *ifltab, int level, int lock, int flush)
*
*  Parameters:	long long *ifltab
*					The integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				int level
*					The level of the calling function.  A high level takes precedence; thus if a level 1 call is
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
*	Functions:
*				zlockActive (this function) -  Primary function to control lock and unlock a file for writing
*				zlockDss -  Does the physical locks and unlocks (for all locking functions)
*				zlockPassive - Locks, or checks for a lock, in the passive lock area to indicate that
*							 more than one process wants to either write to the file, and it should go into
*							 multiple user access mode, or read from the file, and any writers
*							 should periodically flush to disk.
*
*
*	How locking works in DSS
*
*				The first three words after the permanent section is the "active" lock area. Word 100 is the
*				main file lock word, that when locked indicates that no other process can write to the file.
*				Word 101 is the advisory or request write lock, which is locked by another process when it
*				wants write access to the file. The original process is supposed to check that word occasionally
*				and if it is locked, both processes go into a multi-user mode. For multi-user mode, the main
*				lock must be obtained, the file written to and then synced to disk, then unlocked. Syncing
*				greatly slows down both processes. Word 102 indicates an exclusive lock, such as when the file
*				is being squeezed. When this word (along with the first two) are locked, another process will
*				know that it cannot access the file.
*
*				Following the active lock area is the passive lock area. Each process accessing a DSS file has 2
*				passive lock words, a read lock word (e.g., word 103, 105,…) and a write lock word (e.g., word
*				104, 106, …). When a process first opens a DSS file, it locks its read lock. This indicates that any
*				other process writing to the file should occasionally sync the file to disk so that this process
*				can read what it is writing. When a process first writes to a file, it locks its write lock. This tells
*				any other processes writing to the file to go into multi-user mode. The read and write lock
*				remain on until the file is closed.
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

int zlockingOld(long long *ifltab, int level, int lock, int flush);

 int zlockActive(long long *ifltab, int level, int lock, int flush)
 {
	int handle;
	int i;
	int j;
	int count;
	int status;
	int numberAttempts;
#ifdef _MSC_VER
	DWORD sleepTime;
#else
	useconds_t sleepTime;
#endif
	double d;
	long long *fileHeader;
	char messageString[90];


	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	handle = (int)ifltab[zdssKeys.khandle];
	status = STATUS_OKAY;


	if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d;", handle);
		zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "Enter active lock;  Handle ", messageString);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "Multi-user Access %d, locked: %d, level: %d, lock: %d:, flush: %d",
			(int)ifltab[zdssKeys.kmultiUserAccess], (int)ifltab[zdssKeys.klocked], level, lock, flush);
		zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, messageString, "");
	}

	//  Check for old DSS files that use a different locking scheme
	if (fileHeader[zdssFileKeys.klockReadArrayAddress] == 0) {
		return zlockingOld(ifltab, level, lock, flush);
	}


	//  If a higher level function owns this lock, then we do nothing,
	//  As that function is controlling locks, reads and writes
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




	status = STATUS_OKAY;

	//  Check that this is a multiple user access file
	if (ifltab[zdssKeys.kmultiUserAccess] > READ_ACCESS) {

		//  Lock file
		if (lock == LOCKING_LOCK_ON) {

			//  If this is our first write, lock a word in the file to let other processes
			//  that we are writing (and they need to go into multi-user mode
			if (ifltab[zdssKeys.klockWriteMyAddress] == 0) {
				zlockPassive(ifltab, LOCKING_LOCK_ON, LOCKING_ACCESS_WRITE);
			}

			//  Check to see if there are other processes that want to write (are writing) to the file
			//  If so, go into multi-user mode, which will cause a file flush at the end of each major write.
			//  Do we need to go into multi-user mode because another process wants to write to the file?
			if (ifltab[zdssKeys.kmultiUserAccess] == SINGLE_USER_ADVISORY_ACCESS) {
				//  Test to see if someone else has a write lock
				status = zlockPassive(ifltab, LOCKING_LOCK_TEST, LOCKING_ACCESS_WRITE);
				if (status > 0) {
					//  Someone else is writing!
					ifltab[zdssKeys.kmultiUserAccess] = MULTI_USER_ACCESS;
					zpidUpdate(ifltab);
					if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_CRITICAL)) {
						_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, ENTER_MULTI_USER_MESS, handle);
						zmessage(ifltab, messageString);
					}
				}
			}
			//  If we are already in multi-user mode, have the other process left and can we
			//  return to single access mode?
			else if (ifltab[zdssKeys.kmultiUserAccess] == MULTI_USER_ACCESS) {
				if (ifltab[zdssKeys.kopenMode] == SINGLE_USER_ADVISORY_ACCESS) {
					//  Can we pop-out of multi-user mode, because other processes
					//  have left (closed the file?)
//  Disabled because profiling showed excessive time consumed doing the following test
//  (Maybe 500 to 1000 times slower)
	/*				status = zlockPassive(ifltab, LOCKING_LOCK_TEST, LOCKING_ACCESS_WRITE);
					if (status == 0) {
						//  No other users.  Go back to single user
						ifltab[zdssKeys.kmultiUserAccess] = SINGLE_USER_ADVISORY_ACCESS;
						zpidUpdate(ifltab);
						if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_CRITICAL)) {
							_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, ENTER_SINGLE_USER_MESS, handle);
							zmessage(ifltab, messageString);
						}
					}  */
				}
			}


			//  If the file has already been locked, ignore this request
			//  (this allows a smart program to lock and save buffers before zwrite does)
			if (!ifltab[zdssKeys.klocked]) {
				//  Is this an exclusive lock (no other users allowed access)?
				if (ifltab[zdssKeys.kmultiUserAccess] == EXCLUSIVE_ACCESS) {
					//  Test that no one else is accessing the file; check the read lock area
					status = zlockPassive(ifltab, LOCKING_LOCK_TEST, LOCKING_ACCESS_READ);
					if (status != 0) {
						return zerrorProcessing(ifltab, DSS_FUNCTION_zlocking_ID, zdssErrorCodes.CANNOT_LOCK_EXCLUSIVE,
										 0, 0, zdssErrorSeverity.WARNING, "", "");
					}
					status = zlockDss(ifltab, handle, 2, fileHeader[zdssFileKeys.klockAddressWord], 16);
					if (status != 0) {
						return zerrorProcessing(ifltab, DSS_FUNCTION_zlocking_ID, zdssErrorCodes.CANNOT_LOCK_EXCLUSIVE,
										 0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "");
					}
				}
				else {
					//  Standard lock
					//  Most processes will use this lock
					status = zlockDss(ifltab, handle, 2, fileHeader[zdssFileKeys.klockAddressWord], 8);
					if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
						_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d,  status: %d", handle, status);
						zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "attempt no-wait lock primary lock;  Handle:", messageString);
					}
				}

				//  If are in the "advisory mode" (IFLTAB(KMULT) = 3), and cannot
				//  access the file because it is in use, attempt a lock on the advisory lock,
				//  the word following the primary lock, and see if the other user
				//  will unlock the file (go to mode 2)
				if (status != 0) {
					// Do a quick test to see if we have access to the file
					// (Does someone else have the file in exclusive access mode?)
					status = zlockDss(ifltab, handle, 3, ifltab[zdssKeys.klockExclusiveWord], 8);
					if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
						_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d,  status: %d", handle, status);
						zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "test exclusive lock;  Handle:", messageString);
					}
					if (status != 0) {
						//  Can not lock the exclusive access word.  Be sure this is not just
						//  a fluke (someone else could be testing lock at the same time!
						//  (This is not common)
						for (i=0; i<5; i++) {
							#ifdef _MSC_VER
								Sleep(200);
							#else
								usleep((200 * 1000));
							#endif
							status = zlockDss(ifltab, handle, 3, ifltab[zdssKeys.klockExclusiveWord], 8);
							if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
								_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d,  status: %d", handle, status);
								zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID,"additional test exclusive lock word;  Handle:", messageString);
							}
							if (status == STATUS_OKAY) {
								break;
							}
						}
						if (status != 0) {
							//  No access to file - someone else has exclusive access (uncommon)
							zlockDss(ifltab, handle, 0, ifltab[zdssKeys.klockExclusiveWord], 8);
							return zerrorProcessing(ifltab, DSS_FUNCTION_zlocking_ID, zdssErrorCodes.CANNOT_LOCK_FILE,
										 0, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS,
										 "", "Exclusive lock denied");
						}
					}
					//  We failed to lock the primary lock area.... someone else must have it locked.
					if (ifltab[zdssKeys.kmultiUserAccess] == SINGLE_USER_ADVISORY_ACCESS) {
						//  If we are in a multi-user "advisory" mode, then we need to go into regular mode
						//  Mode 2 will cause standard locking and flushing after every write
						//  This can greatly slow down writes, but is the only way for safe access.
						ifltab[zdssKeys.kmultiUserAccess] = MULTI_USER_ACCESS;
						zpidUpdate(ifltab);
						if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_TERSE))  {
							_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, ENTER_MULTI_USER_MESS, handle);
							zmessage(ifltab, messageString);
						}
					}
					//  Now try to re-lock the original area until the other program
					//  releases it, or we time out
					numberAttempts = DSS_NUMBER_LOCK_ATTEMPS;
					sleepTime = 50;
					//  For remote files, or file with many other processes writing, increase values
					if (ifltab[zdssKeys.kremote]) {
						d = (double)numberAttempts * 1.5;
						numberAttempts = (int)d;
						sleepTime = 100;
					}
					count = 0;
					for (i=0; i<numberAttempts; i++) {
						status = zlockDss(ifltab, handle, 1, fileHeader[zdssFileKeys.klockAddressWord], 8);
						if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
							_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d,  status: %d", handle, status);
							zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "attempt wait lock of primary lock;  Handle:", messageString);
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
						ifltab[zdssKeys.klocksDenied]++;
						#ifdef _MSC_VER
							Sleep(sleepTime);
						#else
							usleep((sleepTime * 1000));
						#endif
						j = i / 5;
						j *= 5;
						if (j == i) {
							if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_GENERAL)) {
								count++;
								_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, WAIT_MULTI_USER_MESS, count, handle);
								zmessage(ifltab, messageString);
							}
						}
					}
					if (status != 0) {
						//  Hmmm.  Failed lock.  Error out
						return zerrorProcessing(ifltab, DSS_FUNCTION_zlocking_ID, zdssErrorCodes.CANNOT_LOCK_MULTI_USER,
										 numberAttempts, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS, "", "");
					}
				}

				//  Successful lock.

				ifltab[zdssKeys.klocked] = 1;
				ifltab[zdssKeys.klockLevel] = level;
				if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
					zmessageDebugInt(ifltab, DSS_FUNCTION_zlocking_ID, "File Locked;  Handle:", zhandle(ifltab));
				}
				if (flush) {
					//  Read the file header
					//	On Multiple users systems, be sure this is a
					//  physical read (not just a buffer in memory)!!
					zpermRead(ifltab);
				}
			}
			//  Save PID on first write
			if (ifltab[zdssKeys.kpidMyAddress] == 0) {
				zpidWrite(ifltab);
			}
		}
		else {
			//  Unlock file

			//  If we are unlocking the file, then we need to return the
			//  level to 0 to be sure perm and other areas are written to disk
			ifltab[zdssKeys.klockLevel] = 0;

			//  If a flag to exit exclusive mode, then flush and unlock
			if (ifltab[zdssKeys.kmultiUserAccess] == RELEASE_ACCESS) {
				//  Release exclusive access
				if (flush && ifltab[zdssKeys.kwritingNow]) {
					zpermWrite(ifltab);
					zflushToDisk(ifltab, 1);
					ifltab[zdssKeys.kwritingNow] = 0;
				}
				//  Now unlock the file
				status = zlockDss(ifltab, handle, 0, fileHeader[zdssFileKeys.klockAddressWord], 16);
				ifltab[zdssKeys.klocked] = 0;
				if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
					zmessageDebugInt(ifltab, DSS_FUNCTION_zlocking_ID, "File Unlocked;  Handle:", zhandle(ifltab));
				}
			}

			//  If we are in an exclusive write lock mode, don't unlock file
			if (ifltab[zdssKeys.klocked] == 1)  {
				if (flush && ifltab[zdssKeys.kwritingNow]) {
					zpermWrite(ifltab);
					ifltab[zdssKeys.kwritingNow] = 0;
				}
				if (ifltab[zdssKeys.kmultiUserAccess] != EXCLUSIVE_ACCESS) {

					if (ifltab[zdssKeys.klocked] && (ifltab[zdssKeys.kmultiUserAccess] == MULTI_USER_ACCESS)) {
						//  Flush the file if in multi-user access mode...
						if (flush) {
							zflushToDisk(ifltab, 1);
						}

						//  Now unlock the file
						status = zlockDss(ifltab, handle, 0, fileHeader[zdssFileKeys.klockAddressWord], 8);
						if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
							_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d,  status: %d", handle, status);
							zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "unlock primary lock;  Handle:", messageString);
						}
						ifltab[zdssKeys.klocked] = 0;
						ifltab[zdssKeys.klockCheckSet] = 0;
						if (status != 0) {
							//   hmmm...  shouldn't get here
							if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_TERSE)) {
								_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, UNABLE_UNLOCK_MESS, handle);
								zmessage(ifltab, messageString);
							}
						}
					}
				}
			}
		}
	}
	else {
		if (lock == LOCKING_LOCK_ON) {
			if (!ifltab[zdssKeys.klocked]) {
				ifltab[zdssKeys.klocked] = 1;
				//  Read the file header (Always!)
				//  Don't need to force (not multi-user)
				if (flush) {
					zpermRead(ifltab);
				}
			}
		}
		else {
			if (flush && ifltab[zdssKeys.kwritingNow]) {
				zpermWrite(ifltab);
				ifltab[zdssKeys.kwritingNow] = 0;
			}
			ifltab[zdssKeys.klocked] = 0;
		}
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

