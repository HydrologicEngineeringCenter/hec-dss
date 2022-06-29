#if defined(__linux__)
#define _LARGEFILE64_SOURCE 
#endif

#include <stdio.h>

#include "heclib7.h"
#include "hecdssInternal.h"
#include "zdssMessages.h"

/**
*  Function:	zlockDss
*
*  Use:			Private (Internal)
*
*  Description:	Machine dependent low level lock function.  Seeks to the lock word, then attempts lock function.
*
*  Declaration: int zlockDss (long long *ifltab, int ihandle, int mode, long long wordAddress, long nbytes)
*
*  Parameters:	long long *ifltab
*					The integer array dimensioned to int*8 [250] that contains file specific information.
*
*				int ihandle:  handle to file
*
*				int mode
*					0:  Unlock the word
*					1:  Lock the word.  If already locked, wait until available
*					2:  Lock the word.  If already locked, return != 0
*					3:  Test to see if the word is already locked, without locking
*						If already locked, return != 0
*
*				long long wordAddress:  address to start lock
*
*				int nbytes:  Number of bytes to lock from address
*
*
*	Returns:	int status
*					STATUS_OKAY for successful operation.
*					Non-zero for function failed (may be okay)
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*/

#ifdef _MSC_VER

#include <io.h>
#include <sys/locking.h>
#include <errno.h>

void testLock (long long *ifltab, int ihandle)
{
	long long address;
	long long jpos;
	 int status;
	 int i;

	 for (i=100; i< 120; i++) {
		 address = i * 8;
		jpos = _lseeki64(ihandle, address, SEEK_SET);
		 status = _locking (ihandle, _LK_NBLCK, 8);
		 if (status == 0) {
                  _locking (ihandle, _LK_UNLCK, 8);
		 }
		if (status) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_zlocking_ID, "*** Lock on at word ",i);
		}
	 }
}

int zlockDss (long long *ifltab, int ihandle, int mode, long long wordAddress, int nbytes)
{
	long long address;
	long long jpos;
	long numberBytes;
	int i;
	 int status;
	 char messageString[90];
	 int boolDEBUG = 0;


	 address = wordAddress * 8;
	 jpos = _lseeki64(ihandle, address, SEEK_SET);
     if (jpos < 0) {
        status = STATUS_NOT_OKAY;
		return status;
     }

	 numberBytes = (long)nbytes;

	 if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
		 _snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d, mode: %d, word address: %d, number bytes: %d", ihandle, mode, (int)wordAddress, nbytes);
		 zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "zlockDss;  Handle ", messageString);
	 }

/*   Unlock the record */
	 if (mode == 0) {
		 status = _locking(ihandle, _LK_UNLCK, numberBytes);
		 if (status == -1) {
			 zerrorProcessing(ifltab, DSS_FUNCTION_zlocking_ID, zdssErrorCodes.CANNOT_LOCK_FILE, errno,
				 wordAddress, zdssErrorSeverity.WARNING, "", "Unlock failed");
			 zmessageDebugLong(ifltab, DSS_FUNCTION_zlocking_ID, "Unlock failed for address ", wordAddress);
		//	 zmessageDebugInt(ifltab, DSS_FUNCTION_zlocking_ID, "System message ", strerror(errno));
			 _set_errno(0);
		 }
	 }
/*   Lock the record.  If already locked, wait until available */
     else if (mode == 1) {
		 for (i = 0; i < 10; i++) {
			 status = _locking(ihandle, _LK_LOCK, numberBytes);
			 if (!status) {
				 break;
			 }
		 }
     }
/*   Lock the record.  If already locked, return with status != 0 */
     else if (mode == 2) {
          status = _locking (ihandle, _LK_NBLCK, numberBytes);
     }
/*   Test to see if the record is already locked (but do not lock!) */
     else if (mode == 3) {
          status = _locking (ihandle, _LK_NBLCK, numberBytes);
		  if (status == 0) {
				_locking (ihandle, _LK_UNLCK, numberBytes);
		  }
     }


	 ///////////////////////////
/*	 _snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
		 "%d,  mode: %d,  address: %d, number bytes: %d, status: %d",
		 ihandle, mode, (int)wordAddress, nbytes, status);
	 zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "Low level lock, Handle: ", messageString);
*/	 //////////////////////////
	 if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
			"%d,  mode: %d,  address: %d, number bytes: %d, status: %d",
			ihandle, mode, (int)wordAddress, nbytes, status);
		if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
			//zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "Low level lock, Handle: ", messageString);
		}
		else if ((mode == 3) && (status == 0)) {
			//  Don't print debug here; just testing and too much output
		}
		else {
			zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "Low level lock, Handle: ", messageString);
		}
		//if (status == -1) perror ("\nError: Lock Failed:  ");

	}
	/* if (status != 0) {
		 if (mode < 2) {
			 if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_GENERAL)) {
				 zmessage(ifltab, "");
				 zmessage(ifltab, "**** File lock failed ****");
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
					"%d,  mode: %d,  address: %d, number bytes: %d, status: %d",
					ihandle, mode, (int)wordAddress, nbytes, status);
				zmessage(ifltab, messageString);
				zmessage(ifltab, "");
			 }
		 }
	}
   /*if (*status == -1) perror ("\nError: Lock Failed:  %s\n"); */
	 return status;

}

#else

#include <unistd.h>
#include <stdint.h>
#if defined __APPLE__
#define lseek64 lseek
#endif

void testLock(long long* ifltab, int ihandle)
{
	long long address;
	long long jpos;
	int status;
	int i;

	for (i = 100; i < 120; i++) {
		address = i * 8;
		jpos = lseek64(ihandle, address, SEEK_SET);
		status = lockf(ihandle, F_TLOCK, 8);
		if (status == 0) {
			lockf(ihandle, F_ULOCK, 8);
		}
		if (status) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_zlocking_ID, "*** Lock on at word ", i);
		}
	}
}


int zlockDss(long long *ifltab, int ihandle, int mode, long long wordAddress, int nbytes)
{
	long long address;
	long long jpos;
	long numberBytes;
	int i;
	int status;
	char messageString[90];
	int boolDEBUG = 0;


	address = wordAddress * 8;
	jpos = lseek64(ihandle, address, SEEK_SET);
	if (jpos < 0) {
		status = STATUS_NOT_OKAY;
		return status;
	}

	numberBytes = (long)nbytes;

	/*   Unlock the record */
	if (mode == 0) {
		status = lockf(ihandle, F_ULOCK, numberBytes);
		if (status) {
			zerrorProcessing(ifltab, DSS_FUNCTION_zlocking_ID, zdssErrorCodes.CANNOT_LOCK_FILE, status,
				wordAddress, zdssErrorSeverity.WARNING, "", "Unlock failed");
			zmessageDebugLong(ifltab, DSS_FUNCTION_zlocking_ID, "Unlock failed for address ", wordAddress);
			//	 zmessageDebugInt(ifltab, DSS_FUNCTION_zlocking_ID, "System message ", strerror(errno));
		}
	}
	/*   Lock the record.  If already locked, wait until available */
	else if (mode == 1) {
		for (i = 0; i < 10; i++) {
			status = lockf(ihandle, F_LOCK, numberBytes);
			if (!status) {
				break;
			}
		}
	}
	/*   Lock the record.  If already locked, return with status != 0 */
	else if (mode == 2) {
		status = lockf(ihandle, F_TLOCK, numberBytes);
	}
	/*   Test to see if the record is already locked (but do not lock!) */
	else if (mode == 3) {
		status = lockf(ihandle, F_TEST, numberBytes);
	}

	if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
			"%d,  mode: %d,  address: %d, number bytes: %d, status: %d",
			ihandle, mode, (int)wordAddress, nbytes, status);
		if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
			//zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "Low level lock, Handle: ", messageString);
		}
		else if ((mode == 3) && (status == 0)) {
			//  Don't print debug here; just testing and too much output
		}
		else {
			zmessageDebug(ifltab, DSS_FUNCTION_zlocking_ID, "Low level lock, Handle: ", messageString);
		}

	}

	return status;
}
#endif

