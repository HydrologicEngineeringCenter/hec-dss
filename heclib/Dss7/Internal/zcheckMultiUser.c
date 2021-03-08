#include <stdio.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssLocking.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"

/**
*  Function:	zcheckMultiUser
*
*  Description:	If we are in an "advisory" access level, check to see if another process has
*				requested write access to this file.  If so, go into a regular multi-user state.
*				If another process has read access to this fill and we have written to it, flush
*               buffers, but do not enter multi-user state.  (Only flush when this call is made)
*
*  Use:			Private (Internal)
*
*  Declaration: int zcheckMultiUser(long long *ifltab);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*	Returns:	int status
*					STATUS_OKAY for successful operation.
*					errorCode for error
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

 int zcheckMultiUser(long long *ifltab)
 {

	int handle;
	int status;
	char messageString[80];

	status = STATUS_OKAY;
	handle = (int)ifltab[zdssKeys.khandle];
	if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcheckMultiUser_ID, "Handle: ", zhandle(ifltab));
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d,   locked: %d",
			(int)ifltab[zdssKeys.kmultiUserAccess], (int)ifltab[zdssKeys.klocked]);
		zmessageDebug(ifltab, DSS_FUNCTION_zcheckMultiUser_ID, "Multi-user Access: ", messageString);
	}

	//  We only need to check for multi user access if:
	//  We are in multi-user access mode 3 - advisory
	//  We are writing to the file (don't need to check on reads)
	//  If another program wants to write, go to mode 2, which
	//  flushes to disk just before every unlock (complete write)
	if ((ifltab[zdssKeys.kmultiUserAccess] == SINGLE_USER_ADVISORY_ACCESS) && (ifltab[zdssKeys.kfileWritten] != 0)) {

		// Do a quick test to see if we have access to the file
		// (Does someone else have the advisory request lock locked?)
		status = zlockPassive(ifltab, LOCKING_LOCK_TEST, LOCKING_ACCESS_WRITE);
		if (status > 0) {
			//  Someone else is writing!
			ifltab[zdssKeys.kmultiUserAccess] = MULTI_USER_ACCESS;
			zpidUpdate(ifltab);
			if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_CRITICAL)) {
				zmessageInt(ifltab, ENTER_MULTI_USER_MESS, zhandle(ifltab));
			}
			// Let go of it, if we are not in the middle of a write
			if (ifltab[zdssKeys.klocked] && (ifltab[zdssKeys.kwritingNow] == 0)) {
				zlockActive(ifltab, 0, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
			}
		}
		else {
			//  Check to see if someone has read access on this file (and we've written to it)
			if (ifltab[zdssKeys.kwritesSinceFlush] > 0) {
			/*	status = zlockPassive(ifltab, 2, 0);
				if (status != 0) {
					if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
						zmessageDebugInt(ifltab, DSS_FUNCTION_zcheckMultiUser_ID,
							"Other process detected accessing file; flush buffers.  Handle: ", zhandle(ifltab));
					}
					zflushToDisk(ifltab, 0);
				}  */
			}
		}
	}

	if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d;", handle);
		zmessageDebug(ifltab, DSS_FUNCTION_zcheckMultiUser_ID, "Exit;  Handle: ", messageString);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d,   locked: %d",
			(int)ifltab[zdssKeys.kmultiUserAccess], (int)ifltab[zdssKeys.klocked]);
		zmessageDebug(ifltab, DSS_FUNCTION_zcheckMultiUser_ID, "Multi-user Access: ", messageString);
	}

	return status;
}


void zcheckmultiuser_(long long *ifltab)
{
	zcheckMultiUser(ifltab);
}

