#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssLocking.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "zerrorCodes.h"
#include "hecdssInternal.h"


/**
*  Function:	zcloseInternal
*
*  Use:			Private: zclose should be called instead.
*
*  Description:	Closes a DSS file.   This function must be called at end of use of the DSS file
*					to ensure that memory is released.
*
*  Declaration: zcloseInternal(long long *ifltab)
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				int boolReopen
*					A flag that indicates if standard messages should be displayed.  This is used when the file
*					is closed and then reopened to try and reduce the chance that it will go into mutli-user access mode.
*
*	Returns:	int status
*					STATUS_OKAY for successful operation.
*					1 (one) for file already closed.  NOP, no issue.
*					errorCode for error.
*
*	Note:		Use "zclose" instead.   zcloseInternal will fail with a DSS Version 6 file.
*				Opening and closing files is resource intensive.  Generally, a file
*				should be opened and closed only once during an execution,
*				or closed if another user wants to access.
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zcloseInternal(long long *ifltab, int boolReopen)
{
	int status;
	int i;
	int ihandle;
	long long zero;
	char messageString[120];
	char fullFilename[256];


	//  Has this file already been closed (or not even opened?)
	if (zgetVersion(ifltab) != 7) {
		if (ifltab[0] == 0) {
			//  Already closed -
			if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_GENERAL) && !boolReopen) {
				//  "     -----DSS---File previously closed, Handle %d;  Process: %d"
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
					ZCLOSE_ERR1_MESS, zhandle(ifltab), zdssVals.pid);
			}
			return STATUS_NOT_OKAY;
		}
		else {
			//  Looks like the ifltab is all messed up.
			if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_CRITICAL)) {  //
				//  "     -----DSS---ZCLOSE:  ifltab not recognizable"
				zmessage(ifltab, ZCLOSE_ERR2_MESS);
				charLong((void *)ifltab[zdssKeys.kfullFilename], fullFilename, 0, sizeof(fullFilename), 0, 1);
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
					"     First value: %lld;  Process: %d,   File: ", ifltab[0], zdssVals.pid);
				zmessage2(ifltab, messageString, fullFilename);
			}
			return zerrorProcessing(ifltab, DSS_FUNCTION_zclose_ID, zdssErrorCodes.IFLTAB_CORRUPT,
				0, ifltab[0], zdssErrorSeverity.MEMORY_ERROR, "", ZCLOSE_ERR2_MESS);
		}
	}

	//  Force all locks to be released
	if (ifltab[zdssKeys.kmultiUserAccess] == EXCLUSIVE_ACCESS) {
		ifltab[zdssKeys.kmultiUserAccess] = RELEASE_ACCESS;
	}

	// If the file has been written to, set the file PID to zero
	if (ifltab[zdssKeys.kpidMyAddress] > 20) {
		zlockActive(ifltab, LOCKING_LEVEL_LOW, LOCKING_LOCK_ON, LOCKING_FLUSH_ON);
		zero = 0;
		zput(ifltab, ifltab[zdssKeys.kpidMyAddress], (int *)&zero, 1, 2);
		zlockActive(ifltab, LOCKING_LEVEL_SUPER, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
	}
	else if (ifltab[zdssKeys.klocked] > 0) {
		zlockActive(ifltab, LOCKING_LEVEL_SUPER, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
	}

	if (ifltab[zdssKeys.kfileWritten] > 0) {
		zlockPassive(ifltab, LOCKING_LOCK_OFF, LOCKING_ACCESS_WRITE);
	}
	zlockPassive(ifltab, LOCKING_LOCK_OFF, LOCKING_ACCESS_READ);

	ihandle = (int)ifltab[zdssKeys.khandle];

	//  print some statistics
	if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_TERSE) && !boolReopen) {
		//  "     -----DSS---ZCLOSE  Handle %d;  Process: %d;  File: "
		charLong((void *)ifltab[zdssKeys.kfullFilename], fullFilename, 0, sizeof(fullFilename), 0, 1);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, ZCLOSE_MESS, zhandle(ifltab), zdssVals.pid);
		zmessage2(ifltab, messageString, fullFilename);
		if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_GENERAL)) {
			zprintFileInfo(ifltab);
		}
	}

	//  Close the file
	status = closeFile(ihandle);

	//  Release any memory allocated
	zmemoryFree(ifltab);
	//  Clean out the start of ifltab
	for (i = 0; i < 50; i++) {
		ifltab[i] = 0;
	}

	 return status;
}


