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
*  Function:	zcheckAccessReset
*
*  Use:			Public
*
*  Description:	Check to see if we can reset multi-user access to single user access.
*					This is used to attempt to avoid multi-user access mode
*					when doing minor writes, such as updating the catalog sort order.
*					Resets by closing and then reopening the file.
*					Can also be used to force a full flush to disk (set force =1)
*
*  Declaration: int zcheckAccessReset(long long *ifltab, int boolForce, int quiescentTimeMills);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				int boolForce
*					A flag to force the close and reopen.  If set to 0, the operation will not be done if
*					there have not been any writes or the file is not in multi-user access mode.
*					If set to 1, the operation will be performed, regardless.
*
*				int quiescentTimeMills
*					How long since our last write before we reset.  If we are actively writing, don't
*					reset the access; wait until we idle for a while (e.g., 4 seconds)
*					If we know that we won't be writing for awhile, then set this to zero.
*
*	Returns:	int status
*					STATUS_OKAY for successful operation.
*					errorCode for error.
*
*	Note:		.
*
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zcheckAccessReset(long long *ifltab, int boolForce, int quiescentTimeMills)
{
	int status;
	int access;
	long long ltime;
	char fullFilename[256];



	//  Has this file already been closed (or not even opened?)
	if (zgetVersion(ifltab) != 7) {
		return 0;
	}

	if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zopen_ID, "Enter function zcheckAccessReset for handle: ", zhandle(ifltab));
	}

	//  Do we really need to do this?
	if (boolForce == 0) {
		if (ifltab[zdssKeys.kmultiUserAccess] != MULTI_USER_ACCESS) boolForce = -1; // no
		//  We should be quiescent for a period of time...  say 4 seconds (?)
		if ((boolForce == 0) && (quiescentTimeMills > 0)) {
			ltime = getCurrentTimeMillis() - (long long)quiescentTimeMills;  //  current time minus 5 seconds
			if (ifltab[zdssKeys.kmyLastWriteTime] > ltime) boolForce = -1; // no
		}
		if (boolForce == -1) {
			if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
				zmessageDebug(ifltab, DSS_FUNCTION_zopen_ID, "Exit function zcheckAccessReset with no need for reset", "");
			}
			return STATUS_OKAY;
		}
	}

	charLong((void *)ifltab[zdssKeys.kfullFilename], fullFilename, 0, sizeof(fullFilename), 0, 1);
	access =       (int)ifltab[zdssKeys.kopenMode];

	if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zopen_ID, "Function zcheckAccessReset, resetting with close / open for file ", fullFilename);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zopen_ID, "zcheckAccessReset quiescentTimeMills: ", quiescentTimeMills);
		if (boolForce == 0) {
			ltime = getCurrentTimeMillis() - ifltab[zdssKeys.kmyLastWriteTime];
			zmessageDebugLong(ifltab, DSS_FUNCTION_zopen_ID, "zcheckAccessReset optional call, milliseconds since last write: ", ltime);
		}
		else {
			zmessageDebug(ifltab, DSS_FUNCTION_zopen_ID, "Function zcheckAccessReset, Force close / open.", "");
		}
	}

	//  Now close the file
	status = zcloseInternal(ifltab, 0);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zclose_ID);
	}

	//  Reopen the file (as if it had not been written to)
	status = zopenInternal(ifltab, fullFilename, access, 0, 0, 0, 1);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zclose_ID);
	}

	if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zopen_ID, "Exit zcheckAccessReset, status: ", status);
	}

	 return status;
}

/*
int zcheckAccessReset(long long *ifltab, int boolForce, int quiescentTimeMills)
{
	int status;
	int ihandle;
	int access;
	int numberReads;
	int numberWrites;
	long long ltime;
	char messageString[120];
	char fullFilename[256];


	charLong((void *)ifltab[zdssKeys.kfullFilename], fullFilename, 0, sizeof(fullFilename), 0, 1);
	if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zclose_ID, "Enter zcheckAccessReset for file ", fullFilename);
	}

	//  Has this file already been closed (or not even opened?)
	if (zgetVersion(ifltab) != 7) {
		if (ifltab[0] == 0) {
			//  Already closed -
			if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
				//  "     -----DSS---ZCLOSE  File previously closed, Handle %d;  File: "
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
					ZCLOSE_ERR1_MESS, zhandle(ifltab), zdssVals.pid);
				zmessage2(ifltab, messageString, fullFilename);
			}
			return STATUS_NOT_OKAY;
		}
		else if (zgetVersion(ifltab) == 6) {
			//  No for version 6 files.
			if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
				zmessageDebug(ifltab, DSS_FUNCTION_zclose_ID, "zcheckAccessReset call on a DSS Version 6 file.", "");
			}
			return STATUS_NOT_OKAY;
		}
		else {
			//  Looks like the ifltab is all messed up.
			if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_CRITICAL)) {  //
																					//  "     -----DSS---ZCLOSE:  ifltab not recognizable"
				zmessage(ifltab, ZCLOSE_ERR2_MESS);
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
					"     First value: %lld;  File: ", ifltab[0]);
				zmessage2(ifltab, messageString, fullFilename);
			}
			return zerrorProcessing(ifltab, DSS_FUNCTION_zclose_ID, zdssErrorCodes.IFLTAB_CORRUPT,
				0, ifltab[0], zdssErrorSeverity.MEMORY_ERROR, "", ZCLOSE_ERR2_MESS);
		}
	}

	//  Do we really need to do this?
	if (boolForce == 0) {
		if (ifltab[zdssKeys.kopenStatus] == OPEN_STAT_READ_ONLY) boolForce = -1; // no
		if (ifltab[zdssKeys.kmultiUserAccess] == EXCLUSIVE_ACCESS) boolForce = -1; // no
		if (ifltab[zdssKeys.kfileWritten] == 0) boolForce = -1; // no
		if (ifltab[zdssKeys.kmultiUserAccess] != MULTI_USER_ACCESS) boolForce = -1; // no
																					//  We should be quiescent for a period of time...  say 4 seconds (?)
		if ((boolForce == 0) && (quiescentTimeMills > 0)) {
			ltime = getCurrentTimeMillis() - (long long)quiescentTimeMills;  //  current time minus 5 seconds
			if (ifltab[zdssKeys.kmyLastWriteTime] > ltime) boolForce = -1; // no
		}
		if (boolForce == -1) {
			if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
				zmessageDebug(ifltab, DSS_FUNCTION_zclose_ID,
					"zcheckAccessReset return with no need to reset, file ", fullFilename);
			}
			return STATUS_OKAY;
		}
	}

	ihandle = (int)ifltab[zdssKeys.khandle];
	access = (int)ifltab[zdssKeys.kopenMode];
	numberReads = (int)ifltab[zdssKeys.knumberReads];
	numberWrites = (int)ifltab[zdssKeys.knumberWrites];

	//  Now close the file
	status = zcloseInternal(ifltab, 1);
	if (zisError(status)) {
		//  FIX ME
		if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			zmessageDebug(ifltab, DSS_FUNCTION_zclose_ID,
				"zcheckAccessReset error on close, file ", fullFilename);
		}
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zclose_ID);
	}

	//  Reopen the file (as if it had not been written to)
	status = zopenInternal(ifltab, fullFilename, access, 0, 0, 0, 1);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zclose_ID);
	}

	if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_CRITICAL)) {
		zmessageInt(ifltab, RESET_MULTI_USER_MESS, zhandle(ifltab));
	}

	//  Update number of reads and writes
	ifltab[zdssKeys.knumberReads] += numberReads;
	ifltab[zdssKeys.knumberWrites] += numberWrites;

	return status;
}

*/

