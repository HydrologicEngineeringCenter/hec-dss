#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"
#include "hecdssFort.h"


/**
*  Function:	zmessLevel, zmessageLevel
*
*  Use:			Semi-public
*
*  Description:	Quick test to see if the message level is set so that output is to be provided
*
*  Called By:	Various (most functions)
*
*  Declaration: int zmessLevel(int level)
*				int zmessageLevel(long long *ifltab, int callingMethodId, int level)
*				int zmessageLevel(long long *ifltab, int callingMethodId, int level)
*
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				int level
*					The level the calling function is testing for.  For example, if level is passed in as "3", and the
*                   current message level is "4" (3 or above), then true ("1") is returned.  If "2" is passed in,
*					then false ("0") is returned.
*					For current debug, there is only one level - on ("1") or off ("0") (i.e., level can only be 0 or 1).
*					Level is included in the debug call for future expansion.
*
*				int callingMethod
*					An integer from the struct zmessaging_x that represents the calling function.
*					For global, this should be zero ("0") (and you should be just calling zmessLevel)
*
*
*	Retuns:	int boolOnOff
*					Zero (0) if set level is set below that inquired (i.e., don't print message)
*					One (1) is set level is at or above level passed in (print the message)
*
*	Examples:	(assuming this is used to print a message)
*				if (zmessLevel(5)) {   //  If the global message level is 5 or greater, print message
*				if (zmessageLevel(ifltab, MESS_METHOD_OPEN, MESS_LEVEL_GENERAL)) {   //  If the message level for zopen is MESS_LEVEL_GENERAL or greater, print message
*				if (zmessageLevel(ifltab, MESS_METHOD_OPEN, MESS_LEVEL_INTERNAL_DIAG_1)) {   //  If debug is on for zopen, print message
*
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

/*
*	Message Levels for individual methods (operation):
*
*	0:	No messages, including errors (not guaranteed).  Highly discourage
*	1:	Critical (Error) Messages.  Discouraged.
*	2:	Minimal (terse) output:  zopen, zclose, critical errors.
*	3:	General Log Messages.  Default.
*	4:	Diagnostic User Messages (e.g., input parameters)
*	5:	Diagnostic Internal Messages level 1 (debug).   Not recommended for users
*	6:	Diagnostic Internal Messages level 2 (full debug)
*
*	Note:  Levels 1-3 are for Unicode.  Level 4+ is ASCII (hardwired)
*
*
*	Compatibility for version 6 MLVL with no method set (general)
*
*	0:	No messages, including error (not guaranteed).  Highly discourage
*	1:	Critical (Error) Messages.  Discouraged
*	2:	Minimal (terse) output:  zopen, zclose, critical errors.
*	3:	General Log Messages: zwrites.  Default.
*	4:	Log messages, including zread
*	10:	Diagnostic User Messages (e.g., input parameters)
*	12:	Diagnostic Internal Messages level 1 (debug).   Not recommended for users
*	15:	Diagnostic Internal Messages level 2 (full debug)
*
*
*/

int zmessageLevel(long long *ifltab, int callingMethodId, int level)
{

	if (zgetVersion(ifltab) == 7) {
		if (ifltab[zdssKeys.kmessLevel] > -1) {
			if (level <= (int)ifltab[zdssKeys.kmessLevel]) {
				return 1;
			}
		}
	}


	//  At this point, we know that a debug level has been set for
	//  a function.  We need to determine the message level for this function
	if (level <= zmessaging.methodLevel[callingMethodId]) {
		return 1;
	}

	//  Check global settings (compatibility for version 6)
	if (zmessaging.methodLevel[MESS_METHOD_GENERAL_ID] > MESS_LEVEL_GENERAL) {
		if ((level <= MESS_LEVEL_USER_DIAG) && (zmessaging.methodLevel[MESS_METHOD_GENERAL_ID] >= 10)) {
			return 1;
		}
		if ((level <= MESS_LEVEL_INTERNAL_DIAG_1) && (zmessaging.methodLevel[MESS_METHOD_GENERAL_ID] >= 12)) {
			return 1;
		}
		if ((level <= MESS_LEVEL_INTERNAL_DIAG_2) && (zmessaging.methodLevel[MESS_METHOD_GENERAL_ID] >= 15)) {
			return 1;
		}
	}

	return 0;
}

int zmessLevel(int level)
{
	long long ift[1];
	ift[0] = 0;
	return zmessageLevel(ift, MESS_METHOD_GLOBAL_ID, level);
}

