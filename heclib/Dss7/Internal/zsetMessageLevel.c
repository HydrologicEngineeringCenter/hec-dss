
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "zdssKeys.h"
#include "zdssLocking.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "heclib6.h"
#include "hecdss7.h"
#include "hecdssInternal.h"


/**
*  Function:	zsetMessageLevel
*				zsetMessageGroupLevel
*				zsetMessageLevelFile
*
*  Use:			Public / Semi-public
*
*  Description:	Set message and/or debug level
*
*  Declaration: void zsetMessageGroupLevel(const char *functionGroup, int level)  // Public
*				void zsetMessageLevel(int group, int level)  //  Semi-Public
*				void zsetMessageLevelFile(long long *ifltab, int level)  //  Public
*
*
*  Parameters:	const char* functionGroup:  The name of the function group defined in zdssMessages.h, as follows
*				int level:  The message or debug level defined in zdssMessages.h, as follows
*				long long *ifltab:  The ifltab array
*
*		group	functionGroup	level		group int def				function char def
*		0		"global"		0-15		MESS_METHOD_GLOBAL_ID		MESS_METHOD_GLOBAL
*		1		"general"		0-6			MESS_METHOD_GENERAL_ID		MESS_METHOD_GENERAL
*		2		"get"			0-6			MESS_METHOD_GET_ID			MESS_METHOD_GET
*		3		"put"			0-6			MESS_METHOD_PUT_ID			MESS_METHOD_PUT
*		4		"read"			0-6			MESS_METHOD_READ_ID			MESS_METHOD_READ
*		5		"write"			0-6			MESS_METHOD_WRITE_ID		MESS_METHOD_WRITE
*		6		"perm"			0-6			MESS_METHOD_PERM_ID			MESS_METHOD_PERM
*		7		"open"			0-6			MESS_METHOD_OPEN_ID			MESS_METHOD_OPEN
*		8		"check"			0-6			MESS_METHOD_CHECK_ID		MESS_METHOD_CHECK
*		9		"locking"		0-6			MESS_METHOD_LOCKING_ID		MESS_METHOD_LOCKING
*		10		"tsread"		0-6			MESS_METHOD_TS_READ_ID		MESS_METHOD_TS_READ
*		11		"tswrite"		0-6			MESS_METHOD_TS_WRITE_ID		MESS_METHOD_TS_WRITE
*		12		"alias"			0-6			MESS_METHOD_ALIAS_ID		MESS_METHOD_ALIAS
*		13		"copy"			0-6			MESS_METHOD_COPY_ID			MESS_METHOD_COPY
*		14		"utility"		0-6			MESS_METHOD_UTILITY_ID		MESS_METHOD_UTILITY
*		15		"catalog"		0-6			MESS_METHOD_CATALOG_ID		MESS_METHOD_CATALOG
*		16		"filecheck"		0-6			MESS_METHOD_FILE_CHECK_ID	MESS_METHOD_FILE_CHECK
*		17		"jni"			0-6			MESS_METHOD_JNI_ID			MESS_METHOD_JNI
*
*
*	MESS_LEVEL_NONE				0:	No messages, including error (not guaranteed).  Highly discourage
*	MESS_LEVEL_CRITICAL			1:	Critical (Error) Messages.  Discouraged.
*	MESS_LEVEL_TERSE			2:	Minimal (terse) output:  zopen, zclose, critical errors.
*	MESS_LEVEL_GENERAL			3:	General Log Messages.  Default.
*	MESS_LEVEL_USER_DIAG		4:	Diagnostic User Messages (e.g., input parameters)
*	MESS_LEVEL_INTERNAL_DIAG_1	5:	Diagnostic Internal Messages level 1 (debug).   Not recommended for users
*	MESS_LEVEL_INTERNAL_DIAG_2	6:	Diagnostic Internal Messages level 2 (full debug)
*
*
*
*  Notes:	zsetMessageGroupLevel and zsetMessageLevel are the same, but
*			zsetMessageGroupLevel should be called by users, as the group number may change
*			zsetMessageLevelFile sets the message level for a specific file
*
*  Example:		Suppose you are having issues reading time series data, then call either
*					zsetMessageGroupLevel(MESS_METHOD_TS_READ, MESS_LEVEL_USER_DIAG);  //  Preferred
*					zsetMessageGroupLevel("tsread", 4);
*
*  See Also:	zset and zsetFile for setting message unit or file
*				zset("munit", "", iunit);		//  Global
*				zset("mhandle", "", ihandle);
*				zsetFile(ifltab, "munit", "", iunit);	//  File specific
*				zsetFile(ifltab, "mhandle", "", ihandle);
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


void zsetMessageLevel(int group, int level)
{
	int i;


	//  Check for initialization
	if (zdssVals.integrityKey != DSS_INTEGRITY_KEY) {
		zinit();
	}
	if ((group < 0) || (group > (NUMBER_METHOD_NAMES-1))) {
		return;
	}
/*	if (group == 0) {
		zset6_("MLVL", "", &level, (size_t)4, (size_t)0);
	}
*/
	zmessaging.methodLevel[group] = level;

	//  If global (DSS-6 and DSS-7) or general (DSS-7)
	//  set all message levels to this
	if ((group == 0) || (group == 1)) {
		for (i=1; i<NUMBER_METHOD_NAMES; i++) {
			zmessaging.methodLevel[i] = level;
		}
	}
}

int zgetMessageLevel(int group)
{
	//  Check for initialization
	if (zdssVals.integrityKey != DSS_INTEGRITY_KEY) {
		zinit();
	}
	if ((group < 0) || (group > (NUMBER_METHOD_NAMES-1))) {
		return -1;
	}
	return zmessaging.methodLevel[group];
}

void zresetMessageLevel()
{
	int i;

	for (i=0; i<NUMBER_METHOD_NAMES; i++) {
		zmessaging.methodLevel[i] = MESS_LEVEL_GENERAL;
	}
}


void zsetMessageGroupLevel(const char *functionGroup, int level)
{
	int group;
	int minLen;

	group = -1;
	if (strlen(functionGroup) < 2) {
		group = 0;
	}
	else {
		minLen = 3;
		if (strncmp(functionGroup, MESS_METHOD_GLOBAL, minLen) == 0) {
			group = MESS_METHOD_GLOBAL_ID;
		}
		else if (strncmp(functionGroup, MESS_METHOD_GENERAL, minLen) == 0) {
			group = MESS_METHOD_GENERAL_ID;
		}
		else if (strncmp(functionGroup, MESS_METHOD_GET, minLen) == 0) {
			group = MESS_METHOD_GET_ID;
		}
		else if (strncmp(functionGroup, MESS_METHOD_PUT, minLen) == 0) {
			group = MESS_METHOD_PUT_ID;
		}
		else if (strncmp(functionGroup, MESS_METHOD_READ, minLen) == 0) {
			group = MESS_METHOD_READ_ID;
		}
		else if (strncmp(functionGroup, MESS_METHOD_WRITE, minLen) == 0) {
			group = MESS_METHOD_WRITE_ID;
		}
		else if (strncmp(functionGroup, MESS_METHOD_PERM, minLen) == 0) {
			group = MESS_METHOD_PERM_ID;
		}
		else if (strncmp(functionGroup, MESS_METHOD_OPEN, minLen) == 0) {
			group = MESS_METHOD_OPEN_ID;
		}
		else if (strncmp(functionGroup, MESS_METHOD_CHECK, minLen) == 0) {
			group = MESS_METHOD_CHECK_ID;
		}
		else if (strncmp(functionGroup, MESS_METHOD_LOCKING, minLen) == 0) {
			group = MESS_METHOD_LOCKING_ID;
		}
		else if (strncmp(functionGroup, MESS_METHOD_TS_READ, minLen) == 0) {
			group = MESS_METHOD_TS_READ_ID;
		}
		else if (strncmp(functionGroup, MESS_METHOD_TS_WRITE, minLen) == 0) {
			group = MESS_METHOD_TS_WRITE_ID;
		}
		else if (strncmp(functionGroup, MESS_METHOD_ALIAS, minLen) == 0) {
			group = MESS_METHOD_ALIAS_ID;
		}
		else if (strncmp(functionGroup, MESS_METHOD_COPY, minLen) == 0) {
			group = MESS_METHOD_COPY_ID;
		}
		else if (strncmp(functionGroup, MESS_METHOD_UTILITY, minLen) == 0) {
			group = MESS_METHOD_UTILITY_ID;
		}
		else if (strncmp(functionGroup, MESS_METHOD_CATALOG, minLen) == 0) {
			group = MESS_METHOD_CATALOG_ID;
		}
		else if (strncmp(functionGroup, MESS_METHOD_FILE_CHECK, minLen) == 0) {
			group = MESS_METHOD_FILE_CHECK_ID;
		}
		else if (strncmp(functionGroup, MESS_METHOD_JNI, minLen) == 0) {
			group = MESS_METHOD_JNI_ID;
		}
	}
	if (group < 0) return;
	zsetMessageLevel(group, level);
}


void zsetMessageLevelFile(long long *ifltab, int level)
{
	//  Check for initialization
	 if (zdssVals.integrityKey != DSS_INTEGRITY_KEY) {
		zinit();
	 }
	ifltab[zdssKeys.kmessLevel] = level;
}

void zsetmessagelevel_(int *group, int *level)
{
	zsetMessageLevel(*group, *level);
}

