
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssLocking.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "zerrorCodes.h"
#include "hecdssInternal.h"

//  Set file parameters

int zsetFile(long long *ifltab, const char* parameter, const char* charVal, int integerVal)
{
	char cparm[5];
	char cVal[20];
	int len;
	int i;
	int status;
	char messageString[90];
	long long *fileHeader;


	if (!parameter) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zsetFile_ID, zdssErrorCodes.NULL_ARGUMENT,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "parameter is null");
	}

	//  Check for initialization
	 if (zdssVals.integrityKey != DSS_INTEGRITY_KEY) {
		zinit();
	 }

	 fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	//  Make the parameter lower case
	len = (int)strlen(parameter);
	//len = min(len, 4);
	if (len > 4) len = 4;
	for (i=0; i<len; i++) {
		cparm[i] = tolower(parameter[i]);
	}
	for (i=len; i<5; i++) {
		cparm[i] = 0;
	}

	//  Make the charVal lower case
	len = (int)strlen(charVal);
	//len = min(len, sizeof(cVal));
	if (len > sizeof(cVal)) len = sizeof(cVal);
	for (i=0; i<len; i++) {
		cVal[i] = tolower(charVal[i]);
	}
	for (i=len; i<sizeof(cVal); i++) {
		cVal[i] = 0;
	}

	if (!strncmp(cparm, "mlvl", 4)) {
		ifltab[zdssKeys.kmessLevel] = integerVal;
	}
	else if (!strncmp(cparm, "mlev", 4)) {
		ifltab[zdssKeys.kmessLevel] = integerVal;
	}
	else if (!strncmp(cparm, "munit", 4)) {
		//  Fortran message unit
		ifltab[zdssKeys.kfortMessUnit] = integerVal;
		zdssVals.fortranMessageUnit = integerVal;
	}
	else if (!strncmp(cparm, "mhandle", 4)) {
		//  C message handle
		ifltab[zdssKeys.kmessHandle] = integerVal;
		zdssVals.messageHandle = integerVal;
	}
	else if (!strncmp(cparm, "mflush", 4)) {
		//  message flush
		zmessageFlush(ifltab);
	}
	else if (!strncmp(cparm, "readonly", 4)) {
		//  Read Only permission
		if (!strncmp(cVal, "on", 2)) {
			if (ifltab[zdssKeys.kopenStatus] != OPEN_STAT_CLOSED) {
				ifltab[zdssKeys.kopenStatus] = OPEN_STAT_READ_ONLY;
			}
		}
		else if (!strncmp(cVal, "off", 3)) {
			//  Can only turn read only off if it's on (not some other status)
			if (ifltab[zdssKeys.kopenStatus] == OPEN_STAT_READ_ONLY) {
				ifltab[zdssKeys.kopenStatus] = OPEN_STAT_WRITE;
			}
		}
	}
	else if (!strncmp(cparm, "ndat", 4)) {
		//  Logical number of data to set in info area.
		//  This is for DSS-6 calling conventions compatibility
		ifltab[zdssKeys.ksetLogicalNumberData] = integerVal;
	}
	else if (!strncmp(cparm, "detu", 4)) {
		//  detune file, only for testing purposes
		//  This will cause all records written to file to become inaccessible
		//  and will tremendously slow down all I/O
		//  Everything should still work, just really slow
		status = zlockActive(ifltab, 0, LOCKING_LOCK_ON, LOCKING_FLUSH_ON);
		if (zisError(status)) return status;
		zpermRead(ifltab);
		if (fileHeader[zdssFileKeys.knumberRecords] == 0) {
			fileHeader[zdssFileKeys.kdetune] = integerVal;
			zpermWrite(ifltab);
		}
		zlockActive(ifltab, 0, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
		if (fileHeader[zdssFileKeys.knumberRecords] != 0) {  //  WARNING
			return zerrorProcessing(ifltab, DSS_FUNCTION_zsetFile_ID,
				zdssErrorCodes.NON_EMPTY_FILE, (int)fileHeader[zdssFileKeys.knumberRecords], 0,
				zdssErrorSeverity.WARNING, "", parameter);
		}
	}
	else if (!strncmp(cparm, "acce", 4) || !strncmp(cparm, "mult", 4)) {
	/*
	int access:  read/write access to the file  (integerVal)
	*					0 - GENERAL_ACCESS:  Doesn't matter (no error if file doesn't have write permission)
	*					1 - READ_ACCESS:  Read only (will not allow writing to file)
	*					2 - MULTI_USER_ACCESS:  Read/Write permission with full mutil-user access
	*						(usually slow, but necessary for multiple processes)
	*					3 - SINGLE_USER_ADVISORY_ACCESS:  Read/Write permission with mutil-user advisory access
	*						(throws an error if file is read only).  Best (and default access)
	*					4 - EXCLUSIVE_ACCESS:  Exclusive write (used for squeezing).  Throws an error if not available.
	*/
		if ((integerVal >= 0) && (integerVal <= 6)) {
			//  Has the file been opened yet?
			if (ifltab[zdssKeys.kintegrityKey1] == zdssVals.integrityKey) {
				// Yes - set the mode now
				if (ifltab[zdssKeys.klocked]) {
					//  Is it locked?  We'll need to unlock before changing modes
					zlockActive(ifltab, 3, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
				}
				ifltab[zdssKeys.kmultiUserAccess] = integerVal;
				zpidUpdate(ifltab);
				//  If exclusive, lock now
				if (ifltab[zdssKeys.kmultiUserAccess] == EXCLUSIVE_ACCESS) {
					 status = zlockActive(ifltab, 0, LOCKING_LOCK_ON, LOCKING_FLUSH_ON);
					 if (zisError(status)) return status;
				}
				if (ifltab[zdssKeys.kmultiUserAccess] == MULTI_USER_ACCESS) {
					zlockActive(ifltab, LOCKING_LEVEL_HIGH, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
					if (zmessageLevel(ifltab, MESS_METHOD_LOCKING_ID, MESS_LEVEL_TERSE))  {
						_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, ENTER_MULTI_USER_MESS, zhandle(ifltab));
						zmessage(ifltab, messageString);
					}
				}
				//  Else, wait until next write access
			}
			else {
				//  File not opened yet - set a flag to use this access level
				ifltab[zdssKeys.klocksDenied] = DSS_INTEGRITY_KEY;
				ifltab[zdssKeys.kmultiUserAccess] = integerVal;
				zpidUpdate(ifltab);
			}
		}
	}
	else if (!strncmp(cparm, "maxp", 4)) {
		status = zlockActive(ifltab, 0, LOCKING_LOCK_ON, LOCKING_FLUSH_ON);
		if (zisError(status)) return status;
		zpermRead(ifltab);
		fileHeader[zdssFileKeys.kmaxExpectedPathnames] = integerVal;
		zpermWrite(ifltab);
		zlockActive(ifltab, 0, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
	}
	else if (!strncmp(cparm, "recl", 4)) {
		//  Reclaim space level
		if ((integerVal >= RECLAIM_NONE) && (integerVal <= RECLAIM_ALL)) {
			ifltab[zdssKeys.kreclaimLevel] = integerVal;
		}
		else {
			return zerrorProcessing(ifltab, DSS_FUNCTION_zsetFile_ID,
				zdssErrorCodes.INVALID_NUMBER, integerVal, 0,
				zdssErrorSeverity.INFORMATION, "", "Reclaim");
		}
	}
	else {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zsetFile_ID,
				zdssErrorCodes.INVALID_PARAMETER, 0, 0,
				zdssErrorSeverity.INFORMATION, "", parameter);
	}
	return 0;
}

//  Fortran compatible interface
void zsetfi7_(long long *ifltab, const char* parameter, const char* charVal, int *integerVal,
			  int *status, size_t lenParam, size_t lenCharVal)
{
	char param[5];
	char cval[41];

	stringCopy(param, sizeof(param), parameter, lenParam);
	if (lenParam > 4)
		lenParam = 4;
	param[(int)lenParam] = '\0';
	stringCopy(cval, sizeof(cval), charVal, lenCharVal);
	if (lenCharVal > 40)
		lenCharVal = 40;
	cval[(int)lenCharVal] = '\0';

	*status = zsetFile(ifltab, param, cval, *integerVal);
}

