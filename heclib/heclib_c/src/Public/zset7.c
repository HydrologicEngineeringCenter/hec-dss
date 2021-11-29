#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include "zdssKeys.h"
#include "zdssVals.h"
#include "zprogress.h"
#include "hecdssInternal.h"
#include "heclib7.h"
#include "zdssMessages.h"
#include "verticalDatum.h"

/**
*  Function:	zset
*
*  Use:			Public
*
*  Description:	Sets global parameters (independent of file being operated on)
*
*  Declaration: int zset7(const char* parameter, const char* charVal, int integerValue)
*
*  Parameters:	const char* parameter:  The parameter to be set.  This includes:
*					mess, mlvl, mlev:  Message level (for compatibility)
*									   See zsetMessageLevel for more info.
*
*				const char* charVal:  The character value associated with this parameter,
*									  if any.  If none, this should be "".
*
*				int integerValue:  The integer value associated with this parameter, or zero
*
*	Returns:	int status
*					STATUS_OKAY for successful operation.
*					STATUS_NOT_OKAY if parameter not recognized
*
*
*  See Also:	zsetFile for setting parameters for an individual file.
*
*	Remarks:	Although compatible with most of the version 6 parameters, there were quite a few
*					that were to set a parameter for the next record written.  This methodology
*					is not applicable in version 7 (and not thread safe), so those parameters
*					are not implemented (and can give a warning message).

*	Parameters

	Parameter	String Value	Numeric Value	Description

	mlvl		ignored			0-15			Set global message level, with 2 terse, 4 normal, > 6 debug levels.
												For compatibility only; use function zsetMessageLevel instead
	munit		ignored			Fortran unit	Sets the Fortran unit number to write messages to.
												File must be opened prior to calling.  See also mhandle
	mhandle		ignored			C Handle		Sets the C handle number to write messages to.
												File must be opened prior to calling.
	program		Name of program   ignored		Sets the name of the program writing data to the file.
				writing							For informational purposes only.
	current		ignored			any, usually 0	Sets the current count number for a loop process, such as
												copying 100 records.  Usually zero to reset the count
	total		ignored			any 			Sets the total number for a loop process.  For copying 100
												records, the number would be 100.
	nerror		ignored			any				Sets the number of error encountered in a loop process, such as a file integrity check.
	maxerrors	ignored			any				Sets the maximum number of errors to encounter in a loop process that would exit the loop.
	interrupt	ignored			1				Sets a flag (1) that tells a loop process to interrupt and exit loop (i.e., cancel process)
	clear		ignored			ignored			Clears any previous errors encountered from local memory.
	maxpath		ignored			any				Sets the expected maximum number of pathnames for the next new file to be created
												to correctly size tables.  Use zopenExtended instead
	collection	ignored			ignored			Cause the next new file to be created to be sized for collections.   A collection set uses
												the same hash code for all records in the set, so that the collection can be quickly traversed.
	empty		ignored			1				When the file is copied, this causes empty time series records (all missing data)
												to be copied also.  Normally empty records are not copied

*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zset7(const char* parameter, const char* charVal, int integerValue)
{
	char cparm[5];
	char messageString[20];
	long long ifltabTemp[1];
	int len;
	int lenCharVal;
	int i;

	//  Check for initialization
	 if (zdssVals.integrityKey != DSS_INTEGRITY_KEY) {
		zinit();
	 }

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

	ifltabTemp[0] = 0;
	if (zmessageLevel(ifltabTemp, MESS_METHOD_UTILITY_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
		zmessageDebug(ifltabTemp, DSS_FUNCTION_zset_ID, "Entering", "");
		zmessageDebug(ifltabTemp, DSS_FUNCTION_zset_ID, "Parameter:     ", parameter);
		zmessageDebug(ifltabTemp, DSS_FUNCTION_zset_ID, "            character val: ", charVal);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d", integerValue);
		zmessageDebug(ifltabTemp, DSS_FUNCTION_zset_ID, "            integer val:   ", messageString);
	}


	if (!strncmp(cparm, "mess", 4)  || !strncmp(cparm, "mlvl", 4) || !strncmp(cparm, "mlev", 4)) {
		if (strlen(charVal) < 3) {
			if (integerValue < 1) {
				integerValue = MESS_LEVEL_NONE;
			}
			else if (integerValue < 2) {
				integerValue = MESS_LEVEL_CRITICAL;
			}
			else if (integerValue < 4) {
				integerValue = MESS_LEVEL_TERSE;
			}
			else if (integerValue < 5) {
				integerValue = MESS_LEVEL_GENERAL;
			}
			else if (integerValue < 11) {
				integerValue = MESS_LEVEL_USER_DIAG;
			}
			else if (integerValue < 13) {
				integerValue = MESS_LEVEL_INTERNAL_DIAG_1;
			}
			else {
				integerValue = MESS_LEVEL_INTERNAL_DIAG_2;
			}
		}
		zsetMessageGroupLevel(charVal, integerValue);
	}
	else if (!strncmp(cparm, "munit", 4)) {
		zdssVals.fortranMessageUnit = integerValue;
	}
	else if (!strncmp(cparm, "mhandle", 4)) {
		zdssVals.messageHandle = integerValue;
	}
	else if (!strncmp(cparm, "prog", 4)) {
		len = sizeof(zdssVals.cprogramName)-1;
		lenCharVal = (int)strlen(charVal);
		if (lenCharVal > len) lenCharVal = len;
		stringCopy(zdssVals.cprogramName, sizeof(zdssVals.cprogramName), charVal, (size_t)lenCharVal);
	}
	else if (!strncmp(cparm, "curr", 4)) {
		zprogress.currentNumber = integerValue;
	}
	else if (!strncmp(cparm, "tota", 4)) {
		zprogress.totalNumber = integerValue;
	}
	else if (!strncmp(cparm, "nerr", 4)) {
		zprogress.numberErrors = integerValue;
	}
	else if (!strncmp(cparm, "maxe", 4)) {
		zprogress.maxErrors = integerValue;
	}
	else if (!strncmp(cparm, "inte", 4)) {
		zprogress.interrupt = integerValue;
	}
	else if (!strncmp(cparm, "clea", 4)) {
		//  Clear errors
		zdssVals.globalErrorFlag = 0;
		zdssVals.globalErrorMess[0] = '\0';
	}
	else if (!strncmp(cparm, "maxp", 4)) {
		zdssVals.maxExpectedPathnames = integerValue;
	}
	else if (!strncmp(cparm, "coll", 4)) {
		zdssVals.newCollectionFile = 1;
	}
	else if (!strncmp(cparm, "empt", 4)) {
		zdssVals.copyEmptyRecords = integerValue;
	}
	else if (!strncmp(cparm, "vdtm", 4)) {
		if (charVal == NULL || charVal[0] == '\0') {
			if (integerValue == IVERTICAL_DATUM_UNSET) {
				zdssVals.iverticalDatum = IVERTICAL_DATUM_UNSET;
				stringCopy(zdssVals.cverticalDatum, sizeof(zdssVals.cverticalDatum), CVERTICAL_DATUM_UNSET, _TRUNCATE);
			}
			else if (integerValue == IVERTICAL_DATUM_NAVD88) {
				zdssVals.iverticalDatum = IVERTICAL_DATUM_NAVD88;
				stringCopy(zdssVals.cverticalDatum, sizeof(zdssVals.cverticalDatum), CVERTICAL_DATUM_NAVD88, _TRUNCATE);
			}
			else if (integerValue == IVERTICAL_DATUM_NGVD29) {
				zdssVals.iverticalDatum = IVERTICAL_DATUM_NGVD29;
				stringCopy(zdssVals.cverticalDatum, sizeof(zdssVals.cverticalDatum), CVERTICAL_DATUM_NGVD29, _TRUNCATE);
			}
			else if (integerValue == IVERTICAL_DATUM_OTHER) {
				zdssVals.iverticalDatum = IVERTICAL_DATUM_OTHER;
				stringCopy(zdssVals.cverticalDatum, sizeof(zdssVals.cverticalDatum), CVERTICAL_DATUM_OTHER, _TRUNCATE);
			}
			else {
				char buf[16];
				sprintf(buf, "%d", integerValue);
				if (zmessageLevel(ifltabTemp, MESS_METHOD_UTILITY_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
					zmessageDebug(ifltabTemp, DSS_FUNCTION_zset_ID, "Invalid integer value for \"VDTM\":   ", buf);
				}
				printf("In zset7: zdssVals.cverticalDatum set to %s (%d)\n", zdssVals.cverticalDatum, zdssVals.iverticalDatum);
				return STATUS_NOT_OKAY;
			}
		}
		else if (!strncasecmp(charVal, CVERTICAL_DATUM_UNSET, lenCharVal)) {
			zdssVals.iverticalDatum = IVERTICAL_DATUM_UNSET;
			stringCopy(zdssVals.cverticalDatum, sizeof(zdssVals.cverticalDatum), CVERTICAL_DATUM_UNSET, _TRUNCATE);
		}
		else if (!strncasecmp(charVal, CVERTICAL_DATUM_NAVD88, lenCharVal)) {
			zdssVals.iverticalDatum = IVERTICAL_DATUM_NAVD88;
			stringCopy(zdssVals.cverticalDatum, sizeof(zdssVals.cverticalDatum), CVERTICAL_DATUM_NAVD88, _TRUNCATE);
		}
		else if (!strncasecmp(charVal, CVERTICAL_DATUM_NGVD29, lenCharVal)) {
			zdssVals.iverticalDatum = IVERTICAL_DATUM_NGVD29;
			stringCopy(zdssVals.cverticalDatum, sizeof(zdssVals.cverticalDatum), CVERTICAL_DATUM_NGVD29, _TRUNCATE);
		}
		else if (!strncasecmp(charVal, CVERTICAL_DATUM_OTHER, lenCharVal)) {
			zdssVals.iverticalDatum = IVERTICAL_DATUM_OTHER;
			stringCopy(zdssVals.cverticalDatum, sizeof(zdssVals.cverticalDatum), CVERTICAL_DATUM_OTHER, _TRUNCATE);
		}
		else {
			// named local vertical datum
			zdssVals.iverticalDatum = IVERTICAL_DATUM_OTHER;
			stringCopy(zdssVals.cverticalDatum, sizeof(zdssVals.cverticalDatum), charVal, _TRUNCATE);
		}
		printf("In zset7: zdssVals.cverticalDatum set to %s (%d)\n", zdssVals.cverticalDatum, zdssVals.iverticalDatum);
	}
	else if (!strncmp(cparm, "vdow", 4)) {
		zdssVals.icanOverwriteLocationVerticalDatum = integerValue ? TRUE : FALSE;
	}
	else {
		if (zmessageLevel(ifltabTemp, MESS_METHOD_UTILITY_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
			zmessageDebug(ifltabTemp, DSS_FUNCTION_zset_ID, "Parameter not recognized:   ", parameter);
		}
		return STATUS_NOT_OKAY;
	}
	return STATUS_OKAY;
}

