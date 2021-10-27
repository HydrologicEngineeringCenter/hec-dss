#include <string.h>
#include <ctype.h>

#include "zdssKeys.h"
#include "zdssVals.h"
#include "zprogress.h"
#include "zdssMessages.h"
#include "heclib.h"

/**
*  Function:	zquery
*
*  Use:			Public
*
*  Description:	Query global parameters
*
*  Declaration: int zquery(const char* parameter,  char* charVal, size_t sizeofCharVal, int *integerValue);
*
*  Parameters:	const char* parameter:
*					The parameter to obtain the value for.
*
*				char* charVal (output)
*					Returns the character value associated with this parameter.
*
*				size_t sizeofCharVal:
*					The size of charVal, in bytes.
*
*				int *integerValue  (output)
*					Returns the integer value associated with this parameter.
*
*	Returns:	int status
*					STATUS_OKAY for successful operation.
*					STATUS_NOT_OKAY if parameter not recognized
*
*
*  See Also:	zinquir for parameters associated with an individual file.
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int zquery(const char* parameter,  char* charVal, size_t lenCharVal, int *integerValue)
{
	char cparm[5];
	int len;
	int i;
	int status;
	long long ifl[1];

	//  Check for initialization
	 if (zdssVals.integrityKey != DSS_INTEGRITY_KEY) {
		zinit();
	 }

	 ifl[0] = 0;
	 if (zmessageLevel(ifl, MESS_METHOD_UTILITY_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		 zmessageDebug(ifl, DSS_FUNCTION_zquery_ID, "Enter, parameter: ", parameter);
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

	if (!strncmp(cparm, "mlvl", 4)) {
		charVal[0] = '\0';
		*integerValue = zmessaging.methodLevel[MESS_METHOD_GLOBAL_ID];
		status = STATUS_OKAY;
	}
	else if (!strncmp(cparm, "mlev", 4)) {
		charVal[0] = '\0';
		*integerValue = zmessaging.methodLevel[MESS_METHOD_GLOBAL_ID];
		status = STATUS_OKAY;
	}
	else if (!strncmp(cparm, "muni", 4)) {
		charVal[0] = '\0';
		*integerValue =  zdssVals.fortranMessageUnit;
		status = STATUS_OKAY;
	}
	else if (!strncmp(cparm, "vers", 4)) {
		stringCopy(charVal, (size_t)lenCharVal, zdssVals.czVersion, _TRUNCATE);
		*integerValue =  7;
		status = STATUS_OKAY;
	}
	else if (!strncmp(cparm, "date", 4)) {
		stringCopy(charVal, (size_t)lenCharVal, zdssVals.czVersionDate, _TRUNCATE);
		*integerValue =  7;
		status = STATUS_OKAY;
	}
	else if (!strncmp(cparm, "curr", 4)) {
		//  Use zprogress.totalNumber as a flag to determine whether
		//  values are coming from DSS-7 or DSS-6
		if (zprogress.totalNumber <= 0) {
			return STATUS_NOT_OKAY;
		}
		*integerValue = zprogress.currentNumber;
		status = STATUS_OKAY;
	}
	else if (!strncmp(cparm, "tota", 4)) {
		if (zprogress.totalNumber <= 0) {
			status = -1;
		}
		*integerValue = zprogress.totalNumber;
		status = STATUS_OKAY;
	}
	else if (!strncmp(cparm, "nerr", 4)) {
		if (zprogress.totalNumber <= 0) {
			status = -1;
		}
		*integerValue = zprogress.numberErrors;
		status = STATUS_OKAY;
	}
	else if (!strncmp(cparm, "maxe", 4)) {
		if (zprogress.totalNumber <= 0) {
			status = -1;
		}
		*integerValue = zprogress.maxErrors;
		status = STATUS_OKAY;
	}
	else if (!strncmp(cparm, "inte", 4)) {
		if (zprogress.totalNumber <= 0) {
			status = -1;
		}
		*integerValue = zprogress.interrupt;
		status = STATUS_OKAY;
	}
	else if (!strncmp(cparm, "erro", 4)) {
		*integerValue = zdssVals.globalErrorFlag;
		stringCopy(charVal, (size_t)lenCharVal, zdssVals.globalErrorMess, _TRUNCATE);
		status = STATUS_OKAY;
	}
	else if (!strncmp(cparm, "empt", 4)) {
		if (zdssVals.copyEmptyRecords) {
			*integerValue = 1;
		}
		else {
			*integerValue = 0;
		}
		charVal[0] = '\0';
		status = STATUS_OKAY;
	}
	else if (!strncmp(cparm, "vdtm", 4)) {
		*integerValue = zdssVals.iverticalDatum;
		stringCopy(charVal, (size_t)lenCharVal, zdssVals.cverticalDatum, _TRUNCATE);
		status = STATUS_OKAY;
	}
	else {
		status = STATUS_NOT_OKAY;;
	}

	 if (zmessageLevel(ifl, MESS_METHOD_UTILITY_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		 if (status == STATUS_OKAY) {
			 zmessageDebug(ifl, DSS_FUNCTION_zquery_ID, "Exit, character val: ", charVal);
			 zmessageDebugInt(ifl, DSS_FUNCTION_zquery_ID, "Integer Value: ", *integerValue);
		 }
		 else {
			 zmessageDebug(ifl, DSS_FUNCTION_zquery_ID, "Exit, parameter NOT found: ", parameter);
		 }
	}

	return status;

}

