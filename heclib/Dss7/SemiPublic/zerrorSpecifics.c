#include <string.h>

#include "heclib.h"
#include "zerrorCodes.h"
#include "hecdssInternal.h"

/**
*  Function:	zerrorSpecifics
*
*  Use:			Public
*
*  Description:	When an error occurs, this returns specifics about that error.
*					For last error, set optionalErrorCode to zero
*					"errorMessage" is always from last error, not error code input
*					All other info is from error code
*
*  Declaration: void zerrorSpecifics(int errorCode, int *severity, int *errorType, int *errorNumber,
*					char *errorMessage, size_t sizeofErrorMessage, char *systemMessage, size_t sizeofSystemMessage,
*					char *functionName, size_t sizeofFunctionName, char *calledByFunction, size_t sizeofCalledByFunction);
*
*  Parameters:	int errorCode
*					The error code returned by a function that has experienced an error.  This is usually a large negative number.
*
*				int *severity
*					Returns the severity of the error, ranging from 1 (information) to 9 (memory error)
*
*				int *errorType
*					indicates what kind of error occurred and what should be done about it:
*						0 - Warning or Informational (do nothing)
*						1 - Access error (file does not exist, permission problem, ...)  (User needs to fix.)
*						2 - File error (broken file, junk in file)  (Squeeze file)
*						3 - Memory error or memory exhausted.   Exit program without saving anything.
*
*				int *errorNumber
*					Returns the actual number of the error (usually a 2 digit number)
*
*				int *errorMessage
*					Returns a description of the error
*
*				size_t sizeofErrorMessage
*					The size of string errorMessage
*
*				int *systemMessage
*					If the error was a system error (e.g., no permission to write), then this is that message.  Otherwise blank
*
*				size_t sizeofSystemMessage
*					The size of string systemMessage
*
*				int *functionName
*					The name of the function where the error occurred
*
*				size_t sizeofFunctionName
*					The size of string functionName
*
*				int *calledByFunction
*					The name of the highest level function when the error occurred
*
*				size_t sizeofFunctionName
*					The size of string functionName
*
*
*	Author:			Bill Charley
*	Date:			2013
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

void zerrorSpecifics(int optionalErrorCode, int *severity, int *errorType, int *errorNumber,
	char *errorMessage, size_t sizeofErrorMessage, char *systemMessage, size_t sizeofSystemMessage,
	char *functionName, size_t sizeofFunctionName, char *calledByFunction, size_t sizeofCalledByFunction)
{
	int highFunction;
	int lowFunction;
	int systemError;
	char *c;

	if (optionalErrorCode == 0) {
		optionalErrorCode = (int)zdssVals.globalErrorFlag;
	}

	*severity = zerrorDecode(optionalErrorCode, &highFunction, &lowFunction, errorNumber, &systemError);

	if (*severity >= zdssErrorSeverity.MEMORY_ERROR) {
		*errorType = ERROR_TYPE_MEMORY;
	}
	else if (*severity >= zdssErrorSeverity.WRITE_ERROR) {
		*errorType = ERROR_TYPE_FILE;
	}
	else if (*severity >= zdssErrorSeverity.WARNING_NO_WRITE_ACCESS) {
		*errorType = ERROR_TYPE_ACCESS;
	}
	else {
		*errorType = ERROR_TYPE_WARNING;
	}

	stringCopy(errorMessage, sizeofErrorMessage, zdssVals.globalErrorMess, strlen(zdssVals.globalErrorMess));

	if (systemError) {
#ifdef _MSC_VER
		strerror_s(systemMessage, MAX_LEN_ERROR_MESS, systemError);
#else
		strerror_r(systemError, systemMessage, MAX_LEN_ERROR_MESS);
#endif
	}
	else if (sizeofErrorMessage > 0) {
		systemMessage[0] = '\0';
	}

	if (lowFunction && sizeofFunctionName) {
		c = zgetFunctionName(lowFunction);
		stringCopy(functionName, sizeofFunctionName, c, strlen(c));
	}

	if (highFunction && sizeofCalledByFunction) {
		c = zgetFunctionName(highFunction);
		stringCopy(calledByFunction, sizeofCalledByFunction, c, strlen(c));
	}

}

