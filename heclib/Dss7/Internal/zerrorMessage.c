#include <string.h>
#include <stdio.h>

#include "heclib7.h"
#include "hecdssInternal.h"
#include "zerrorCodes.h"

/**
*  Function:	zerrorMessage
*
*  Use:			Private (Internal)
*
*  Description:	Part of the error processing functions, this returns a pointer to the character string for this error
*
*  Called By:	zerrorProcessing
*
*  Declaration: char *zerrorMessage(char *message, size_t sizeofMessage, int severity, int errorNumber, int functionID);
*
*  Parameters:	char *message
*					A string dimensioned to sizeofMessage to contain the error message.  This char is also returned.
*
*				size_t sizeofMessage
*					The size of message.  No more than sizeofMessage will be returned.  Generally, set to about 300.
*
*				int severity
*					The severity of the error
*
*				int errorNumber
*					The basic DSS error number.  This can not include calling functions or other info,
*					but is a standard error in zdssErrorCodes.
*
*				int functionID
*					The ID number of the primary calling function.  The corresponding name might be used in preamble.
*
*
*	Returns:	int message
*					A pointer to the input message.
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

char *zerrorMessage(char *message, size_t sizeofMessage, int severity, int errorNumber, int functionID)
{
	char mess[50];

	if (severity == zdssErrorSeverity.INFORMATION) {
		_snprintf_s(mess, sizeof(mess), _TRUNCATE, INFORMATION_PREAMBLE, zgetFunctionName(functionID));
		stringCopy(message, sizeofMessage, mess, _TRUNCATE);
	}
	else if (severity < zdssErrorSeverity.WARNING_NO_WRITE_ACCESS) {
		_snprintf_s(mess, sizeof(mess), _TRUNCATE, WARNING_PREAMBLE, zgetFunctionName(functionID));
		stringCopy(message, sizeofMessage, mess, _TRUNCATE);
	}
	else {
		_snprintf_s(mess, sizeof(mess), _TRUNCATE, ERROR_PREAMBLE, zgetFunctionName(functionID));
		stringCopy(message, sizeofMessage, mess, _TRUNCATE);
	}

	if ((errorNumber >= 0) && (errorNumber < NUMBER_ERROR_MESSAGES)) {
		stringCat(message, sizeofMessage, errorMess[errorNumber], _TRUNCATE);
	}
	else {
		stringCat(message, sizeofMessage, errorMess[zdssErrorCodes.UNDEFINED_ERROR], _TRUNCATE);
	}


	return message;
}

