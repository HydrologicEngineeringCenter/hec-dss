#include "heclib7.h"
#include "hecdssInternal.h"
#include "zerrorCodes.h"

/**
*  Function:	zisError
*
*  Use:			Semi-private
*
*  Description:	 A convenience function returns 1 (true) if the passed in status is an error
*
*  Declaration: int zisError (int status);
*
*  Parameters:
*				int status - the status from a DSS function
*
*  Returns:		0:  Not an error
*				1:  An error
*
*  Remarks:		Used primarily to make code more readable.
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zisError (int errorCode)
{
	int highFunction;
	int lowFunction;
	int dssError;
	int status;
	int severity;

	if (errorCode >= STATUS_OKAY) {
		return 0;
	}
	else if (errorCode == STATUS_RECORD_NOT_FOUND) {
		return 0;
	}
	else if (errorCode > -10) {
		return 1;
	}

	severity = zerrorDecode(errorCode, &highFunction, &lowFunction, &dssError, &status);

	if (severity >= zdssErrorSeverity.INVALID_ARGUMENT) {
		return 1;
	}
	else {
		return 0;
	}
}

