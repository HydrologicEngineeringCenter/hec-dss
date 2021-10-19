#include "hecdssInternal.h"

/**
*  Function:	zerrorEncode
*
*  Use:			Private (Internal)
*
*  Description:	Part of the error processing functions, this generates an error code from
*					the severity, high function, low function, error and status
*
*  Called By:	zerrorProcessing
*
*  Declaration: int zerrorEncode(int severity, int highFunction, int lowFunction, int dssError, int status);
*
*  Parameters:	int severity
*					The severity of this error, as defined in "zerrorCodes.h", with 1 being informational and 9 being critical.
*
*				int highFunction
*					The function code of the highest calling function.  For example, this may tsStore.
*
*				int lowFunction
*					The function code of the lowest function, generally where the error actually occurred.
*						For example, this may be disk write.
*
*				int dssError
*					The DSS error number or code associated with this error
*
*				int systemError
*					The system error associated with this.  For example, this may be a
*						system code that the file does not have write permission.
*
*
*	Returns:	int errorCode
*					The errorCode that includes all error information to identify the error and where it was called.
*
*	Remarks:	Except for severity, each error code is 2 digits long (0-99).  Severity is only 1 digit (0-9)
*					Error codes are always negative.
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/



int zerrorEncode(int severity, int highFunction, int lowFunction, int dssError, int systemError)
{
	int errorCode;

	if ((systemError > 99) || (systemError < -9)) {
		systemError = 0;
	}
	errorCode  =  systemError;
	errorCode +=  dssError * 100;
	errorCode +=  lowFunction * 10000;
	errorCode +=  highFunction * 1000000;
	errorCode +=  severity * 100000000;

	if (errorCode > 0) {
		errorCode = -errorCode;
	}
	return errorCode;
}


