#include "heclib7.h"
#include "zdssMessages.h"
#include "zerrorCodes.h"
#include "zdssVals.h"
#include "hecdssInternal.h"


/**
*  Function:	zerrorUpdate
*
*  Use:			Private (Internal)
*
*  Description:	Updates the error code with the name and ID of the top level function
*
*  Called By:	various functions
*
*  Declaration: int zerrorUpdate(long long *ifltab, int errorCodeIn, int functionID, const char *function_name);
*
*  Parameters:	long long ifltab
*					The integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				int errorCodeIn
*					Any error generated by a lower functionName
*
*				int functionID
*					The function identifier of the top level function
*
*				const char *function_name
*					The name of the top level function.
*
*
*	Returns:	int errorCode
*					The updated errorCode that includes the error, the low level function where the error
*					actual occurred and the high level function that called it.
*
*	Remarks:	When an error occurs in a lower level function, this function combines the name/ID of the
*					highest calling function to clarify the error.  For example, if a disk write error
*					occurs when time series data is being written, the final error code will contain
*					both the disk write error and that it was called from the time series store function.
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int zerrorUpdate(long long *ifltab, int errorCodeIn, int functionID)
{
	int errorCode;
	char *function_name;
	char errorMessage[MAX_LEN_ERROR_MESS];


	if (errorCodeIn == STATUS_OKAY) {
		return errorCodeIn;
	}

	errorCode = zerrorEncodeHigh(errorCodeIn, functionID);

	if (zmessaging.methodLevel[MESS_METHOD_GENERAL_ID] > 0) {
		if (zmessageLevel(ifltab, MESS_METHOD_GENERAL_ID, MESS_LEVEL_CRITICAL)) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_other_ID, "ZerrorUpdate.  Error code: ", errorCodeIn);
			zmessageDebug(ifltab, DSS_FUNCTION_other_ID, "Called from function: ", zgetFunctionName(functionID));
		}
		//  Don't write out function names if this is just a warning
		if (zerrorSeverity(errorCodeIn) > zdssErrorSeverity.WARNING_NO_FILE_ACCESS) {
			errorMessage[0] = '\0';
			function_name = zgetFunctionName(functionID);
			zmessConcat2(errorMessage, MAX_LEN_ERROR_MESS, ERROR_CALLED_BY, function_name);
			zmessageInterface(ifltab, errorMessage, 1);
			//  Make sure the error message is flushed to disk or output
			zmessageFlush(ifltab);
		}
	}
	return errorCode;
}

