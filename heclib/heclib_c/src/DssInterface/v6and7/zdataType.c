#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssVals.h"
#include "hecdssInternal.h"
#include "zdssMessages.h"



/**
*  Function:	zdataType
*
*  Use:			Public
*
*  Description:	Obtains the data type (time series, paired, etc.) for a record
*					Does not guarantee that the record exists (use zcheck for that)
*
*  Declaration: int zdataType (long long *ifltab, const char* pathname);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				const char* pathname
*					The pathname of the record to check.
*
*
*	Returns:	int dataType
					> 0  The data type for the record
					See file zdssRecordDescriptions.h for data types and descriptions
*					STATUS_NOT_OKAY (=STATUS_RECORD_NOT_FOUND, =-1) if the data type can not be determined
*					< -1.   error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*	Remarks:	Version 6 and 7
*					If the type is not time series, then a positive dataType indicates the record does exist
*					If time series, then no guarantee that the record exists (uses the E part to identify)
*
*	See Also:	ztypeName - returns the name of the data type
*					const char *ztypeName(int recordType, int boolAbbreviation);
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*
*/

int zdataType (long long *ifltab, const char* pathname)
{
	int dataType;
	int status;
	int catSort;
	int interval;


	if (!pathname) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zreadInfo_ID, zdssErrorCodes.NULL_PATHNAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "pathname is null");
	}

	if (zmessageLevel(ifltab, MESS_METHOD_GENERAL_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zreadInfo_ID, "Enter zdataType;  Handle: ",  zhandle(ifltab));
		zmessageDebug(ifltab, DSS_FUNCTION_zreadInfo_ID, " Pathname: ",pathname);
	}


	status = zcheck(ifltab, pathname);
	if (zisError(status)) {
		return status;
	}

	if (status == STATUS_RECORD_FOUND) {
		i8toi4(ifltab[zdssKeys.kbinTypeAndCatSort], &dataType, &catSort);
	}
	else {
		//  If it is time series, the D part may not be exact...
		//  Check to see if a valid E (Interval) part
		interval = ztsGetInterval(zgetVersion(ifltab), pathname);
		if (interval > 0) {
			dataType = 100;
		}
		else if (interval < 0) {
			dataType = 110;
		}
		else {
			dataType = 0;
		}
	}

	if (zmessageLevel(ifltab, MESS_METHOD_GENERAL_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zreadInfo_ID, "zdataType, Found: ", status);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zreadInfo_ID, "zdataType, Data Type: ", dataType);
	}
	return dataType;
}
