#include <stdio.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"

/**
*  Function:	zpermWrite
*
*  Use:			Private
*
*  Description:	This function writes the updated permanent section of the DSS file.
*
*
*  Declaration: int zpermWrite(long long *ifltab)
*
*  Parameters:	long long *ifltab
*					The integer file table array passed among DSS functions.
*
*	Returns:	int status
*					STATUS_OKAY for successful operation.
*					errorCode for error
*
*
*  Note:		Assumes that other functions are writing to the file and updating perm section parameters.
*				This is usually called near the end of a write sequence.  File must be write locked.
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zpermWrite(long long *ifltab)
{
	int status;
	long long iaddress = 0;



	status = zput(ifltab, iaddress, (void *)ifltab[zdssKeys.kfileHeader], (int)ifltab[zdssKeys.kfiHeadSize], 2);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zpermWrite_ID);
	}
	//  Indicate that this file is in a write state (most likely so, already)
	ifltab[zdssKeys.kfileWritten] = 1;

	if (zmessageLevel(ifltab, MESS_METHOD_PERM_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zpermWrite_ID, "Write perm;  Handle:", zhandle(ifltab));
		zmessageDebugInt(ifltab, DSS_FUNCTION_zpermWrite_ID, "Write perm;  Status:", status);
	}

	return status;
}

