
#include <stdio.h>

#include "heclib7.h"
#include "zdssVals.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"

/**
*  Function:	zwriteEOF
*
*  Use:			Private
*
*  Description:	Writes an EOF flag at the current end of file position
*
*
*  Declaration: int zwriteEOF(long long *ifltab)
*
*  Parameters:	long long ifltab
*					The integer file table array passed among DSS functions.
*
*	Returns:	int status
*					STATUS_OKAY for successful operation.
*					errorCode for error
*
*
*  Note:		Generally zwriteEOFandFlush is called for writes.
*				This function should not be called when buffering is in use.
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zwriteEOF(long long *ifltab)
{
	int status;
	long long lWord;
	char messageString[20];
	long long *fileHeader;


	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%lld",fileHeader[zdssFileKeys.kfileSize]);
		zmessageDebug(ifltab, DSS_FUNCTION_zwriteEOF_ID, "Write EOF at address          ", messageString);
	}

	lWord = DSS_END_FILE_FLAG;
	status = zput(ifltab, fileHeader[zdssFileKeys.kfileSize], (int *)&lWord, 1, 2);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zwriteEOF_ID);
	}
	return status;
}

