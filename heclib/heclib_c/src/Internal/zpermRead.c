
#include <stdio.h>
#include <string.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"
#include "zerrorCodes.h"


/**
*  Function:	zpermRead
*
*  Description:	Read the permanent section of the DSS file.
*
*  Declaration: int zpermRead (long long *ifltab)
*
*  Parameters:	long long *ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*
*	Returns:	int status
*					STATUS_OKAY for successful operation.
*					errorCode for error
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zpermRead (long long *ifltab)
{

	int status;
	long long *fileHeader;
	long long iaddress = 0;


	if (zmessageLevel(ifltab, MESS_METHOD_PERM_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zpermRead_ID, "Handle: ", zhandle(ifltab));
	}

	//  Don't read in perm if we are currently writing and perm has changed
	//  (This doesn't happen, just a saftey check in case someone in the
	//  future changes the code and does not take this into account)
	if (ifltab[zdssKeys.kwritingNow]) {
		if (zmessageLevel(ifltab, MESS_METHOD_PERM_ID, MESS_LEVEL_GENERAL)) {
			zmessageDebugLong(ifltab, DSS_FUNCTION_zpermRead_ID,
				"Programming error; read file header before saving changed header.  Write flag: ", ifltab[zdssKeys.kwritingNow]);
		}
		return STATUS_NOT_OKAY;
	}

	//  Do we need to allocate memory for the file header first?
	if (ifltab[zdssKeys.kfileHeader] == 0) {
		if (ifltab[zdssKeys.kfiHeadSize] == 0) {
			ifltab[zdssKeys.kfiHeadSize] = 100;   //  DSS_MIN_FILE_HEADER_SIZE;
		}
		 status = zmemoryGet(ifltab, zdssKeys.kfileHeader, (int)ifltab[zdssKeys.kfiHeadSize], "DSS file header");
		 if (zisError(status)) {
			 return zerrorUpdate(ifltab, status, DSS_FUNCTION_zpermRead_ID);
		 }
	}

	status = zget(ifltab, iaddress, (int*)ifltab[zdssKeys.kfileHeader], (int)ifltab[zdssKeys.kfiHeadSize], 2);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zpermRead_ID);
	}

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];

	if (fileHeader[zdssFileKeys.kendFileHeader] != DSS_END_HEADER_FLAG) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zpermRead_ID, zdssErrorCodes.INVALID_FILE_HEADER,	0,
			fileHeader[zdssFileKeys.kendFileHeader], zdssErrorSeverity.CORRUPT_FILE, "", "End of header flag is invalid");
	}

	//  If we have not allocated the other arrays yet, do so now.
	//  The file header we just read will have the sizes

	//  Pathname Bin
	if (ifltab[zdssKeys.kpathBin] == 0) {
		if ((fileHeader[zdssFileKeys.kbinSize] < 20) || (fileHeader[zdssFileKeys.kbinSize] >1000)){
			return zerrorProcessing(ifltab, DSS_FUNCTION_zpermRead_ID, zdssErrorCodes.INVALID_HEADER_PARAMETER,	0,
				fileHeader[zdssFileKeys.kbinSize], zdssErrorSeverity.CORRUPT_FILE, "", "Invalid pathname bin size: ");
		}
		status = zmemoryGet(ifltab, zdssKeys.kpathBin, (int)fileHeader[zdssFileKeys.kbinSize], "Pathname bin");
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zpermRead_ID);
		}
	}

	//  Record header (info area)
	 if (ifltab[zdssKeys.kinfo] == 0) {
		 status = zmemoryGet(ifltab, zdssKeys.kinfo, zdssVals.maxInfoSize, "Record header (info area)");
		 if (zisError(status)) {
			 return zerrorUpdate(ifltab, status, DSS_FUNCTION_zpermRead_ID);
		 }
	}

	return status;
}

