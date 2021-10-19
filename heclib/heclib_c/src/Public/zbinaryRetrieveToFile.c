#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>

#include "heclib7.h"


/**
*  Function:	zbinaryRetrieveToFile
*
*  Use:			Public
*
*  Description:	Retrieve a binary record (BLOB) and save to an external file
*
*  Declaration: int zbinaryRetrieveToFile(long long *ifltab, const char *pathname, const char *filename);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				const char *pathname
*					The pathname of the dataset to retrieve
*
*				const char *filename
*					The name of the file to write the dataset to.
*
*
*
*	Returns:	int status
*					STATUS_OKAY
*					error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*	Comments:	After storing the dataset in the external file, you can usually tell Windows to run
*					the application associated with the extension of the file using the command:
*					"exec: rundll32 url.dll,FileProtocolHandler fileName",
*
*
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*
*/

int zbinaryRetrieveToFile(long long *ifltab, const char *pathname, const char *filename)
{
	int status;
	zStructTransfer* ztransfer;
	int ihandle;
	int access;
	char messageString[80];



	if (!pathname) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zbinaryRetrieve_ID, zdssErrorCodes.NULL_PATHNAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "");
	}
	if (!filename) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zbinaryRetrieve_ID, zdssErrorCodes.NULL_FILENAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "");
	}

	if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zbinaryRetrieve_ID, "Enter Retrieve to File, Pathname: ", pathname);
		zmessageDebug(ifltab, DSS_FUNCTION_zbinaryRetrieve_ID, "File name: ", filename);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zbinaryRetrieve_ID, "Handle: ",  zhandle(ifltab));
	}

	ztransfer = zstructTransferNew(pathname, 1);
	if (!ztransfer) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zbinaryRetrieve_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
								zdssErrorSeverity.MEMORY_ERROR,
								pathname, "Allocating ztransfer struct");
	}

	status = zread(ifltab, ztransfer);
	if (zisError(status)) {
		//  An error code
		status = zerrorUpdate(ifltab, status, DSS_FUNCTION_zbinaryRetrieve_ID);
		zstructFree(ztransfer);
		return status;
	}
	if (status != STATUS_RECORD_FOUND) {
		if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_GENERAL))  {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, ZREAD_NOT_EXIST, zhandle(ifltab));
			zmessage2(ifltab, messageString, pathname);
		}
		zstructFree(ztransfer);
		return status;
	}

	access = 0;
	status = zopenDisk(filename, &ihandle, access, 0);
	if (status != STATUS_OKAY) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zbinaryRetrieve_ID,
								zdssErrorCodes.UNABLE_TO_WRITE_FILE, 0, 0,
								zdssErrorSeverity.WARNING_NO_FILE_ACCESS,
								filename, "Opening file to save to");
	}
	status = writeBytes(ihandle, (const char *)ztransfer->values1, ztransfer->logicalNumberValues);
	if (status < 0) {
		perror("Write status");
		return zerrorProcessing(ifltab, DSS_FUNCTION_zbinaryRetrieve_ID,
								zdssErrorCodes.UNABLE_TO_WRITE_FILE, 0, 0,
								zdssErrorSeverity.WARNING_NO_FILE_ACCESS,
								filename, "Writing to file");
	}

	closeFile(ihandle);

	zstructFree(ztransfer);

	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zbinaryRetrieve_ID, "Exit Pathname: ", pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zbinaryRetrieve_ID, "  Status: ",  status);
	}

	return status;
}

