#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <stdlib.h>

#include "heclib7.h"


/**
*  Function:	zbinaryStoreFromFile
*
*  Use:			Public
*
*  Description:	Save a binary record (BLOB) from data in an external file
*
*  Declaration: int zbinaryStoreFromFile(long long *ifltab, const char *pathname, const char *filename);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				const char *pathname
*					The pathname of the dataset to save the file under
*
*				const char *filename
*					The name of the file to read the dataset from.
*
*
*
*	Returns:	int status
*					STATUS_OKAY
*					error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*
*/

int zbinaryStoreFromFile(long long *ifltab, const char *pathname, const char *filename)
{

	long long numberCharacters;
	int status;
	zStructTransfer* transferStruct;
	int ihandle;
	int access;



	if (!pathname) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zbinaryStore_ID, zdssErrorCodes.NULL_PATHNAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "");
	}
	if (!filename) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zbinaryStore_ID, zdssErrorCodes.NULL_FILENAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "");
	}

	if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zbinaryStore_ID, "Enter Store to File, Pathname: ", pathname);
		zmessageDebug(ifltab, DSS_FUNCTION_zbinaryStore_ID, "File name: ", filename);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zbinaryStore_ID, "Handle: ",  zhandle(ifltab));
	}

	//  Be sure that we can access the file first

	access = 0;
	status = zopenDisk(filename, &ihandle, access, 0);
	if (status != STATUS_OKAY) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zbinaryStore_ID,
								zdssErrorCodes.UNABLE_TO_ACCESS_FILE, 0, 0,
								zdssErrorSeverity.WARNING_NO_FILE_ACCESS,
								filename, "Opening file to read from");
	}

	numberCharacters = zfileSize(ihandle);
	if (numberCharacters <= 0) {
		perror("Read status");
		return zerrorProcessing(ifltab, DSS_FUNCTION_zbinaryStore_ID,
			zdssErrorCodes.UNABLE_TO_ACCESS_FILE, 0, 0,
			zdssErrorSeverity.WARNING_NO_FILE_ACCESS,
			filename, "Reading from file");
	}

	transferStruct = zstructTransferNew(pathname, 0);
	if (!transferStruct) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zbinaryStore_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
								zdssErrorSeverity.MEMORY_ERROR,
								pathname, "Allocating ztransfer struct");
	}

	transferStruct->logicalNumberValues = (int)numberCharacters;
	transferStruct->values1Number = numberIntsInBytes(transferStruct->logicalNumberValues);
	transferStruct->numberValues = transferStruct->values1Number;
	transferStruct->dataType = 600;   ///  CHANGE ME!!!!!

	transferStruct->values1 = (int *)malloc((size_t)transferStruct->values1Number * 4);

	status = readBytes(ihandle, (char *)transferStruct->values1, transferStruct->logicalNumberValues);
	if (status < 0) {
		perror("Read status");
		return zerrorProcessing(ifltab, DSS_FUNCTION_zbinaryStore_ID,
								zdssErrorCodes.UNABLE_TO_ACCESS_FILE, 0, 0,
								zdssErrorSeverity.WARNING_NO_FILE_ACCESS,
								filename, "Reading from file");
	}

	closeFile(ihandle);

	status = zwrite(ifltab, transferStruct);
	if (zisError(status)) {
		//  An error code
		status = zerrorUpdate(ifltab, status, DSS_FUNCTION_zbinaryStore_ID);
		zstructFree(transferStruct);
		return status;
	}

	free(transferStruct->values1);
	transferStruct->values1 = (int *)0;
	zstructFree(transferStruct);

	if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zbinaryStore_ID, "Exit Pathname: ", pathname);
		zmessageDebug(ifltab, DSS_FUNCTION_zbinaryStore_ID, "File name: ", filename);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zbinaryStore_ID, "  Status: ",  status);
	}

	return status;
}

