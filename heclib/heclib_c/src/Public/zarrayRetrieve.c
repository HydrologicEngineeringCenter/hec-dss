#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "heclib.h"


/**
*  Function:	zarrayRetrieve
*
*  Use:			Public
*
*  Description:	Retrieve a generic array record
*
*  Declaration: int zarrayRetrieve(long long *ifltab, zStructArray *arrayStruct);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				zStructArray *arrayStruct
*					An array struct that will contain either an int array, or
*					a float array or a double array.  You can only have one
*					array in a struct array.
*					This struct is created by the following method:
*						zStructArray* zstructArrayNew(const char* pathname);
*					with pathname being an valid existing pathname
*					When you are finished with the struct, the struct must be freed by a call to
*						void zstructFree(zStructArray* zstructArrayNew)
*					NEVER REUSE A zStructArray, always free and create a new on.
*
*	Returns:	int status
*					STATUS_OKAY
*					error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*	Comments:
*
*		For further description, see zarrayStore().
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*
*/

int zarrayRetrieve(long long *ifltab, zStructArray *arrayStruct)
{
	int status;
	char messageString[80];
	zStructTransfer* ztransfer;


	if (!arrayStruct) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zarrayRetrieve_ID, zdssErrorCodes.NULL_ARGUMENT,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "arrayStruct is null");
	}
	if (!arrayStruct->pathname) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zarrayRetrieve_ID, zdssErrorCodes.NULL_PATHNAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "arrayStruct pathname is null");
	}

	//  Messages and debug
	if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zarrayRetrieve_ID, "Enter Pathname: ", arrayStruct->pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zarrayRetrieve_ID, "  Handle: ",  zhandle(ifltab));
	}

	ztransfer = zstructTransferNew(arrayStruct->pathname, 1);
	if (!ztransfer) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zarrayRetrieve_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
								zdssErrorSeverity.MEMORY_ERROR,
								arrayStruct->pathname, "Allocating ztransfer struct");
	}

	status = zread(ifltab, ztransfer);
	if (zisError(status)) {
		//  An error code
		status = zerrorUpdate(ifltab, status, DSS_FUNCTION_zarrayRetrieve_ID);
		zstructFree(ztransfer);
		return status;
	}
	if (status != STATUS_RECORD_FOUND) {
		if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_GENERAL))  {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, ZREAD_NOT_EXIST, zhandle(ifltab));
			zmessage2(ifltab, messageString, arrayStruct->pathname);
		}
		zstructFree(ztransfer);
		return status;
	}
	if (ztransfer->dataType != DATA_TYPE_ARRAY) {
		status = zerrorProcessing(ifltab, DSS_FUNCTION_zarrayRetrieve_ID,
			zdssErrorCodes.WRONG_RECORD_TYPE, DATA_TYPE_ARRAY,
			(long long)ztransfer->dataType,
			zdssErrorSeverity.WARNING, ztransfer->pathname, "");
		zstructFree(ztransfer);
		return status;
	}

	if (zgetVersion(ifltab) == 7) {
		if ((ztransfer->values1) && (ztransfer->values1Number > 0)) {
			arrayStruct->numberIntArray = ztransfer->values1Number;
			arrayStruct->intArray = (int *)malloc(ztransfer->values1Number * INT_SIZE);
			convertIntArray(ztransfer->values1, arrayStruct->intArray, ztransfer->values1Number, 1, 1);
			arrayStruct->allocated[zSTRUCT_ARRAY_INT] = 1;
		}
		if ((ztransfer->values2) && (ztransfer->values2Number > 0)) {
			arrayStruct->numberFloatArray = ztransfer->values2Number;
			arrayStruct->floatArray = (float *)malloc(ztransfer->values2Number * FLOAT_SIZE);
			convertDataArray(ztransfer->values2, (void *)arrayStruct->floatArray, ztransfer->values2Number, 1, 1);
			arrayStruct->allocated[zSTRUCT_ARRAY_FLOAT] = 1;
		}
		if ((ztransfer->values3) && (ztransfer->values3Number > 0)) {
			arrayStruct->numberDoubleArray = ztransfer->values3Number / 2;
			arrayStruct->doubleArray = (double *)malloc(arrayStruct->numberDoubleArray * DOUBLE_SIZE);
			convertDataArray(ztransfer->values3, (void *)arrayStruct->doubleArray, arrayStruct->numberDoubleArray, 2, 2);
			arrayStruct->allocated[zSTRUCT_ARRAY_DOUBLE] = 1;
		}
	}
	else {
		if ((ztransfer->internalHeader && (ztransfer->internalHeaderNumber > 0)) &&
			 ((ztransfer->values1) && (ztransfer->values1Number > 0))) {
			if (ztransfer->internalHeader[0] == DATA_TYPE_INT_ARRAY) {
				arrayStruct->numberIntArray = ztransfer->values1Number;
				arrayStruct->intArray = (int *)malloc(arrayStruct->numberIntArray * INT_SIZE);
				convertIntArray(ztransfer->values1, arrayStruct->intArray, arrayStruct->numberIntArray, 1, 1);
				arrayStruct->allocated[zSTRUCT_ARRAY_INT] = 1;
			}
			else if (ztransfer->internalHeader[0] == DATA_TYPE_FLOAT_ARRAY) {
				arrayStruct->numberFloatArray = ztransfer->values1Number;
				arrayStruct->floatArray = (float *)malloc(arrayStruct->numberFloatArray * FLOAT_SIZE);
				convertDataArray(ztransfer->values1, (void *)arrayStruct->floatArray, arrayStruct->numberFloatArray, 1, 1);
				arrayStruct->allocated[zSTRUCT_ARRAY_FLOAT] = 1;
			}
			else if (ztransfer->internalHeader[0] == DATA_TYPE_DOUBLE_ARRAY) {
				arrayStruct->numberDoubleArray = ztransfer->values1Number / 2;
				arrayStruct->doubleArray = (double *)malloc(arrayStruct->numberDoubleArray * DOUBLE_SIZE);
				convertDataArray(ztransfer->values1, (void *)arrayStruct->doubleArray, arrayStruct->numberDoubleArray, 2, 2);
				arrayStruct->allocated[zSTRUCT_ARRAY_DOUBLE] = 1;
			}
			else {
				status = zerrorProcessing(ifltab, DSS_FUNCTION_zarrayRetrieve_ID,
					zdssErrorCodes.WRONG_RECORD_TYPE, DATA_TYPE_ARRAY, (long long)ztransfer->internalHeader[0],
					zdssErrorSeverity.WARNING, ztransfer->pathname, "Unrecognized array/data type provided in internal header");
				zstructFree(ztransfer);
				return status;
			}
		}
		else {
			status = zerrorProcessing(ifltab, DSS_FUNCTION_zarrayRetrieve_ID,
				zdssErrorCodes.WRONG_RECORD_TYPE, DATA_TYPE_ARRAY, (long long)ztransfer->dataType,
				zdssErrorSeverity.WARNING, ztransfer->pathname, "No array/data type provided in internal header");
			zstructFree(ztransfer);
			return status;
		}
	}


	if (ztransfer->userHeaderNumber > 0) {
		arrayStruct->userHeader = ztransfer->userHeader;
		arrayStruct->userHeaderNumber = ztransfer->userHeaderNumber;
		ztransfer->allocated[zSTRUCT_userHeader] = 0;
		arrayStruct->allocated[zSTRUCT_userHeader] = 1;
	}
	else {
		arrayStruct->userHeader = 0;
		arrayStruct->userHeaderNumber = 0;
	}

	arrayStruct->dataType = ztransfer->dataType;
	stringCopy(arrayStruct->programName,  sizeof(arrayStruct->programName), ztransfer->programName, strlen(ztransfer->programName));
	arrayStruct->lastWrittenTime = ztransfer->lastWrittenTime;
	arrayStruct->fileLastWrittenTime = ztransfer->fileLastWrittenTime;


	zstructFree(ztransfer);

	if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zarrayRetrieve_ID, "Exit Pathname: ", arrayStruct->pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zarrayRetrieve_ID, "  Status: ",  status);
	}

	return status;
}


