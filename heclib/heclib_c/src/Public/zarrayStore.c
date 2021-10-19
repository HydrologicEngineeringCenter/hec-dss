#include <string.h>
#include <stdlib.h>

#include "heclib.h"


/**
*  Function:	zarrayStore
*
*  Use:			Public
*
*  Description:	Store a generic array
*
*  Declaration: int zarrayStore(long long *ifltab, zStructArray *arrayStruct);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				zStructArray *arrayStruct
*					An array struct that contains either an int array, or
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
*	Comments:	Works with both DSS Version 6 and 7
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*
*/

int zarrayStore(long long *ifltab, zStructArray *arrayStruct)
{
	int status;
	zStructTransfer* ztransfer;


	if (!arrayStruct) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zarrayStore_ID, zdssErrorCodes.NULL_ARGUMENT,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "arrayStruct is null");
	}
	if (!arrayStruct->pathname) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zarrayStore_ID, zdssErrorCodes.NULL_PATHNAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "arrayStruct pathname is null");
	}

	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zarrayStore_ID, "Enter Pathname: ", arrayStruct->pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zarrayStore_ID, "  Handle: ",  zhandle(ifltab));
	}

	ztransfer = zstructTransferNew(arrayStruct->pathname, 0);
	if (!ztransfer) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zarrayStore_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
								zdssErrorSeverity.MEMORY_ERROR,
								arrayStruct->pathname, "Allocating ztransfer struct");
	}

	
	ztransfer->numberValues = 0;
	ztransfer->logicalNumberValues = 0;
	ztransfer->dataType = DATA_TYPE_ARRAY;
	if (zgetVersion(ifltab) == 7) {
		if ((arrayStruct->intArray != 0) && (arrayStruct->numberIntArray > 0)) {
			ztransfer->values1Number = arrayStruct->numberIntArray;
			ztransfer->values1 = (int *)arrayStruct->intArray;
			ztransfer->numberValues += arrayStruct->numberIntArray;
			ztransfer->logicalNumberValues += arrayStruct->numberIntArray;
		}
		if ((arrayStruct->floatArray != 0) && (arrayStruct->numberFloatArray > 0)) {
			ztransfer->values2Number = arrayStruct->numberFloatArray;
			ztransfer->values2 = (int *)arrayStruct->floatArray;
			ztransfer->numberValues += arrayStruct->numberFloatArray;
			ztransfer->logicalNumberValues += arrayStruct->numberFloatArray;
		}
		if ((arrayStruct->doubleArray != 0) && (arrayStruct->numberDoubleArray > 0)) {
			ztransfer->values3Number = arrayStruct->numberDoubleArray * 2;
			ztransfer->values3 = (int *)arrayStruct->doubleArray;
			ztransfer->numberValues += arrayStruct->numberDoubleArray;
			ztransfer->logicalNumberValues += arrayStruct->numberDoubleArray;
		}
	}
	else {
		ztransfer->internalHeader = (int *)calloc(1, 4);
		ztransfer->internalHeaderNumber = 1;
		ztransfer->allocated[zSTRUCT_TRANS_internalHeader] = 1;
		if ((arrayStruct->intArray != 0) && (arrayStruct->numberIntArray > 0)) {
			ztransfer->internalHeader[0] = DATA_TYPE_INT_ARRAY;
			ztransfer->values1Number = arrayStruct->numberIntArray;
			ztransfer->values1 = (int *)arrayStruct->intArray;
			ztransfer->numberValues += arrayStruct->numberIntArray;
			ztransfer->logicalNumberValues += arrayStruct->numberIntArray;
		}
		else if ((arrayStruct->floatArray != 0) && (arrayStruct->numberFloatArray > 0)) {
			ztransfer->internalHeader[0] = DATA_TYPE_FLOAT_ARRAY;
			ztransfer->values1Number = arrayStruct->numberFloatArray;
			ztransfer->values1 = (int *)arrayStruct->floatArray;
			ztransfer->numberValues += arrayStruct->numberFloatArray;
			ztransfer->logicalNumberValues += arrayStruct->numberFloatArray;
		}
		else if ((arrayStruct->doubleArray != 0) && (arrayStruct->numberDoubleArray > 0)) {
			ztransfer->internalHeader[0] = DATA_TYPE_DOUBLE_ARRAY;
			ztransfer->values1Number = arrayStruct->numberDoubleArray * 2;
			ztransfer->values1 = (int *)arrayStruct->doubleArray;
			ztransfer->numberValues += arrayStruct->numberDoubleArray;
			ztransfer->logicalNumberValues += arrayStruct->numberDoubleArray;
		}
	}

	ztransfer->userHeader = arrayStruct->userHeader;
	ztransfer->userHeaderNumber = arrayStruct->userHeaderNumber;

	status = zwrite(ifltab, ztransfer);

	zstructFree(ztransfer);

	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zarrayStore_ID, "Exit Pathname: ", arrayStruct->pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zarrayStore_ID, "  Status: ",  status);
	}

	if (zisError(status)) {
		//  An error code
		status = zerrorUpdate(ifltab, status, DSS_FUNCTION_zarrayStore_ID);
	}

	return status;
}



