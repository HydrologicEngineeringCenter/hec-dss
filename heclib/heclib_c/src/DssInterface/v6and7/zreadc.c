
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"
#include "hecdssFort.h"
#include "fortran_string_len_size.h"


//  z read Complete  (complete argument lists)
//  Non-struct version.  zread is preferred.
//  C callable
//
//  All "numbers" are regular 4 byte ints
//
//  NOTE:  CANNOT READ VERSION 7 ARGS FROM A VERSION 6 FILE
//         i.e., DSS-6 does not store header2, so it cannot be read
//


int zreadc (long long *ifltab, const char* pathname,
			 int *internalHeader, int internalHeaderArraySize , int *internalHeaderNumber,
			 int *header2, int header2ArraySize, int *header2Number,
			 int *values3, int values3ArraySize, int *values3Number,
			 int *userHeader, int userHeaderArraySize, int *userHeaderNumber,
			 int *values1, int values1Size, int *values1Number,
			 int *values2, int values2Size, int *values2Number,
			 int *numberValues, int *logicalNumberValues,
			 int *totalAllocatedSize, int *totalExpandedSize, int *dataType)
{

	int status;
	int recordFound;
	int zero;
	char messageString[70];
	char path[MAX_PATHNAME_LENGTH];
	zStructTransfer* ztransfer;

	int *buffer=0; long long bufferControl[4] ={0,0,0,0};


	if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zread_ID, "Enter zreadc;  Handle: ",  zhandle(ifltab));
		zmessageDebug(ifltab, DSS_FUNCTION_zread_ID, " Pathname: ",pathname);
	}


	if (zgetVersion(ifltab) == 6) {
		*values3Number = 0;
		*values2Number = 0;
		*numberValues = 0;
		*logicalNumberValues = 0;
		*totalAllocatedSize = 0;
		*totalExpandedSize = 0;
		stringCToFort(path, sizeof(path),  pathname);
		zero = 0;
		zreadx6_(ifltab, path,
				 internalHeader, &internalHeaderArraySize, internalHeaderNumber,
				 header2, &header2ArraySize, header2Number,
				 userHeader, &userHeaderArraySize, userHeaderNumber,
				 values1, &values1Size, values1Number,
				 &zero, &recordFound, strlen(pathname));
		if (recordFound) {
			//  Get the data type
			zinqir6_(ifltab, "TYPE", messageString, dataType, (size_t)4, (size_t)20);
			return STATUS_RECORD_FOUND;
		}
		else {
			//  Check for error
			return STATUS_RECORD_NOT_FOUND;
		}
	}
	else {
		ztransfer = zstructTransferNew(pathname, 0);
		if (!ztransfer) {
			//  error out
			return -1;
		}

		ztransfer->internalHeader = internalHeader;
		ztransfer->internalHeaderMode = internalHeaderArraySize;

		ztransfer->header2 = header2;
		ztransfer->header2Mode = header2ArraySize;

		ztransfer->values3 = values3;
		ztransfer->values3Mode = values3ArraySize;

		ztransfer->userHeader = userHeader;
		ztransfer->userHeaderMode = userHeaderArraySize;

		ztransfer->values1 = values1;
		ztransfer->values1Mode = values1Size;

		ztransfer->values2 = values2;
		ztransfer->values2Mode = values2Size;

		status = zreadInternal(ifltab, ztransfer,
								bufferControl, buffer, 0);

		if (status == STATUS_RECORD_FOUND) {
			*internalHeaderNumber = ztransfer->internalHeaderNumber;
			*header2Number = ztransfer->header2Number;
			*values3Number = ztransfer->values3Number;
			*userHeaderNumber = ztransfer->userHeaderNumber;
			*values1Number = ztransfer->values1Number;
			*values2Number = ztransfer->values2Number;
			*numberValues = ztransfer->numberValues;
			*logicalNumberValues = ztransfer->logicalNumberValues;
			*totalAllocatedSize = ztransfer->totalAllocatedSize;
			*totalExpandedSize = ztransfer->totalExpandedSize;
			*dataType = ztransfer->dataType;
		}
		zstructFree(ztransfer);
	}

	return status;
}


//  Fortran callable
void zreadc_(long long *ifltab, const char* pathname,
			 int *internalHeader, int *internalHeaderArraySize , int *internalHeaderNumber,
			 int *header2, int *header2ArraySize, int *header2Number,
			 int *values3, int *values3ArraySize, int *values3Number,
			 int *userHeader, int *userHeaderArraySize, int *userHeaderNumber,
			 int *values1, int *values1Size, int *values1Number,
			 int *values2, int *values2Size, int *values2Number,
			 int *numberValues, int *logicalNumberValues,
			 int *totalAllocatedSize, int *totalExpandedSize, int *dataType,
			 int *status, slen_t pathnameLen)
{

	char path[MAX_PATHNAME_LENGTH];

	copyAndTrim(path, sizeof(path), pathname, pathnameLen);

	*status = zreadc(ifltab, pathname,
			 internalHeader, *internalHeaderArraySize , internalHeaderNumber,
			 header2, *header2ArraySize, header2Number,
			 values3, *values3ArraySize, values3Number,
			 userHeader, *userHeaderArraySize, userHeaderNumber,
			 values1, *values1Size, values1Number,
			 values2, *values2Size, values2Number,
			 numberValues, logicalNumberValues,
			 totalAllocatedSize, totalExpandedSize, dataType);
}

