#include <string.h>
#include <stdio.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "hecdssFort.h"
#include "hecdssInternal.h"
#include "fortran_string_len_size.h"

//  C Callable
//  Accesses both DSS-6 and DSS-7 files.
//  Uses version 6 argument list.

//  For compatibility purposes.

//  Use of zread is preferred.


void zreadx(long long *ifltab, const char *pathname,
			 int *internalHeader, int *internalHeaderArraySize , int *internalHeaderNumber,
			 int *header2, int *header2ArraySize, int *header2Number,
			 int *userHeader, int *userHeaderArraySize, int *userHeaderNumber,
			 int *values, int *valuesSize, int *valuesNumber,
			 int *readPlan, int *recordFound)

{
	int status;
	zStructTransfer* ztransfer;
	char path[MAX_PATHNAME_LENGTH];



	if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zread_ID, "Enter zreadx;  Handle: ",  zhandle(ifltab));
		zmessageDebug(ifltab, DSS_FUNCTION_zread_ID, " Pathname: ",pathname);
	}


	if (zgetVersion(ifltab) == 6) {
		stringCToFort(path, sizeof(path),  pathname);
		zreadx6_(ifltab, pathname,
				 internalHeader, internalHeaderArraySize, internalHeaderNumber,
				 header2, header2ArraySize, header2Number,
				 userHeader, userHeaderArraySize, userHeaderNumber,
				 values, valuesSize, valuesNumber,
				 readPlan, recordFound, strlen(pathname));
	}
	else {
		ztransfer = zstructTransferNew(pathname, 0);
		if (!ztransfer) {
			*recordFound = 0;
			zstructFree(ztransfer);
			return;
		}
		ztransfer->internalHeader = internalHeader;
		ztransfer->internalHeaderMode = *internalHeaderArraySize;

		ztransfer->header2 = header2;
		ztransfer->header2Mode = *header2ArraySize;

		ztransfer->userHeader = userHeader;
		ztransfer->userHeaderMode = *userHeaderArraySize;

		ztransfer->values1 = values;
		ztransfer->values1Mode = *valuesSize;

		status = zread(ifltab, ztransfer);

		if (status == STATUS_RECORD_FOUND) {
			*recordFound = 1;
			*internalHeaderNumber = ztransfer->internalHeaderNumber;
			*header2Number = ztransfer->header2Number;
			*userHeaderNumber = ztransfer->userHeaderNumber;
			*valuesNumber = ztransfer->values1Number;
			//  For compatibility with DSS-6 (only),
			//  zreadx reported the number stored, not number returned
			*internalHeaderNumber =	(int)ztransfer->info[zdssInfoKeys.kinfoInternalHeadNumber];
			*header2Number =		(int)ztransfer->info[zdssInfoKeys.kinfoHeader2Number];
			*userHeaderNumber =		(int)ztransfer->info[zdssInfoKeys.kinfoUserHeadNumber];
			*valuesNumber =			(int)ztransfer->info[zdssInfoKeys.kinfoValues1Number];
		}
		else {
			*recordFound = 0;
			*userHeaderNumber = 0;
			*header2Number = 0;
			*valuesNumber = 0;
		}
		zstructFree(ztransfer);
	}

}


//  Fortran compatible
void zreadx_(long long *ifltab, const char *pathname,
			 int *internalHeader, int *internalHeaderArraySize , int *internalHeaderNumber,
			 int *header2, int *header2ArraySize, int *header2Number,
			 int *userHeader, int *userHeaderArraySize, int *userHeaderNumber,
			 int *values, int *valuesSize, int *valuesNumber,
			 int *readPlan, int *recordFound, slen_t pathLen)
{
	char path[MAX_PATHNAME_LENGTH];

	copyAndTrim(path, sizeof(path), pathname, pathLen);

	zreadx(ifltab, pathname,
			 internalHeader, internalHeaderArraySize , internalHeaderNumber,
			 header2, header2ArraySize, header2Number,
			 userHeader, userHeaderArraySize, userHeaderNumber,
			 values, valuesSize, valuesNumber,
			 readPlan, recordFound);
}

