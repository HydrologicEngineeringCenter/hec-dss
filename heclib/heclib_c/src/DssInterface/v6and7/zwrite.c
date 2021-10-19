#include <string.h>

#include "heclib7.h"
#include "hecdssFort.h"
#include "hecdssInternal.h"


//  C Callable - Accesses both DSS-6 and DSS-7 files

//  This is the preferred call, as it will use the complete struct and
//  write to either DSS-7 or DSS-6 files

int zwrite(long long *ifltab, zStructTransfer* ztransfer)

{
	int zero;
	int recordFound;
	int status;
	int len;
	char pathname[MAX_PATHNAME_LENGTH];

	int *buffer=0; long long bufferControl[4] ={0,0,0,0};

	if (!ztransfer) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zwrite_ID, zdssErrorCodes.NULL_ARGUMENT,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "ztransfer is null");
	}
	if (!ztransfer->pathname) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zwrite_ID, zdssErrorCodes.NULL_PATHNAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "ztransfer pathname is null");
	}


	if (zgetVersion(ifltab) == 6) {

		stringCToFort(pathname, sizeof(pathname),  ztransfer->pathname);

		zero = 0;
		len = (int)strlen(ztransfer->pathname);
		zwritex6_(ifltab, pathname, &len,
				 ztransfer->internalHeader, &ztransfer->internalHeaderNumber,
				 ztransfer->header2, &ztransfer->header2Number,
				 ztransfer->userHeader, &ztransfer->userHeaderNumber,
				 ztransfer->values1, &ztransfer->values1Number,
				 &ztransfer->dataType,
				 &zero, &status, &recordFound, strlen(ztransfer->pathname));

		return status;

	}
	else if (zgetVersion(ifltab) == 7) {
		return zwriteInternal(ifltab, ztransfer, 0, bufferControl, buffer, 0);
	}
	else {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zwriteInternal_ID, zdssErrorCodes.NOT_OPENED,
			0, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS, ztransfer->pathname, "");
	}

}

