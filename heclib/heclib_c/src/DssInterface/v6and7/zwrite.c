#include <string.h>

#include "heclib7.h"
#include "hecdssFort.h"
#include "hecdssInternal.h"
#include "verticalDatum.h" // for userHeaderToString and stringToUserHeader


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

	// ensure the user header doesn't get truncated if used for a string
	int *oldHeader = ztransfer->userHeader;
	int *newHeader = ztransfer->userHeader;
	int oldHeaderNumber = ztransfer->userHeaderNumber;
	int newHeaderNumber = ztransfer->userHeaderNumber;
	if (ztransfer->userHeaderMode < 2 && ztransfer->userHeaderNumber % 2) {
		char *headerString = userHeaderToString(ztransfer->userHeader, ztransfer->userHeaderNumber);
		if (headerString && strlen(headerString)  && (strlen(headerString)-1) / 4 + 1 == ztransfer->userHeaderNumber) {
			char *cp = (char *)realloc(headerString, strlen(headerString)+5);
			if (cp) {
				headerString = cp;
				strcat(headerString, "    ");
				newHeader = stringToUserHeader(headerString, &newHeaderNumber);
				ztransfer->userHeader = newHeader;
				ztransfer->userHeaderNumber = newHeaderNumber;
			}
		}
		free(headerString);
	}
	// do the writing

	if (zgetVersion(ifltab) == 7) {
		status = zwriteInternal(ifltab, ztransfer, 0, bufferControl, buffer, 0);
	}
	else {
		status = zerrorProcessing(ifltab, DSS_FUNCTION_zwriteInternal_ID, zdssErrorCodes.NOT_OPENED,
			0, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS, ztransfer->pathname, "");
	}
	// restore user header if we lengthed it for writing
	if (newHeader != oldHeader) {
		ztransfer->userHeader = oldHeader;
		ztransfer->userHeaderNumber = oldHeaderNumber;
		free(newHeader);
	}

	return status;

}

