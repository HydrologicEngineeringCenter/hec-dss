#include <string.h>
#include <stdlib.h>

#include "heclib7.h"
#include "hecdssFort.h"
#include "hecdssInternal.h"
#include "zdssMessages.h"
#include "zerrorCodes.h"

//  C Callable - Accesses both DSS-6 and DSS-7 files

//  This is the preferred call, as it will use the complete struct and read
//  from either DSS-7 or DSS-6 files

//  All data is passed through zStructTransfer.
//  Use zstructTransferNew() before calling zread
//  and then zstructFree() after read and copy data


int zread(long long *ifltab, zStructTransfer* ztransfer)

{
	int zero;
	int recordFound;
	int nihead, nchead, nuhead, ndata, istatus;
	int internalHeaderArraySize;
	int header2ArraySize;
	int userHeaderArraySize;
	int valuesSize;
	int len;
	int *buffer=0; long long bufferControl[4] ={0,0,0,0};
	char pathname[MAX_PATHNAME_LENGTH];


	if (!ztransfer) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zread_ID, zdssErrorCodes.NULL_ARGUMENT,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "ztransfer is null");
	}
	if (!ztransfer->pathname) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zread_ID, zdssErrorCodes.NULL_PATHNAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "ztransfer pathname is null");
	}
	return zreadInternal(ifltab, ztransfer, bufferControl, buffer, 0);
}
