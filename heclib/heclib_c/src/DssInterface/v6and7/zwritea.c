#include "heclib7.h"
#include "hecdssFort.h"
#include "hecdssInternal.h"


//  C Callable - uses original arguments
//  For compatibility purposes.

//	SUBROUTINE zwrite ( IFLTAB, CPATH, NPATH, IUHEAD, NUHEAD,
//     * IDATA, NDATA, IPLAN, LFOUND)


void zwritea(long long *ifltab, const char *path, int *npath,
			int *userHeader, int *userHeaderNumber,
			int *values, int *valuesNumber,
			int *flag, int *recordFound)

{
	zStructTransfer* ztransfer;

	ztransfer = zstructTransferNew(path, 0);
	if (!ztransfer) {
		*recordFound = 0;
		zstructFree(ztransfer);
		return;
	}
	ztransfer->userHeader = userHeader;
	ztransfer->userHeaderNumber = *userHeaderNumber;
	ztransfer->values1 = values;
	ztransfer->values1Number = *valuesNumber;
	int status = zwrite(ifltab, ztransfer);
	if (status == STATUS_RECORD_FOUND) {
		*recordFound = 1;
	}
	else {
		*recordFound = 0;
	}
	zstructFree(ztransfer);

}

