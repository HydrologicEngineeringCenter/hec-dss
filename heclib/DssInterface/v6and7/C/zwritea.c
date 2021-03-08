#include "heclib7.h"
#include "hecdssFort.h"
#include "hecdssInternal.h"

//  C Callable - uses original arguments
//  For compatibility purposes.
//  Accesses both DSS-6 and DSS-7 files.
//  Uses version 6 argument list.


//	SUBROUTINE zwrite ( IFLTAB, CPATH, NPATH, IUHEAD, NUHEAD,
//     * IDATA, NDATA, IPLAN, LFOUND)


void zwritea(long long *ifltab, const char *path, int *npath,
			int *userHeader, int *userHeaderNumber,
			int *values, int *valuesNumber,
			int *flag, int *recordFound)

{
	int status;
	zStructTransfer* ztransfer;
	char pathname[MAX_PATHNAME_LENGTH];


	if (zgetVersion(ifltab) == 6) {
		stringCToFort(pathname, (int)sizeof(pathname),  path);
		zwrite6_(ifltab, pathname, npath, userHeader, userHeaderNumber,
				 values, valuesNumber, flag, recordFound, (size_t)*npath);
	}
	else {
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
		status = zwrite(ifltab, ztransfer);
		if (status == STATUS_RECORD_FOUND) {
			*recordFound = 1;
		}
		else {
			*recordFound = 0;
		}
		zstructFree(ztransfer);
	}

}

//  Fortran compatible
void zwritea_(long long *ifltab, const char *path, int *npath,
			int *userHeader, int *userHeaderNumber,
			int *values, int *valuesNumber,
			int *flag, int *recordFound, size_t pathLen)
{
	char *pathname;

	pathname = stringFortToC(path, pathLen);
	zwritea(ifltab, pathname, npath,
			userHeader, userHeaderNumber,
			values, valuesNumber,
			flag, recordFound);
	free(pathname);
}

