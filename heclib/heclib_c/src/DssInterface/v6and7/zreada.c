#include "heclib7.h"
#include "hecdssFort.h"
#include "hecdssInternal.h"
#include "fortran_string_len_size.h"

//  zread Abbreviated (arguments).
//	C Callable - uses original arguments

//  For compatibility purposes.

//  Accesses both DSS-6 and DSS-7 files.
//  Uses version 6 argument list.




void zreada(long long *ifltab, const char *path, int *npath,
			int *userHeader, int *userHeaderNumber,
			int *values, int *valuesNumber,
			int *readFlag, int *recordFound)

{
	int status;
	zStructTransfer* ztransfer;
	char pathname[MAX_PATHNAME_LENGTH];


	if (zgetVersion(ifltab) == 6) {
		stringCToFort(pathname, (int)sizeof(pathname),  path);
		zread6_(ifltab, pathname, npath, userHeader, userHeaderNumber,
			values, valuesNumber, readFlag, recordFound, (size_t)*npath);
	}
	else {
		ztransfer = zstructTransferNew(path, 0);
		if (!ztransfer) {
			*recordFound = 0;
			zstructFree(ztransfer);
			return;
		}
		ztransfer->userHeader = userHeader;
		ztransfer->userHeaderMode = *userHeaderNumber;
		ztransfer->values1 = values;
		ztransfer->values1Mode = *valuesNumber;
		status = zread(ifltab, ztransfer);
		if (status == STATUS_RECORD_FOUND) {
			*recordFound = 1;
			if (*recordFound) {
				*userHeaderNumber = ztransfer->userHeaderNumber;
				*valuesNumber = ztransfer->values1Number;
			}
		}
		else {
			*recordFound = 0;
			*userHeaderNumber = 0;
			*valuesNumber = 0;
		}
		zstructFree(ztransfer);
	}

}


//  Fortran compatible
void zreada_(long long *ifltab, const char *path, int *npath,
			int *userHeader, int *userHeaderNumber,
			int *values, int *valuesNumber,
			int *readFlag, int *recordFound, slen_t pathLen)
{
	char pathname[MAX_PATHNAME_LENGTH];

	copyAndTrim(pathname, sizeof(pathname), path, pathLen);
	zreada(ifltab, pathname, npath,
			userHeader, userHeaderNumber,
			values, valuesNumber,
			readFlag, recordFound);
}

