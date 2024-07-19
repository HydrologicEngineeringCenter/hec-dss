#include <string.h>

#include "heclib7.h"
#include "hecdssFort.h"
#include "hecdssInternal.h"




int zwritec(long long *ifltab, const char* pathname,
			void *internalHeader, int internalHeaderNumber,
			void *header2, int header2Number,
			void *values3, int values3Number,
			void *userHeader, int userHeaderNumber,
			void *values1, int values1Number,
			void *values2, int values2Number,
			int numberValues, int logicalNumberValues,
			int totalAllocatedSize, int totalExpandedSize, int dataType)
{
	int status;
	int recordFound;
	int zero;
	int len;
	zStructTransfer* ztransfer;
	char path[MAX_PATHNAME_LENGTH];


	ztransfer = zstructTransferNew(pathname, 0);
	if (!ztransfer) {
		//  error out
		return -1;
	}

	ztransfer->internalHeader = internalHeader;
	ztransfer->internalHeaderNumber = internalHeaderNumber;
	ztransfer->header2 = header2;
	ztransfer->header2Number = header2Number;
	ztransfer->values3 = values3;
	ztransfer->values3Number = values3Number;
	ztransfer->userHeader = userHeader;
	ztransfer->userHeaderNumber = userHeaderNumber;

	ztransfer->values1 = values1;
	ztransfer->values1Number = values1Number;
	ztransfer->values2 = values2;
	ztransfer->values2Number = values2Number;

	ztransfer->numberValues = numberValues;
	ztransfer->logicalNumberValues = logicalNumberValues;
	ztransfer->totalAllocatedSize = totalAllocatedSize;
	ztransfer->totalExpandedSize = totalExpandedSize;
	ztransfer->dataType = dataType;

	status = zwrite(ifltab, ztransfer);

	zstructFree(ztransfer);


	return status;
}


//  Fortran callable
void zwritec_(long long *ifltab, const char* path,
			 int *internalHeader, int *internalHeaderNumber,
			 int *header2, int *header2Number,
			 int *values3, int *values3Number,
			 int *userHeader, int *userHeaderNumber,
			 int *values1, int *values1Number,
			 int *values2, int *values2Number,
			 int *numberValues, int *logicalNumberValues,
			 int *totalAllocatedSize, int *totalExpandedSize, int *dataType,
			 int *status, size_t pathLen)
{

	char *pathname;

	pathname = stringFortToC(path, pathLen);

	*status = zwritec(ifltab, pathname,
			 internalHeader, *internalHeaderNumber,
			 header2, *header2Number,
			 values3, *values3Number,
			 userHeader, *userHeaderNumber,
			 values1, *values1Number,
			 values2, *values2Number,
			 *numberValues, *logicalNumberValues,
			 *totalAllocatedSize, *totalExpandedSize,* dataType);
	free(pathname);
}
