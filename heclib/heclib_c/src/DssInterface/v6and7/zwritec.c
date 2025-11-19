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
	zStructTransfer* ztransfer;

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

