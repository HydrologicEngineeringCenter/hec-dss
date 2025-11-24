#include <string.h>

#include "heclib7.h"
#include <stdlib.h>
#include "hecdssInternal.h"


void zwritex(long long *ifltab, const char *path, int *npath,
			 int *internalHeader, int *internalHeaderNumber,
			 int *header2, int *header2Number,
			 int *userHeader, int *userHeaderNumber,
			 int *values, int *valuesNumber,
			 int *dataType, int *plan,
			 int *status, int *recordFound)

{
	int* data = 0;
	zStructTransfer* ztransfer;
	
	ztransfer = zstructTransferNew(path, 0);
	if (!ztransfer) {
		*recordFound = 0;
		zstructFree(ztransfer);
		return;
	}
	ztransfer->internalHeader = internalHeader;
	ztransfer->internalHeaderNumber = *internalHeaderNumber;
	ztransfer->header2 = header2;
	ztransfer->header2Number = *header2Number;
	ztransfer->userHeader = userHeader;
	ztransfer->userHeaderNumber = *userHeaderNumber;
	ztransfer->values1 = values;
	// make valuesNumber multiple numberOfLongs
	int num = numberLongsInInts(*valuesNumber);
	if (num / 2 != *valuesNumber)
	{
		int numInts = num * 2;
		data = calloc(4, numInts);
		for (int i = 0; i < *valuesNumber; i++)
		{
			data[i] = values[i];
		}
		ztransfer->values1 = data;
		ztransfer->values1Number = numInts;
	}
	else
	{
		ztransfer->values1 = values;
		ztransfer->values1Number = *valuesNumber;
	}
	ztransfer->dataType = *dataType;

	*status = zwrite(ifltab, ztransfer);
	if (data) {
		free(data);
	}

	if (*status == STATUS_RECORD_FOUND) {
		*recordFound = 1;
	}
	else {
		*recordFound = 0;
	}
	zstructFree(ztransfer);
	

}


