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

	if (zgetVersion(ifltab) == 6) {
		len = (int)strlen(ztransfer->pathname);
		stringCToFort(pathname, sizeof(pathname), ztransfer->pathname);
		//  Initialize array sizes... they will be either
		//  0 - don't read
		//  1 - allocate space here, and pass in size obtained from file
		// >1 - array already allocated, use up to this size
		internalHeaderArraySize = ztransfer->internalHeaderMode;
		header2ArraySize = ztransfer->header2Mode;
		userHeaderArraySize = ztransfer->userHeaderMode;
		valuesSize = ztransfer->values1Mode;

		//  Is the user passing in space, or do we need to read sizes to allocate space?
		if ((ztransfer->internalHeaderMode == 1) ||
			(ztransfer->header2Mode == 1) ||
			(ztransfer->userHeaderMode == 1) ||
			(ztransfer->values1Mode == 1)) {

			zgetrecsize6_(ifltab, pathname, &nihead, &nchead, &nuhead, &ndata, &istatus, (size_t)len);
			if (istatus != 0) {
				return STATUS_RECORD_NOT_FOUND;
			}

			if (ztransfer->internalHeaderMode == 1) {
				internalHeaderArraySize = nihead;
				if (internalHeaderArraySize > 0) {
					ztransfer->internalHeader = (int *)malloc((size_t)internalHeaderArraySize * 4);
					if (!ztransfer->internalHeader) {
						return zerrorProcessing(ifltab, DSS_FUNCTION_zreadInternal_ID,
												zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, internalHeaderArraySize, 0,
												zdssErrorSeverity.MEMORY_ERROR, ztransfer->pathname, "Allocating internalHeader");
					}
					ztransfer->allocated[zSTRUCT_TRANS_internalHeader] = 1;
				}
			}

			if (ztransfer->header2Mode == 1) {
				header2ArraySize = nchead;
				if (header2ArraySize > 0) {
					ztransfer->header2 = (int *)malloc((size_t)header2ArraySize * 4);
					if (!ztransfer->header2) {
						return zerrorProcessing(ifltab, DSS_FUNCTION_zreadInternal_ID,
												zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, header2ArraySize, 0,
												zdssErrorSeverity.MEMORY_ERROR, ztransfer->pathname, "Allocating header2");
					}
					ztransfer->allocated[zSTRUCT_TRANS_header2] = 1;
				}
			}

			if (ztransfer->userHeaderMode == 1) {
				userHeaderArraySize = nuhead;
				if (userHeaderArraySize > 0) {
					ztransfer->userHeader = (int *)malloc((size_t)userHeaderArraySize * 4);
					if (!ztransfer->userHeader) {
						return zerrorProcessing(ifltab, DSS_FUNCTION_zreadInternal_ID,
												zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, userHeaderArraySize, 0,
												zdssErrorSeverity.MEMORY_ERROR, ztransfer->pathname, "Allocating header2");
					}
					ztransfer->allocated[zSTRUCT_userHeader] = 1;
				}
			}

			if (ztransfer->values1Mode == 1) {
				valuesSize = ndata;
				if (valuesSize > 0) {
					ztransfer->values1 = (int *)malloc((size_t)valuesSize * 4);
					if (!ztransfer->values1) {
						return zerrorProcessing(ifltab, DSS_FUNCTION_zreadInternal_ID,
											zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, valuesSize, 0,
											zdssErrorSeverity.MEMORY_ERROR, ztransfer->pathname, "Allocating values");
					}
					ztransfer->allocated[zSTRUCT_TRANS_values1] = 1;
				}
			}
		}

		ztransfer->values3Number = 0;
		ztransfer->values2Number = 0;
		ztransfer->numberValues = 0;
		ztransfer->logicalNumberValues = 0;
		ztransfer->totalAllocatedSize = 0;
		ztransfer->totalExpandedSize = 0;
		zero = 0;
		zreadx6_(ifltab, pathname,
				 ztransfer->internalHeader, &internalHeaderArraySize , &ztransfer->internalHeaderNumber,
				 ztransfer->header2, &header2ArraySize, &ztransfer->header2Number,
				 ztransfer->userHeader, &userHeaderArraySize, &ztransfer->userHeaderNumber,
				 ztransfer->values1, &valuesSize, &ztransfer->values1Number,
				 &zero, &recordFound, (size_t)len);

		if (recordFound) {
			//  Get the data type
			zinqir6_(ifltab, "TYPE", pathname, &ztransfer->dataType, (size_t)4,(size_t) 20);
			return STATUS_RECORD_FOUND;
		}
		else {
			//  Check for error
			return STATUS_RECORD_NOT_FOUND;
		}

	}
	else {
		//  This one is a little simpler...
		return zreadInternal(ifltab, ztransfer, bufferControl, buffer, 0);
	}

}

