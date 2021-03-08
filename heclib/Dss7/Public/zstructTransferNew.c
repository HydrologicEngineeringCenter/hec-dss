#include <stdlib.h>
#include <string.h>

#include "hecdss7.h"

/**
*  Function:	zstructTransferNew
*
*  Use:			Public
*
*  Description:	Creates a new transfer struct, the struct that is used for all basic reads and writes
*
*  Declaration: zStructTransfer* zstructTransferNew(const char* pathname, int mode);
*
*  Parameters:	const char* pathname
*					The pathname for this struct.  Must be a valid pathname.
*					A copy of the pathname is used in struct.
*
*				int mode
*					A flag/value assigned to all array modes.  Use only 0 or 1 for this call.
*					Generally, set mode to 0 for storing, mode to 1 for reading.
*					If mode == 0, then no space will be allocated for the array (when needed)
*					If mode == 1, then whatever space is needed will be allocated (when needed)
*					If mode > 1, then this is the space allocated for the array.  Will not access > mode.
*
*
*	Returns:	zstructTransferNew*
*					An address to the transfer struct created.
*
*   Note:		ALWAYS call function zstructFree(struct) when finished using.
*
*	Remarks:	This is the basic struct used for all low level reads and writes.
*
*	See:		zread and zwrite for use.
*
*	See Also:	zstructFree()
*				Various struct functions.
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


zStructTransfer* zstructTransferNew(const char* pathname, int mode)
{
	zStructTransfer *ztransfer;
	int i;
	int len;

	len = sizeof(zStructTransfer);
	ztransfer = (zStructTransfer*)calloc((size_t)len, BYTE_SIZE);
	if (!ztransfer) {
		return ztransfer;
	}

	for (i=0; i<zSTRUCT_length; i++) {
		ztransfer->allocated[i] = 0;
	}

	if (pathname) {
		ztransfer->pathname = mallocAndCopyPath(pathname);
		if (!ztransfer->pathname) {
			return (void *)0;
		}
		ztransfer->allocated[zSTRUCT_pathname] = 1;
		ztransfer->pathnameLength = (int)strlen(ztransfer->pathname);
	}

	ztransfer->internalHeaderMode = mode;
	ztransfer->header2Mode = mode;
	ztransfer->userHeaderMode = mode;
	ztransfer->values1Mode = mode;
	ztransfer->values2Mode = mode;
	ztransfer->values3Mode = mode;

	//  Transfer struct
	ztransfer->structType = STRUCT_TYPE_TRANSFER;

	return ztransfer;
}

