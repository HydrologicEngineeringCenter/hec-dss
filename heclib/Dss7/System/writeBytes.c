#ifdef _MSC_VER
#include <io.h>
#include <errno.h>
#else
#include <unistd.h>
#include <stdint.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "hecdssInternal.h"


/**
*  Function:	writeBytes
*
*  Use:			Private (Internal)
*
*  Description:	Write bytes disk; generic for different OSs
*
*  Declaration: int writeBytes (int ihandle, const char *carray, int numberBytes)
*
*  Parameters:	int ihandle
*					The c handle associated with this file (from open)
*
*				const char *carray
*					The array with the data to be written.
*
*				int numberBytes
*					The number of bytes to write.
*
*
*	Returns:
*				Zero (0) for successful operation.
*				-1:  Undefined error
*				-2:  Negative address
*				-3:  Unable to seek
*				>0:  System error, error code returned.
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int writeBytes(int ihandle, const char *carray, int numberBytes)
{

    int status;
	int ntrans;

#ifdef _MSC_VER
	errno_t err;

	ntrans = _write(ihandle, carray, (size_t)numberBytes);
#else
	ntrans = write(ihandle, carray, numberBytes);
#endif
	status  = ((ntrans == numberBytes) ? 0 : -1);

#ifdef _MSC_VER
	if (status != 0) {
		_get_errno( &err );
		if (err != 0) {
			status = errno;
		}
	}
#endif

    return status;
}

