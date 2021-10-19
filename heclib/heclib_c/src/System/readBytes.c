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
#include "hecdssInternal.h"

/**
*  Function:	readBytes
*
*  Use:			Private (Internal)
*
*  Description:	Read from disk
*
*  Declaration: int readBytes(int ihandle, char *carray, int numberBytes);
*
*  Parameters:	int ihandle
*					The c handle associated with this file (from open)
*
*				char *carray (output)
*					The array with the data to be read.
*
*				int numberBytes
*					The number of bytes to read.
*
*	Returns:
*				Zero (0) for successful operation.
*				-1:  Undefined error
*				-2:  Negative address
*				-3:  Unable to seek
*				-4:  Read error
*				<-10: Partial read
*					  Number bytes read = abs(return val) - 10;
*					  e.g., return -50 means 40 bytes read   (50-10)
*				>0:  System error, error code returned.
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/
int readBytes(int ihandle, char *carray, int numberBytes)
{

    int status;
	int ntrans;

#ifdef _MSC_VER
	errno_t err;
	ntrans = _read(ihandle, carray, (size_t)numberBytes);
#else
	ntrans = read(ihandle, carray, numberBytes);
#endif
	status  = ((ntrans == numberBytes) ? 0 : -4);

	if (status) {
		if (ntrans > 0) {
			status = -10 -ntrans;
		}
		else {
#ifdef _MSC_VER
			_get_errno( &err );
			if (err != 0) {
				status = errno;
			}
#endif
		}
	}

    return status;
}

