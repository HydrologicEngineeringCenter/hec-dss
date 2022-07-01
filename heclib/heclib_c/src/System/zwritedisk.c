#ifdef _MSC_VER
#include <io.h>
#include <errno.h>
#else
#include <unistd.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "hecdssInternal.h"


/**
*  Function:	zwriteDisk
*
*  Use:			Private (Internal)
*
*  Description:	Write int data to disk; generally a physical write, although OS's buffer themselves.
*
*  Declaration: int zwriteDisk (int ihandle, int iswap, long long address, void *iarray, int numberInts)
*
*  Parameters:	int ihandle
*					The c handle associated with this file (from open)
*
*				int iswap
*					A flag indicating this is a file on a big endian system and the bytes need to be swapped
*					to match those on a little endian system
*
*				long long iaddress
*					The int*8 address to write to the DSS file.  Note, this is not a byte address,
*					but the word address in int*8 (double) words.  Thus, if the address were "2",
*					this would be 16 bytes into the file.  All addresses in DSS version 7 are in int*8 words.
*
*				int *iarray
*					The array with the data to be written.
*
*				int numberInts
*					The number of integers (int*4) to write.  Although addressing is in int*8,
*					write lengths are integer*4.
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
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zwriteDisk (int ihandle, int iswap, long long address, void *iarray, int numberInts)
{

    int status;
	int ntrans;
	int nbytes;
	long long jpos;
#ifdef _MSC_VER
	errno_t err;
#endif


	if (address < 0)
	return -2;

	address *= (long long)8;
	nbytes = numberInts * 4;

#ifdef _MSC_VER
	jpos = _lseeki64(ihandle, address, 0);
	status = ((jpos == -1) ? -3 : 0);
	if (status != 0) {
		_get_errno(&err);
		if (err != 0) {
			status = errno;
		}
		return status;
	}
#else
	jpos = lseek(ihandle, address, 0);
	status = ((jpos == -1) ? -3 : 0);
#endif



    // Swap bytes for big endian computers
	//  For those computers that do swapping,
	//  iarray is long long and number ints is even
	//  If not even, round up (highly unlikly)
	//  The int after our array may not be right
   if (iswap) {
		zswap((void *)iarray, numberInts);
	}
	if (status == 0) {
#ifdef _MSC_VER
		ntrans = _write(ihandle, iarray, (size_t)nbytes);
#else
		ntrans = write(ihandle, iarray, nbytes);
#endif
		status  = ((ntrans == nbytes) ? 0 : -1);
	}

#ifdef _MSC_VER
	if (status != 0) {
		_get_errno( &err );
		if (err != 0) {
			status = errno;
		}
	}
#endif

	//  Swap back
	if (iswap) {
		zswap((void *)iarray, numberInts);
	}

    return status;
}

