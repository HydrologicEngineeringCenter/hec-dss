
#include <stdio.h>
#include <stdlib.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "zerrorCodes.h"
#include "hecdssInternal.h"


/**
*  Function:	zput
*
*  Use:			Private (Internal)
*
*  Description:	Write function to write to the DSS file.  All writes come through this function.
*				The address and number are verified, then the machine dependent function zwriteDisk
*				is called to perform the physical write.
*
*  Declaration: int zput(long long *ifltab, long long iaddress, int *iarray, int numberWords, int wordSize)
*
*
*  Parameters:	long long *ifltab
*					The integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				long long iaddress
*					The int*8 address to start writing to the DSS file.  Note, this is not a byte address,
*					but the word address in int*8 (double) words.  Thus, if the address were "2",
*					this would be 16 bytes into the file.  All addresses in DSS version 7 are in int*8 words.
*
*				int *iarray
*					The array containing the data to write.
*
*				int numberWords
*					The number of words to write (where the word size is given in the next argument).
*					If this is negative, then abs(numbInts) zeros will be written.
*
*				int wordSize
*					The word size in int*4 words.  Must be either 1 or 2.
*					1 - writing int*4 words
*					2 - writing int*8 words
*
*	Returns:	int status
*					STATUS_OKAY for successful operation.
*					errorCode for error.   See error processing for codes.
*
*	See Also:	zget, zput8 or zputBuff
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zput(long long *ifltab, long long iaddress, int *iarray, int numberWords, int wordSize)
{
	int status;
	int ihandle;
	int iswap;
	int numberInts;
//	int ary[2];
//	long long add;
	long long *space;
	long long *fileHeader;


	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];

	//  Get number of int*4 words
	numberInts = numberWords * wordSize;

	//  We always have to have the file locked when writing,
	//  except when creating the file
	if (!ifltab[zdssKeys.klocked]) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zput_ID, zdssErrorCodes.CANNOT_LOCK_FILE,
								0, iaddress, zdssErrorSeverity.WARNING_NO_WRITE_ACCESS,
								"", "Enter zput, file is not locked (required before write)");
	}

	//  Make sure we have a valid address
	if ((iaddress < 0) || (iaddress > (fileHeader[zdssFileKeys.kfileSize] + 10000))) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zput_ID, zdssErrorCodes.INVALID_ADDRESS,
										 0, iaddress, zdssErrorSeverity.CORRUPT_FILE, "", "");
	}
	//  Make sure the number of bytes to write is a reasonable number (negative value is a flag and okay)
	if (numberInts > 100000000) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zput_ID, zdssErrorCodes.INVALID_NUMBER_TO_WRITE,
										 numberInts, iaddress, zdssErrorSeverity.CORRUPT_FILE, "", "");
	}

	if (numberInts == 0) {
		//  No op
		return STATUS_OKAY;
	}

	ihandle = (int)ifltab[zdssKeys.khandle];
	iswap   = (int)ifltab[zdssKeys.kswap];
	ifltab[zdssKeys.kaddLast] = iaddress;
	if (numberInts > 0) {
		status = zwriteDisk (ihandle, iswap, iaddress, iarray, numberInts);
	}
	else {
/////////////////////////////////////////
///////////////   Fix me - for zeros, block
		//  If numberInts is negative, then we need to write abs(numberInts) zeros (0).
		numberInts = -numberInts;
		space = (long long*)calloc((size_t)numberInts, WORD_SIZE);
		if (!space) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_zput_ID,
									zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, numberInts, 0,
									zdssErrorSeverity.MEMORY_ERROR, "", "array to hold zeros");
		}
		status = zwriteDisk (ihandle, iswap, iaddress, space, numberInts);
		free(space);
	}

	if (status != 0) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zwriteDisk_ID, zdssErrorCodes.WRITE_ERROR,
								status, iaddress, zdssErrorSeverity.WRITE_ERROR, "", "");
	}

	if (zmessageLevel(ifltab, MESS_METHOD_PUT_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
		zmessageZput(ifltab, iaddress, numberInts, wordSize, BUFF_NO_ACTION, status, DSS_FUNCTION_zwriteDisk_ID, "");
	}

	//  Was this a odd number of ints and we need to write
	//  a zero in the last int of the last long word?
/*	if (isOdd(numberInts)) {
		//  if odd, be sure to zero out the ending part of the last cell
		ary[0] = iarray[numberInts-1];
		ary[1] = 0;
		add = iaddress + (long long)(numberInts/2);
		status = zput(ifltab, add, ary, 1, 2);
		if (zisError(status)) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_zwriteDisk_ID, zdssErrorCodes.WRITE_ERROR,
									status, add, zdssErrorSeverity.WRITE_ERROR, "", "Zero last word");
		}
	}
	else {
	*/
		//  Indicate that this file has been written too
		//  (if above code is used, that will record the write.)
		ifltab[zdssKeys.kfileWritten] = 1;
		ifltab[zdssKeys.kwritingNow] = 1;
		ifltab[zdssKeys.knumberWrites]++;
		ifltab[zdssKeys.kwritesSinceFlush]++;
	//}

	return status;
}

