#include <stdio.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "zerrorCodes.h"
#include "zdssVals.h"
#include "hecdssInternal.h"


/**
*  Function:	zget
*
*  Use:			Private (Internal)
*
*  Description:	Read from the DSS file.  All reads come through this function.
*				The address and number are verified, then the machine dependent function readDisk
*				is called to perform the physical read.
*
*  Declaration: int zget(long long *ifltab, long long iaddress, int *iarray, int numberWords, int wordSize)
*
*  Parameters:	long long *ifltab
*					The integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				long long iaddress
*					The int*8 address to start reading from the DSS file.  Note, this is not a byte address,
*					but the word address in int*8 (double) words.  Thus, if the address were "2",
*					this would be 16 bytes into the file.  All addresses in DSS version 7 are in int*8 words.
*
*				int *iarray
*					The array to hold the data read.
*
*				int numberWords
*					The number of words to read (where the word size is given in the next argument).
*					Note:  it is assumed that the calling function has allocated
*					sufficient space in iarray to hold numberInts.
*
*				int wordSize
*					The word size in int*4 words.  Must be either 1 or 2.
*					1 - reading int*4 words
*					2 - reading int*8 words
*
*	Returns:	int status
*				STATUS_OKAY for successful operation.
*				errorCode for invalid operations or other errors.  See error processing for codes.
*
*	See Also:	zput, zget8 or zgetBuff
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zget(long long *ifltab, long long iaddress,  int *iarray, int numberWords, int wordSize)
{

	int status;
	int ihandle;
	int iswap;
	int ntrans;
	int numberInts;
	long long iadd;
	long long *fileHeader;


	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];

	//  Get number of int*4 words
	numberInts = numberWords * wordSize;

	//  Make sure we have a valid address
	if (iaddress < 0) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zget_ID, zdssErrorCodes.INVALID_ADDRESS,
										 0, iaddress, zdssErrorSeverity.CORRUPT_FILE, "", "");
	}
	if (iaddress > (fileHeader[zdssFileKeys.kfileSize] + 10000)) {
		//  Update perm area, in case another process is writing
		if (zmessageLevel(ifltab, MESS_METHOD_GENERAL_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebug(ifltab, DSS_FUNCTION_zget_ID, "Read request beyond end of file, updating perm ", "");
		}
		status = zpermRead(ifltab);
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zget_ID);
		}
		if (iaddress > (fileHeader[zdssFileKeys.kfileSize] + 100000)) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_zget_ID, zdssErrorCodes.INVALID_ADDRESS,
											 0, iaddress, zdssErrorSeverity.CORRUPT_FILE, "", "");
		}
	}
	//  Make sure the number of bytes to read is a reasonable number
	if ((numberInts < 0) || (numberInts > 10000000)) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zget_ID, zdssErrorCodes.INVALID_NUMBER_TO_READ,
										 numberInts, iaddress, zdssErrorSeverity.CORRUPT_FILE, "", "");
	}

	ihandle = (int)ifltab[zdssKeys.khandle];
	iswap   = (int)ifltab[zdssKeys.kswap];

	status = zreadDisk (ihandle, iswap, iaddress, iarray, numberInts);

	if (zmessageLevel(ifltab, MESS_METHOD_GET_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
		zmessageZget(ifltab, iaddress, numberInts, wordSize, BUFF_NO_ACTION, status, DSS_FUNCTION_zreadDisk_ID, "");
	}

	if (zisError(status)) {
			/*
			 *   0 if successful
				-1:  Undefined error
				-2:  Negative address
				-3:  Unable to seek
				-4:  Read error
			 * <-10:  Partial read
			 *        Number bytes read = abs(return val) - 10;
			 *        e.g., return -50 means 40 bytes read   (50-10)
			 *  >0:  System error, error code returned.
			*/
		ntrans = -(status + 10);
		iadd = iaddress + (numberInts / 2) + 1;
		if (zmessageLevel(ifltab, MESS_METHOD_GET_ID, MESS_LEVEL_TERSE)) {
			zmessage(ifltab, "Read Error");
			zmessageInt(ifltab, "status: ", status);
			zmessageLong(ifltab, "At word address: ", iaddress);
			zmessageLong(ifltab, "Ending word address: ", iadd);
			zmessageLong(ifltab, "File size (words): ", fileHeader[zdssFileKeys.kfileSize]);
		}
		if (status < -10) {
			
			if (iadd > fileHeader[zdssFileKeys.kfileSize]) {
				//  Tried to read beyond EOF  READ_BEYOND_EOF
				return zerrorProcessing(ifltab, DSS_FUNCTION_zget_ID, zdssErrorCodes.READ_BEYOND_EOF,
										status, iaddress, zdssErrorSeverity.READ_ERROR, "", "");
			}
			else {
				//  Truncated file
				return zerrorProcessing(ifltab, DSS_FUNCTION_zget_ID, zdssErrorCodes.READ_ERROR,
										status, iaddress, zdssErrorSeverity.READ_ERROR, "", "");
			}
		}
		else if (status > 0) {
			//  System error
			return zerrorProcessing(ifltab, DSS_FUNCTION_zget_ID, zdssErrorCodes.READ_ERROR,
									status, iaddress, zdssErrorSeverity.READ_ERROR, "", "");
		}
		else if (status == -2) {
			//  Negative address
			return zerrorProcessing(ifltab, DSS_FUNCTION_zget_ID, zdssErrorCodes.INVALID_ADDRESS,
									status, iaddress, zdssErrorSeverity.CORRUPT_FILE, "", "");
		}
		else if (status == -3) {
			//  Unable to seek
			return zerrorProcessing(ifltab, DSS_FUNCTION_zget_ID, zdssErrorCodes.READ_ERROR,
									status, iaddress, zdssErrorSeverity.READ_ERROR, "", "");
		}
		else if (status == -4) {
			//  Unable to read
			return zerrorProcessing(ifltab, DSS_FUNCTION_zget_ID, zdssErrorCodes.READ_ERROR,
									status, iaddress, zdssErrorSeverity.READ_ERROR, "", "");
		}
		else {
			//  Undefined read error
			return zerrorProcessing(ifltab, DSS_FUNCTION_zget_ID, zdssErrorCodes.UNDEFINED_ERROR,
									status, iaddress, zdssErrorSeverity.READ_ERROR, "", "");
		}
	}

	ifltab[zdssKeys.knumberReads]++;
	ifltab[zdssKeys.kaddLast] = iaddress;

	return status;
}

