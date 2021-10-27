
#include <stdio.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"


/**
*  Function:	zgetBuff
*
*  Use:			Private (Internal)
*
*  Description:	 Reads from disk into a buffer, then parts of the buffer can be read
*
*  Declaration: int zgetBuff (long long *ifltab, long long iaddress, int *iarray, int numberWords, int wordSize,
*				 int bufferAction, long long bufferControl[4], int *buffer)
*
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
*				int *iarray (Output)
*					The array to copy data from the buffer to.
*
*				int numberWords
*					The number of words to copy, or the number to load from disk.
*
*				int wordSize
*					The word size in int*4 words.  Must be either 1 or 2.
*					1 - reading int*4 words
*					2 - reading int*8 words
*
*				int bufferAction
*					A flag indicating how to handle buffering for this call:
*						BUFF_NO_ACTION – no buffer use in this io; read data directly from disk
*						BUFF_READ - Copy from buffer; if not in it, return
*						BUFF_LOAD - load buffer only - don't return values
*
*				long long *bufferControl  (long long bufferControl[4])
*					An int*8 array dimensioned to 4 to hold pointers/information used in buffering.
*					This array should be zeroed out before using, except for the first value.
*					The first element has to be set to the size of buffer in int*4 words.
*						bufferControl[BUFF_SIZE] is (max) int*4 size
*						bufferControl[BUFF_STAT] is write status;  0 - unused, 1 - not dirty (read only), 2 - dirty (needs to write)
*						bufferControl[BUFF_ADDRESS] is file address (int*8)
*						bufferControl[BUFF_INTS_USED] is current int number loaded or used
*
*				int *buffer
*					The integer array to hold buffered data.  The integer*4 size must be specified in bufferControl[BUFF_SIZE].
*
*	Returns:	int status
*				STATUS_OKAY No errors, although data may not have been buffered
*				error code
*
*  Remarks:		The buffer must be loaded (action = C) before accessing data in the buffer.
*				BUFF_LOAD does not copy data into iarray - that is done on a subsequent call.
*				The buffer size must be passed in bufferControl[0]  (bufferControl[BUFF_SIZE]).
*				If called incorrectly, function will bypass buffering and just read from disk.
*				(No errors will be returned, except from zget).
*               The number loaded will be numberInts, but less than or equal to bufferControl[BUFF_SIZE].
*
*	See Also:	zget, zputbuff
*
*	Example Without Error Checking (please add in your code)
*
*	//  Read 200, then 300, then 500 ints, sequentially
*	int *buffer;
*	long long bufferControl[4];
*
*	buffer = (int *)malloc(4 * 1000);
*	bufferControl[0] = 1000;
*	for (i=1; i<4; i++) bufferControl[i] = 0;
*	status = zgetBuff(ifltab, iaddress, iarray, 1000, BUFF_LOAD, bufferControl, buffer);  //  No data returned, but I-O here
*	status = zgetBuff(ifltab, iaddress, iarray, 200, BUFF_READ, bufferControl, buffer);
*	iaddress += (200/2);
*	status = zgetBuff(ifltab, iaddress, jarray, 300, BUFF_READ, bufferControl, buffer);
*	iaddress += (300/2);
*	status = zgetBuff(ifltab, iaddress, karray, 500, BUFF_READ, bufferControl, buffer);
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zgetBuff (long long *ifltab, long long iaddress, int *iarray, int numberWords, int wordSize,
			  int bufferAction, long long bufferControl[4], int *buffer)
{

	int status;
	int maxInts;
	long long addressEndBuffer;
	long long addressEndData;
	long long *fileHeader;
	int i;
	int ipos;
	int numberInts;
	int boolLoad;
	char messageString[50];


	//  Get number of int*4 words
	numberInts = numberWords * wordSize;
	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	//  Is this a bypass flag, or a read and the buffer has not been loaded?  (Buffer must always be loaded first!)
	if ((bufferAction == BUFF_NO_ACTION) || ((bufferAction == BUFF_READ) && (bufferControl[BUFF_STAT] == BUFF_STAT_UNUSED))) {
		status = zget(ifltab, iaddress, iarray, numberWords, wordSize);
		return status;
	}

	status = STATUS_OKAY;
	maxInts = (int)bufferControl[BUFF_SIZE];

	//  Do we have a dirty buffer, and a situation where we need to write it to disk first?
	if (bufferControl[BUFF_STAT] == BUFF_STAT_DIRTY) {
		//  Load will replace buffer and too small of a size will bypass buffering
		if ((bufferAction == BUFF_LOAD) || (numberInts > maxInts)) {
			//  Yes, store on disk
			status = zput(ifltab, bufferControl[BUFF_ADDRESS], buffer, (int)bufferControl[BUFF_INTS_USED], 1);
			//  Show that buffer is no longer dirty
			bufferControl[BUFF_STAT] = BUFF_STAT_NOT_DIRTY;
			if (zmessageLevel(ifltab, MESS_METHOD_GET_ID, MESS_LEVEL_INTERNAL_DIAG_2) ||
				zmessageLevel(ifltab, MESS_METHOD_PUT_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
				zmessageZget(ifltab, bufferControl[BUFF_ADDRESS], (int)bufferControl[BUFF_SIZE], wordSize, 3, status, DSS_FUNCTION_zgetBuff_ID, "");
			}
			if (zisError(status)) {
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_zgetBuff_ID);
			}
		}
	}

	//  Load Buffer, if not loaded already
	if (bufferAction == BUFF_LOAD) {
		boolLoad = 0;
		if (bufferControl[BUFF_STAT] == BUFF_STAT_UNUSED) {
			boolLoad = 1;
		}
		else if (bufferControl[BUFF_ADDRESS] != iaddress) {
			boolLoad = 1;
		}
		else if (bufferControl[BUFF_INTS_USED] < numberInts) {
			boolLoad = 1;
		}
		if (boolLoad) {
			if (bufferControl[BUFF_SIZE] <= 0) {
				///   FIX ME - do beign error processing
			}
			else {
				if (numberInts > bufferControl[BUFF_SIZE]) {
					numberInts = (int)bufferControl[BUFF_SIZE];
				}
				//  Sometimes we estimate the amount to load and it is too high
				//  and can go beyond EOF.  In this case, just read to EOF
				addressEndBuffer = iaddress + numberLongsInInts(numberInts);
				if (addressEndBuffer > fileHeader[zdssFileKeys.kfileSize]) {
					addressEndBuffer -= fileHeader[zdssFileKeys.kfileSize];
					numberInts = numberIntsInLongs(addressEndBuffer);
				}
				if (numberInts <= 0) {
					/////   FIX ME - do error processing here!!!
				}
				bufferControl[BUFF_STAT] = BUFF_STAT_NOT_DIRTY;
				bufferControl[BUFF_ADDRESS] = iaddress;
				bufferControl[BUFF_INTS_USED] = numberInts;
				status = zget(ifltab, bufferControl[BUFF_ADDRESS], buffer, numberInts, 1);
				if (zmessageLevel(ifltab, MESS_METHOD_GET_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
					zmessageZget(ifltab, iaddress, numberInts, wordSize, bufferAction, status, DSS_FUNCTION_zgetBuff_ID, "");
				}
				if (zisError(status)) {
					return zerrorUpdate(ifltab, status, DSS_FUNCTION_zgetBuff_ID);
				}
			}
		}
		else if (zmessageLevel(ifltab, MESS_METHOD_GET_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			zmessageDebug(ifltab, DSS_FUNCTION_zreadInternal_ID, "Buffer load bypassed because already loaded","");
		}
	 }

	else if (bufferAction == BUFF_READ) {
		//  Read from buffer, if data is in it.
		//  If we don't have enough space, read from disk
		if (numberInts > maxInts) {
			status = zget(ifltab, iaddress, iarray, numberInts, 1);
			if (zisError(status)) {
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_zgetBuff_ID);
			}
			return status;
		}
		//  First, be sure that the data lies totally within range of buffer
		//  Compute the ending address of the buffer
		//				   beg address + number long longs in buffer
		addressEndBuffer = bufferControl[BUFF_ADDRESS] + (long long)numberLongsInInts((int)bufferControl[BUFF_INTS_USED]);
		//  Compute the ending address of data
		//				 beg address + number long longs in data
		addressEndData = iaddress + (long long)numberLongsInInts(numberInts);
		//  Are we within range?
		//   data address >= buffer add & data address <= end buffer add
		if ((iaddress >= bufferControl[BUFF_ADDRESS]) & (addressEndData <= addressEndBuffer)) {
			// Yes!  Copy data from buffer
			// Compute position within buffer to start
			ipos = numberIntsInLongs(iaddress - bufferControl[BUFF_ADDRESS]);
			for (i=0; i<numberInts; i++) {
				iarray[i] = buffer[ipos++];
			}
			if (zmessageLevel(ifltab, MESS_METHOD_GET_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
				zmessageZget(ifltab, iaddress, numberInts, wordSize, bufferAction, 0, DSS_FUNCTION_zgetBuff_ID, "");
			}
		}
		else {
			//  No, we are not within range of the buffer;
			//  The values to buffered are outside the range of the buffer
			//  Is the buffer within range and dirty (so that we need to dump to sync?)
			if (bufferControl[BUFF_STAT] == BUFF_STAT_DIRTY) {
				// In range?
				if ((iaddress > addressEndBuffer) || (addressEndData < bufferControl[BUFF_ADDRESS])) {
					//  Nope - ignore buffer all together and just read from disk
				}
				else {
					//  Yes, need to dump (what's in buffer and on disk need to be in sync)
					status = zput(ifltab, bufferControl[BUFF_ADDRESS], buffer, (int)bufferControl[BUFF_INTS_USED], 1);
					//  Show that buffer is no longer dirty
					bufferControl[BUFF_STAT] = BUFF_STAT_NOT_DIRTY;
					if (zisError(status)) {
						return zerrorUpdate(ifltab, status, DSS_FUNCTION_zgetBuff_ID);
					}
				}
			}
			status = zget(ifltab, iaddress, iarray, numberInts, 1);
			if (zmessageLevel(ifltab, MESS_METHOD_GET_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
					"Address of buffer: %lld.", bufferControl[BUFF_ADDRESS]);
				zmessageZget(ifltab, iaddress, numberInts, wordSize, 4, status, DSS_FUNCTION_zgetBuff_ID, messageString);
			}
		}
	}

	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zgetBuff_ID);
	}
    return status;
}

