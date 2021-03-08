
#include <stdio.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"


/**
*  Function:	zputBuff
*
*  Use:			Private (Internal)
*
*  Description:	 Combines several small writes into one large write using a buffer.
*
*  Declaration: int zputBuff (long long *ifltab, long long iaddress, int *iarray, int numberWords, int wordSize,
*				 int bufferAction, long long bufferControl[4], int *buffer)
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
*					The array containing the data to write buffer.
*
*				int numberWords
*					The number of words to write (where the word size is given in the next argument).
*
*				int wordSize
*					The word size in int*4 words.  Must be either 1 or 2.
*					1 - writing int*4 words
*					2 - writing int*8 words
*
*				int bufferAction
*					A flag indicating how to handle buffering for this call:
*						BUFF_NO_ACTION – No buffer used; write data directly to disk
*						BUFF_WRITE - Write this data to buffer
*						BUFF_WRITE_FLUSH - Write buffer and data to disk
*
*				long long *bufferControl  (long long bufferControl[4])
*					An int*8 array dimensioned to 4 to hold pointers/information used in buffering.
*					This array should be zeroed out before using, except for the first value.
*					The first element has to be set to the size of buffer in int*4 words.
*						bufferControl[BUFF_SIZE] is (max) int*4 size
*						bufferControl[BUFF_STAT] is write status;  0 - unused, 1 - not dirty (read only), 2 - dirty (needs to write)
*						bufferControl[BUFF_ADDRESS] is file address (int*8)
*						bufferControl[BUFF_INTS_USED] is current int number used
*
*				int *buffer
*					The integer array to hold buffered data.  The integer*4 size must be specified in bufferControl[BUFF_SIZE].
*
*	Returns:	int status
*					STATUS_OKAY for successful operation, although data may not have been buffered
*					errorCode for error
*
*
* Remarks:  Always write using zputBuff(), never zput().  zput is controlled by zputbuff().
*
* Called By:  Various
*
*	See Also:	zput, zgetBuff
*
*	Example Without Error Checking (please add in your code)
*
*	//  Write 200, then 300, then 500 ints, sequentially
*	int *buffer;
*	long long bufferControl[4];
*
*	buffer = (int *)malloc((size_t)4 * 1000);
*	bufferControl[0] = 1000;
*	for (i=1; i<4; i++) bufferControl[i] = 0;
*	status = zputBuff(ifltab, iaddress, iarray, 200, 1, BUFF_WRITE, bufferControl, buffer);
*	iaddress += (200/2);
*	status = zputBuff(ifltab, iaddress, jarray, 300, 1, BUFF_WRITE, bufferControl, buffer);
*	iaddress += (300/2);
*	status = zputBuff(ifltab, iaddress, karray, 500, 1, BUFF_WRITE_FLUSH, bufferControl, buffer);  //  I-O done here
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zputBuff (long long *ifltab, long long iaddress, int *iarray, int numberWords, int wordSize,
			  int bufferAction, long long bufferControl[4], int *buffer)
{

	int status;
	int maxInts;
	long long addressEndBuffer;
	long long addressEndData;
	int numberInts;
	int i;
	int ipos;
	int boolDirty;



	//  Get number of int*4 words
	numberInts = numberWords * wordSize;

	//  Is this a bypass flag?  Just have zput write.
	if (bufferAction == BUFF_NO_ACTION) {
		status = zput(ifltab, iaddress, iarray, numberWords, wordSize);
		return status;
	}

	status = 0;
	maxInts = (int)bufferControl[BUFF_SIZE];
	if (bufferControl[BUFF_STAT] == BUFF_STAT_DIRTY) {
		boolDirty = 1;
	}
	else {
		boolDirty = 0;
	}

	//  Is the buffer being used?
	if (bufferControl[BUFF_STAT] != BUFF_STAT_UNUSED) {
		//   Yes.  First, be sure that the data to be buffered lies totally within range of buffer
		//  Compute the ending address of the buffer
		//				   beg address + number long longs in buffer
		addressEndBuffer = bufferControl[BUFF_ADDRESS] + (long long)numberLongsInInts((int)bufferControl[BUFF_SIZE]);
		//  Compute the ending address of data to be buffered
		//				 beg address + number long longs in data
		addressEndData = iaddress + (long long)numberLongsInInts(numberInts);
		//  Are we within range?
		//   data address >= buffer add & data address <= end buffer add
		if ((iaddress >= bufferControl[BUFF_ADDRESS]) & (addressEndData <= addressEndBuffer)) {
			// Yes!  Copy data into buffer
			//  Compute position within buffer to start
			ipos = numberIntsInLongs(iaddress - bufferControl[BUFF_ADDRESS]);
			for (i=0; i<numberInts; i++) {
				buffer[ipos++] = iarray[i];
			}
			bufferControl[BUFF_STAT] = BUFF_STAT_DIRTY;
			//  Save the last position, and returned that we stored to buffer
			if (ipos > bufferControl[BUFF_INTS_USED]) {
				bufferControl[BUFF_INTS_USED] = ipos;
			}
			if (zmessageLevel(ifltab, MESS_METHOD_PUT_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
				if (boolDirty) {
					zmessageZput(ifltab, iaddress, numberInts, wordSize, bufferAction, 0, DSS_FUNCTION_zputBuff_ID, "Buff was dirty.");
				}
				else {
					zmessageZput(ifltab, iaddress, numberInts, wordSize, bufferAction, 0, DSS_FUNCTION_zputBuff_ID, "Buff was clean.");
				}
			}
		}
		else {
			//  No, we are not within range of the buffer;
			//  The vaules to buffered are outside the range of the buffer
			//  Dump the buffer to disk, if needed (what's in buffer and on disk need to be in sync)
			if (bufferControl[BUFF_STAT] == BUFF_STAT_DIRTY) {
				status = zput(ifltab, bufferControl[BUFF_ADDRESS], buffer, (int)bufferControl[BUFF_INTS_USED], 1);
				if (zmessageLevel(ifltab, MESS_METHOD_PUT_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
					zmessageZput(ifltab, bufferControl[BUFF_ADDRESS], (int)bufferControl[BUFF_INTS_USED], wordSize, 3, status, DSS_FUNCTION_zputBuff_ID, "");
				}
				if (zisError(status)) {
					return status;
				}
			}
			//  Mark the buffer as not dirty (or dumped to disk)
			//  Either we just dumped it or there was not room for our data
			bufferControl[BUFF_STAT] = BUFF_STAT_NOT_DIRTY;
		}
	}

	if (bufferControl[BUFF_STAT] != BUFF_STAT_DIRTY) {
		//  Do we have space in the buffer?
		if (numberInts >= maxInts) {
			//  If we don't have enough space, just bypass buffer and write to disk
			if (zmessageLevel(ifltab, MESS_METHOD_PUT_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
				//  Don't write message if buffering not used.
				if (bufferControl[BUFF_SIZE] != 0) {
					zmessageZput(ifltab, bufferControl[BUFF_ADDRESS], (int)bufferControl[BUFF_INTS_USED], wordSize, 4, 0, DSS_FUNCTION_zputBuff_ID, "Buffer bypassed.");
				}
			}
			status = zput(ifltab, iaddress, iarray, numberInts, 1);
			return status;
		}
		//  We do have space in the (cleared) buffer, copy the data into it
		for (i=0; i<numberInts; i++) {
			buffer[i] = iarray[i];
		}
		bufferControl[BUFF_STAT] = BUFF_STAT_DIRTY;
		bufferControl[BUFF_ADDRESS] = iaddress;
		bufferControl[BUFF_INTS_USED] = numberInts;
		if ((zmessageLevel(ifltab, MESS_METHOD_PUT_ID, MESS_LEVEL_INTERNAL_DIAG_2)) && (numberInts > 0)) {
			if (boolDirty) {
				zmessageZput(ifltab, iaddress, numberInts, wordSize, bufferAction, 0, DSS_FUNCTION_zputBuff_ID, "Buff was dirty.");
			}
			else {
				zmessageZput(ifltab, iaddress, numberInts, wordSize, bufferAction, 0, DSS_FUNCTION_zputBuff_ID, "Buff was clean.");
			}
		}
	}


	//  Have we been told to dump the buffer to disk?
	//  action 2 is write data to buffer and then dump to disk
	//  and is the buffer dirty (needs to be written)?
	 if ((bufferAction == BUFF_WRITE_FLUSH) && (bufferControl[BUFF_STAT] == BUFF_STAT_DIRTY)) {
		//  Yes, store on disk
		status = zput(ifltab, bufferControl[BUFF_ADDRESS], buffer, (int)bufferControl[BUFF_INTS_USED], 1);
		//  Show that buffer is no longer dirty
		bufferControl[BUFF_STAT] = BUFF_STAT_NOT_DIRTY;
		if (zmessageLevel(ifltab, MESS_METHOD_PUT_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
			zmessageZput(ifltab, bufferControl[BUFF_ADDRESS], (int)bufferControl[BUFF_INTS_USED], wordSize, 3, status, DSS_FUNCTION_zputBuff_ID, "");
		}
	}

    return status;
}

