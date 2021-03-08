#include <stdio.h>

#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"

/**
*  Function:	zflushBuffers
*
*  Use:			Private
*
*  Description:	Flushes internal buffers; writes what DSS has in memory.  Does a "physical write", but not a flush (commit).
*
*  Declaration: int zflushBuffers(long long *ifltab, long long bufferControl[4], int *buffer)
*
*  Parameters:	long long ifltab
*					The integer file table array passed among DSS functions.
*
*				long long bufferControl[4]
*					An int*8 array dimensioned to 4 to hold pointers/information used in buffering
*                   and is the control array for the buffer, which follows this argument
*					This array should be zeroed out before using, except for the first value.
*					The first element has to be set to the size of buffer in int*4 words.
*						bufferControl[BUFF_SIZE] is (max) int*4 size
*						bufferControl[BUFF_STAT] is write status;  0 - unused, 1 - not dirty (read only), 2 - dirty (needs to write)
*						bufferControl[BUFF_ADDRESS] is file address (int*8)
*						bufferControl[BUFF_INTS_USED] is current int number used
*
*				int *buffer
*					An integer*4 array that is used for buffering.
*                   It is usually passed between reading or writing functions to minimize physical reads or writes.
*					Generally, this array is malloced space that is as least as large as the record and info area to
*					be written or read. For example buffer = malloc((end Data - beginning info) * 8), assuming values are int*8 addresses
*					The actual int size of the array needs to be passed in bufferControl[BUFF_SIZE] (e.g., (end Data - beginning info) * 2)
*
*	Returns:	status:
*				STATUS_OKAY, or
*				error code
*
*
*  Note:		This does a "normal write", which in itself does not guarantee that data is physically on disk.
*				(You only need to make sure physically on disk in multi-user access mode.  If so,
*				use zflushToDisk afterwards.)
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zflushBuffers(long long *ifltab, long long bufferControl[4], int *buffer)
{
	long long nada=0;
	int zero = 0;
	int status;

	if (zmessageLevel(ifltab, MESS_METHOD_PUT_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zput_ID, "Flush Buffers", "");
	}

	status = zputBuff(ifltab, nada, &zero, 0, 2, BUFF_WRITE_FLUSH, bufferControl, buffer);
	return status;
}

