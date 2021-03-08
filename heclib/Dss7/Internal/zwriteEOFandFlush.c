
#include <stdio.h>

#include "heclib7.h"
#include "zdssVals.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"

/**
*  Function:	zwriteEOFandFlush
*
*  Use:			Private
*
*  Description:	Writes an EOF flag at the current end of file position and then flushes buffers
*
*  Declaration: int zwriteEOFandFlush(long long *ifltab, long long bufferControl[4], int *buffer)
*
*  Parameters:	long long ifltab
*					The integer file table array passed among DSS functions.
*
*				long long bufferControl bufferControl[4]
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
*					The actual size of the array needs to be passed in bufferControl[BUFF_SIZE]
*
*	Returns:	int status
*					STATUS_OKAY for successful operation.
*					errorCode for error
*
*  Note:		Generally zwriteEOFandFlush is called at the end of writes, just before unlocking.
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zwriteEOFandFlush(long long *ifltab, long long bufferControl[4], int *buffer)
{
	int status;
	long long lWord;
	long long *fileHeader;
	char messageString[20];


	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%lld",fileHeader[zdssFileKeys.kfileSize]);
		zmessageDebug(ifltab, DSS_FUNCTION_zwriteEOF_ID, "Write EOF and flush at        ", messageString);
	}

	lWord = DSS_END_FILE_FLAG;
	status = zputBuff(ifltab, fileHeader[zdssFileKeys.kfileSize], (int *)&lWord, 1, 2,
		BUFF_WRITE_FLUSH, bufferControl, buffer);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zwriteEOF_ID);
	}
	return status;
}

