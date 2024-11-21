#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"
#include "zerrorCodes.h"

/**
*  Function:	zreadInternal
*
*  Use:			Private (Internal)
*				zread7 is the public version of this.
*
*  Description:	Primary function to read a record (identified by its pathname).
*
*  Declaration: int zreadInternal(long long *ifltab, zStructTransfer* ztransfer,
*								  long long bufferControl[4], int *buffer, int boolUseBuffer)
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				zStructTransfer* zStructTransfer
*					A struct that will contain all the data for this record.
*					The struct should be created by a call to
*						zStructTransfer* zstructTransferNew(const char* pathname, int mode)
*					And freed when complete by a call to
*						void zstructFree(zStructTransfer *ztransfer)
*					NEVER REUSE A zStructTransfer, always free and create a new on.
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
*				int *buffer (optional)
*					An integer*4 array that is used for buffering.
*                   It is usually passed between reading or writing functions to minimize physical reads or writes.
*					Generally, this array is malloced space that is as least as large as the record and info area to
*					be written or read. For example buffer = malloc((end Data - beginning info) * 8), assuming values are int*8 addresses
*					The actual int size of the array needs to be passed in bufferControl[BUFF_SIZE] (e.g., (end Data - beginning info) * 2)
*
*				int boolUseBuffer
*					A flag that allows buffering to be bypassed (without having to mess with the buffer arguments.)
*
*				Note:  Buffering is mainly useful only for small data sets.  If in doubt, do not use.
*
*
*
*  zStructTransfer parameters used in this call:
*
*	Input Parameters:
*
*				const char* pathname
*					The pathname of the record to read.  The pathname must be exact, but is case insensitive
*					(i.e., "Apart" is the same as "APART")
*
*  Output Parameters (these will be returned):
*
*				int dataType
*					The record data type associated with this record, as defined in zdssVals.h
*
*				int version
*					The number of times this record has been written.
*
*				int lastWrittenTime
*					The system time of the last write, in seconds since Jan 01, 1970.
*					System time means that the time zone is included, but daylight time is not.
*
*				int fileLastWrittenTime
*					The system time of the files last write, in seconds since Jan 01, 1970.
*					If this record was the last write, then the lastWrittenTime will be the same.
*
*				int *internalHeader
*					An array to hold the internal header of the record
*
*				int internalHeaderMode
*					A flag indicating how to handle this array:
*						0:  Do not read this array.
*						1:  zread will allocate (the correct amount of) space needed for this array when it is to be read.
*					  > 1:  The space has already been allocated (by you!) and this is the size of the array.
*
*				int internalHeaderNumber
*					Returns the number read (and size) into the array.
*					If you allocated space, it will not be larger than internalHeaderMode.
*
*				int *header2
*					An array to hold the second header of the record.  The second header usually
*					holds compression information for the data.
*
*				int header2Mode
*					A flag indicating how to handle this array:
*						0:  Do not read this array.
*						1:  zread will allocate (the correct amount of) space needed for this array when it is to be read.
*					  > 1:  The space has already been allocated (by you!) and this is the size of the array.
*
*				int header2Number
*					Returns the number read (and size) into the array.
*					If you allocated space, it will not be larger than header2Mode.
*
*				int *values3
*					An array to hold the second header of the record.  The third header is reserved
*					for any additional information needed to be stored.  It is often not used.
*
*				int *values1
*					The main array to hold the data to be read.  This array is usually floats or doubles,
*					but is declared as int for an minimum element size of 4 bytes.
*
*				int values1Mode
*					A flag indicating how to handle this array:
*						0:  Do not read this array.
*						1:  zread will allocate (the correct amount of) space needed for this array when it is to be read.
*					  > 1:  The space has already been allocated (by you!) and this is the size of the array.
*
*				int values1Number
*					Returns the number read (and size) into the array.
*					If you allocated space, it will not be larger than values1Mode.
*
*				int *values2
*					The secondary data array.
*
*				int values2Mode
*					A flag indicating how to handle this array:
*						0:  Do not read this array.
*						1:  zread will allocate (the correct amount of) space needed for this array when it is to be read.
*					  > 1:  The space has already been allocated (by you!) and this is the size of the array.
*
*				int values2Number
*					Returns the number read (and size) into the array.
*					If you allocated space, it will not be larger than values2Mode.
*
*				int *values3
*					The third data array.
*
*				int values3Mode
*					A flag indicating how to handle this array:
*						0:  Do not read this array.
*						1:  zread will allocate (the correct amount of) space needed for this array when it is to be read.
*					  > 1:  The space has already been allocated (by you!) and this is the size of the array.
*
*				int values3Number
*					Returns the number read (and size) into the array.
*					If you allocated space, it will not be larger than values2Mode.
*
*
*				int numberValues
*					Returns the number of values associated with this data set.
*					For example, if the data is stored as doubles, it will often be values1Number / 2.
*					However, this number is passed in on a zwrite
*
*				int logicalNumberValues
*					Returns the logical number of values stored.  This is the number after expansion
*					and including all missing data values.  For example a year of daily data, this
*					number would always be 365 or 366.
*
*				int totalAllocatedSize
*					The size allocted on disk for values1 and values2 (in 4 byte ints.)
*					Note, this can be larger then the actual size needed, to allow for some expansion.
*
*				int totalExpandedSize
*					The total logical size of values1 and values2 (in 4 byte ints.)
*
*				char programName[PARAMETER_NAME_SIZE]
*					The name of the program that last wrote this record (if set).
*
*				int insufficientSpace
*					If arrays are pre-allocated and there is more data to be read
*					from disk than space allocated, insufficientSpace will contain the number
*					of ints that could not be read due to allocation limits (combined space)
*
*	Private - do not use
*				int totalExpandedSize
*				long long *info
*				char allocated[zSTRUCT_length]
*
*
*
*	Returns:	int status
*					STATUS_RECORD_FOUND for successful read.
*					STATUS_RECORD_NOT_FOUND for non-existent record.
*					errorCode for error
*
*
*  Remarks:
*				zcheck() may be called prior to zreadInternal to be sure the record exists,
*					and to load the info block.
*
*				Data sets often consist of two parts (values1 and values2).  For example,
*					with paired data, the ordinates are stored in values1 and the curves in values2.
*
*				Buffering does not need to be used (and frequently it is not), as its only
*					purpose is to make reads more efficient and quicker.  Regardless, you still
*					must pass in a long long bufferControl[4], but set bufferControl[0] = 0.
*					Otherwise, try to make *buffer as large as the largest record to be read,
*					including the info block, then set bufferControl[0] = size (int 4), and
*					zero bufferControl[1] through bufferControl[3].
*					How large should the buffer be set?
*					The largest record sizes are kept in the perm section just for this question
*					fileHeader[zdssFileKeys.kmaxRecordSize] is the maximum record size in the file in 8 byte words
*					fileHeader[zdssFileKeys.kmaxRtdSize] is the maximum regular time series double record, etc.
*					For example,
*					buffer = malloc(fileHeader[zdssFileKeys.kmaxRtsSize] * 8);	//  size is in 8 byte words
*					bufferControl[0] = fileHeader[zdssFileKeys.kmaxRtsSize] * 2;	//  buffer control works in 4 byte words
*					So, if you were going to read a series of time series records, you could allocate the space
*					needed once, without the functions having to reallocate over and over
*					Note:  Buffering is only used for I/O; it is not used for expanding compressed data
*
*
*  See Also:	zread7(),  for the public version of this call.
*				zreadx7(), for a public accessible read without using the struct (mainly for other languages)
*				zread(),   for a short public accessible version (user header and data only).
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int zreadInternal(long long *ifltab, zStructTransfer* ztransfer,
				  long long bufferControl[4], int *buffer, int boolUseBuffer)
{
	int status;
	int bufferAction;
	int totalNumber;
	int number;
	int dummy;
	char messageString[80];


	if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zreadInternal_ID, "Enter handle: ", zhandle(ifltab));
		zmessageDebug(ifltab, DSS_FUNCTION_zreadInternal_ID, "Pathname: ", ztransfer->pathname);
	}


	//  Read info block, if it has not been read
	if ((!ztransfer->info) || (ztransfer->info[0] != DSS_INFO_FLAG)){
		if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			zmessageDebug(ifltab, DSS_FUNCTION_zreadInternal_ID, "Record not loaded, (invalid info block), loading","");
		}
		status = zreadInfo(ifltab, ztransfer->pathname, REC_STATUS_VALID);
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zreadInternal_ID);
		}
		if (status == STATUS_RECORD_NOT_FOUND) {
			if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
				zmessageDebug(ifltab, DSS_FUNCTION_zreadInternal_ID, "Record not found","");
			}
			return status;
		}
		ztransfer->info = (long long *)ifltab[zdssKeys.kinfo];
		if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			zmessageDebug(ifltab, DSS_FUNCTION_zreadInternal_ID, "Record loaded (info block read)","");
		}
	}
	i8toi4(ztransfer->info[zdssInfoKeys.kinfoTypeVersion], &ztransfer->dataType, &ztransfer->version);

	//  Load into buffer (only)
	if (buffer && (bufferControl[BUFF_SIZE] && boolUseBuffer)) {
		//  Load all of this record into buffer, if we can
		totalNumber = 0;
		totalNumber += (int)ztransfer->info[zdssInfoKeys.kinfoInternalHeadNumber];
		totalNumber += (int)ztransfer->info[zdssInfoKeys.kinfoHeader2Number];
		totalNumber += (int)ztransfer->info[zdssInfoKeys.kinfoValues3Number];
		totalNumber += (int)ztransfer->info[zdssInfoKeys.kinfoUserHeadNumber];
		totalNumber += (int)ztransfer->info[zdssInfoKeys.kinfoValues1Number];
		totalNumber += (int)ztransfer->info[zdssInfoKeys.kinfoValues2Number];
		dummy = 0;
		status = zgetBuff(ifltab, ztransfer->info[zdssInfoKeys.kinfoInternalHeadAddress], &dummy,
			totalNumber, 2, BUFF_LOAD, bufferControl, buffer);
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zreadInternal_ID);
		}
	}
	else {
		bufferControl[BUFF_STAT] = BUFF_STAT_UNUSED;
	}

	if (ztransfer->internalHeaderMode) {
		ztransfer->internalHeaderNumber = (int)ztransfer->info[zdssInfoKeys.kinfoInternalHeadNumber];
		if (ztransfer->internalHeaderNumber > 0) {
			if (ztransfer->internalHeaderMode == 1) {
				number = ztransfer->internalHeaderNumber;
				//  We always have to work on 64-bit boundaries (an odd number stops in the middle of last 64 bit word)
				if (isOdd(number)) number++;
				ztransfer->internalHeader = (int *)calloc((size_t)number, (size_t)4);
				if (!ztransfer->internalHeader) {
					return zerrorProcessing(ifltab, DSS_FUNCTION_zreadInternal_ID,
											zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, ztransfer->internalHeaderNumber, 0,
											zdssErrorSeverity.MEMORY_ERROR, ztransfer->pathname, "Allocating internalHeader");
				}
				ztransfer->allocated[zSTRUCT_TRANS_internalHeader] = 1;
				bufferAction = BUFF_READ;
			}
			else {
				number = ztransfer->internalHeaderNumber - ztransfer->internalHeaderMode;
				if (number > 0) {
					ztransfer->insufficientSpace += number;
					ztransfer->internalHeaderNumber = ztransfer->internalHeaderMode;
				}
				bufferAction = BUFF_NO_ACTION;
			}
			status = zgetBuff(ifltab, ztransfer->info[zdssInfoKeys.kinfoInternalHeadAddress], ztransfer->internalHeader,
							  ztransfer->internalHeaderNumber, 1, bufferAction, bufferControl, buffer);
			if (zisError(status)) {
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_zreadInternal_ID);
			}
		}
	}

	if (ztransfer->header2Mode) {
		ztransfer->header2Number = (int)ztransfer->info[zdssInfoKeys.kinfoHeader2Number];
		if (ztransfer->header2Number > 0) {
			if (ztransfer->header2Mode == 1) {
				number = ztransfer->header2Number;
				//  We always have to work on 64-bit boundaries (an odd number stops in the middle of last 64 bit word)
				if (isOdd(number)) number++;
				ztransfer->header2 = (int *)calloc((size_t)number, (size_t)4);
				if (!ztransfer->header2) {
					return zerrorProcessing(ifltab, DSS_FUNCTION_zreadInternal_ID,
											zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, ztransfer->header2Number, 0,
											zdssErrorSeverity.MEMORY_ERROR, ztransfer->pathname, "Allocating header2");
				}
				ztransfer->allocated[zSTRUCT_TRANS_header2] = 1;
				bufferAction = BUFF_READ;
			}
			else {
				number = ztransfer->header2Number - ztransfer->header2Mode;
				if (number > 0) {
					ztransfer->insufficientSpace += number;
					ztransfer->header2Number = ztransfer->header2Mode;
				}
				bufferAction = BUFF_NO_ACTION;
			}
			status = zgetBuff(ifltab, ztransfer->info[zdssInfoKeys.kinfoHeader2Address], ztransfer->header2,
							  ztransfer->header2Number, 1, bufferAction, bufferControl, buffer);
			if (zisError(status)) {
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_zreadInternal_ID);
			}
		}
	}

	if (ztransfer->userHeaderMode) {
		ztransfer->userHeaderNumber = (int)ztransfer->info[zdssInfoKeys.kinfoUserHeadNumber];
		if (ztransfer->userHeaderNumber > 0) {
			if (ztransfer->userHeaderMode == 1) {
				//  This is usually a string, so add one word for null terminator
				number = ztransfer->userHeaderNumber + 1;
				//  We always have to work on 64-bit boundaries (an odd number stops in the middle of last 64 bit word)
				if (isOdd(number)) number++;
				ztransfer->userHeader = (int *)calloc((size_t)number, (size_t)4);
				if (!ztransfer->userHeader) {
					return zerrorProcessing(ifltab, DSS_FUNCTION_zreadInternal_ID,
											zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, ztransfer->userHeaderNumber, 0,
											zdssErrorSeverity.MEMORY_ERROR, ztransfer->pathname, "Allocating userHeader");
				}
				ztransfer->allocated[zSTRUCT_userHeader] = 1;
				bufferAction = BUFF_READ;
			}
			else {
				number = ztransfer->userHeaderNumber - ztransfer->userHeaderMode;
				if (number > 0) {
					ztransfer->insufficientSpace += number;
					ztransfer->userHeaderNumber = ztransfer->userHeaderMode;
				}
				bufferAction = BUFF_NO_ACTION;
			}
			if (isOdd(ztransfer->userHeaderNumber)) {
				status = zgetBuff(ifltab, ztransfer->info[zdssInfoKeys.kinfoUserHeadAddress], ztransfer->userHeader,
					ztransfer->userHeaderNumber+1, 1, bufferAction, bufferControl, buffer);
			}
			else {
				status = zgetBuff(ifltab, ztransfer->info[zdssInfoKeys.kinfoUserHeadAddress], ztransfer->userHeader,
					ztransfer->userHeaderNumber, 1, bufferAction, bufferControl, buffer);
			}
			if (zisError(status)) {
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_zreadInternal_ID);
			}
		}
	}

	if (ztransfer->values1Mode) {
		ztransfer->values1Number = (int)ztransfer->info[zdssInfoKeys.kinfoValues1Number];
		if (ztransfer->values1Number > 0) {
			if (ztransfer->values1Mode == 1) {
				number = ztransfer->values1Number;
				if (isOdd(number)) number++;
				ztransfer->values1 = (int *)calloc((size_t)number, (size_t)4);
				if (!ztransfer->values1) {
					return zerrorProcessing(ifltab, DSS_FUNCTION_zreadInternal_ID,
											zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, ztransfer->values1Number, 0,
											zdssErrorSeverity.MEMORY_ERROR, ztransfer->pathname, "Allocating values1");
				}
				ztransfer->allocated[zSTRUCT_TRANS_values1] = 1;
				bufferAction = BUFF_READ;
			}
			else {
				number = ztransfer->values1Number - ztransfer->values1Mode;
				if (number > 0) {
					ztransfer->insufficientSpace += number;
					ztransfer->values1Number = ztransfer->values1Mode;
				}
				bufferAction = BUFF_NO_ACTION;
			}
			status = zgetBuff(ifltab, ztransfer->info[zdssInfoKeys.kinfoValues1Address], ztransfer->values1,
							  ztransfer->values1Number, 1, bufferAction, bufferControl, buffer);
			if (zisError(status)) {
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_zreadInternal_ID);
			}
		}
	}

	if (ztransfer->values2Mode) {
		ztransfer->values2Number = (int)ztransfer->info[zdssInfoKeys.kinfoValues2Number];
		if (ztransfer->values2Number > 0) {
			if (ztransfer->values2Mode == 1) {
				number = ztransfer->values2Number;
				if (isOdd(number)) number++;
				ztransfer->values2 = (int *)calloc((size_t)number, (size_t)4);
				if (!ztransfer->values2) {
					return zerrorProcessing(ifltab, DSS_FUNCTION_zreadInternal_ID,
											zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, ztransfer->values2Number, 0,
											zdssErrorSeverity.MEMORY_ERROR, ztransfer->pathname, "Allocating values2");
				}
				ztransfer->allocated[zSTRUCT_TRANS_values2] = 1;
				bufferAction = BUFF_READ;
			}
			else {
				number = ztransfer->values2Number - ztransfer->values2Mode;
				if (number > 0) {
					ztransfer->insufficientSpace += number;
					ztransfer->values2Number = ztransfer->values2Mode;
				}
				bufferAction = BUFF_NO_ACTION;
			}
			status = zgetBuff(ifltab, ztransfer->info[zdssInfoKeys.kinfoValues2Address], ztransfer->values2,
							  ztransfer->values2Number, 1, bufferAction, bufferControl, buffer);
			if (zisError(status)) {
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_zreadInternal_ID);
			}
		}
	}

	if (ztransfer->values3Mode) {
		ztransfer->values3Number = (int)ztransfer->info[zdssInfoKeys.kinfoValues3Number];
		if (ztransfer->values3Number > 0) {
			if (ztransfer->values3Mode == 1) {
				number = ztransfer->values3Number;
				if (isOdd(number)) number++;
				ztransfer->values3 = (int *)calloc((size_t)number, (size_t)4);
				if (!ztransfer->values3) {
					return zerrorProcessing(ifltab, DSS_FUNCTION_zreadInternal_ID,
											zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, ztransfer->values3Number, 0,
											zdssErrorSeverity.MEMORY_ERROR, ztransfer->pathname, "Allocating values3");
				}
				ztransfer->allocated[zSTRUCT_TRANS_values3] = 1;
				bufferAction = BUFF_READ;
			}
			else {
				number = ztransfer->values3Number - ztransfer->values3Mode;
				if (number > 0) {
					ztransfer->insufficientSpace += number;
					ztransfer->values3Number = ztransfer->values3Mode;
				}
				bufferAction = BUFF_NO_ACTION;
			}
			status = zgetBuff(ifltab, ztransfer->info[zdssInfoKeys.kinfoValues3Address], ztransfer->values3,
							  ztransfer->values3Number, 1, bufferAction, bufferControl, buffer);
			if (zisError(status)) {
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_zreadInternal_ID);
			}
		}
	}

	ztransfer->numberValues = (int)ztransfer->info[zdssInfoKeys.kinfoNumberData];
	ztransfer->logicalNumberValues = (int)ztransfer->info[zdssInfoKeys.kinfoLogicalNumber];
	ztransfer->totalAllocatedSize = (int)ztransfer->info[zdssInfoKeys.kinfoAllocatedSize];

	ztransfer->lastWrittenTime = ztransfer->info[zdssInfoKeys.kinfoLastWriteTime];
	ztransfer->fileLastWrittenTime = zgetLastWriteTimeFile(ifltab);
	charLong(&ztransfer->info[zdssInfoKeys.kinfoProgram], ztransfer->programName, zdssVals.numberProgram,sizeof(ztransfer->programName), 0, 1);

	if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d", status);
		zmessageDebug(ifltab, DSS_FUNCTION_zreadInternal_ID, "Exit; status: ", messageString);
		zmessageDebug(ifltab, DSS_FUNCTION_zreadInternal_ID, "Pathname: ", ztransfer->pathname);
	}

	return status;
}

