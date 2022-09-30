#include <stdio.h>

#include "heclib.h"
#include "zdssKeys.h"
#include "zdssLocking.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"
#include "zerrorCodes.h"


/**
*  Function:	zwriteInternal
*
*  Use:			Private (Internal)
*
*  Description:	Writes a record (identified by a pathname), and all components
*
*  Declaration: int zwriteInternal(long long *ifltab, zStructTransfer* ztransfer, int checked,
*								   long long bufferControl[4], int *buffer, int boolUseBuffer);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				const char* pathname
*					The pathname of the record to written.  The pathname must be exact, but is case insensitive
*					(i.e., "Apart" is the same as "APART")
*
*				int checked
*					A flag indicating if the info block has just be read (and doesn't need to be re-read.)
*					0:	No, it has not been read
*					1:  Yes, but the record was not found (a new record)
*					2:  Yes, and it was loaded into memory (an existing or old record)
*					Failsafe - if in doubt, 0 will aways work
*
*				long long bufferControl[4] (optional)
*					An int*8 array dimensioned to 4 to hold pointers/information used in buffering.
*					This array should be zeroed out before using, except for the first value.
*					The first element has to be set to the size of buffer in int*4 words.
*						bufferControl[BUFF_SIZE] is (max) int*4 size
*						bufferControl[BUFF_STAT] is write status;  0 - unused, 1 - not dirty (read only), 2 - dirty (needs to write)
*						bufferControl[BUFF_ADDRESS] is file address (int*8)
*						bufferControl[BUFF_INTS_USED] is current int number loaded or used
*					If buffering is not used, then this must still be a long long int*8[4] array with all elements set to zero.
*
*				int *buffer (optional)
*					The integer array to hold buffered data.  The integer*4 size must be specified in bufferControl[BUFF_SIZE].
*					To be effective, the size should be at least the combined size of the info block, header arrays and values array.
*					If buffering is not used, this should be a pointer and bufferControl zeroed out.
*
*
*  zStructTransfer parameters used in this call:
*
*				int *internalHeader
*					The internal header array to write.  This usually contains information about the data, such as units.
*
*				int internalHeaderNumber
*					The number of integer (int*4) words to write in the internal header array.
*
*				int *header2
*					The second header array to write.  This usually contains compression information.
*
*				int header2Number
*					The number of integer (int*4) words to write in the second header array.
.
*
*				int *userHeader
*					The user's header array to write.  This is additional information that the user wishes to save.
*
*				int userHeaderNumber
*					The number of integer (int*4) words to write in the user header array
*
*				void *values1
*					The primary data array to store.  This can be ints, floats, doubles, or multiples
*
*				int values1Number
*					The number of integer (int*4) words to write for the values1 array.
*					If this number is negative, then the single values1 is written (filled) (-)values1Number times.
*
*				void *values2
*					The secondary data array to store.  This can be ints, floats, doubles, or multiples
*
*				int values2Number
*					The number of integer (int*4) words to write for the values2 array.
*					If this number is negative, then the single values2 is written (filled) (-)values2Number times.
*
*				void *values3
*					The third data array to store.  This can be ints, floats, doubles, or multiples
*
*				int values3Number
*					The number of integer (int*4) words to write for the values3 array.
*					If this number is negative, then the single values3 is written (filled) (-)values3Number times.
*
*				int numberValues
*					The number of values to store, in whatever size values is given in.
*					For example, if you are storing 100 double values, then this would be 100.
*					This number is informational only and not used for storage.
*
*				int logicalNumberValues
*					The number of values to report to the user, in whatever size values is given in.
*					The logical number is the fully expanded data set and includes missing flags.
*					The difference between numberValues and logicalNumberValues is that numberValues
*					might not include trailing missing place holders and logicalNumberValues does.
*					For example, if you are only storing 30 daily values, numberValues would be 30,
*					but logicalNumberValues would be 365, the entire logical record size.
*					This number is informational only and not used for storage.

*
*				int totalAllocatedSize (Optional)
*					The number of integer (int 4) words to allocate for the data area (values)
*					for this record.  If you are expecting this record to grow over time, then
*					you can allocate additional space to accommodate that growth (without having
*					to re-write the record at the end of the file.)  This parameter is used
*					to allocate additional space when a record is expanding.
*					For example, if storing hourly real-time reporting data, and you are
*					at the beginning of the month, most of the data for the month will be missing place
*					holders, which will be removed by data compression and lesser space will be
*					allocated on disk.  This value is the amount to allocate to accommodate some
*					of the expected expansion.  (Usually  totalAllocatedSize < totalExpandedSize)
*					If too small of a value is used, an expanded record will just be written at
*					the end of the file (less efficient).  Too large will just waste some space.
*					Set to 0 (zero) to use default (values1Number), with no additional expansion space.
*
*				int totalExpandedSize (Optional)
*					The number of integer (int 4) words that the record would be if totally expanded.
*					This parameter is used to limit the amount of space to allocate, if the record is
*					expanding.  For example, if storing hourly real-time reporting data, and you are
*					at the beginning of the month, most of the data for the month will be missing place
*					holders, which will be removed by data compression and lesser space will be
*					allocated on disk.  This value is the total amount of space needed if there were
*					no missing place holders and the data was not compressed.  The amount to allocate
*					will not be more than this, unless values1Number is greater than this.
*					If too small of a value is used, an expanded record will just be written at
*					the end of the file (less efficient).  Too large will just waste some space.
*					Set to 0 (zero) to use default (values1Number), when you expect no expansions.
*
*				int dataType
*					The record type flag (e.g., regular interval time-series), as defined in zdssVals.h
*
*
*
*	Returns:	int status
*					STATUS_OKAY for successful operation.
*					errorCode for error
*					errorCode may be decoded by function zerrorDecode()
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int zwriteInternal(long long *ifltab, zStructTransfer* ztransfer, int checked,
				   long long bufferControl[4], int *buffer, int boolUseBuffer)

{
	char messageString[100];
	int status;
	int version;
	int temp;
	long long *info;
	long long *fileHeader;
	int totalNumber;
	int wroteAtEOF;
	int totalNumberInts;
	int zero = 0;
	int bufferAction;
	int boolFillValues1 = 0;
	int boolFillValues2 = 0;
	int boolFillValues3 = 0;


	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];

	//  Are we filling (repeating) space?
	if (ztransfer->values1Number < 0) {
		ztransfer->values1Number = -ztransfer->values1Number;
		boolFillValues1 = 1;
	}
	if (ztransfer->values2Number < 0) {
		ztransfer->values2Number = -ztransfer->values2Number;
		boolFillValues2 = 1;
	}
	if (ztransfer->values3Number < 0) {
		ztransfer->values3Number = -ztransfer->values3Number;
		boolFillValues3 = 1;
	}
	if (boolUseBuffer) {
		bufferAction = BUFF_WRITE;
	}
	else {
		bufferAction = BUFF_NO_ACTION;
	}

	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zwriteInternal_ID, "Enter; handle: ", zhandle(ifltab));
		zmessageDebug(ifltab, DSS_FUNCTION_zwriteInternal_ID, "Pathname: ", ztransfer->pathname);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d,  header2 %d, User header %d",
			ztransfer->internalHeaderNumber, ztransfer->header2Number, ztransfer->userHeaderNumber);
		zmessageDebug(ifltab, DSS_FUNCTION_zwriteInternal_ID, "Internal header: ", messageString);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d,  Logical Number Data: %d",
			ztransfer->numberValues, ztransfer->logicalNumberValues);
		zmessageDebug(ifltab, DSS_FUNCTION_zwriteInternal_ID, "Number Data Values: ", messageString);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d,  Size second data array: %d,  Size third data array: %d",
			ztransfer->values1Number, ztransfer->values2Number, ztransfer->values3Number);
		zmessageDebug(ifltab, DSS_FUNCTION_zwriteInternal_ID, "Size first data array:  ", messageString);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zwriteInternal_ID, "Total Allocated Space:  ", ztransfer->totalAllocatedSize);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d,   File size: %lld",
			ztransfer->dataType, fileHeader[zdssFileKeys.kfileSize]);
		zmessageDebug(ifltab, DSS_FUNCTION_zwriteInternal_ID, "Data type: ",messageString);
		if ((bufferControl[BUFF_SIZE] == 0) || !boolUseBuffer) {
			zmessageDebug(ifltab, DSS_FUNCTION_zwriteInternal_ID, "Buffering set Off","");
		}
		else {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d,   Buffer status: %d,   Number currently in use: %d",
				(int)bufferControl[BUFF_SIZE], (int)bufferControl[BUFF_STAT], (int)bufferControl[BUFF_INTS_USED]);
			zmessageDebug(ifltab, DSS_FUNCTION_zwriteInternal_ID, "Buffering on, Buffer size: ",messageString);
		}
		if (boolFillValues1) {
			zmessageDebug(ifltab, DSS_FUNCTION_zwriteInternal_ID, "Data array 1 in fill mode.","");
		}
		if (boolFillValues2) {
			zmessageDebug(ifltab, DSS_FUNCTION_zwriteInternal_ID, "Data array 2 in fill mode.","");
		}
		if (boolFillValues3) {
			zmessageDebug(ifltab, DSS_FUNCTION_zwriteInternal_ID, "Data array 3 in fill mode.","");
		}
	}

	//  Be sure we have write access
	if (zgetVersion(ifltab) != 7) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zwriteInternal_ID, zdssErrorCodes.NOT_OPENED,
			0, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS, ztransfer->pathname, "");
	}
	if (ifltab[zdssKeys.kopenStatus] != OPEN_STAT_WRITE) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zwriteInternal_ID,
			zdssErrorCodes.WRITE_ON_READ_ONLY, 0, 0,
			zdssErrorSeverity.WARNING_NO_WRITE_ACCESS, ztransfer->pathname, "");
	}


	//  Lock the file if we are in a multi-user access mode
	//  And read the file header
	status =  zlockActive(ifltab, LOCKING_LEVEL_LOW, LOCKING_LOCK_ON, LOCKING_FLUSH_ON);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zwriteInternal_ID);
	}

	//  The total number of int values is the long number *2 of each
	//  to take care of addressing.  values2 must start on a long boundary.
	//  e.g., 3 floats take 2 longs, and 2 longs * 2 ints/long take 4 ints of space
	totalNumberInts = numberLongsInInts(ztransfer->values1Number) + numberLongsInInts(ztransfer->values2Number) +
		numberLongsInInts(ztransfer->values3Number);
	totalNumberInts *= 2;
	if (totalNumberInts > ztransfer->totalAllocatedSize) {
		ztransfer->totalAllocatedSize = totalNumberInts;
	}


	//  Check to see if area lengths are greater than max for this file
	if (ztransfer->internalHeaderNumber > (int)fileHeader[zdssFileKeys.kmaxInternalHeader]) {
		fileHeader[zdssFileKeys.kmaxInternalHeader] = (long long)ztransfer->internalHeaderNumber;
	}
	if (ztransfer->header2Number > (int)fileHeader[zdssFileKeys.kmaxHeader2]) {
		fileHeader[zdssFileKeys.kmaxHeader2] = (long long)ztransfer->header2Number;
	}
	if (ztransfer->userHeaderNumber > (int)fileHeader[zdssFileKeys.kmaxUserHeader]) {
		fileHeader[zdssFileKeys.kmaxUserHeader] = (long long)ztransfer->userHeaderNumber;
	}
	if (ztransfer->values1Number > (int)fileHeader[zdssFileKeys.kmaxValues1Size]) {
		fileHeader[zdssFileKeys.kmaxValues1Size] = (long long)ztransfer->values1Number;
	}
	if (ztransfer->values2Number > (int)fileHeader[zdssFileKeys.kmaxValues2Size]) {
		fileHeader[zdssFileKeys.kmaxValues2Size] = (long long)ztransfer->values2Number;
	}
	if (ztransfer->values3Number > (int)fileHeader[zdssFileKeys.kmaxValues3Size]) {
		fileHeader[zdssFileKeys.kmaxValues3Size] = (long long)ztransfer->values3Number;
	}
	//  Now the total amount of space - info area
	totalNumber = numberLongsInInts(ztransfer->internalHeaderNumber) +
		numberLongsInInts(ztransfer->header2Number) +
		numberLongsInInts(ztransfer->userHeaderNumber) +
		numberLongsInInts(ztransfer->values1Number) +
		numberLongsInInts(ztransfer->values2Number) +
		numberLongsInInts(ztransfer->values3Number);
	if (totalNumber > (int)fileHeader[zdssFileKeys.kmaxRecordSize]) {
		fileHeader[zdssFileKeys.kmaxRecordSize] = (long long)totalNumber;
	}


	//  Save the max total size in the perm section for this data type
	if (ztransfer->dataType == DATA_TYPE_RTS) {
		if (totalNumber > fileHeader[zdssFileKeys.kmaxRtsSize]) fileHeader[zdssFileKeys.kmaxRtsSize] = totalNumber;
	}
	else if (ztransfer->dataType == DATA_TYPE_RTD) {
		if (totalNumber > fileHeader[zdssFileKeys.kmaxRtdSize]) fileHeader[zdssFileKeys.kmaxRtdSize] = totalNumber;
	}
	else if (ztransfer->dataType == DATA_TYPE_ITS) {
		if (totalNumber > fileHeader[zdssFileKeys.kmaxItsSize]) fileHeader[zdssFileKeys.kmaxItsSize] = totalNumber;
	}
	else if (ztransfer->dataType == DATA_TYPE_ITD) {
		if (totalNumber > fileHeader[zdssFileKeys.kmaxItdSize]) fileHeader[zdssFileKeys.kmaxItdSize] = totalNumber;
	}
	else if (ztransfer->dataType == DATA_TYPE_PD) {
		if (totalNumber > fileHeader[zdssFileKeys.kmaxPdSize]) fileHeader[zdssFileKeys.kmaxPdSize] = totalNumber;
	}
	else if (ztransfer->dataType == DATA_TYPE_PDD) {
		if (totalNumber > fileHeader[zdssFileKeys.kmaxPdSize]) fileHeader[zdssFileKeys.kmaxPdSize] = totalNumber;
	}
	else  {
		if (totalNumber > fileHeader[zdssFileKeys.kmaxOtherSize]) fileHeader[zdssFileKeys.kmaxOtherSize] = totalNumber;
	}



	//  Read info block if we need to
	if (checked == 0) {
		status = zreadInfo(ifltab, ztransfer->pathname, 0);
		if (zisError(status)) {
			//  Error occured; update and return
			zlockActive(ifltab, LOCKING_LEVEL_LOW, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zwriteInternal_ID);
		}
	}
	else if (checked == 1) {
		status = STATUS_RECORD_NOT_FOUND;
	}
	else {  //  if (checked == 2) {
		status = STATUS_RECORD_FOUND;
	}

	// Allocate or update space and sizes
	if (status == STATUS_RECORD_NOT_FOUND) {
		//  New record
		//  Create record location and addresses to it.
		status = zwriteNew(ifltab, ztransfer,
			bufferControl, buffer, bufferAction, &wroteAtEOF);

	}
	else {
		// existing record
		//  Update sizes
		status = zwriteExisting(ifltab, ztransfer,
			bufferControl, buffer, bufferAction, &wroteAtEOF);
	}


	if (zisError(status)) {
		//  Error code generated in the lower functions
		zlockActive(ifltab, LOCKING_LEVEL_LOW, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zwriteInternal_ID);
	}

	info = (long long *)ifltab[zdssKeys.kinfo];


	// Now the record location on disk is ready.
	// Store the headers and data arrays

	//  Write header areas

	if ((ztransfer->internalHeaderNumber > 0) && ztransfer->internalHeader) {
		if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
			zmessageDebug(ifltab, DSS_FUNCTION_zwriteInternal_ID, "Writing internal header array", "");
		}		
		status = zputBuff(ifltab, info[zdssInfoKeys.kinfoInternalHeadAddress], ztransfer->internalHeader,
			ztransfer->internalHeaderNumber, 1, bufferAction, bufferControl, buffer);
		if (status != STATUS_OKAY) return zerrorUpdate(ifltab, status, DSS_FUNCTION_zwriteInternal_ID);
	}

	if ((ztransfer->header2Number > 0) && ztransfer->header2) {
		if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
			zmessageDebug(ifltab, DSS_FUNCTION_zwriteInternal_ID, "Writing second header array", "");
		}
		status = zputBuff(ifltab, info[zdssInfoKeys.kinfoHeader2Address], ztransfer->header2,
			ztransfer->header2Number, 1, bufferAction, bufferControl, buffer);
		if (status != STATUS_OKAY) return zerrorUpdate(ifltab, status, DSS_FUNCTION_zwriteInternal_ID);
	}

	if ((ztransfer->userHeaderNumber > 0) && ztransfer->userHeader) {
		if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
			zmessageDebug(ifltab, DSS_FUNCTION_zwriteInternal_ID, "Writing user header array", "");
		}
		status = zputBuff(ifltab, info[zdssInfoKeys.kinfoUserHeadAddress], ztransfer->userHeader,
			ztransfer->userHeaderNumber, 1, bufferAction, bufferControl, buffer);
		if (status != STATUS_OKAY) return zerrorUpdate(ifltab, status, DSS_FUNCTION_zwriteInternal_ID);
	}

	//  Write data values
	if ((ztransfer->values1Number > 0) && ztransfer->values1) {
		if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
			zmessageDebug(ifltab, DSS_FUNCTION_zwriteInternal_ID, "Writing first data values array", "");
		}
		if (boolFillValues1) {
			if (bufferControl[0] > 0) {
				//  Cannot use buffer in fill mode
				status = zflushBuffers(ifltab, bufferControl, buffer);
			}
			status = zput(ifltab, info[zdssInfoKeys.kinfoValues1Address], ztransfer->values1,
						  -ztransfer->values1Number, 1);
		}
		else {
			status = zputBuff(ifltab, info[zdssInfoKeys.kinfoValues1Address], ztransfer->values1,
				ztransfer->values1Number, 1, bufferAction, bufferControl, buffer);
		}
		if (status != STATUS_OKAY) return zerrorUpdate(ifltab, status, DSS_FUNCTION_zwriteInternal_ID);
	}

	if ((ztransfer->values2Number > 0) && ztransfer->values2) {
		if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
			zmessageDebug(ifltab, DSS_FUNCTION_zwriteInternal_ID, "Writing second data values array", "");
		}
		if (boolFillValues2) {
			if (bufferControl[0] > 0) {
				//  Cannot use buffer in fill mode
				status = zflushBuffers(ifltab, bufferControl, buffer);
			}
			status = zput(ifltab, info[zdssInfoKeys.kinfoValues2Address], ztransfer->values2,
				-ztransfer->values2Number, 1);
		}
		else {
			status = zputBuff(ifltab, info[zdssInfoKeys.kinfoValues2Address], ztransfer->values2,
				ztransfer->values2Number, 1, bufferAction, bufferControl, buffer);
		}
		if (status != STATUS_OKAY) return zerrorUpdate(ifltab, status, DSS_FUNCTION_zwriteInternal_ID);
	}

	if ((ztransfer->values3Number > 0) && ztransfer->values3) {
		if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
			zmessageDebug(ifltab, DSS_FUNCTION_zwriteInternal_ID, "Writing third data values array", "");
		}
		if (boolFillValues3) {
			if (bufferControl[0] > 0) {
				//  Cannot use buffer in fill mode
				status = zflushBuffers(ifltab, bufferControl, buffer);
			}
			status = zput(ifltab, info[zdssInfoKeys.kinfoValues3Address], ztransfer->values3,
				-ztransfer->values3Number, 1);
		}
		else {
			status = zputBuff(ifltab, info[zdssInfoKeys.kinfoValues3Address], ztransfer->values3,
				ztransfer->values3Number, 1, bufferAction, bufferControl, buffer);
		}
		if (status != STATUS_OKAY) return zerrorUpdate(ifltab, status, DSS_FUNCTION_zwriteInternal_ID);
	}

	//  If more space is allocated than written, don't bother writing this area

	//  Flush buffers and perm area and unlock the file
	if (wroteAtEOF) {
		//  Write the EOF flag
		status = zwriteEOFandFlush(ifltab, bufferControl, buffer);
	}
	else {
		if (bufferControl[0] > 0) {
			//  Only need to flush if data was buffered.
			status = zflushBuffers(ifltab, bufferControl, buffer);
		}
	}
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zwriteInternal_ID);
	}

	//  Write perm area (if multi-user) and unlock
	status = zlockActive(ifltab, LOCKING_LEVEL_LOW, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);



	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_GENERAL)) {
		if (status == STATUS_OKAY) {
			i8toi4(info[zdssInfoKeys.kinfoTypeVersion], &temp, &version);
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, ZWRITE_MESS, zhandle(ifltab), version);
			zmessage2(ifltab, messageString, ztransfer->pathname);
		}
		else {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, ZWRITE_MESS_ERROR, zhandle(ifltab), status);
			zmessage2(ifltab, messageString, ztransfer->pathname);
		}
		if ((status == STATUS_OKAY) && zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%lld, Path bin: %lld, Info block: %lld, Data Array: %lld",
				ifltab[zdssKeys.kaddTableHash], ifltab[zdssKeys.kpathBinAddress], ifltab[zdssKeys.kaddInfoLastPath], info[zdssInfoKeys.kinfoValues1Address]);
			zmessageDebug(ifltab, DSS_FUNCTION_zwriteInternal_ID, "Table Hash: ", messageString);
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d, Size compression header: %d",
				ztransfer->internalHeaderNumber, ztransfer->header2Number);
			zmessageDebug(ifltab, DSS_FUNCTION_zwriteInternal_ID, "Size internal header: ", messageString);
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d, Size second data array: %d",
				ztransfer->values1Number, ztransfer->values2Number);
			zmessageDebug(ifltab, DSS_FUNCTION_zwriteInternal_ID, "Size first data array: ", messageString);
			zmessageDebug(ifltab, DSS_FUNCTION_zwriteInternal_ID, "Exit", "");
		}
	}

	//  Reset data lengths, if flagged for fill
	if (boolFillValues1) ztransfer->values1Number = -ztransfer->values1Number;
	if (boolFillValues2) ztransfer->values2Number = -ztransfer->values2Number;
	if (boolFillValues3) ztransfer->values3Number = -ztransfer->values3Number;

	if (status == STATUS_OKAY) {
		//  Check ifltab for corruption - this function has a higher risk because of the writes.
		zcheckKeys(ifltab);
		if (ifltab[zdssKeys.kerrorSevere]) {
			status = (int)ifltab[zdssKeys.kerrorCode];
		}
	}
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zwriteInternal_ID);
	}
	return status;
}

