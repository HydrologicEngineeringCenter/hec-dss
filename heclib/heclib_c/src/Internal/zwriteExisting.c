#include <stdio.h>
#include <string.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "zerrorCodes.h"


/**
*  Function:	zwriteExisting
*
*  Use:			Private (Internal)
*
*  Description:	Prepares pointers and allocates space for over writing an existing record.
*
*  Declaration: int zwriteExisting(long long *ifltab, zStructTransfer* ztransfer,
*							  long long bufferControl[4], int *buffer,
*							  int bufferAction, int *wroteAtEOF);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				zStructTransfer* ztransfer
*					A struct that contains data for this record.
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
*				int bufferAction
*					A flag that indicates if buffering is to be bypassed.
*
*				int *wroteAtEOF (output)
*					Returns "1" if this write is at the end of the file, "0" if not.  This tells
*					zwriteInternal to write an EOF flag following the data set.
*
*
*  zStructTransfer Parameters Used:
*
*				const char* pathname
*					The pathname of the record to written.  The pathname must be exact, but is case insensitive
*					(i.e., "Apart" is the same as "APART")
*
*				int internalHeaderNumber
*					The number of integer (int*4) words in the internal header array.
*
*				int header2Number
*					The number of integer (int*4) words in the second header array.
*
*				int userHeaderNumber
*					The number of integer (int*4) words in the user header array
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
*				int values1Number
*					The number of integer (int*4) words to write for the values1 array.
*
*				int values2Number
*					The number of integer (int*4) words to write for the values2 array.
*
*				int values3Number
*					The number of integer (int*4) words to write for the values3 array.
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
*  Called By:		zwriteInternal only
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zwriteExisting(long long *ifltab, zStructTransfer* ztransfer,
			  long long bufferControl[4], int *buffer, int bufferAction, int *wroteAtEOF)

{
	int status;
	int numberInfo;
	int originalSize;
	int newSize;
	int longSize;
	int temp;
	int diff;
	int type;
	int version;
	int expansion;
	int expansionNumber;
	int expansionSize;
	int expansionFlag;
	int julianFirst;
	int julianLast;
	int total;
	int totalNumberInts;

	char messageString[120];

	long long icurrentTime;
	long long address;
	long long *info;
	long long *fileHeader;


	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zwriteExisting_ID, "Pathname: ", ztransfer->pathname);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d;", zhandle(ifltab));
		zmessageDebug(ifltab, DSS_FUNCTION_zwriteExisting_ID, "Handle: ", messageString);
	}

	status = STATUS_OKAY;
	//  The total number of int values is the long number *2 of each
	//  to take care of addressing.  values2 must start on a long boundary.
	//  e.g., 3 floats take 2 longs, and 2 longs * 2 ints/long take 4 ints of space
	totalNumberInts = numberLongsInInts(ztransfer->values1Number) + numberLongsInInts(ztransfer->values2Number) +
		numberLongsInInts(ztransfer->values3Number);
	totalNumberInts *= 2;

	//  Use the same last written (current time) for record and file header
	icurrentTime = getCurrentTimeMillis();
	fileHeader[zdssFileKeys.klastWriteTime] = icurrentTime;
	ifltab[zdssKeys.kmyLastWriteTime] = fileHeader[zdssFileKeys.klastWriteTime];
	//  Update pathname bin
	ifltab[zdssKeys.kbinLastWrite] = icurrentTime;
	//  Update the first and last julian in the pathname bin, if needed
	//  These dates were put here by a time series write function
	i8toi4(ifltab[zdssKeys.kdataFirstDate], &julianFirst, &temp);
	i8toi4(ifltab[zdssKeys.kdataLastDate],  &julianLast,  &temp);
	ifltab[zdssKeys.kbinDates] = i4toi8(julianFirst, julianLast);
	address = ifltab[zdssKeys.kinfoAddInBin] - zdssBinKeys.kbinInfoAdd +  zdssBinKeys.kbinLastWrite;

	status = zput(ifltab, address, (int *)&ifltab[zdssKeys.kbinLastWrite], 2, 2);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zwriteExisting_ID);
	}


	info = (long long *)ifltab[zdssKeys.kinfo];
	//  Check that we are writing the same data type.
	//  You cannot overwrite a different data type - must first delete the record
	i8toi4(info[zdssInfoKeys.kinfoTypeVersion], &type, &version);
	
	if (type != ztransfer->dataType) {

		int isSupported = 0;
		if (type == 100 && ztransfer->dataType == 105) {
			isSupported = 1;
		}
		if (type == 105 && ztransfer->dataType == 100) {
			isSupported = 1;
		}
		
		if (!isSupported) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_zwriteExisting_ID,
				zdssErrorCodes.DIFFERENT_RECORD_TYPE, type, (long long)ztransfer->dataType,
				zdssErrorSeverity.WRITE_ERROR, ztransfer->pathname, "");
		}
	}

	//  Be sure we don't try to store negative lengths
	if (ztransfer->internalHeaderNumber < 0) ztransfer->internalHeaderNumber = 0;
	if (ztransfer->header2Number < 0) ztransfer->header2Number = 0;
	if (ztransfer->userHeaderNumber < 0) ztransfer->userHeaderNumber = 0;
	if (ztransfer->values1Number < 0) ztransfer->values1Number = 0;
	if (ztransfer->values2Number < 0) ztransfer->values2Number = 0;
	if (ztransfer->values3Number < 0) ztransfer->values3Number = 0;
	if (ztransfer->numberValues < 0) ztransfer->numberValues = 0;

	ifltab[zdssKeys.kbinStatus] = 1;  //  Mark as good
	//  Update perm area file statistics
	//  This just indicates how many records use an array type (e.g., header 2)
	//  If the existing record doesn't have it, but the updated one does, increment the counter
	//  (we won't worry about decrementing, if removing a header; this is just for file statics.)
	if ((info[zdssInfoKeys.kinfoInternalHeadNumber] == 0) && (ztransfer->internalHeaderNumber > 0))	fileHeader[zdssFileKeys.knumberInternalHeader]++;
	if ((info[zdssInfoKeys.kinfoHeader2Number] == 0) && (ztransfer->header2Number > 0))				fileHeader[zdssFileKeys.knumberHeader2]++;
	if ((info[zdssInfoKeys.kinfoUserHeadNumber] == 0) && (ztransfer->userHeaderNumber > 0))			fileHeader[zdssFileKeys.knumberUserHeader]++;
	if ((info[zdssInfoKeys.kinfoValues1Number] == 0) && (ztransfer->values1Number > 0))				fileHeader[zdssFileKeys.knumberDataArea1]++;
	if ((info[zdssInfoKeys.kinfoValues2Number] == 0) && (ztransfer->values2Number > 0))				fileHeader[zdssFileKeys.knumberDataArea2]++;
	if ((info[zdssInfoKeys.kinfoValues3Number] == 0) && (ztransfer->values3Number > 0))				fileHeader[zdssFileKeys.knumberDataArea3]++;
	numberInfo = zdssVals.infoSize + numberLongsInBytes((int)ifltab[zdssKeys.klenLastPath]);

	//  Compute original and new record sizes.
	originalSize =
		(int)info[zdssInfoKeys.kinfoInternalHeadNumber] +
		(int)info[zdssInfoKeys.kinfoHeader2Number] +
		(int)info[zdssInfoKeys.kinfoUserHeadNumber] +
		(int)info[zdssInfoKeys.kinfoAllocatedSize];
	newSize =
		ztransfer->internalHeaderNumber +
		ztransfer->header2Number +
		ztransfer->userHeaderNumber +
		ztransfer->totalAllocatedSize;


	if (newSize > originalSize) {
		//  Need to expand!
/**/		if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			zmessageDebug(ifltab, DSS_FUNCTION_zwriteExisting_ID, "Record size has increased - Expanding", "");
			longSize = numberLongsInInts(newSize);
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d;   New Size: %d 32-bit words;  %d 64-bit words",
				originalSize, newSize, longSize);
			zmessageDebug(ifltab, DSS_FUNCTION_zwriteExisting_ID,  "Old size: ", messageString);
			i8toi4(info[zdssInfoKeys.kinfoExpansion], &expansionNumber, &temp);
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d", expansionNumber);
			zmessageDebug(ifltab, DSS_FUNCTION_zwriteExisting_ID,  "Number of expansions to now: ", messageString);
/**/		}

		//  Save the iformation block with a moved status
		info[zdssInfoKeys.kinfoStatus] = REC_STATUS_MOVED;
		status = zput(ifltab, ifltab[zdssKeys.kaddInfoLastPath], (int *)info, numberInfo, 2);
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zwriteExisting_ID);
		}

		//  Check that the old info and data areas are contiguous.  If so, we can reclaim their space
		//  A rename op may have moved the info block
		if ((ifltab[zdssKeys.kaddInfoLastPath] + (long long)(originalSize + numberInfo)) ==
			(info[zdssInfoKeys.kinfoValues1Address] + info[zdssInfoKeys.kinfoAllocatedSize])) {
			zreleaseFileSpace(ifltab, ifltab[zdssKeys.kaddInfoLastPath], (originalSize + numberInfo));
		}
		else {
			//  this would have been done in release space
			fileHeader[zdssFileKeys.kdead] += (long long)(originalSize) + numberInfo;
		}

		//  Update the version and expansionNumber values
		i8toi4(info[zdssInfoKeys.kinfoExpansion], &expansionNumber, &expansionFlag);
		i8toi4(info[zdssInfoKeys.kinfoTypeVersion], &temp, &version);
		version++;  // Version number of record
		expansionNumber++;  //  Number times actually expanded

		//  Since we are expanding, expand more than the current write suggests,
		//  so that we can accommodate a limited number of future expansions
		expansion = expansionNumber;  //  Number of expansions that we can change to get best size
		if (expansion == 1) {
			//  Allocate space to 1/4 totally expanded space
			expansionSize = (ztransfer->totalExpandedSize / 4);
			if (expansionSize > ztransfer->totalAllocatedSize) {
				ztransfer->totalAllocatedSize = expansionSize;
			}
			else {
				//  Space needed is larger than the expansion size - move up
				expansion = 2;
			}
		}
		if (expansion == 2) {
			//  Allocate space to 1/2 totally expanded space
			expansionSize = (ztransfer->totalExpandedSize / 2);
			if (expansionSize > ztransfer->totalAllocatedSize) {
				ztransfer->totalAllocatedSize = expansionSize;
			}
			else {
				//  Space needed is larger than the expansion size - move up
				expansion = 3;
			}
		}
		if (expansion == 3) {
			//  Allocate space to the totally expanded space
			expansionSize = ztransfer->totalExpandedSize;
			if (expansionSize > ztransfer->totalAllocatedSize) {
				ztransfer->totalAllocatedSize = expansionSize;
			}
			else {
				//  Space needed is larger than the expansion size - move up
				expansion = 4;
			}
		}
		if (expansion >= 4) {
			//  Add an additional 10%
			expansionSize = (int)((float)ztransfer->totalExpandedSize * 1.10);
			if (expansionSize > ztransfer->totalAllocatedSize) {
				ztransfer->totalAllocatedSize = expansionSize;
			}
			if (totalNumberInts > ztransfer->totalAllocatedSize) {
				ztransfer->totalAllocatedSize = (int)((float)totalNumberInts * 1.10);
			}
		}
		//  Be sure we have enough room
		if (totalNumberInts > ztransfer->totalAllocatedSize) {
			ztransfer->totalAllocatedSize = totalNumberInts;
		}
		info[zdssInfoKeys.kinfoExpansion] = i4toi8 (expansionNumber, expansionFlag);
		info[zdssInfoKeys.kinfoTypeVersion] = i4toi8 (ztransfer->dataType, version);

		if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
				"%d,  New values allocated space: %d, Space actually used: %d",
				expansionNumber, ztransfer->totalAllocatedSize, totalNumberInts);
			zmessageDebug(ifltab, DSS_FUNCTION_zwriteExisting_ID, "Expansion level used: ", messageString);
		}

		//  We always want to keep the info and data contiguous
		//  This is important for squeezes, broken files, etc.
		//  and the size of the info is small in comparison.
		//  Renaming can be an exception to this

		//  Rewrite the pathname bin block with a pointer to the
		//  new info - at the end of the file
		total = numberInfo +
			numberLongsInInts(ztransfer->values3Number) +
			numberLongsInInts(ztransfer->internalHeaderNumber) +
			numberLongsInInts(ztransfer->header2Number) +
			numberLongsInInts(ztransfer->userHeaderNumber) +
			numberLongsInInts(ztransfer->totalAllocatedSize);
		//  We can use reclaimed space - even if it includes this same record
		address = zgetFileSpace(ifltab, total, 1, wroteAtEOF);
		ifltab[zdssKeys.kaddInfoLastPath] = address;
		fileHeader[zdssFileKeys.knumberExpansions] += 1;

		//  Write the address of the new information area in the existing pathname bin
		status = zput(ifltab, ifltab[zdssKeys.kinfoAddInBin], (int *)&ifltab[zdssKeys.kaddInfoLastPath], 1, 2);
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zwriteExisting_ID);
		}

		info[zdssInfoKeys.kinfoFlag] = DSS_INFO_FLAG;
		info[zdssInfoKeys.kinfoStatus] = REC_STATUS_PRIMARY;

		//  Store the internal header array location and length
		//  The header array starts just after the info area
		address += (long long)numberInfo;
		info[zdssInfoKeys.kinfoInternalHeadAddress] = address;
		info[zdssInfoKeys.kinfoInternalHeadNumber] = ztransfer->internalHeaderNumber;
		address += numberLongsInInts(ztransfer->internalHeaderNumber);

		//  Store the second internal header (compresssion info) array location and length
		info[zdssInfoKeys.kinfoHeader2Address] = address;
		info[zdssInfoKeys.kinfoHeader2Number] = ztransfer->header2Number;
		address += numberLongsInInts(ztransfer->header2Number);

		//  Store the user header array location and length
		info[zdssInfoKeys.kinfoUserHeadAddress] = address;
		info[zdssInfoKeys.kinfoUserHeadNumber] = ztransfer->userHeaderNumber;
		address += numberLongsInInts(ztransfer->userHeaderNumber);

		//  Store the first data array location and length
		info[zdssInfoKeys.kinfoValues1Address] = address;
		info[zdssInfoKeys.kinfoValues1Number] = ztransfer->values1Number;
		address += numberLongsInInts(ztransfer->values1Number);

		//  Store the second data array location and length
		info[zdssInfoKeys.kinfoValues2Address] = address;
		info[zdssInfoKeys.kinfoValues2Number] = ztransfer->values2Number;
		address += numberLongsInInts(ztransfer->values2Number);

		//  Store the third data array location and length
		info[zdssInfoKeys.kinfoValues3Address] = address;
		info[zdssInfoKeys.kinfoValues3Number] = ztransfer->values3Number;
		//  address += numberLongsInInts(ztransfer->values3Number);  (not used)

		info[zdssInfoKeys.kinfoAllocatedSize] = ztransfer->totalAllocatedSize;
		info[zdssInfoKeys.kinfoNumberData] = ztransfer->numberValues;
		if (ztransfer->logicalNumberValues > 0) {
			info[zdssInfoKeys.kinfoLogicalNumber] = ztransfer->logicalNumberValues;
		}
		else {
			info[zdssInfoKeys.kinfoLogicalNumber] = ztransfer->numberValues;
		}
	}
	else {
		if (newSize < originalSize) {
			//  Don't let the record size shrink - keep left over
			//  space in the allocated space area
			//  (The allocated space on input should be the data size,
			//  not the size allocated in the earlier record.)
			diff = originalSize - newSize;
			ztransfer->totalAllocatedSize += diff;
		}
		if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
				"%d,  Original size: %d", newSize, originalSize);
			zmessageDebug(ifltab, DSS_FUNCTION_zwriteExisting_ID, "Current record size is adequate, size: ", messageString);
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
				"%d,  Space actually used: %d",
				ztransfer->totalAllocatedSize, totalNumberInts);
			zmessageDebug(ifltab, DSS_FUNCTION_zwriteExisting_ID, "Values (data) allocated size: ", messageString);
		}

		//  Store the internal header array location and length
		//info[zdssInfoKeys.kinfoInternalHeadAddress] = info[zdssInfoKeys.kinfoInternalHeadAddress];
		info[zdssInfoKeys.kinfoInternalHeadNumber] = ztransfer->internalHeaderNumber;

		//  Store the second internal header (compression info) array location and length
		address = info[zdssInfoKeys.kinfoInternalHeadAddress] + numberLongsInInts(ztransfer->internalHeaderNumber);
		info[zdssInfoKeys.kinfoHeader2Address] = address;
		info[zdssInfoKeys.kinfoHeader2Number] = ztransfer->header2Number;
		address += numberLongsInInts(ztransfer->header2Number);

		//  Store the user header array location and length
		info[zdssInfoKeys.kinfoUserHeadAddress] = address;
		info[zdssInfoKeys.kinfoUserHeadNumber] = ztransfer->userHeaderNumber;
		address += numberLongsInInts(ztransfer->userHeaderNumber);

		//  Store the first data array location and length
		info[zdssInfoKeys.kinfoValues1Address] = address;
		info[zdssInfoKeys.kinfoValues1Number] = ztransfer->values1Number;
		address += numberLongsInInts(ztransfer->values1Number);

		//  Store the second data array location and length
		info[zdssInfoKeys.kinfoValues2Address] = address;
		info[zdssInfoKeys.kinfoValues2Number] = ztransfer->values2Number;
		address += numberLongsInInts(ztransfer->values2Number);

		//  Store the third data array location and length
		info[zdssInfoKeys.kinfoValues3Address] = address;
		info[zdssInfoKeys.kinfoValues3Number] = ztransfer->values3Number;
		//  address += numberLongsInInts(ztransfer->values3Number);

		info[zdssInfoKeys.kinfoAllocatedSize] = ztransfer->totalAllocatedSize;
		info[zdssInfoKeys.kinfoNumberData] = ztransfer->numberValues;
		if (ztransfer->logicalNumberValues > 0) {
			info[zdssInfoKeys.kinfoLogicalNumber] = ztransfer->logicalNumberValues;
		}
		else {
			info[zdssInfoKeys.kinfoLogicalNumber] = ztransfer->numberValues;
		}

		i8toi4(info[zdssInfoKeys.kinfoTypeVersion], &temp, &version);
		version++;  // Version number of record
		info[zdssInfoKeys.kinfoTypeVersion] = i4toi8 (ztransfer->dataType, version);
		*wroteAtEOF = 0;
	}

	//  Store the data type, version number, date, time
	info[zdssInfoKeys.kinfoLastWriteTime] = icurrentTime;  // date/time record last written

	//  Store program name that last wrote this
	charLong(zdssVals.cprogramName, &info[zdssInfoKeys.kinfoProgram], 0, zdssVals.numberProgram, 1, 1);
	info[zdssInfoKeys.kinfoFirstDate] = ifltab[zdssKeys.kdataFirstDate];  // date and time of first valid value
	info[zdssInfoKeys.kinfoLastDate] = ifltab[zdssKeys.kdataLastDate];

	//  Now write the information area to disk
	ifltab[zdssKeys.kinfoAddress] = ifltab[zdssKeys.kaddInfoLastPath];
	ifltab[zdssKeys.kinfoSize] = numberInfo;
	status = zputBuff(ifltab, ifltab[zdssKeys.kaddInfoLastPath], (int *)info, numberInfo, 2, bufferAction, bufferControl, buffer);
	if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zwriteExisting_ID);
	}

	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
		i8toi4(info[zdssInfoKeys.kinfoTypeVersion], &ztransfer->dataType, &version);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%lld; Data Address %lld; Data Type %d",
			ifltab[zdssKeys.kinfoAddress], info[zdssInfoKeys.kinfoValues1Address], ztransfer->dataType);
		zmessageDebug(ifltab, DSS_FUNCTION_zwriteExisting_ID, "Exit; Info address: ", messageString);
	}

	return status;
}

