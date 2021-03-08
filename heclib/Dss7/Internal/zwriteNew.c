
#include <stdio.h>
#include <string.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"


/**
*  Function:	zwriteNew
*
*  Use:			Private (Internal)
*
*  Description:	Prepares pointers and allocates space for writing a new record.
*
*  Declaration: int zwriteNew(long long *ifltab, zStructTransfer* ztransfer,
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


int zwriteNew(long long *ifltab, zStructTransfer* ztransfer,
			  long long bufferControl[4], int *buffer, int bufferAction, int *wroteAtEOF)
{
	int status;
	int numberInfo;
	int temp;
	int i;
	int julianFirst;
	int julianLast;
	int total;

	char messageString[120];

	long long icurrentTime;
	long long address;
	long long *info;
	long long *fileHeader;


	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zwriteNew_ID, "Pathname: ", ztransfer->pathname);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d;", zhandle(ifltab));
		zmessageDebug(ifltab, DSS_FUNCTION_zwriteNew_ID, "Handle: ", messageString);
	}

	//  Use the same last written (current time) for record and file header
	icurrentTime = getCurrentTimeMillis();
	fileHeader[zdssFileKeys.klastWriteTime] = icurrentTime;
	ifltab[zdssKeys.kmyLastWriteTime] = fileHeader[zdssFileKeys.klastWriteTime];
	ifltab[zdssKeys.kbinLastWrite] = icurrentTime;

	//  Update the first and last julian, if needed
	i8toi4(ifltab[zdssKeys.kdataFirstDate], &julianFirst, &temp);
	i8toi4(ifltab[zdssKeys.kdataLastDate],  &julianLast,  &temp);
	ifltab[zdssKeys.kbinDates] = i4toi8(julianFirst, julianLast);

	if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %lld;  julianFirst: %d, julianLast: %d",
			ifltab[zdssKeys.kbinDates], julianFirst, julianLast);
		zmessageDebug(ifltab, DSS_FUNCTION_zwriteNew_ID, "Combined Long date: ", messageString);
	}

	//  If this pathname's hash code was not in the hash table
	//  create a new bin, and save that bin's address in the table
	if (ifltab[zdssKeys.khashTableBinAdd] == 0) {
		//  Write a new pathname bin
		status = zbinNew(ifltab);
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zwriteNew_ID);
		}
		//  Write address of new pathname bin to hash location in table
		ifltab[zdssKeys.khashTableBinAdd] = ifltab[zdssKeys.kbinAddCurrent];
		status = zput(ifltab, ifltab[zdssKeys.kaddTableHash], (int *)&ifltab[zdssKeys.khashTableBinAdd], 1, 2);
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zwriteNew_ID);
		}
		fileHeader[zdssFileKeys.khashsUsed]++;
	}
	else {
		//  Did we stumble on a non-unique pathname hash code?  (rare)
		if (ifltab[zdssKeys.ksameHash]) {
			fileHeader[zdssFileKeys.khashCollisions]++;
			ifltab[zdssKeys.ksameHash] = 0;
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


	//  Now create the information block

	//  Update the file size to include this new block
	numberInfo = zdssVals.infoSize + numberLongsInBytes((int)ifltab[zdssKeys.klenLastPath]);
	if (numberInfo > zdssVals.maxInfoSize) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zwriteNew_ID, zdssErrorCodes.ARRAY_TOO_SMALL,
								numberInfo, (long long)zdssVals.maxInfoSize,
								zdssErrorSeverity.WARNING, ztransfer->pathname,
								"Information block allocated is too small");
	}
	total = numberInfo +
		numberLongsInInts(ztransfer->internalHeaderNumber) +
		numberLongsInInts(ztransfer->header2Number) +
		numberLongsInInts(ztransfer->userHeaderNumber) +
		numberLongsInInts(ztransfer->totalAllocatedSize);

	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%d,  Total size needed: %d",
			numberInfo, total);
		zmessageDebug(ifltab, DSS_FUNCTION_zwriteNew_ID, "Info block size: ", messageString);
	}
	address = zgetFileSpace(ifltab, total, 1, wroteAtEOF);

	//  Save the location of info in the pathname bin
	status = zbinUpdate(ifltab, ztransfer->pathname, address, 1, ztransfer->dataType);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zwriteNew_ID);
	}
	info = (long long *)ifltab[zdssKeys.kinfo];
	ifltab[zdssKeys.kinfoSize] = numberInfo;
	address += (long long)numberInfo;

	//  Zero out info area, to be sure it is clean
	for (i=0; i<numberInfo; i++) {
		info[i] = 0;
	}

	//  Fill in the info block
	info[zdssInfoKeys.kinfoFlag] = DSS_INFO_FLAG;
	info[zdssInfoKeys.kinfoStatus] = REC_STATUS_PRIMARY;
	info[zdssInfoKeys.kinfoPathnameLength] = ifltab[zdssKeys.klenLastPath];
	info[zdssInfoKeys.kinfoHash] = ifltab[zdssKeys.kpathnameHash];

	//  Store the internal header array location and length
	info[zdssInfoKeys.kinfoInternalHeadAddress] = address;
	info[zdssInfoKeys.kinfoInternalHeadNumber] = ztransfer->internalHeaderNumber;
	address += numberLongsInInts(ztransfer->internalHeaderNumber);

	//  Store the second internal header (compression info) array location and length
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

	//  Store the data type, version number, date, time
	info[zdssInfoKeys.kinfoTypeVersion] = i4toi8 (ztransfer->dataType, 1);
	info[zdssInfoKeys.kinfoExpansion] = i4toi8 (0, 0); // Expansion
	info[zdssInfoKeys.kinfoLastWriteTime] = icurrentTime;  // date/time record last written
	info[zdssInfoKeys.kinfoCreationTime] = icurrentTime;
	info[zdssInfoKeys.kinfoFirstDate] = ifltab[zdssKeys.kdataFirstDate];  // date and time of first valid value
	info[zdssInfoKeys.kinfoLastDate] = ifltab[zdssKeys.kdataLastDate];
	info[zdssInfoKeys.kinfoReserved] = 0;  // Reserved space

	//  Store program name that last wrote this
	charLong(zdssVals.cprogramName, &info[zdssInfoKeys.kinfoProgram], 0, zdssVals.numberProgram, 1, 1);
	//  Store the record password

	//  Store the pathname, make sure last cell is clear;
	charLong((void *)ztransfer->pathname, (void *)&info[zdssInfoKeys.kinfoPathname], 0, (int)ifltab[zdssKeys.klenLastPath], 1, 1);

	//  Now write the information area to disk
	ifltab[zdssKeys.kinfoAddress] = ifltab[zdssKeys.kaddInfoLastPath];
	ifltab[zdssKeys.kinfoSize] = numberInfo;
	status = zputBuff(ifltab, ifltab[zdssKeys.kaddInfoLastPath], (int *)info, numberInfo, 2, bufferAction, bufferControl, buffer);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zwriteNew_ID);
	}

	//  Update file statistics
	zupdatePathStats(ifltab, ztransfer->pathname, (size_t)ifltab[zdssKeys.klenLastPath]);
	fileHeader[zdssFileKeys.knumberRecords]++;
	fileHeader[zdssFileKeys.kcatSortNewWrites]++;
	if (ifltab[zdssKeys.kisaCollection]) {
		fileHeader[zdssFileKeys.knumberCollections]++;
	}
	if ((fileHeader[zdssFileKeys.kcatSortStatus] > 0) && (fileHeader[zdssFileKeys.kcatSortStatus] < 2)) {
		fileHeader[zdssFileKeys.kcatSortStatus] = 2;
	}
	if (info[zdssInfoKeys.kinfoInternalHeadNumber] > 0) fileHeader[zdssFileKeys.knumberInternalHeader]++;
	if (info[zdssInfoKeys.kinfoHeader2Number] > 0)		fileHeader[zdssFileKeys.knumberHeader2]++;
	if (info[zdssInfoKeys.kinfoUserHeadNumber] > 0)     fileHeader[zdssFileKeys.knumberUserHeader]++;
	if (info[zdssInfoKeys.kinfoValues1Number] > 0)      fileHeader[zdssFileKeys.knumberDataArea1]++;
	if (info[zdssInfoKeys.kinfoValues2Number] > 0)      fileHeader[zdssFileKeys.knumberDataArea2]++;
	if (info[zdssInfoKeys.kinfoValues3Number] > 0)		fileHeader[zdssFileKeys.knumberDataArea3]++;

	//  Force the path bin to be reloaded if read right after this.
	ifltab[zdssKeys.kfound] = -1;

	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%lld; First Data Address %lld; Data Type %d",
			ifltab[zdssKeys.kinfoAddress], info[zdssInfoKeys.kinfoValues1Address], ztransfer->dataType);
		zmessageDebug(ifltab, DSS_FUNCTION_zwriteNew_ID, "Exit; Info address: ", messageString);
	}
	return status;
}

