#include <stdlib.h>
#include <string.h>

#include "heclib.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "zdssLocking.h"
#include "hecdssInternal.h"


/**
*  Function:	zcopyRecordInternal
*
*  Use:			Private
*
*  Description:	Copies a record from one DSS file to another DSS file, or duplicates a record withing a file.  
*				Both have to be version 7.
*
*  Declaration: int zcopyRecordInternal(long long *ifltabFrom, long long *ifltabTo, const char *pathnameTo, int boolDuplicate);
*
*  Parameters:
*				long long ifltabFrom
*					The ifltabFrom of the DSS file being copied from. The record being copied
*					must have its info area loaded.
*
*				long long *ifltabTo
*					The ifltabFrom of the DSS file being copied to.
*
*				const char *pathnameTo
*					The pathname for the record being copied to.  You can copy the record to a
*					new name, or duplicate the record in the same file with a different name.
*
*				int boolDuplicate
*					Set to 1 if duplicating a record in the smae file, or 0 if copying to another file
*					If duplicating, ifltabTo is ignored (some compilers have issues with ifltabTo = ifltabFrom,
*					thus the need for the duplicate flag)
*
*
*	Returns:	int status
*					STATUS_OKAY for successful operation.
*					errorCode for error
*					errorCode may be decoded by function zerrorDecode()
*
*
*	Remarks:	Only works with DSS-7 files.  The info array (record header) must be loaded into
*					ifltabFrom first, such as calling zreadInfo.
*
*
*	Author:			Bill Charley
*	Date:			2010
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/
//  Assumes that the record header (infoFrom) block from has already been loaded in ifltabFrom
//
int zcopyRecordInternal(long long *ifltabFrom, long long *ifltabTo, const char *pathnameTo, int boolDuplicate)
{
	int status;
	int number;
	int numberInfo;
	int version;
	int totalNumberInts;
	int expansionNumber;
	int expansionFlag;
	int dataType;
	int boolIsCurrent;
	zStructTransfer* ztransfer;
	char pathname[MAX_PATHNAME_LENGTH];
	int pathLength;
	long long bufferControl[4] = { 0, 0, 0, 0 };
	int buffer[1];
	long long *infoFrom;
	long long *infoTo;
	long long *fileHeader;
	zStructLocation* locationStruct;

	/////////////////////
	char carray[200];
	int ipos;
	int len;
	int i;
	int numberInts;
	int *internalHeader;
	char *type=0;
	char *units=0;
	char *timezone=0;
	//////////////////////




	//  Error checks were done by zcopyFile

	if (boolDuplicate) {
		if (zgetVersion(ifltabFrom) != 7) {
			return zerrorProcessing(ifltabFrom, DSS_FUNCTION_zcopyRecord_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
				zgetVersion(ifltabFrom), 0, zdssErrorSeverity.WARNING, "", "");

		}
	}
	else {
		if (zgetVersion(ifltabTo) != 7) {
			return zerrorProcessing(ifltabTo, DSS_FUNCTION_zcopyRecord_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
				zgetVersion(ifltabTo), 0, zdssErrorSeverity.WARNING, "", "");

		}
	}

	infoFrom = (long long *)ifltabFrom[zdssKeys.kinfo];
	pathLength = (int)infoFrom[zdssInfoKeys.kinfoPathnameLength];
	charInt((void *)&infoFrom[zdssInfoKeys.kinfoPathname], pathname, pathLength, sizeof(pathname), 0, 1, 0);
	pathname[pathLength] = '\0';

	if (zmessageLevel(ifltabFrom, MESS_METHOD_COPY_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltabFrom, DSS_FUNCTION_zcopyRecord_ID, "Enter, Handle copying from: ", zhandle(ifltabFrom));
		zmessageDebug(ifltabFrom, DSS_FUNCTION_zcopyRecord_ID, "Pathname copying from: ", pathname);
		if (boolDuplicate) {
			zmessageDebug(ifltabFrom, DSS_FUNCTION_zcopyRecord_ID, "Duplicating Pathname to: ", pathnameTo);
		}
		else {
			zmessageDebugInt(ifltabTo, DSS_FUNCTION_zcopyRecord_ID, "Handle copying to: ", zhandle(ifltabTo));
			zmessageDebug(ifltabFrom, DSS_FUNCTION_zcopyRecord_ID, "Pathname copying to: ", pathnameTo);
		}
	}


	ztransfer = zstructTransferNew(pathname, 1);
	if (!ztransfer) {
		return zerrorProcessing(ifltabFrom, DSS_FUNCTION_zcopyRecord_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
								zdssErrorSeverity.MEMORY_ERROR,
								pathnameTo, "Allocating ztransfer struct");
	}
	ztransfer->info = infoFrom;
	status = zreadInternal(ifltabFrom, ztransfer, bufferControl, buffer, 0);
	if (zisError(status)) {
		zstructFree(ztransfer);
		return zerrorUpdate(ifltabFrom, status, DSS_FUNCTION_zcopyRecord_ID);
	}

	//  Determine if this is time-series data that needs to have it's allocation changed
	//  That is, is there unused allocated space that should be removed?
	i8toi4(infoFrom[zdssInfoKeys.kinfoExpansion], &expansionNumber, &expansionFlag);
	i8toi4(infoFrom[zdssInfoKeys.kinfoTypeVersion], &dataType, &version);

	if (expansionNumber > 0) {
		//  Time series?
		if ((dataType >= DATA_TYPE_RTS) && (dataType <= 119)) {
			//  We know that the data is TS and has expanded.
			//  We don't know why it expanded.  If it is "real-time data",
			//  then it will probably continue to be expanded and we want
			//  to be sure to keep any additional allocated space.
			//  If the data is "historic" or non-real time (e.g., most modeling data),
			//  then we should remove any extra allocated space and just store the data only.
			//  This will cover most situations;  when we are wrong, it will only cost some space

			//  We "assume" that the data is real time if
			//  the date period of the record includes the current time, and
			//  the data set has expanded at least once.
			boolIsCurrent = ztsIsCurrentTimeSeries(ifltabFrom, pathname);
			//  If boolIsCurrent true, keep the allocated space, as the data
			//  set is still most likely expanding
			//  If not, then remove extra allocated space and only allocate data size
			if (!boolIsCurrent) {
				ztransfer->totalAllocatedSize = ztransfer->numberValues;
				ztransfer->totalExpandedSize = ztransfer->numberValues;
				expansionNumber = 0;
				if (zmessageLevel(ifltabFrom, MESS_METHOD_COPY_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
					totalNumberInts = numberLongsInInts((int)infoFrom[zdssInfoKeys.kinfoValues1Number]) +
						numberLongsInInts((int)infoFrom[zdssInfoKeys.kinfoValues2Number]);
					totalNumberInts *= 2;
					if (infoFrom[zdssInfoKeys.kinfoAllocatedSize] > totalNumberInts) {
						number = (int)infoFrom[zdssInfoKeys.kinfoAllocatedSize] - totalNumberInts;
						if (!boolDuplicate) {
							zmessageDebugInt(ifltabTo, DSS_FUNCTION_zcopyRecord_ID, "Time series reduction in allocated space by: ", number);
						}
						zmessageDebug(ifltabFrom, DSS_FUNCTION_zcopyRecord_ID, "Pathname: ", pathnameTo);
					}
				}
			}
		}
	}

	//  Check if time series and "old style" units, padded with nulls instead of blanks
	if ((dataType >= DATA_TYPE_RTS) && (dataType <= 119)) {
		len = ztransfer->internalHeaderNumber - INT_HEAD_units;
		//  Do we have any units or type?  (which is okay)
		if (len > 1) {
		
			if (!unitsHavePadding(ztransfer->internalHeader,INT_HEAD_units) ) {
				len *= 4;
				charInt(&ztransfer->internalHeader[INT_HEAD_units], carray, len, sizeof(carray), 0, 0, 0);

				ipos = 0;
				units = readStringFromHeader(&carray[ipos], &ipos, sizeof(carray));
				type = readStringFromHeader(&carray[ipos], &ipos, sizeof(carray));
				timezone = readStringFromHeader(&carray[ipos], &ipos, sizeof(carray));
				
				ipos = 0;
				appendStringToHeader(units, carray, &ipos, sizeof(carray));
				free(units);

				appendStringToHeader(type, carray, &ipos, sizeof(carray));
				free(type);

				appendStringToHeader(timezone, carray, &ipos, sizeof(carray));
				free(timezone);

				//  ipos should be divisble by 8 and numberInts should be even to work with 64-bit words / addressing
				numberInts = numberIntsInBytes(ipos) + INT_HEAD_units + 1;
				internalHeader = (int *)calloc(numberInts, 4);
				for (i = 0; i < INT_HEAD_units; i++) {
					internalHeader[i] = ztransfer->internalHeader[i];
				}

				//  Blank fill int word 17 (INT_HEAD_units) for compatibility with big endian machines
				charInt("    ", &internalHeader[INT_HEAD_units], 4, 4, 1, 0, isOdd(INT_HEAD_units));
				charLong(carray, &internalHeader[INT_HEAD_units + 1], ipos, ipos, 1, 1);

				free(ztransfer->internalHeader);
				ztransfer->internalHeader = internalHeader;
				ztransfer->internalHeaderNumber = numberInts;
			}
		}
	}

	//  Retain first last dates
	if (boolDuplicate) {
		ifltabFrom[zdssKeys.kdataFirstDate] = infoFrom[zdssInfoKeys.kinfoFirstDate];;
		ifltabFrom[zdssKeys.kdataLastDate] = infoFrom[zdssInfoKeys.kinfoLastDate];
	}
	else {
		ifltabTo[zdssKeys.kdataFirstDate] = infoFrom[zdssInfoKeys.kinfoFirstDate];;
		ifltabTo[zdssKeys.kdataLastDate] = infoFrom[zdssInfoKeys.kinfoLastDate];
	}


	//  Change the transfer pathname to the new one
	free(ztransfer->pathname);
	ztransfer->pathname = mallocAndCopyPath(pathnameTo);
	if (!ztransfer->pathname) {
		return zerrorProcessing(ifltabFrom, DSS_FUNCTION_zcopyRecord_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
								zdssErrorSeverity.MEMORY_ERROR,
								pathnameTo, "Allocating ztransfer pathname");
	}
	ztransfer->pathnameLength = (int)strlen(ztransfer->pathname);

	//  Now lock the file we are writing to
	if (!boolDuplicate) {
		status = zlockActive(ifltabTo, LOCKING_LEVEL_MID, LOCKING_LOCK_ON, LOCKING_FLUSH_ON);
		if (zisError(status)) {
			return zerrorEncodeHigh(status, DSS_FUNCTION_zcopyRecord_ID);
		}
		//  Write the dataset to the file
		status = zwrite(ifltabTo, ztransfer);
		if (zisError(status)) {
			zlockActive(ifltabTo, LOCKING_LEVEL_SUPER, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);
			zstructFree(ztransfer);
			return zerrorUpdate(ifltabFrom, status, DSS_FUNCTION_zcopyRecord_ID);
		}
		infoTo = (long long *)ifltabTo[zdssKeys.kinfo];
	}
	else {
		status = zlockActive(ifltabFrom, LOCKING_LEVEL_MID, LOCKING_LOCK_ON, LOCKING_FLUSH_ON);
		if (zisError(status)) {
			return zerrorEncodeHigh(status, DSS_FUNCTION_zcopyRecord_ID);
		}
		//  Write the dataset to the file
		status = zwrite(ifltabFrom, ztransfer);
		if (zisError(status)) {
			zlockActive(ifltabTo, LOCKING_LEVEL_SUPER, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);
			zstructFree(ztransfer);
			return zerrorUpdate(ifltabFrom, status, DSS_FUNCTION_zcopyRecord_ID);
		}
		infoTo = (long long *)ifltabFrom[zdssKeys.kinfo];
	}
	zstructFree(ztransfer);

	//  We need to update the to infoFrom block, which we have in memory
	numberInfo = zdssVals.infoSize + numberLongsInBytes((int)infoTo[zdssInfoKeys.kinfoPathnameLength]);
	//  Now reset record information back to old record.
	infoTo[zdssInfoKeys.kinfoTypeVersion] = infoFrom[zdssInfoKeys.kinfoTypeVersion];
	infoTo[zdssInfoKeys.kinfoExpansion] = i4toi8(expansionNumber, expansionFlag);
	infoTo[zdssInfoKeys.kinfoLastWriteTime] = infoFrom[zdssInfoKeys.kinfoLastWriteTime];
	stringCopy((void *)&infoTo[zdssInfoKeys.kinfoProgram], zdssVals.numberProgram, (void *)&infoFrom[zdssInfoKeys.kinfoProgram], zdssVals.numberProgram);
	infoTo[zdssInfoKeys.kinfoFirstDate] = infoFrom[zdssInfoKeys.kinfoFirstDate];
	infoTo[zdssInfoKeys.kinfoReserved] = infoFrom[zdssInfoKeys.kinfoReserved];
	infoTo[zdssInfoKeys.kinfoCreationTime] = infoFrom[zdssInfoKeys.kinfoCreationTime];
	infoTo[zdssInfoKeys.kinfoReserved1] = infoFrom[zdssInfoKeys.kinfoReserved1];

	if (!boolDuplicate) {
		status = zput(ifltabTo, ifltabTo[zdssKeys.kaddInfoLastPath], (int *)infoTo, numberInfo, 2);
		if (zisError(status)) {
			zlockActive(ifltabTo, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
			return zerrorUpdate(ifltabTo, status, DSS_FUNCTION_zcopyRecord_ID);
		}
		if (expansionNumber > 0) {
			fileHeader = (long long *)ifltabTo[zdssKeys.kfileHeader];
			fileHeader[zdssFileKeys.knumberExpansions] += expansionNumber;
		}
		//  Do we need to get location information?
		locationStruct = zstructLocationNew(pathname);
		if (locationStruct) {
			status = zlocationRetrieve(ifltabFrom, locationStruct);
			if (status == STATUS_RECORD_FOUND) {
				zlocationStore(ifltabTo, locationStruct, 0);
			}
			zstructFree(locationStruct);
		}

		//  Make sure the perm section gets written if we are copying the file
		//  (If we are in super lock, the mid lock will not write the perm section)
		if (ifltabTo[zdssKeys.klockLevel] > LOCKING_LEVEL_MID) {
			if (ifltabTo[zdssKeys.kwritingNow] != 0) {
				zpermWrite(ifltabTo);
			}
		}
		zlockActive(ifltabTo, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
	}
	else {
		status = zput(ifltabFrom, ifltabFrom[zdssKeys.kaddInfoLastPath], (int *)infoTo, numberInfo, 2);
		if (zisError(status)) {
			zlockActive(ifltabFrom, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
			return zerrorUpdate(ifltabFrom, status, DSS_FUNCTION_zcopyRecord_ID);
		}
		if (expansionNumber > 0) {
			fileHeader = (long long *)ifltabFrom[zdssKeys.kfileHeader];
			fileHeader[zdssFileKeys.knumberExpansions] += expansionNumber;
		}
		//  Do we need to get location information?
		locationStruct = zstructLocationNew(pathname);
		if (locationStruct) {
			status = zlocationRetrieve(ifltabFrom, locationStruct);
			if (status == STATUS_RECORD_FOUND) {
				zlocationStore(ifltabFrom, locationStruct, 0);
			}
			zstructFree(locationStruct);
		}

		//  Make sure the perm section gets written if we are copying the file
		//  (If we are in super lock, the mid lock will not write the perm section)
		if (ifltabFrom[zdssKeys.klockLevel] > LOCKING_LEVEL_MID) {
			if (ifltabFrom[zdssKeys.kwritingNow] != 0) {
				zpermWrite(ifltabFrom);
			}
		}
		zlockActive(ifltabFrom, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
	}

	if (zmessageLevel(ifltabFrom, MESS_METHOD_COPY_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltabFrom, DSS_FUNCTION_zcopyRecord_ID, "Exit, Handle copying from: ", zhandle(ifltabFrom));
	}

	return STATUS_OKAY;
}

