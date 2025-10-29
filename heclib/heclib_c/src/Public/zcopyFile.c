#include <stdio.h>
#include <stdlib.h>
#include <stdlib.h>

#include "zdssKeys.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "zdssLocking.h"
#include "zprogress.h"
#include "hecdssInternal.h"
#include "hecdssFort.h"
#include "heclib.h"



/**
*  Function:	zcopyFile
*
*  Use:			Public
*
*  Description:	Copies (merges) one DSS file into another.  File being copied to can be version 6 or 7.
*					If the versions are different, zcopyFile will perform the appropriate conversion.
*
*  Declaration: int zcopyFile(long long *ifltabFrom, long long *ifltabTo, int handleIfDamaged, int statusWanted);
*
*  Parameters:
*				long long ifltabFrom
*					The ifltabFrom of the DSS file being copied from.  Generally, this is a version 7 file,
*					but the appropriate version 6 function is called if it is a version 6 file.
*
*				long long *ifltabTo
*					The ifltabFrom of the DSS file being copied to.  This can either be a version 6 or 7 file.
*
*				int statusWanted
*					Usually 0.  This is the status of the records to copy, normally all valid = 0.
*					However, you could copy only deleted records:
*
*						REC_STATUS_VALID (0):		All valid (primary and alaises)
*						REC_STATUS_PRIMARY (1):		Primary only
*						REC_STATUS_ALIAS (2):		Alias only
*						REC_STATUS_DELETED (11):	Deleted only
*						REC_STATUS_RENAMED (12):	Renamed only
*						REC_STATUS_ANY (100):		Any, regardless if deleted, renamed, etc.
*
*
*	Returns:	int status
*					STATUS_OKAY for successful operation.
*					errorCode for error
*					errorCode may be decoded by function zerrorDecode()
*
*
*	Remarks:	This function uses a "brute force" approach.  If the file or tables have been
*					corrupted, this function will recover all available data, making it
*					the method to call to restore a damaged file.  Only looks for record
*					headers (info areas) for data.
*
*
*	Author:			Bill Charley
*	Date:			2010
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zcopyFile(long long *ifltabFrom, long long *ifltabTo, int statusWanted)
{
#define FILE_BUFF_SIZE	4028

	int status;
	int i;
	int count;
	long long *info;
	long long infoAddress;
	long long address;
	long long fileSize;
	long long *fileHeader;
	int numberInfo;
	int readSize;
	long long fileBuff[FILE_BUFF_SIZE];
	long long recHeader[3];
	char pathname[MAX_PATHNAME_LENGTH];
	int pathLength;
	char fullFilename[256];


	if (zgetVersion(ifltabFrom) == 0) {
		return zerrorProcessing(ifltabFrom, DSS_FUNCTION_zcopyFile_ID, zdssErrorCodes.NOT_OPENED,
									0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "");
	}

	if (zgetVersion(ifltabTo) == 0) {
		return zerrorProcessing(ifltabFrom, DSS_FUNCTION_zcopyFile_ID, zdssErrorCodes.NOT_OPENED,
									0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "");
	}

	if (zmessageLevel(ifltabFrom, MESS_METHOD_COPY_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltabFrom, DSS_FUNCTION_zcopyFile_ID, "Enter, Handle copying FROM: ", zhandle(ifltabFrom));
		if (zgetVersion(ifltabFrom) == 7) {
			charLong((void *)ifltabFrom[zdssKeys.kfullFilename], fullFilename, 0, sizeof(fullFilename), 0, 1);
			zmessageDebug(ifltabFrom, DSS_FUNCTION_zcopyFile_ID, "File name copying FROM: ", fullFilename);
		}
		zmessageDebugInt(ifltabFrom, DSS_FUNCTION_zcopyFile_ID, "Handle copying TO: ", zhandle(ifltabTo));
		if (zgetVersion(ifltabTo) == 7) {
			charLong((void *)ifltabTo[zdssKeys.kfullFilename], fullFilename, 0, sizeof(fullFilename), 0, 1);
			zmessageDebug(ifltabTo, DSS_FUNCTION_zcopyFile_ID, "File name copying TO: ", fullFilename);
		}
	}

	//  get enough space to copy largest record in file
/*	maxRec = numberIntsInLongs(fileHeader[zdssFileKeys.kmaxRecordSize]) + 1;
	buffer = (int*)calloc((size_t)maxRec, INT_SIZE);
	if (buffer == 0) {
		return zerrorProcessing(ifltabFrom, DSS_FUNCTION_zcopyFile_ID, zdssErrorCodes.CANNOT_ALLOCATE_MEMORY,
									maxRec, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS, "",
									"Unable to allocate buffer.");
	}
*/
	status = zcheckKeys(ifltabFrom);
	if (status != 0) {
		status = zerrorEncodeHigh(status, DSS_FUNCTION_zcopyFile_ID);
		return status;
	}

	if (zgetVersion(ifltabTo) == 7) {
		status = zcheckKeys(ifltabTo);
		if (status != 0) {
			status = zerrorEncodeHigh(status, DSS_FUNCTION_zcopyFile_ID);
			return status;
		}
	}


	fileHeader = (long long *)ifltabFrom[zdssKeys.kfileHeader];
	fileSize = fileHeader[zdssFileKeys.kfileSize];
	zresetProgress(zhandle(ifltabFrom), fileHeader[zdssFileKeys.knumberRecords]);


	count = 0;
	address = 0;
	zprogress.handle = zhandle(ifltabFrom);
	//  Brute force through the file, ignoring hash, addresses, etc.
	//  This will allow recovery of a damaged file
	while (address < fileSize) {
		readSize = FILE_BUFF_SIZE;
		if ((address + readSize)  > fileSize) {
			readSize = (int)(fileSize - address);
		}
		status = zget(ifltabFrom, address, (int *)fileBuff, readSize, 2);
		if (zisError(status)) {
			return zerrorUpdate(ifltabFrom, status, DSS_FUNCTION_zcopyFile_ID);
		}
		for (i=0; i<readSize; i++) {
			if (fileBuff[i] == DSS_INFO_FLAG) {
				//  Found a flag - check to be sure this is the flag and not a fluke
				//  Make sure the next word contains the status and the following the path length
				if ((i+2) < readSize) {
					recHeader[0] = fileBuff[i];
					recHeader[1] = fileBuff[i+1];
					recHeader[2] = fileBuff[i+2];
				}
				else {
					infoAddress = address + (long long)i;
					status = zget(ifltabFrom, infoAddress, (void *)recHeader, 3, 2);
					if (zisError(status)) {
						return zerrorUpdate(ifltabFrom, status, DSS_FUNCTION_zcopyFile_ID);
					}
				}
				pathLength = (int)recHeader[zdssInfoKeys.kinfoPathnameLength];
				//  Check for a valid pathname length
				if ((pathLength > 0) && (pathLength < MAX_PATHNAME_LENGTH)) {
					//  Check the status flag
					//  Be sure this was not deleted or renamed....
					if (zcompareRecordStatus((int)recHeader[zdssInfoKeys.kinfoStatus], statusWanted)) {
						//  Good - copy the record
						infoAddress = address + (long long)i;
						numberInfo = zdssVals.infoSize + numberLongsInBytes(pathLength);
						if (numberInfo > zdssVals.maxInfoSize) {
							numberInfo = zdssVals.maxInfoSize;
						}
						status = zget(ifltabFrom, infoAddress, (int *)ifltabFrom[zdssKeys.kinfo], numberInfo, 2);
						if (zisError(status)) {
							return zerrorUpdate(ifltabFrom, status, DSS_FUNCTION_zcopyFile_ID);
						}
						ifltabFrom[zdssKeys.kaddInfoLastPath] = infoAddress;
						ifltabFrom[zdssKeys.kinfoAddress] = ifltabFrom[zdssKeys.kaddInfoLastPath];
						ifltabFrom[zdssKeys.kinfoSize] = numberInfo;
						info = (long long *)ifltabFrom[zdssKeys.kinfo];
						charInt((void *)&info[zdssInfoKeys.kinfoPathname], pathname, pathLength, sizeof(pathname), 0, 1, 0);
						pathname[pathLength] = '\0';

						if (zgetVersion(ifltabTo) == 7) {
							status = zcopyRecordInternal(ifltabFrom, ifltabTo, pathname, 0);
						}
						else {
							status = zcopyRecord(ifltabFrom, ifltabTo, pathname, pathname);
						}

						if (zisError(status)) {
							return zerrorUpdate(ifltabFrom, status, DSS_FUNCTION_zcopyFile_ID);
						}

						count++;
						zprogress.currentNumber = count;

					}
				}
			}
		}
		address += readSize;
	}

	//  Be sure to write to disk.
	if (zgetVersion(ifltabTo) == 7) {
		ifltabTo[zdssKeys.kwritingNow] = 1;
		zlockActive(ifltabTo, LOCKING_LEVEL_SUPER, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
	}

	if (count > 0) {
		if (zmessageLevel(ifltabFrom, MESS_METHOD_COPY_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			zmessageDebugInt(ifltabFrom, DSS_FUNCTION_zcopyFile_ID, "Copy file completed, number records copied: ", count);
		}
		return STATUS_OKAY;
	}
	else {
		zerrorProcessing(ifltabFrom, DSS_FUNCTION_zcopyFile_ID,
						zdssErrorCodes.EMPTY_FILE, 0, 0, zdssErrorSeverity.WARNING, "",
						"No records found in file.");
		zerrorStructClear();
		return STATUS_OKAY;
	}
}



void zcopyfile7_(long long *ifltabFrom, long long *ifltabTo, int *status)
{
	*status = zcopyFile(ifltabFrom, ifltabTo, 0);
}

void zcopyfile_(long long *ifltabFrom, long long *ifltabTo, int *status)
{
	int versFileFrom;
	int versFileTo;

	versFileFrom = zgetVersion(ifltabFrom);
	versFileTo   = zgetVersion(ifltabTo);

	if (versFileFrom == 7) {
		*status = zcopyFile(ifltabFrom, ifltabTo, 0);
	}
}

