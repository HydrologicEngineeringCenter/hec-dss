#include <stdio.h>
#include <stdlib.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssLocking.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"
#include "zerrorCodes.h"

/**
*  Function:	zdeleteInternal (same as zdelete, zundelete)
*
*  Use:			Private
*
*  Description:	Marks a single record as deleted in a DSS file, or
*					marks a deleted record as good.  This function performs both delete and undelete functions.
*
*  Declaration: int zdeleteInternal(long long *ifltab, const char *pathname, int boolUndelete);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				const char *pathname
*					The pathname of the record to delete.  This must be a fully defined pathname, with the
*					correct D (date) part and E (time interval) parts for time series (i.e., no date range).
*					The pathname must match exactly (except for case) the record being searched for.
*
*				int boolUndelete
*					A flag to indicate if the record is to be deleted or undeleted.
*					Set to zero to delete, One to undelete
*
*
*
*	Returns:	int status
*					STATUS_OKAY - record deleted or restored.
*					STATUS_RECORD_NOT_FOUND
*					errorCode for an error
*
*	Note:		Record is not physically removed until a squeeze.  A record cannot be undeleted if the
*					file uses space reclamation, as the deleted space is moved into the reclamation array.
*					Since both zdelete and zundelete essentially do the same thing (changing the status
*					of a record), both functions are combined into one.
*
*
*	See Also:	zundeleteAll()
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zdeleteInternal(long long *ifltab, const char *pathname, int boolUndelete)
{
	long long *info;
	long long address;
	long long deleteStatus;
	long long endAddress1;
	long long endAddress2;
	long long icurrentTime;
	long long *fileHeader;
	char messageString[80];
	char *path;
	int numberInfo;
	int releasedAmount;
	int functionID;
	int reclaimFlag;
	int readAction;
	int status;


	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	if (boolUndelete) {
		functionID = DSS_FUNCTION_zundelete_ID;
		deleteStatus = (long long)REC_STATUS_VALID;
		readAction = REC_STATUS_DELETED;
	}
	else {
		functionID = DSS_FUNCTION_zdelete_ID;
		deleteStatus = (long long)REC_STATUS_DELETED;
		readAction = REC_STATUS_VALID;
	}

	if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, functionID, "Enter.  Pathname: ",pathname);
		zmessageDebugInt(ifltab, functionID, "Handle: ", zhandle(ifltab));
	}


	if (zgetVersion(ifltab) != 7) {
		return zerrorProcessing(ifltab, functionID, zdssErrorCodes.INCOMPATIBLE_VERSION,
			zgetVersion((void *)ifltab), 0, zdssErrorSeverity.WARNING, pathname, "");
	}

	if (ifltab[zdssKeys.kopenStatus] != OPEN_STAT_WRITE) {
		return zerrorProcessing(ifltab, functionID,
			zdssErrorCodes.WRITE_ON_READ_ONLY, 0, 0,
			zdssErrorSeverity.WARNING_NO_WRITE_ACCESS, pathname, "");
	}

	//  Check reclamation setting
	if (boolUndelete && (ifltab[zdssKeys.kreclaimLevel] != RECLAIM_NONE)) {
		reclaimFlag = (int)ifltab[zdssKeys.kreclaimLevel];
	}
	else {
		reclaimFlag = -1;
	}

	//  Lock the file if we are in a multi-user access mode
	//  And read the file header
	status =  zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_ON, LOCKING_FLUSH_ON);
	if (zisError(status)) {
		return status;
	}


	//  Force the internals to be loaded during the check
	ifltab[zdssKeys.kaddInfoLastPath] = 0;
	status = zreadInfo(ifltab, pathname, readAction);
	if (zisError(status)) {
		zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
		return zerrorUpdate(ifltab, status, functionID);
	}

	if (status == STATUS_OKAY) {
		//  If undeleting, make sure reclamation is off, otherwise this space
		//  could be used by something else
		//  (at this point, we know it has not been used because hashes match in info.)
		if (boolUndelete && (ifltab[zdssKeys.kreclaimLevel] != RECLAIM_NONE)) {
			//  Turn off reclamation for this delete
			status = zreclaimSet(ifltab, RECLAIM_NONE);
			if (zisError(status)) {
				zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
				return zerrorUpdate(ifltab, status, functionID);
			}
		}

		info = (long long *)ifltab[zdssKeys.kinfo];

		//  If this has an alias, or is an alias, we need the primary pathname,
		//  then remove all aliases, then delete primary
		if (info[zdssInfoKeys.kinfoAliasesBinAddress]) {
			path = (char *)calloc(MAX_PATHNAME_SIZE, 1);
			status = zaliasGetPrimary(ifltab, pathname, path, MAX_PATHNAME_SIZE);
			status = zaliasRemoveAll(ifltab, pathname);
			ifltab[zdssKeys.kaddInfoLastPath] = 0;
			status = zreadInfo(ifltab, path, readAction);
			free(path);
			path = 0;
			if (zisError(status)) {
				zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
				return zerrorUpdate(ifltab, status, functionID);
			}
		}

		//  First, mark in the pathname bin  to indicate that it has been deleted
		address = ifltab[zdssKeys.kpathBinAddress] + zdssBinKeys.kbinStatus;
		status = zput(ifltab, address, (int *)&deleteStatus, 1, 2);
		if (zisError(status)) {
			zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
			return zerrorUpdate(ifltab, status, functionID);
		}

		//  Now mark the status flag in the info block.
		info[zdssInfoKeys.kinfoStatus] = deleteStatus;
		numberInfo = zdssVals.infoSize + numberLongsInBytes((int)ifltab[zdssKeys.klenLastPath]);
		if (numberInfo > zdssVals.maxInfoSize) {
			numberInfo = zdssVals.maxInfoSize;
		}
		status = zput(ifltab, ifltab[zdssKeys.kaddInfoLastPath], (int *)info, numberInfo, 2);
		if (zisError(status)) {
			zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
			return zerrorUpdate(ifltab, status, functionID);
		}

		//  Determine the amount of space released / restored
		releasedAmount = numberInfo +
			numberLongsInInts((int)info[zdssInfoKeys.kinfoInternalHeadNumber]) +
			numberLongsInInts((int)info[zdssInfoKeys.kinfoHeader2Number]) +
			numberLongsInInts((int)info[zdssInfoKeys.kinfoValues3Number]) +
			numberLongsInInts((int)info[zdssInfoKeys.kinfoUserHeadNumber]) +
			numberLongsInInts((int)info[zdssInfoKeys.kinfoValues1Number]) +
			numberLongsInInts((int)info[zdssInfoKeys.kinfoValues2Number]);

		if (boolUndelete) {
			fileHeader[zdssFileKeys.kdead] -= releasedAmount;
			if (fileHeader[zdssFileKeys.kdead] < 0) fileHeader[zdssFileKeys.kdead] = 0;
			//  Increment counters
			fileHeader[zdssFileKeys.knumberRecords]++;
			fileHeader[zdssFileKeys.knumberDeletes]--;
			fileHeader[zdssFileKeys.kcatSortDeletes]--;
			if (fileHeader[zdssFileKeys.kcatSortStatus] > 0) {
				fileHeader[zdssFileKeys.kcatSortStatus] = 3;
			}
			fileHeader[zdssFileKeys.knumberInternalHeader]++;
			fileHeader[zdssFileKeys.knumberHeader2]++;
			fileHeader[zdssFileKeys.knumberDataArea3]++;
			fileHeader[zdssFileKeys.knumberUserHeader]++;
			fileHeader[zdssFileKeys.knumberDataArea1]++;
			fileHeader[zdssFileKeys.knumberDataArea2]++;
		}
		else {
			if (ifltab[zdssKeys.kreclaimLevel] > RECLAIM_NONE) {
				//  Is this record contiguous?  (Supposed to be!)
				endAddress1 = ifltab[zdssKeys.kaddInfoLastPath] + (long long)releasedAmount;
				endAddress2 = info[zdssInfoKeys.kinfoValues1Address] +
					numberLongsInInts((int)info[zdssInfoKeys.kinfoValues1Number]) +
					numberLongsInInts((int)info[zdssInfoKeys.kinfoValues2Number]);
				if (endAddress1 == endAddress2) {
					zreleaseFileSpace(ifltab, ifltab[zdssKeys.kaddInfoLastPath], releasedAmount);
				}
				else {
					//  Shouldn't have this happen
					//fileHeader[zdssFileKeys.kdead] += releasedAmount;
					zreleaseFileSpace(ifltab, ifltab[zdssKeys.kaddInfoLastPath], releasedAmount);
				}
			}
			//  Decrement counters
			fileHeader[zdssFileKeys.knumberRecords]--;
			fileHeader[zdssFileKeys.knumberDeletes]++;
			fileHeader[zdssFileKeys.kcatSortDeletes]++;
			if (fileHeader[zdssFileKeys.kcatSortStatus] > 0) {
				fileHeader[zdssFileKeys.kcatSortStatus] = 3;
			}
			if (info[zdssInfoKeys.kinfoInternalHeadNumber] > 0)		fileHeader[zdssFileKeys.knumberInternalHeader]--;
			if (info[zdssInfoKeys.kinfoHeader2Number] > 0)			fileHeader[zdssFileKeys.knumberHeader2]--;
			if (info[zdssInfoKeys.kinfoValues3Number] > 0)			fileHeader[zdssFileKeys.knumberDataArea3]--;
			if (info[zdssInfoKeys.kinfoUserHeadNumber] > 0)			fileHeader[zdssFileKeys.knumberUserHeader]--;
			if (info[zdssInfoKeys.kinfoValues1Number] > 0)          fileHeader[zdssFileKeys.knumberDataArea1]--;
			if (info[zdssInfoKeys.kinfoValues2Number] > 0)          fileHeader[zdssFileKeys.knumberDataArea2]--;
		}

		if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_GENERAL)) {
			if (boolUndelete) {
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
					ZUNDELETE_MESS, zhandle(ifltab));
			}
			else {
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
					ZDELETE_MESS, zhandle(ifltab));
			}
			zmessage2(ifltab, messageString, pathname);
		}
	}
	else {
		if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_GENERAL)) {
			if (boolUndelete) {
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
					ZUNDELETE_MESS_ERROR, zhandle(ifltab));
			}
			else {
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
					ZDELETE_MESS_ERROR, zhandle(ifltab));
			}
			zmessage2(ifltab, messageString, pathname);
		}
	}

	//  Reset reclaim level, if we need to
	if (boolUndelete && (reclaimFlag > -1)) {
		zreclaimSet(ifltab, reclaimFlag);
	}

	//  Make sure that we don't keep the path in memory
	ifltab[zdssKeys.kfound] = 0;
	ifltab[zdssKeys.kaddInfoLastPath] = 0;
	ifltab[zdssKeys.kpathnameHash] = 0;

	icurrentTime = getCurrentTimeMillis();
	fileHeader[zdssFileKeys.klastWriteTime] = icurrentTime;
	ifltab[zdssKeys.kwritingNow] = 1;

	//  Unlock
	zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
	if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, functionID, "Exit.  Pathname: ",pathname);
		zmessageDebugInt(ifltab, functionID, "Handle: ", zhandle(ifltab));
	}
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, functionID);
	}
	return status;
}

