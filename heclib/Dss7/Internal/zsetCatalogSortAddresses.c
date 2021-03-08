
#include <stdio.h>
#include <stdlib.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssLocking.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"

/**
*  Function:	zsetCatalogSortAddresses
*
*  Use:			Private
*
*  Description:	After a full sort of the catalog, this writes the addresses of the sorted pathnames to the DSS file.
*					Next time a catalog is asked for, these are used to pre-sort the catalog, making the new sort fast.
*
*  Declaration: int zsetCatalogSortAddresses(long long* ifltab, long long* sortAddresses, int sortAddressesLen);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				long long* sortAddresses
*					An array of the pathname bin addresses of the full sorted list of pathnames
*
*				int sortAddressesLen
*					The length of array sortAddresses.
*
*
*	Called by:	HEC-DSSVue
*
*	See also:	zgetCatalogSortAddresses()
*
*
*	Comments:
*
*				The catalog sort order array will contain the address of the pathname hash in pathname bin
*				for all records in the DSS file.  DO NOT write, if only a partial catalog
*				Array will be:
*						-97536		(DSS_START_CAT_SORT_FLAG)
*						123456		(address of hash in pathname bin for first pathname in sort)
*						654321		(address of hash in pathname bin for second pathname in sort)
*						......
*						-97537		(DSS_END_CAT_SORT_FLAG)
*
*					fileHeader[zdssFileKeys.kcatSortAddress] is address for -97536	(DSS_START_CAT_SORT_FLAG)
*					fileHeader[zdssFileKeys.kcatSortSize] is size, excluding end flags, and is equal to number of paths on first write
*					fileHeader[zdssFileKeys.kcatSortNumber] is number of paths in array, and can become less to size, if records are deleted
*
*				Be cautious about when writing sort addresses; we don't want to impact other processes if we can help it.
*				This function is optional.  Don't need to do if there are issues.
*				Not an error to not do because of some reason.
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*/

int zsetCatalogSortAddresses(long long* ifltab, long long* sortAddresses, int sortAddressesLen)
{
	int status;
	int amount;
	int atEOF;
	long long lWord;
	long long address;
	int boolDoSave;
	int boolWriteState;
	long long *fileHeader;


	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	if (zmessageLevel(ifltab, MESS_METHOD_CATALOG_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcatalog_ID, "Enter zsetCatalogSortAddresses, number to write: ", sortAddressesLen);
	}

	status = zcheckKeys(ifltab);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zwriteNew_ID);
	}

	//  Only allow this for a complete catalog, not a partial
	if (sortAddressesLen != fileHeader[zdssFileKeys.knumberRecords]) {
		if (zmessageLevel(ifltab, MESS_METHOD_CATALOG_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			zmessageDebugLong(ifltab, DSS_FUNCTION_zcatalog_ID, "zsetCatalogSortAddresses, number in file is different: ", fileHeader[zdssFileKeys.knumberRecords]);
			zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, " partial sort will not be save in the file", "");
		}
		return STATUS_NO_OP;
	}

	if (ifltab[zdssKeys.kopenStatus] != OPEN_STAT_WRITE) {
		if (zmessageLevel(ifltab, MESS_METHOD_CATALOG_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_zcatalog_ID, "zsetCatalogSortAddresses, read access only for handle: ", zhandle(ifltab));
		}
		return STATUS_NO_OP;
	}

	//  Do we really want to do this?
	boolDoSave = 0;

	//  If we have already written to the file since last close, sure...
	if (ifltab[zdssKeys.kfileWritten] > 0) {
		boolDoSave = 1;
		boolWriteState = 1;
	}
	else {
		boolWriteState = 0;
	}

	//  Has the catalog size changed since last store?
	if ((int)fileHeader[zdssFileKeys.kcatSortNumber] != sortAddressesLen) {
		boolDoSave = 1;
	}

	if (fileHeader[zdssFileKeys.knumberRecords] != fileHeader[zdssFileKeys.kcatSortNumber]) {
		boolDoSave = 1;
	}

	//  Be sure the file is not being written to by someone else
	status = zlockDss(ifltab, (int)ifltab[zdssKeys.khandle], 3, fileHeader[zdssFileKeys.klockAddressWord], 8);
	if (zisError(status)) {
		boolDoSave = 0;
	}

	if (!boolDoSave) {
		if (zmessageLevel(ifltab, MESS_METHOD_CATALOG_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_zcatalog_ID, "zsetCatalogSortAddresses, No need for saving addresses, handle: ", zhandle(ifltab));
		}
		return STATUS_NO_OP;
	}

	//  Lock the file if we are in a multi-user access mode
	//  And read the file header
	status =  zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_ON, LOCKING_FLUSH_ON);
	if (zisError(status)) {
		zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcatalog_ID);
	}


	if (((int)fileHeader[zdssFileKeys.kcatSortStatus] > 0) && ((int)fileHeader[zdssFileKeys.kcatSortSize] >= (sortAddressesLen + 2))) {
		//  We can re-write the array in place
		//  Note:  fileHeader[zdssFileKeys.kcatSortSize] is not necessarily == sortAddressesLen
		//  But, fileHeader[zdssFileKeys.kcatSortNumber] will == sortAddressesLen
		status = zput(ifltab, (fileHeader[zdssFileKeys.kcatSortAddress] + 1), (int *)sortAddresses, sortAddressesLen, 2);
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcatalog_ID);
		}
	}
	else {
		//  We need to write the array at the end of the file
		//  Note:  fileHeader[zdssFileKeys.kcatSortSize] will == sortAddressesLen == fileHeader[zdssFileKeys.kcatSortNumber]
		//  First release the previous array (if any)
		if (fileHeader[zdssFileKeys.kcatSortSize] > 0) {
			//  Add 2 for beg and ending flags
			amount = (int)fileHeader[zdssFileKeys.kcatSortSize] + 2;
			zreleaseFileSpace(ifltab, fileHeader[zdssFileKeys.kcatSortAddress], amount);
		}

		//  Now write the new array
		//  allow for a flag at the beginning and end of the array (+2)
		fileHeader[zdssFileKeys.kcatSortSize] = sortAddressesLen;
		fileHeader[zdssFileKeys.kcatSortAddress] = zgetFileSpace(ifltab, ((int)fileHeader[zdssFileKeys.kcatSortSize]+2), 0, &atEOF);
		lWord = DSS_START_CAT_SORT_FLAG;
		status = zput(ifltab, fileHeader[zdssFileKeys.kcatSortAddress], (int *)&lWord, 2, 1);
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcatalog_ID);
		}
		status = zput(ifltab, (fileHeader[zdssFileKeys.kcatSortAddress] + 1), (int *)sortAddresses, (int)fileHeader[zdssFileKeys.kcatSortSize], 2);
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcatalog_ID);
		}
		address = fileHeader[zdssFileKeys.kcatSortAddress] + fileHeader[zdssFileKeys.kcatSortSize] + 1;
		lWord = DSS_END_CAT_SORT_FLAG;
		status = zput(ifltab, address, (int *)&lWord, 1, 2);
		if (atEOF && !status) {
			status = zwriteEOF(ifltab);
		}
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcatalog_ID);
		}
	}
	fileHeader[zdssFileKeys.kcatSortNumber] = sortAddressesLen;
	fileHeader[zdssFileKeys.kcatSequenceNumber]++;
	fileHeader[zdssFileKeys.kcatSortStatus] = 1;
	fileHeader[zdssFileKeys.kcatSortNewWrites] = 0;
	fileHeader[zdssFileKeys.kcatSortDeletes] = 0;
	zpermWrite(ifltab);

	//  If we entered in a non-write state for adviosry access, leave in the same state
	if (!boolWriteState && (ifltab[zdssKeys.kmultiUserAccess] == SINGLE_USER_ADVISORY_ACCESS)) {
		zflushToDisk(ifltab, 1);
		status = zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
	}
	else {
	//  Flush buffers and perm area and unlock the file
		status = zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
	}

	//  If we haven't written to the file otherwise, the only way to go back
	//  to a non-write state is to close and reopen the file.
	if (!boolWriteState) {
			zcheckAccessReset(ifltab, 0, 0);
	}

	if (zmessageLevel(ifltab, MESS_METHOD_CATALOG_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcatalog_ID, "Exit zsetCatalogSortAddresses, handle: ", zhandle(ifltab));
	}

	return STATUS_OKAY;
}



