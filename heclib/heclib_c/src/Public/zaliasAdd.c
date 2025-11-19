#include <string.h>
#include <stdlib.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssLocking.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"


/**
*  Function:	zaliasAdd
*
*  Use:			Public
*
*  Description:	Adds and alias pathname to a single record
*
*  Declaration: int zaliasAdd(long long *ifltab, const char* primayPathname, const char* aliasPathname);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				const char* primayPathname
*					The pathname of the existing record to alias to.  Must be the full unique pathname
*					(not part of a dataset; e.g., correct D part for time series.)
*
*				const char* aliasPathname
*					The alias pathname.  Must be a full unique pathname and follow rules
*					(e.g., you cannot change the E or D parts for time series data.)
*
*
*	Returns:	int status
*					STATUS_OKAY if alias added
*					STATUS_RECORD_NOT_FOUND  if primary record does not exist
*					< -1.   error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*	Discussion:		An alias is a pathname that (eventualy) points to a record that has a primary name.
*					You may have serveral pathnames aliased to the same recored (although rare)
*					If just one, then the pathname bin of the alias points to the info block of the primary,
*					and the alias address points to the pathname bin of the alias (not the full bin block,
*					just the bin for the individual alias record.)
*					If more than one alias, the the info block points to the last alias bin.  That alias
*					bin points to the previous alias bin, which either points to the previous alias bin
*					or to the real info block.
*					You only can tell if it is the real info block or an alias bin by reading that
*					area and examining the status flag (for an alias, this will be 2; the real will be 1)
*
*					For serveral alias, you will need to walk the chain down through aliases to find the primary.
*					It is anticipated that aliases will be used infrequently, and that when they are, it will
*					typically be only one.  However, this methodology allows for unlimited aliases for a recored
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*/


//
int zaliasAdd(long long *ifltab, const char* primayPathname, const char* aliasPathname)
{
	long long *info;
	long long infoAddress;
	long long lastAliasAddress;
	long long *fileHeader;
	int numberInfo;
	int dataType;
	int temp;
	int status;


	info = 0;
	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];

	if (zmessageLevel(ifltab, MESS_METHOD_ALIAS_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zaliasAdd_ID, "Enter;  Handle: ",  zhandle(ifltab));
		zmessageDebug(ifltab, DSS_FUNCTION_zaliasAdd_ID, "     Primary Pathname: ",primayPathname);
		zmessageDebug(ifltab, DSS_FUNCTION_zaliasAdd_ID, "     Alias Pathname: ",aliasPathname);
	}
	if (zgetVersion(ifltab) != 7) {
		if (zgetVersion(ifltab) == 0) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_zaliasAdd_ID,
				zdssErrorCodes.NOT_OPENED, 0, 0,
				zdssErrorSeverity.WARNING_NO_FILE_ACCESS, primayPathname, "");
		}
		else {
			return zerrorProcessing(ifltab, DSS_FUNCTION_zaliasAdd_ID,
				zdssErrorCodes.INCOMPATIBLE_VERSION_6, 0, 0,
				zdssErrorSeverity.WARNING_NO_FILE_ACCESS, primayPathname, "");
		}
	}


	if (!zinquire(ifltab, "write")) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zaliasAdd_ID,
			zdssErrorCodes.WRITE_ON_READ_ONLY, 0, 0,
			zdssErrorSeverity.WARNING_NO_WRITE_ACCESS, primayPathname, "");
	}

	//  Lock the file if we are in a multi-user access mode
	//  And read the file header
	status = zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_ON, LOCKING_FLUSH_ON);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasAdd_ID);
	}

	//  Force the internals to be loaded during the check
	ifltab[zdssKeys.kaddInfoLastPath] = 0;
	ifltab[zdssKeys.kpathnameHash] = 0;
	status = zreadInfo(ifltab, primayPathname, 1);
	if (zisError(status)) {
		zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasAdd_ID);
	}

	if (status != STATUS_RECORD_FOUND) {
		zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
		return zerrorProcessing(ifltab, DSS_FUNCTION_zaliasAdd_ID,
								zdssErrorCodes.RECORD_DOES_NOT_EXIST, 0,
								0, zdssErrorSeverity.WARNING, primayPathname, "");
	}

	//  Make a copy of the info block
	numberInfo = zdssVals.infoSize + numberLongsInBytes((int)ifltab[zdssKeys.klenLastPath]);
	if (numberInfo > zdssVals.maxInfoSize) {
		numberInfo = zdssVals.maxInfoSize;
	}
	info = (long long *)calloc(numberInfo, 8);
	convertDataArray((void *)ifltab[zdssKeys.kinfo], (void *)info, numberInfo, 2, 2);
	infoAddress = ifltab[zdssKeys.kaddInfoLastPath];
	//  Save the first and last date from the old info area
	ifltab[zdssKeys.kdataFirstDate] = info[zdssInfoKeys.kinfoFirstDate];
	ifltab[zdssKeys.kdataLastDate] = info[zdssInfoKeys.kinfoLastDate];

	//  Have any aliases already been establisthed?
	lastAliasAddress = info[zdssInfoKeys.kinfoAliasesBinAddress];

	//  Now load the alias pathname hash
	status = zcheck(ifltab, aliasPathname);
	if (zisError(status)) {
		free(info);
		zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasAdd_ID);
	}
	if (status == STATUS_RECORD_FOUND) {
		free(info);
		zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
		return zerrorProcessing(ifltab, DSS_FUNCTION_zaliasAdd_ID,
							zdssErrorCodes.RECORD_ALREADY_EXISTS, 0,
							0, zdssErrorSeverity.WARNING, aliasPathname, "");
	}
	//  If this pathname's hash code was not in the hash table
	//  create a new bin, and save that bin's address in the table
	if (ifltab[zdssKeys.khashTableBinAdd] == 0) {
		//  Write a new pathname bin
		status = zbinNew(ifltab);
		if (zisError(status)) {
			free(info);
			zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasAdd_ID);
		}
		//  Write address of new pathname bin to hash location in table
		ifltab[zdssKeys.khashTableBinAdd] = ifltab[zdssKeys.kbinAddCurrent];
		status = zput(ifltab, ifltab[zdssKeys.kaddTableHash], (int *)&ifltab[zdssKeys.khashTableBinAdd], 1, 2);
		if (zisError(status)) {
			free(info);
			zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasAdd_ID);
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

	//  Update the pathname bin area
	i8toi4(info[zdssInfoKeys.kinfoTypeVersion], &dataType, &temp);
	if (lastAliasAddress == 0) {
		status = zbinUpdate(ifltab, aliasPathname, infoAddress, 2, dataType);
	}
	else {
		status = zbinUpdate(ifltab, aliasPathname, lastAliasAddress, 2, dataType);
	}
	if (zisError(status)) {
		free(info);
		zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasAdd_ID);
	}

	//  Update the info block
	info[zdssInfoKeys.kinfoAliasesBinAddress] = ifltab[zdssKeys.kpathBinAddress];
	numberInfo = (int)ifltab[zdssKeys.kinfoSize];
	status = zput(ifltab, infoAddress, (int *)info, numberInfo, 2);
	free(info);
	if (zisError(status)) {
		zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasAdd_ID);
	}
	//  Clear last path accessed
	ifltab[zdssKeys.kaddInfoLastPath] = 0;
	ifltab[zdssKeys.kpathnameHash] = 0;

	//  Update file statistics
	zupdatePathStats(ifltab, aliasPathname, strlen(aliasPathname));
	fileHeader[zdssFileKeys.knumberAliases]++;
	if ((fileHeader[zdssFileKeys.kcatSortStatus] > 0) && (fileHeader[zdssFileKeys.kcatSortStatus] < 2)) {
		fileHeader[zdssFileKeys.kcatSortStatus] = 2;
	}

	status = zpermWrite(ifltab);
	if (zmessageLevel(ifltab, MESS_METHOD_ALIAS_ID, MESS_LEVEL_GENERAL)) {
		zmessage2(ifltab, "-----DSS---zaliasAdd:  Primary Pathname: ",primayPathname);
		zmessage2(ifltab, "                       Alias Pathname: ",aliasPathname);
	}

	zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
	return status;
}

