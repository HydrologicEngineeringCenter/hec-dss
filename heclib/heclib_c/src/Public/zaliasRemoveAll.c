#include <string.h>
#include <stdlib.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssLocking.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"


/**
*  Function:	zaliasRemoveAll
*
*  Use:			Public
*
*  Description:	Removes all aliases associated with one primary pathname
*
*  Declaration: int zaliasRemoveAll(long long *ifltab, const char* pathname);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				const char* pathname
*					The seed pathname.  May be a primary or alias.
*
*
*	Returns:	int numberPathnames
*					The number of alias (that were removed), excluding the primary
*					0, if no aliases
*					STATUS_RECORD_NOT_FOUND
*					< -1.   error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*	Remarks:	This does not delete any data, just alias pathnames.  zdelete will delete alias and data.
*					The logic of removing all is different enough from removing one, that it warrants
*					it own function
*				See zaliasAdd for a discussion of aliases
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*/


int zaliasRemoveAll(long long *ifltab, const char* pathname)
{
	long long *info;
	long long aliasAddress;
	long long infoAddress;
	long long *fileHeader;
	long long binAlias[4];
	int npath;
	int pathnameSize;
	long long zero;
	long long longStatus;
	int status;
	int numberAliases;
	long long address;
	long long lnumber;
	int numberChars;

	char path[MAX_PATHNAME_LENGTH];


	if (zmessageLevel(ifltab, MESS_METHOD_ALIAS_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zaliasRemove_ID, "Enter;  Handle: ",  zhandle(ifltab));
		zmessageDebug(ifltab, DSS_FUNCTION_zaliasRemove_ID, " Pathname: ",pathname);
	}

	if (!zinquire(ifltab, "write")) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zaliasRemove_ID,
			zdssErrorCodes.WRITE_ON_READ_ONLY, 0, 0,
			zdssErrorSeverity.WARNING_NO_WRITE_ACCESS, pathname, "");
	}

	//  Lock the file if we are in a multi-user access mode
	//  And read the file header
	status = zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_ON, LOCKING_FLUSH_ON);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasRemove_ID);
	}

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];

	//  Read in the info block
	status = zreadInfo(ifltab, pathname, REC_STATUS_ANY);
	if (zisError(status)) {
		zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasRemove_ID);
	}
	if (status != STATUS_RECORD_FOUND) {
		if (zmessageLevel(ifltab, MESS_METHOD_ALIAS_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_zaliasRemove_ID, "Exit zaliasRemove, status: ", status);
			zmessageDebug(ifltab, DSS_FUNCTION_zaliasRemove_ID, "Record not found ", pathname);
		}
		zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
		return status;
	}
	info = (long long *)ifltab[zdssKeys.kinfo];
	infoAddress = ifltab[zdssKeys.kaddInfoLastPath];
	aliasAddress = info[zdssInfoKeys.kinfoAliasesBinAddress];
	if (aliasAddress == 0) {
		if (zmessageLevel(ifltab, MESS_METHOD_ALIAS_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebug(ifltab, DSS_FUNCTION_zaliasRemove_ID, "No aliases found", "");
			charLong(&info[zdssInfoKeys.kinfoPathname], path, (int)info[zdssInfoKeys.kinfoPathnameLength], sizeof(path), 0, 0);
			zmessageDebug(ifltab, DSS_FUNCTION_zaliasRemove_ID, "Primary Pathname: ", path);
		}
		zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
		return STATUS_RECORD_NOT_FOUND;
	}
	//  Remove the alias address from the info area
	infoAddress += zdssInfoKeys.kinfoAliasesBinAddress;
	zero = 0;
	status = zput(ifltab, infoAddress, (int *)&zero, 1, 2);
	if (zisError(status)) {
		zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasRemove_ID);
	}
	infoAddress = ifltab[zdssKeys.kaddInfoLastPath];
	numberAliases = 0;
	//  Need to walk down the alias chain
	while (1) {
		status = zget(ifltab, aliasAddress, (int *)binAlias, 4, 2);
		if (zisError(status)) {
			zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasRemove_ID);
		}
		if (binAlias[0] == DSS_INFO_FLAG) {
			//  We've walked back to the primary info area, we're done
			break;
		}

		//  double check that we are still walking down alias bins
		if (binAlias[zdssBinKeys.kbinStatus] != REC_STATUS_ALIAS) {
			//  Uh-oh, shouldn't have this number - needs to be a "2"!
			zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
			return zerrorProcessing(ifltab, DSS_FUNCTION_zaliasUtil_ID,
								zdssErrorCodes.RECORD_DOES_NOT_EXIST, 0,
								0, zdssErrorSeverity.WARNING, pathname, "");
		}
		//  Store the status flag as deleted
		longStatus = (long long)REC_STATUS_ALIAS_DELETED;
		address = aliasAddress + zdssBinKeys.kbinStatus;
		status = zput(ifltab, address, (int *)&longStatus, 1, 2);
		if (zisError(status)) {
			zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasRemove_ID);
		}
		//  Need to mark deleted and number chars different, so that bin spot will not be reused
		//  Get the length of the pathname
		address = aliasAddress + zdssBinKeys.kbinPathLen;
		status = zget(ifltab, address, (int *)&lnumber, 1, 2);
		if (zisError(status)) {
			zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasRemove_ID);
		}
		i8toi4(lnumber, &numberChars, &pathnameSize);
		//  Now set the number of characters to one less, so will not match in zcheck
		numberChars -= 1;
		lnumber = i4toi8(numberChars, pathnameSize);
		status = zput(ifltab, address, (int *)&lnumber, 1, 2);
		if (zisError(status)) {
			zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasRemove_ID);
		}
		numberAliases++;
		//  Get the next alias or bail
		aliasAddress = binAlias[zdssBinKeys.kbinInfoAdd];
		i8toi4(binAlias[zdssBinKeys.kbinPathLen], &npath, &pathnameSize);
		fileHeader[zdssFileKeys.kdead] += zdssBinKeys.kbinSize + pathnameSize;
		if (aliasAddress == infoAddress) {
			break;
		}
	}
	fileHeader[zdssFileKeys.knumberAliases] -= numberAliases;
	fileHeader[zdssFileKeys.knumberAliasDeletes] += numberAliases;
	fileHeader[zdssFileKeys.kcatSortStatus] = 3;

	status = zpermWrite(ifltab);
	zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasRemove_ID);
	}

	if (zmessageLevel(ifltab, MESS_METHOD_ALIAS_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zaliasRemove_ID, "Exit, status: ", status);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zaliasRemove_ID, "Number of alias: ", numberAliases);
	}

	return numberAliases;
}

