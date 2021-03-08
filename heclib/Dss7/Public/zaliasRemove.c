#include <string.h>
#include <stdlib.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssLocking.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"


/**
*  Function:	zaliasRemove
*
*  Use:			Public
*
*  Description:	Removes an aliases pathname
*
*  Declaration: int zaliasRemove(long long *ifltab, const char* aliasPathname);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				const char* aliasPathname
*					The alias pathname to remove.  Cannot be the primary.
*
*
*	Returns:	int status
*					STATUS_OKAY
*					STATUS_RECORD_NOT_FOUND
*					< -1.   error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*	Remarks:	This does not delete any data, just the alias pathname.  zdelete will delete all aliases and data.
*					see zaliasRemoveAll() to remove all aliases for one pathname.
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*/

//
int zaliasRemove(long long *ifltab, const char* aliasPathname)
{
	long long *info;
	long long infoAddressInBin;
	long long aliasAddress;
	long long lastAliasAddress;
	long long aliasAddressInInfo;
	long long infoAddress;
	long long address;
	long long *fileHeader;
	long long longStatus;
	long long lnumber;
	int status;
	int numberChars;
	int pathnameSize;
	long long binAlias[4];

	char path[MAX_PATHNAME_LENGTH];


	if (zmessageLevel(ifltab, MESS_METHOD_ALIAS_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zaliasRemove_ID, "Enter;  Handle: ",  zhandle(ifltab));
		zmessageDebug(ifltab, DSS_FUNCTION_zaliasRemove_ID, "Alias Pathname: ", aliasPathname);
	}

	if (!zinquire(ifltab, "write")) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zaliasRemove_ID,
			zdssErrorCodes.WRITE_ON_READ_ONLY, 0, 0,
			zdssErrorSeverity.WARNING_NO_WRITE_ACCESS, aliasPathname, "");
	}

	//  Lock the file if we are in a multi-user access mode
	//  And read the file header
	status = zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_ON, LOCKING_FLUSH_ON);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasRemove_ID);
	}

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];

	//  Force the internals to be loaded during the check
	ifltab[zdssKeys.kaddInfoLastPath] = 0;
	status = zcheckInternal(ifltab, aliasPathname, REC_STATUS_ALIAS);
	if (zisError(status)) {
		zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasRemove_ID);
	}
	if (status != STATUS_RECORD_FOUND) {
		if (zmessageLevel(ifltab, MESS_METHOD_ALIAS_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_zaliasRemove_ID, "Exit zaliasRemove, status: ", status);
			zmessageDebug(ifltab, DSS_FUNCTION_zaliasRemove_ID, "Record not found ", aliasPathname);
		}
		zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
		return status;
	}

	//  Save the location of the alias bin
	aliasAddress = ifltab[zdssKeys.kpathBinAddress];
	infoAddressInBin = ifltab[zdssKeys.kaddInfoLastPath];

	//  Read in the info block
	status = zreadInfo(ifltab, aliasPathname, REC_STATUS_ANY);
	if (zisError(status)) {
		zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasRemove_ID);
	}
	if (status != STATUS_RECORD_FOUND) {
		if (zmessageLevel(ifltab, MESS_METHOD_ALIAS_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_zaliasRemove_ID, "Exit zaliasRemove, status: ", status);
			zmessageDebug(ifltab, DSS_FUNCTION_zaliasRemove_ID, "Record not found ", aliasPathname);
		}
		zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
		return status;
	}
	info = (long long *)ifltab[zdssKeys.kinfo];
	infoAddress = ifltab[zdssKeys.kaddInfoLastPath];
	aliasAddressInInfo = info[zdssInfoKeys.kinfoAliasesBinAddress];
	if (aliasAddressInInfo == 0) {
		if (zmessageLevel(ifltab, MESS_METHOD_ALIAS_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebug(ifltab, DSS_FUNCTION_zaliasRemove_ID, "No aliases found", "");
			charLong(&info[zdssInfoKeys.kinfoPathname], path, (int)info[zdssInfoKeys.kinfoPathnameLength], sizeof(path), 0, 0);
			zmessageDebug(ifltab, DSS_FUNCTION_zaliasRemove_ID, "Primary Pathname: ", path);
		}
		zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
		return STATUS_RECORD_NOT_FOUND;
	}


	if ((aliasAddressInInfo == aliasAddress)  && (infoAddressInBin == infoAddress)) {
		// Case 1 - (most common)  One alias.  Just mark info and pathname bin appropriately
		info[zdssInfoKeys.kinfoAliasesBinAddress] = 0;
		status = zput(ifltab, ifltab[zdssKeys.kaddInfoLastPath], (int *)info, zdssVals.infoSize, 2);
		if (zisError(status)) {
			zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasRemove_ID);
		}
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
	}
	else if (aliasAddressInInfo == aliasAddress){
		//  Case 2 - more than one alias and this is the last one (which the info block points to)
		info[zdssInfoKeys.kinfoAliasesBinAddress] = infoAddressInBin;
		status = zput(ifltab, ifltab[zdssKeys.kaddInfoLastPath], (int *)info, zdssVals.infoSize, 2);
		if (zisError(status)) {
			zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasRemove_ID);
		}
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
	}
	else {
		//  Case 3 - more than one alias and this is not the last one.
		//  Need to walk down the alias chain
		address = aliasAddressInInfo;
		lastAliasAddress = address;
		while (1) {
			status = zget(ifltab, address, (int *)binAlias, 4, 2);
			if (zisError(status)) {
				zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasRemove_ID);
			}
			if (binAlias[0] == DSS_INFO_FLAG) {
				//  Shouldn't get here - we walked back to the primary info area!
				break;
			}
			else {
				//  double check that we are still walking down alias bins
				if (binAlias[zdssBinKeys.kbinStatus] != REC_STATUS_ALIAS) {
					//  Uh-oh, shouldn't have this number - needs to be a "2"!
					zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
					return zerrorProcessing(ifltab, DSS_FUNCTION_zaliasUtil_ID,
										zdssErrorCodes.RECORD_DOES_NOT_EXIST, 0,
										0, zdssErrorSeverity.WARNING, aliasPathname, "");
				}
				if (address == aliasAddress) {
					//  found our alias bin
					//  First store the status flag as deleted
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
					//  Now save the next info address in the previous alias bin
					infoAddressInBin = binAlias[zdssBinKeys.kbinInfoAdd];
					lastAliasAddress += zdssBinKeys.kbinInfoAdd;
					status = zput(ifltab, lastAliasAddress, (int *)&infoAddressInBin, 1, 2);
					if (zisError(status)) {
						zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
						return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasRemove_ID);
					}
					break;
				}
				else {
					lastAliasAddress = address;
					//  Use this alias bins info address and read it's bin or info area
					address = binAlias[zdssBinKeys.kbinInfoAdd];
				}
			}
		}
	}

	//  Update file statistics
	fileHeader[zdssFileKeys.knumberAliases]--;
	fileHeader[zdssFileKeys.knumberAliasDeletes]++;
	fileHeader[zdssFileKeys.kcatSortStatus] = 3;
	fileHeader[zdssFileKeys.kdead] += zdssBinKeys.kbinSize + numberLongsInBytes((int)strlen(aliasPathname));

	status = zpermWrite(ifltab);
	zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasRemove_ID);
	}

	if (zmessageLevel(ifltab, MESS_METHOD_ALIAS_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zaliasRemove_ID, "Exit, status: ", status);
	}
	return status;
}


//  Fortran compatible interface

void zaliasremove_(long long *ifltab, const char* pathname,
			  int *istat, size_t lenPathname)
{
	char *path;
	path = stringFortToC(pathname, lenPathname);
	*istat = zaliasRemove(ifltab, path);
	free(path);
}

