#include <string.h>
#include <stdlib.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssLocking.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"


/**
*  Function:	zaliasGetPrimary
*
*  Use:			Public
*
*  Description:	Gets the primary pathname from an alias pathname
*
*  Declaration: iint zaliasGetPrimary(long long *ifltab, const char* aliasPathname,
*									  char* primayPathname, size_t maxLenPrimayPathname);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				const char* aliasPathname
*					The alias pathname.  Must be a full unique pathname and follow rules.
*
*				char* primayPathname
*					A character array that will contain the primary pathname on return.
*
*				size_t maxLenPrimayPathname
*					The size of (number of characters) in primayPathname
*
*
*	Returns:	int status
*					STATUS_RECORD_FOUND if primary pathname found and not returned
*					STATUS_RECORD_NOT_FOUND  if primary record does not exist
*					< -1.   error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*	Remarks:		See zaliasAdd for a discussion of aliases
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*/

//
int zaliasGetPrimary(long long *ifltab, const char* aliasPathname, char* primayPathname, size_t maxLenPrimayPathname)
{
	int npath;
	int numberInfo;
	long long *info;
	long long *pathnameBin;
	long long address;
	long long infoAddress;
	int count;
	int istat = 0;
	int status;



	if (zmessageLevel(ifltab, MESS_METHOD_ALIAS_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zaliasUtil_ID, "Enter zaliasGetPrimary;  Handle: ",  zhandle(ifltab));
	}

	//   Check that the record exists, and get the address of the info block (in zcheck)
	status = zcheck(ifltab, aliasPathname);
	ifltab[zdssKeys.kaddInfoLastPath] = 0;
	status = zcheck(ifltab, aliasPathname);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasUtil_ID);
	}

	if (status != STATUS_RECORD_FOUND) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zaliasUtil_ID,
								zdssErrorCodes.RECORD_DOES_NOT_EXIST, 0,
								0, zdssErrorSeverity.WARNING, aliasPathname, "");
	}

	//  Is this not an alias?
	if (ifltab[zdssKeys.kbinStatus] != REC_STATUS_ALIAS) {
		if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebug(ifltab, DSS_FUNCTION_zaliasUtil_ID, "zaliasGetPrimary, Primary Pathname passed in: ", aliasPathname);
		}
		stringCopy(primayPathname, maxLenPrimayPathname, aliasPathname, strlen(aliasPathname));
		return STATUS_RECORD_FOUND;
	}

	//  Need to walk chain to primary
	address = ifltab[zdssKeys.kaddInfoLastPath];
	info = (long long *)ifltab[zdssKeys.kinfo];
	pathnameBin = (long long *)ifltab[zdssKeys.kpathBin];
	count = 0;
	infoAddress = 0;
	while (1) {
		status = zget(ifltab, address, (int *)pathnameBin, 4, 2);
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasUtil_ID);
		}
		if (pathnameBin[0] == DSS_INFO_FLAG) {
			//  We've walked back to the primary info area, we're done
			infoAddress = address;
			break;
		}

		//  double check that we are still walking down alias bins
		if (pathnameBin[zdssBinKeys.kbinStatus] != REC_STATUS_ALIAS) {
			//  Uh-oh, shouldn't have this number - needs to be a "2"!
			return zerrorProcessing(ifltab, DSS_FUNCTION_zaliasUtil_ID,
								zdssErrorCodes.RECORD_DOES_NOT_EXIST, 0,
								0, zdssErrorSeverity.WARNING, aliasPathname, "");
		}

		//  Get the next alias or bail
		address = pathnameBin[zdssBinKeys.kbinInfoAdd];
		count++;
	}

	//  We've located the primary  info area.
	//  Get the pathname
	if (infoAddress) {
		ifltab[zdssKeys.kaddInfoLastPath] = infoAddress;
		npath = (int)pathnameBin[zdssInfoKeys.kinfoPathnameLength];
		numberInfo = zdssVals.infoSize + numberLongsInBytes(npath);
		if (numberInfo > zdssVals.maxInfoSize) {
			numberInfo = zdssVals.maxInfoSize;
		}
		status = zget(ifltab, ifltab[zdssKeys.kaddInfoLastPath], (int *)ifltab[zdssKeys.kinfo], numberInfo, 2);
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zreadInfo_ID);
		}

		if ((int)maxLenPrimayPathname < (npath+1)) npath = (int)maxLenPrimayPathname - 1;  //  leave room for '\0'
		charInt((void *)&info[zdssInfoKeys.kinfoPathname], (void *)primayPathname, npath, (int)maxLenPrimayPathname, 0, 0, 0);
		primayPathname[npath] = '\0';
	}
	else {
		//  Not found
		status = STATUS_RECORD_NOT_FOUND;
	}

	if (zmessageLevel(ifltab, MESS_METHOD_ALIAS_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zaliasUtil_ID, "Exit zaliasGetPrimary, status: ", status);
		if (status == STATUS_RECORD_FOUND) {
			zmessageDebug(ifltab, DSS_FUNCTION_zaliasUtil_ID, "Primary Pathname: ", primayPathname);
				zmessageDebug(ifltab, DSS_FUNCTION_zaliasUtil_ID, "Alias Pathname: ", aliasPathname);
		}
		else {
			zmessageDebug(ifltab, DSS_FUNCTION_zaliasUtil_ID, "Record not found: ", primayPathname);
		}
	}
	return status;
}
