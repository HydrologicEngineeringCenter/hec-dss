#include <string.h>
#include <stdlib.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssLocking.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"


/**
*  Function:	zaliasNumber
*
*  Use:			Public
*
*  Description:	Gets the number of aliases associated with a pathname
*
*  Declaration: int zaliasNumber(long long *ifltab, const char* pathname);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				const char* pathname
*					The seed pathname.  May be a primary or alias.
*
*
*	Returns:	int numberPathnames
*					The number of alias pathnames in the list, excluding the primary
*					0, if no aliases
*					STATUS_RECORD_NOT_FOUND
*					< -1.   error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*
See zaliasAdd for a discussion of aliases
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*/


//
int zaliasNumber(long long *ifltab, const char* pathname)
{
	long long *info;
	long long aliasAddress;
	long long infoAddress;
	long long binAlias[4];
	int status;
	int numberAliases;

	char path[MAX_PATHNAME_LENGTH];


	if (zmessageLevel(ifltab, MESS_METHOD_ALIAS_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zaliasUtil_ID, "Enter zaliasList;  zaliasNumber: ",  zhandle(ifltab));
		zmessageDebug(ifltab, DSS_FUNCTION_zaliasUtil_ID, "Seed Pathname: ", pathname);
	}

	//  Read in the info block
	status = zreadInfo(ifltab, pathname, 0);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasUtil_ID);
	}
	if (status != STATUS_RECORD_FOUND) {
		if (zmessageLevel(ifltab, MESS_METHOD_ALIAS_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_zaliasUtil_ID, "Exit zaliasNumber, status: ", status);
			zmessageDebug(ifltab, DSS_FUNCTION_zaliasUtil_ID, "Record not found ", "");
		}
		return status;
	}

	info = (long long *)ifltab[zdssKeys.kinfo];
	aliasAddress = info[zdssInfoKeys.kinfoAliasesBinAddress];
	infoAddress = ifltab[zdssKeys.kaddInfoLastPath];
	if (zmessageLevel(ifltab, MESS_METHOD_ALIAS_ID, MESS_LEVEL_USER_DIAG)) {
		charLong(&info[zdssInfoKeys.kinfoPathname], path, (int)info[zdssInfoKeys.kinfoPathnameLength], sizeof(path), 0, 0);
		zmessageDebug(ifltab, DSS_FUNCTION_zaliasUtil_ID, "Primary Pathname: ", path);
	}
	numberAliases = 0;
	if (aliasAddress != 0) {
		//  Need to walk down the alias chain
		while (1) {
			status = zget(ifltab, aliasAddress, (int *)binAlias, 4, 2);
			if (zisError(status)) {
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasUtil_ID);
			}
			if (binAlias[0] == DSS_INFO_FLAG) {
				//  We've walked back to the primary info area, we're done
				break;
			}

			//  double check that we are still walking down alias bins
			if (binAlias[zdssBinKeys.kbinStatus] != REC_STATUS_ALIAS) {
				//  Uh-oh, shouldn't have this number - needs to be a "2"!
				return zerrorProcessing(ifltab, DSS_FUNCTION_zaliasUtil_ID,
									zdssErrorCodes.RECORD_DOES_NOT_EXIST, 0,
									0, zdssErrorSeverity.WARNING, pathname, "");
			}

			numberAliases++;
			//  Get the next alias or bail
			aliasAddress = binAlias[zdssBinKeys.kbinInfoAdd];
			if (aliasAddress == infoAddress) {
				break;
			}
		}
	}
	if (zmessageLevel(ifltab, MESS_METHOD_ALIAS_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zaliasUtil_ID, "Exit zaliasNumber, status: ", status);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zaliasUtil_ID, "Number of alias: ", numberAliases);
	}

	return numberAliases;
}


//  Fortran compatible interface

void zaliasnumber_(long long *ifltab, const char* pathname,
			  int *number, size_t lenPathname)
{
	char *path;
	path = stringFortToC(pathname, lenPathname);
	*number = zaliasNumber(ifltab, path);
	free(path);
}

