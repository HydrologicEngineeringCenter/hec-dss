#include <string.h>
#include <stdlib.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssLocking.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"


/**
*  Function:	zaliasList
*
*  Use:			Public
*
*  Description:	Gets a list of aliases associated with a pathname
*
*  Declaration: int zaliasList(long long *ifltab, const char* pathname, char** pathameList, int *pathnameListLength);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				const char* pathname
*					The seed pathname.  May be a primary or alias
*
*				char** pathameList
*					A pointer to a character array that will contain a list of the alias pathnames on return.
*					Memory is malloced, so be sure to free the list upon completion.
*
*				int *pathnameListLength
*					A pointer to an int that will contain the number of characters in the pathnameList.
*
*
*
*	Returns:	int numberPathnames
*					The number of pathnames in the list, with the primary the first
*					0, if no primary
*					< -1.   error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*	Discussion:	The pathnameList will contain the primary pathname as the first pathname in the list,
*					followed by aliases, usually in the order of last one added to first added.
*					Each pathname is null terminated.
*
*	Example:
*					char* pathnameList;
*					int pathnameListLength;
*
*					status = zaliasList(ifltab, "/A/B/C/D/E/F/", &pathnameList, &pathnameListLength);
*					printf("Primary: %s\n", pathnameList);
*					count = strlen(pathnameList) + 1;
*					while (count <pathnameListLength) {
*						printf("Alias: %s\n", &pathnameList[count]);
*						count += strlen(&pathnameList[count]) + 1;
*					}
*					free(pathnameList);
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
int zaliasList(long long *ifltab, const char* pathname, char** pathameList, int *pathnameListLength)
{
	long long *info;
	long long *bin;
	long long aliasAddress;
	long long infoAddress;
	long long binAlias[4];
	int i;
	int number;
	int numberPathnames;
	int npath;
	int pathnameSize;
	int status;

	char *list;
	char *listTemp;


	if (zmessageLevel(ifltab, MESS_METHOD_ALIAS_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zaliasUtil_ID, "Enter zaliasList;  Handle: ",  zhandle(ifltab));
		zmessageDebug(ifltab, DSS_FUNCTION_zaliasUtil_ID, "Seed Pathname: ", pathname);
	}

	list = 0;
	listTemp = 0;
	*pathnameListLength = 0;
	numberPathnames = 0;

	status = zreadInfo(ifltab, pathname, 1);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasUtil_ID);
	}
	if (status != STATUS_RECORD_FOUND) {
		*pathnameListLength = 0;
		*pathameList = 0;
		return status;
	}


	//  Get primary path
	info = (long long *)ifltab[zdssKeys.kinfo];
	//  Get the number of characters in the pathname and re-read bin with correct length
	npath = (int)info[zdssInfoKeys.kinfoPathnameLength];
	*pathnameListLength = npath+1;
	list = (char *)calloc(*pathnameListLength + 9, 1);
	if (!list) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zaliasAdd_ID,
							zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, *pathnameListLength, 0,
							zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for alias pathnameList");
	}
	charLong((void *)&info[zdssInfoKeys.kinfoPathname], list, npath, (*pathnameListLength + 9), 0, 0);
	numberPathnames++;

	if (zmessageLevel(ifltab, MESS_METHOD_ALIAS_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zaliasUtil_ID, "Primary Pathname: ", list);
	}

	//  Now walk the alias pathname bins
	aliasAddress = info[zdssInfoKeys.kinfoAliasesBinAddress];
	infoAddress = ifltab[zdssKeys.kaddInfoLastPath];
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
									0, zdssErrorSeverity.WARNING, list, "");
			}

			i8toi4(binAlias[zdssBinKeys.kbinPathLen], &npath, &pathnameSize);
			number = zdssBinKeys.kbinSize + pathnameSize;
			bin = (long long *)ifltab[zdssKeys.kinfo];
			status = zget(ifltab, aliasAddress, (int *)bin, number, 2);
			if (zisError(status)) {
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_zaliasUtil_ID);
			}
			listTemp = (char *)calloc(*pathnameListLength + npath + 9, 1);
			if (!listTemp) {
				return zerrorProcessing(ifltab, DSS_FUNCTION_zaliasAdd_ID,
						zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, (*pathnameListLength + npath + 1), 0,
						zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for alias pathnameList");
			}
			//  Copy list including nulls
			for (i = 0; i < (*pathnameListLength + 1); i++) {
				listTemp[i] = list[i];
			}
			charLong((void *)&bin[zdssBinKeys.kbinPath], (void *)&listTemp[*pathnameListLength], npath, (npath + 9), 0, 1);
			*pathnameListLength += npath + 1;
			free(list);
			list = listTemp;
			listTemp = 0;
			numberPathnames++;

			//  Get the next alias or exit if done
			aliasAddress = binAlias[zdssBinKeys.kbinInfoAdd];
			if (aliasAddress == infoAddress) {
				break;
			}
		}
	}
	*pathameList = list;

	if (zmessageLevel(ifltab, MESS_METHOD_ALIAS_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zaliasUtil_ID, "Exit zaliasList, status: ", status);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zaliasUtil_ID, "Number of pathnames in list: ", numberPathnames);
	}

	return numberPathnames;
}

