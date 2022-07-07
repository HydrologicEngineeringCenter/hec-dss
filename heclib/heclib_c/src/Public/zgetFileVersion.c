
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hecdssInternal.h"
#include "hecdss7.h"


/**
*  Function:	zgetFileVersion
*
*  Use:			Public
*
*  Description:	Returns the version number of a (unopened) DSS file
*
*  Declaration: int zgetFileVersion(const char *dssFilename)
*
*  Parameters:
*				char *dssFilename:  The DSS file name to check the version of.
*				The file does not have to exist (in which case zero is returned)
*
*	Returns:	7:  A DSS version 7 file
*				6:  A DSS version 6 file
*				0:  File does not exist
*			   -1:  Not a DSS file (but file exists)
*              -2:  Invalid file name
*			   -3:  Open error (undefined)
*			  <-3:  abs(error) is system open or read error
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/
 int zgetFileVersion(const char *dssFilename)
 {
	 int ihandle;
	 int status;
	 int i;
	 int permission;
	 int iswap;
	 int fileExists;
	 char fullDssFilename[_MAX_PATH];
	 long long iarray[4];
	 char messageString[25];
	 char cdss[5];
	 char cvers[5];	 

	 *fullDssFilename = NULL;
	 fileExists = zfileName (fullDssFilename, _MAX_PATH, dssFilename, &permission);
	 if (fileExists < 0) {
		 return -2;
	 }

	 if (!fileExists) {
		 return 0;
	 }

	status = zopenDisk(fullDssFilename, &ihandle, 0, 0);
	if (status) {
		 if (status > 0) {
			return -status;
		}
		else {
			return -3;
		}
	}

	iswap = getEndian();
	status = zreadDisk (ihandle, iswap, 0L, iarray, 2*4);
	closeFile(ihandle);

	if (zisError(status)) {
		if (status < -10) {
			// an empty file.... can make DSS
			return STATUS_NOT_OKAY;
		}
		if (status > 0) {
			return -status;
		}
		else {
			return status;
		}
	}

	charLong(iarray, messageString, 24, sizeof(messageString), 0, 1);
	for (i=0; i<4; i++) {
		cdss[i] = messageString[i];
		cvers[i] = messageString[i+16];
	}
	cdss[4] = '\0';
	cvers[4] = '\0';

	if (strncmp(cdss, "ZDSS", 4) != 0) {
		return STATUS_NOT_OKAY;
	}

	if (!strncmp(cvers, "6", 1)) {
		return 6;
	}
	else if (!strncmp(cvers, "7", 1)) {
		return 7;
	}
	else if (!strncmp(cvers, "8", 1)) {
		return 8;
	}
	else if (!strncmp(cvers, "5", 1)) {
		return 5;
	}
	else {
		return STATUS_NOT_OKAY;
	}

}

