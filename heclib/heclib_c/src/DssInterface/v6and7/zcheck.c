#include <string.h>

#include "heclib.h"
#include "hecdssInternal.h"
#include "zdssKeys.h"

/**
*  Function:	zcheck
*
*  Use:			Public
*
*  Description:	Function to check if an individual record exists
*
*  Declaration: int zcheck(long long *ifltab, const char* pathname);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				const char* pathname
*					The pathname of the record to check.  Must be the full unique pathname
*					(not part of a dataset; e.g., correct D part for time series.)
*
*
*	Returns:	int status
*					STATUS_RECORD_FOUND (=STATUS_OKAY, =0) if record exists
*					STATUS_RECORD_NOT_FOUND (=STATUS_NOT_OKAY, =-1) if record does not exists
*					< -1.   error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*	Remarks:	Version 6 and 7
*					Corrects for "Minute" E part differences between version 6 and 7
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*
*/
int zcheck(long long *ifltab, const char* pathname)
{
	int status;
	int npath;
	int nhead;
	int ndata;
	int boolFound;
	char *path;

	if (zgetVersion(ifltab) == 6) {
		npath = (int)strlen(pathname);
		zcheck6_(ifltab, pathname, &npath, &nhead, &ndata, &boolFound, npath);
		if (boolFound) {
			return STATUS_RECORD_FOUND;
		}
		else {
			path = ztsPathCompatible(6, pathname, (size_t)npath);
			if (path) {
				zcheck6_(ifltab, pathname, &npath, &nhead, &ndata, &boolFound, npath);
				free(path);
			}
			if (boolFound) {
				return STATUS_RECORD_FOUND;
			}
			else {
				return STATUS_RECORD_NOT_FOUND;
			}
		}
	}
	else {
		status = zcheckInternal(ifltab, pathname, 0);
		if (status == STATUS_RECORD_NOT_FOUND) {
			//  Is this minute interval time series, where the e parts
			//  are not the same in DSS version 6, as they are in version 7?
			//  Look for "MIN/" (and replace with "Minute/"
			path = ztsPathCompatible(7, pathname, strlen(pathname));
			if (path) {
				status = zcheckInternal(ifltab, path, 0);
				free(path);
			}
		}
		return status;
	}
}


//  Fortran compatible interface, for DSS-6 calls also
void zcheck7_(long long *ifltab, const char* pathname, int *numberPathname,
			 int *numberHeader, int *numberData, int *lfound, size_t lenPathname)
{
	char *cpath;
	long long *info;
	int status;


	cpath = stringFortToC(pathname, lenPathname);

	status = zreadInfo(ifltab, cpath, 0);
	free(cpath);
	if (zisError(status)) {
		*lfound = 0;
		return;
	}

	if (status == STATUS_RECORD_FOUND) {
		*lfound = 1;
		info = (long long *)ifltab[zdssKeys.kinfo];
		*numberHeader = (int)info[zdssInfoKeys.kinfoUserHeadNumber];
		*numberData = (int)(info[zdssInfoKeys.kinfoValues1Number] + info[zdssInfoKeys.kinfoValues2Number]);
	}
	else {
		*numberHeader = 0;
		*numberData = 0;
		*lfound = 0;
	}
	if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcheck_ID, "Exit zcheck, size of data: ", *numberData);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcheck_ID, "Size of user header: ", *numberHeader);
	}
}

int zcheckrecord_(long long *ifltab, const char* pathname, int *statusWanted, int *istat, size_t lenPathname)
{
	char *cpath;
	int status;

	cpath = stringFortToC(pathname, lenPathname);
	status = zcheckInternal(ifltab, cpath, *statusWanted);
	free(cpath);
	return status;
}


