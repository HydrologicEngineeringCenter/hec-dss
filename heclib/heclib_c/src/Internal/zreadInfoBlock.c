
#include <stdio.h>
#include <string.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssVals.h"
#include "hecdssInternal.h"
#include "zdssMessages.h"



/**
*  Function:	zreadInfoBlock
*
*  Use:			Private (Internal)
*
*  Description:	zreadInfo, except does not save a pointer into ifltab.
*					Checks for the existence of an individual record then reads its info block into memory
*
*  Declaration: int zreadInfoBlock (long long *ifltab, const char *pathname, int statusWanted,
					long long *info, int maxInfo, int *numberInfo);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				const char *pathname
*					The pathname of the record to search for.  This must be a fully defined pathname, with the
*					correct D (date) part and E (time interval) parts for time series (i.e., no date range).
*					The pathname must match exactly (except for case) the record being searched for.
*
*				int statusWanted:
*					REC_STATUS_VALID (0):		All valid (primary and alaises)
*					REC_STATUS_PRIMARY (1):		Primary only
*					REC_STATUS_ALIAS (2):		Alias only
*					REC_STATUS_DELETED (11):	Deleted only
*					REC_STATUS_RENAMED (12):	Renamed only
*					REC_STATUS_ANY (100):		Any, regardless if deleted, renamed, etc.
*
*				long long *info
*					The array to store the info block in.
*
*				int maxInfo
*					The sze of the info block.
*
*				int *numberInfo
*					The number of ints returned in *info.
*
*	Returns:	int status
*					STATUS_RECORD_NOT_FOUND
*					STATUS_RECORD_FOUND
*					errorCode for an error .
*
*	Note:		Since zreadInfo does a zcheck first, most DSS functions that will access the record
*					will call this instead of zcheck to load in the info area for access
*
*
*	What is stored in info block:
*
*			Information Block flag:		-97534
*			Record Status:				-1:  Record expanded and moved
*										 1:  Good
*										 2:  Deleted
*			Pathname length
*			Data type / Version number
*					Data type is the code for type (e.g., regular time series, paired)
*					Version number - the number of times this record has been written
*			Number Expansions / Expansion Flag
*					Number Expansions - the number of times the size has increase
*					Expansion Flag - A flag indicating how the record should be treated
*					during a squeeze
*			Compression Flag/ Precision
*					Compression 0:  Not used,  > 0 compressed (number indicates type of compression)
*					Precision of data in places past decimal (1=tenth, 2=hundredth)
*			Date and time last written:	In seconds (from Jan 01, 1970)
*			Program name:				Name of program last wrote (limit 8 char)
*			Julian date and time (seconds) of first valid value (if time series)
*			Julian date and time (seconds) of last  valid value (if time series)
*			When the record was originally created (milliseconds)
*			Unused
*			Address Internal Header
*			Length Internal Header
*			Address Compression Header
*			Length Compression Header
*			Address Description Header
*			Length Description Header in bytes
*			Address User Header
*			Length User Header
*			Address of data area
*			Data length (used - this may be less than allocated)
*			Allocated data area / number data values
*			Logical number data / length (ints) per value
*			     Allocated data area (includes unused space for expansion)
*				 Logical number of data  (after uncompression) - what the user would eventually see
*			Number of aliases (addresses follow pathname)
*			Unused (reserved) space
*			Pathname
*			Alias address 1 (optional)
*			Alias address 2 (optional)
*			Alias ...
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zreadInfoBlock (long long *ifltab, const char *pathname, int statusWanted,
					long long *info, int maxInfo, int *numberInfo)
{
	int status;
	int istat;


	if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zreadInfo_ID, "Enter;  Handle: ",  zhandle(ifltab));
		zmessageDebug(ifltab, DSS_FUNCTION_zreadInfo_ID, " Pathname: ",pathname);
	}

	status = zcheckInternal(ifltab, pathname, statusWanted);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zreadInfo_ID);
	}

	// existing record
	if (status == STATUS_RECORD_FOUND) {
		//  Read in the info block
		*numberInfo = zdssVals.infoSize + numberLongsInBytes((int)ifltab[zdssKeys.klenLastPath]);
		if (*numberInfo > maxInfo)
			*numberInfo = maxInfo;
		istat = zget(ifltab, ifltab[zdssKeys.kaddInfoLastPath], (int *)info, *numberInfo, 2);
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zreadInfo_ID);
		}
	}
	else {
		*numberInfo = 0;
	}
	return status;
}

