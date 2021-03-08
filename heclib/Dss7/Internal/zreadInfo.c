
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssVals.h"
#include "hecdssInternal.h"
#include "zdssMessages.h"



/**
*  Function:	zreadInfo
*
*  Use:			Private (Internal)
*
*  Description:	Checks for the existence of an individual record then reads its info block into memory
*
*  Declaration: int zreadInfo(long long *ifltab, const char *pathname, int statusWanted);
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
*					REC_STATUS_VALID (0):		All valid (primary and aliases)
*					REC_STATUS_PRIMARY (1):		Primary only
*					REC_STATUS_ALIAS (2):		Alias only
*					REC_STATUS_DELETED (11):	Deleted only
*					REC_STATUS_RENAMED (12):	Renamed only
*					REC_STATUS_ANY (100):		Any, regardless if deleted, renamed, etc.
*
*
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
*
*  Called By:	Most DSS functions to load pathname bin tables
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int zreadInfo(long long *ifltab, const char *pathname, int statusWanted)
{

	int status;
	int numberInfo;
	int dataType;
	int version;
	long long *info;
	char *path;
	char messageString[120];

	if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zreadInfo_ID, "Pathname: ", pathname);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d;", zhandle(ifltab));
		zmessageDebug(ifltab, DSS_FUNCTION_zreadInfo_ID, "Handle: ", messageString);
	}

	//   Check that the record exists, and get the address of the info block (in zcheck)
	if (statusWanted == 0) {
		status = zcheck(ifltab, pathname);
	}
	else {
		status = zcheckInternal(ifltab, pathname, statusWanted);
	}
	if (zisError(status)) {
		//  An error code
		status = zerrorUpdate(ifltab, status, DSS_FUNCTION_zreadInfo_ID);
		return status;
	}

	//  Is this an alias?
	if (ifltab[zdssKeys.kbinStatus] == REC_STATUS_ALIAS) {
		if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebug(ifltab, DSS_FUNCTION_zreadInfo_ID, "Pathname found is an alias; pathname: ", pathname);
		}
		//  Need to walk chain to primary
		path = (char *)calloc(MAX_PATHNAME_SIZE, 1);
		if (!path) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_zreadInfo_ID,
							zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, MAX_PATHNAME_SIZE, 0,
							zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for pathname");
		}
		status = zaliasGetPrimary(ifltab, pathname, path, MAX_PATHNAME_SIZE);
		if (zisError(status)) {
			free(path);
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zreadInfo_ID);
		}
		if (status != STATUS_RECORD_FOUND) {
			free(path);
			return status;
		}
		//  Load info block
		if (statusWanted == 0) {
			status = zcheck(ifltab, path);
		}
		else {
			status = zcheckInternal(ifltab, path, statusWanted);
		}
		free(path);
		if (zisError(status)) {
			//  An error code
			status = zerrorUpdate(ifltab, status, DSS_FUNCTION_zreadInfo_ID);
			return status;
		}
	}

	// existing record
	info = (long long *)ifltab[zdssKeys.kinfo];
	if (status == STATUS_RECORD_FOUND) {
		//  Read in the info block
		numberInfo = zdssVals.infoSize + numberLongsInBytes((int)ifltab[zdssKeys.klenLastPath]);
		if (numberInfo > zdssVals.maxInfoSize) {
			numberInfo = zdssVals.maxInfoSize;
		}
		status = zget(ifltab, ifltab[zdssKeys.kaddInfoLastPath], (int *)ifltab[zdssKeys.kinfo], numberInfo, 2);
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zreadInfo_ID);
		}
		//  Double check hash (If space reused, could be another record at this spot)
		if (info[zdssInfoKeys.kinfoHash] != ifltab[zdssKeys.kpathnameHash]) {
			status = STATUS_RECORD_NOT_FOUND;
			if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
				zmessageDebug(ifltab, DSS_FUNCTION_zreadInfo_ID, "Info hash does not match (used by another record) ", pathname);
			}
		}
		ifltab[zdssKeys.kiftPathHash] = ifltab[zdssKeys.kpathnameHash];
		ifltab[zdssKeys.kinfoAddress] = ifltab[zdssKeys.kaddInfoLastPath];
		ifltab[zdssKeys.kinfoSize] = numberInfo;
		if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			i8toi4(info[zdssInfoKeys.kinfoTypeVersion], &dataType, &version);
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%lld; Info size %d; Data Type %d; Version %d",
				ifltab[zdssKeys.kinfoAddress], numberInfo, dataType, version);
			zmessageDebug(ifltab, DSS_FUNCTION_zreadInfo_ID, "Exit; Record found.  Info address ", messageString);
		}
	}

	if (status != STATUS_RECORD_FOUND) {
		//  Record does not exist.  Make sure pointers are null now.
		info[0] = 0;
		ifltab[zdssKeys.kiftPathHash] = 0;
		ifltab[zdssKeys.kinfoAddress] = 0;
		if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			zmessageDebug(ifltab, DSS_FUNCTION_zreadInfo_ID, "Exit; Record Not found: ", pathname);
		}
	}

	return status;
}

