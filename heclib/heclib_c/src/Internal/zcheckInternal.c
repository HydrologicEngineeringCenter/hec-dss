#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#include "zdssKeys.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"
#include "hecdss7.h"
#include "zerrorCodes.h"


/**
*  Function:	zcheckInternal
*
*  Use:			Private (Internal)
*
*  Description:	Checks for the existence of an individual record; loads appropriate pathname bins
*
*  Declaration: int zcheckInternal(long long *ifltab, const char* pathname, int statusWanted);
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
*					REC_STATUS_PRIMARY (1):	Primary only
*					REC_STATUS_ALIAS (2):		Alias only
*					REC_STATUS_DELETED (11):	Deleted only
*					REC_STATUS_RENAMED (12):	Renamed only
*					REC_STATUS_ANY (100):			Any, regardless if deleted, renamed, etc.
*
*
*
*	Returns:	int status
*					STATUS_RECORD_NOT_FOUND for record not found.
*					STATUS_RECORD_FOUND.
*					errorCode for an error.
*
*	Note:		Function may be referred to "zcheck" within code.  This is a primary function
*					that is called by most DSS accesses.  It will load the appropriate pathname
*					bins into memory, regardless if the record is found or not.  This allows
*					a new write to save the pathname in the correct bin.
*
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


int zcheckInternal(long long *ifltab, const char* pathname, int statusWanted)
{
	int status;
	int boolFound;
	int pathnameLength;
	int tableHash;
	int isize;
	int boolDoubleCheckPath;
	int type;
	int temp;
	long long pathnameHash;
	long long infoAddress = 0;
	long long binAddress;
	long long *pathnameBin;
	long long binStatus;
	long long *info;
	long long binAlias[8];
	int icount;
	int numberChars;
	int pathnameSize;
	int pathSizeIn;
	long long *fileHeader;
	char messageString[80];



	//  Double check pathnames for a rare collision for pathname hash
	boolDoubleCheckPath = 1;
	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zcheck_ID, "Pathname: ", pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcheck_ID, "Handle: ", zhandle(ifltab));
	}

	//  zcheck is called frequently, so this is a good place to ensure
	//  that ifltab has not become corrupt.
	status = zcheckKeys(ifltab);
	if (zisError(status)) {
		status = zerrorUpdate(ifltab, status, DSS_FUNCTION_zcheck_ID);
		return status;
	}
	//  Since zcheck is called a lot, make sure we don't continue
	//  if a severe error has been thrown
	if (ifltab[zdssKeys.kerrorSevere]) {
		return (int)ifltab[zdssKeys.kerrorCode];
	}

	//  Check if another process wants to write to our file
	if ((ifltab[zdssKeys.kmultiUserAccess] == SINGLE_USER_ADVISORY_ACCESS) && (ifltab[zdssKeys.kfileWritten] != 0)) {
		zcheckMultiUser(ifltab);
	}

	//  The first operation we will do is check to see if this pathname has just been checked
	//  Checking a pathname from different functions can occur frequently enough that it
	//  it is advantageous to make sure that the information checked is kept in memory.

	//  Initialize variables
	ifltab[zdssKeys.kpathsThisHash] = 0;
	ifltab[zdssKeys.ksameHash] = 0;
	pathnameLength = (int)strlen(pathname);
	if (pathnameLength > 392) pathnameLength = 392;
	pathSizeIn = numberLongsInBytes(pathnameLength);

	//  Get the hash code for this pathname and keep in ifltab
	//  The hash code is the key for looking up the pathname in the hash table (which points to record locations)

	//  NOTE:  There are two different kinds of hash codes computed by zhash.
	//  The first is the table hash, which is a smaller number (0 to 10,000) that
	//  is used to look up a pathname bin in the main hash table, called the table hash
	//  The second is an almost unique number for that pathname to quickly
	//  check if it is the pathname being looked for, called the bin hash.  This hash is usually a large number.
	//
	//  zhash also returns if this pathname is a collection path
	ifltab[zdssKeys.kisaCollection] = zhash (ifltab, pathname, pathnameLength, &tableHash, &pathnameHash);

	//  Have we already checked this record and don't need to check again?
	if ((ifltab[zdssKeys.kpathnameHash] == pathnameHash) && ((int)ifltab[zdssKeys.ktableHash] == tableHash) &&
		(ifltab[zdssKeys.klenLastPath] == pathnameLength) && (statusWanted == REC_STATUS_VALID)) {
		//  Just checked - make sure that we are in a mode to use this info
		if ((ifltab[zdssKeys.kmultiUserAccess] != 2) || (ifltab[zdssKeys.kfound] != -1)) {
			boolFound = -1;
			if ((fileHeader[zdssFileKeys.khashCollisions] > 0) || boolDoubleCheckPath){  //  Double check if there are hash collisions
				if (zpathnameCompare(pathname, &ifltab[zdssKeys.kpathAddressInBin], (size_t)pathnameLength)) {
					boolFound = (int)ifltab[zdssKeys.kfound];
				}
				else {
					//  Not it - check normal way
					//  This is very rare (1 out of 100,000,000 or so)
				}
			}
			else {
				boolFound = (int)ifltab[zdssKeys.kfound];
			}
			//  If the address of the info is not set, then the record has not been loaded.
			if (ifltab[zdssKeys.kaddInfoLastPath] == 0) {
				if (boolFound) {
					//  Must have a valid info area
					boolFound = -1;
				}
				else {
					//  If not found, then must have been locked since last check
					if (!ifltab[zdssKeys.klockCheckSet]) {
						boolFound = -1;
					}
					//  Else file has been locked since check - let found remain
				}
			}
			if (boolFound != -1) {
				if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_USER_DIAG)) {
					if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
						_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d;", boolFound);
						zmessageDebug(ifltab, DSS_FUNCTION_zcheck_ID, "Record previously checked; found: ", messageString);
					}
					if (boolFound) {
						zmessage2(ifltab, ZCHECK_FOUND, pathname);
					}
					else {
						zmessage2(ifltab, ZCHECK_NOT_FOUND, pathname);
					}
				}
				if (boolFound) {
					return STATUS_RECORD_FOUND;
				}
				else {
					return STATUS_RECORD_NOT_FOUND;
				}
			}
		}
	}


	//  Record not loaded, get it the normal way
	if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zcheck_ID, "Record not previously checked; Searching file for record", "");
	}
	boolFound = 0;
	//  If we are not locked, get the perm section of the file.
	if (!ifltab[zdssKeys.klocked]) {
		status = zpermRead(ifltab);
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcheck_ID);
		}
	}
	//  From the table hash code computed by zhash, look in the hash table for a bin address.
	//  If that address is zero, there are no records in the file that have this hash (and this one does not exist)
	//  If a valid address, it points to the pathname bin, which lists the pathnames and their addresses to data

	ifltab[zdssKeys.ktableHash] = (long long)tableHash;
	ifltab[zdssKeys.kpathnameHash] = pathnameHash;
	ifltab[zdssKeys.kaddTableHash] = ifltab[zdssKeys.ktableHash] + fileHeader[zdssFileKeys.kaddHashTableStart];
	ifltab[zdssKeys.klenLastPath] = pathnameLength;

	//  Clear pointers
	//  This must be set to zero before walking down any bins
	ifltab[zdssKeys.kbinWithSpace] = 0;
	//  Zero the address of the info area of the last record
	//  checked to clear the last record accessed
	ifltab[zdssKeys.kaddInfoLastPath] = 0;
	ifltab[zdssKeys.kbinAddCurrent] = 0;
	//  ifltab[zdssKeys.kinfo] points to a memory location for the info area for this ifltab
	//  We'll use that to read in the info are for this record, if found.
	info = (long long *)ifltab[zdssKeys.kinfo];
	//  Set the first element of the info array in memory to zero to indicate it is not used
	info[0] = 0;

	//  read the address from the hash table for this pathname table hash
	status = zget(ifltab, ifltab[zdssKeys.kaddTableHash], (void *)&ifltab[zdssKeys.khashTableBinAdd], 1, 2);
	if (zisError(status)) {
		zmessage2(ifltab, "Reading hash table for record ", pathname);
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcheck_ID);
	}

	//  Does an address exist in the hash table for this hash code?
	if (ifltab[zdssKeys.khashTableBinAdd] == 0) {
		//  No hash table entry; record does not exist
		ifltab[zdssKeys.kfound] = 0;
		if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_USER_DIAG)) {
			zmessage2(ifltab, ZCHECK_NOT_FOUND, pathname);
		}
		ifltab[zdssKeys.klockCheckSet] = ifltab[zdssKeys.klocked];
		return STATUS_RECORD_NOT_FOUND;
	}

	//  There is an address associated with this hash code.
	//  Read the pathname bin at that address and walk though the
	//  bin searching for this pathname

	isize = (int)fileHeader[zdssFileKeys.kbinSize];
	binAddress = ifltab[zdssKeys.khashTableBinAdd];
	pathnameBin = (long long *)ifltab[zdssKeys.kpathBin];

	//  Now loop, reading pathname bins checking the bin hash of each pathname found
	//  if none match, then this pathname was not found.
	while (1) {
		ifltab[zdssKeys.kbinAddCurrent] = binAddress;
		//  Read the pathname bin
		//  Check to see if the file size has increased since last read
		//  and this read is beyond what we think the file size is.
		if (binAddress > fileHeader[zdssFileKeys.kfileSize]) {
			status = zpermRead(ifltab);
			if (zisError(status)) {
				zmessage2(ifltab, "Reading file header for record ", pathname);
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcheck_ID);
			}
		}
		
		status = zget(ifltab, binAddress, (void *)pathnameBin, isize, 2);
		if (zisError(status)) {
			zmessage2(ifltab, "Reading pathname bin for record ", pathname);
			zmessageLong(ifltab, "ifltab[zdssKeys.kaddTableHash] ", ifltab[zdssKeys.kaddTableHash]);
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcheck_ID);
		}
		//  The pathname bin contains
		//  pathname hash
		//  status flag
		//  pathname length (in bytes) and size (in 8 byte words)
		//  address to info block (or primary or alias bin block, if alias)
		//  Data type and catalog sort sequence number (when this recored was added)
		//  Last write time
		//  For time Series: Julian start, end;  undefined (0) other types (2 singles in double)
		//  Pathname
		//  next pathname hash
		//  status flag
		//  pathname length
		//  ...
		//  0  (next pathname hash)
		//  0  (next pathname length)
		//  0  (next status flag)
		//  0  (next address loc)
		//  0  (catalog sort sequence number)
		//  0, 0
		//  (next pathname)
		//
		//  address to next pathname bin with same table hash or 0, if no more
		//  At end + 1, address of next set of pathname bin blocks

		//  zdssBinKeys.kbinHash = 0;
		//  zdssBinKeys.kbinStatus = 1;
		//  zdssBinKeys.kbinPathLen = 2;
		//  zdssBinKeys.kbinInfoAdd = 3;
		//  zdssBinKeys.kbinTypeAndCatSort = 4;
		//  zdssBinKeys.kbinLastWrite = 5;
		//  zdssBinKeys.kbinDates = 6;
		//  zdssBinKeys.kbinPath = 7;

		infoAddress = 0;
		icount = 0;
		while (1) {
			//  Are there no more pathnames in this bin?
			if (pathnameBin[icount+zdssBinKeys.kbinHash] == 0) {
				//  No more, didn't find it in this block, continue on
				break;
			}
			i8toi4(pathnameBin[icount+zdssBinKeys.kbinPathLen], &numberChars, &pathnameSize);

			//  Each pathname in the bin has a record status:
			//  zdssBinKeys.kbinStatus:
			//  0 : Unused
			//  1 : good (and primary path for this record)
			//  2 : Alias - address points to primary, or previous alias
			// 11 : Deleted
			// 12 : Renamed
			binStatus = (int)pathnameBin[icount+zdssBinKeys.kbinStatus];
			//  Do the pathname bin hash codes match (NOT the table hash)
			if (pathnameBin[icount+zdssBinKeys.kbinHash] == ifltab[zdssKeys.kpathnameHash]) {
				//  Yes!  Check lengths and status
				if (numberChars == pathnameLength) {
					//  Found it!
					//  Retain the address of the information block
					infoAddress = pathnameBin[icount+zdssBinKeys.kbinInfoAdd];
					//  Should we check for a hash collision (same hash for two pathnames?)
					//  Note, if 2 or more pathnames share the same bin hash, then second pathname
					//  will be later in the pathname bin, and a flag will be set at the beginning
					//  of the file to indicate that a collision conditions exists.  It is very rare that
					//  two paths would share both the same table and pathname hash... but it's possible
					//  If this file does have a collision, always double check... or if requested to double check
					if ((fileHeader[zdssFileKeys.khashCollisions] > 0) || boolDoubleCheckPath) {
						if (zpathnameCompare(pathname, &pathnameBin[icount+zdssBinKeys.kbinPath], (size_t)pathnameLength)) {
							//  Found it for sure
							boolFound = 1;
						}
						else {
							//  Not it, continue on (later in the pathname bin)
							infoAddress = 0;
							ifltab[zdssKeys.ksameHash] = 1;
						}
					}
					else {
						//  No hash collisions in this file (usual case), don't need double check.
						boolFound = 1;
					}
					if (boolFound) {
						//  Save info about this record in ifltab
						ifltab[zdssKeys.kpathBinAddress] = binAddress + icount;
						ifltab[zdssKeys.kaddInfoLastPath] = infoAddress;
						ifltab[zdssKeys.kbinStatus] = binStatus;
						ifltab[zdssKeys.kbinPathLen] = i4toi8(pathnameLength, pathnameSize);
						ifltab[zdssKeys.kinfoAddInBin] = binAddress + icount + zdssBinKeys.kbinInfoAdd;
						ifltab[zdssKeys.kbinTypeAndCatSort] = pathnameBin[icount+zdssBinKeys.kbinTypeAndCatSort];
						i8toi4(ifltab[zdssKeys.kbinTypeAndCatSort], &type, &temp);
						//  Internal records less than 50.  Only provide user records
						if (type > 50) ifltab[zdssKeys.klastType] = type;
						ifltab[zdssKeys.kbinLastWrite] = pathnameBin[icount+zdssBinKeys.kbinLastWrite];
						ifltab[zdssKeys.kbinDates] = pathnameBin[icount+zdssBinKeys.kbinDates];
						ifltab[zdssKeys.kpathAddressInBin] = binAddress + icount + zdssBinKeys.kbinPath;
						if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
							//  Pathname found at bin start address %d, specific address %d
							//  Pathname status: %d, info address %d
							_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%lld, Specific bin address: %lld",
								binAddress, ifltab[zdssKeys.kpathBinAddress]);
							zmessageDebug(ifltab, DSS_FUNCTION_zcheck_ID, "Pathname found at bin start address: ", messageString);							
							zmessageDebugLong(ifltab, DSS_FUNCTION_zcheck_ID, "Info Address: ", ifltab[zdssKeys.kaddInfoLastPath]);
							zmessageDebugLong(ifltab, DSS_FUNCTION_zcheck_ID, "Pathname status: ", binStatus);
						}
						if (!zcompareRecordStatus((int)binStatus, statusWanted)) {
							if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
								zmessageDebugInt(ifltab, DSS_FUNCTION_zcheck_ID, "Status different than wanted; status: ", statusWanted);
							}
							//  Are we dealing with an alias pathname?
							if ((ifltab[zdssKeys.kbinStatus] == REC_STATUS_ALIAS) && (statusWanted != REC_STATUS_ALIAS)) {
							}
							else {
								boolFound = 0;
							}
						}
						if (boolFound) {
							break;
						}
					}
				}
			}
			//  In case we are in a write, see if we can recover unused space
			//  in this bin from a deleted or renamed record to use for this pathname later.
			//  Sometimes a user will write a record, delete it, write it again, delete, etc.
			//  This can cause the bin size to grow large and require a lot of checks.
			//  If we reuse the bin space, then it will remain the same size and not require a lot of checks.
			//  It would only be used with this pathname, if the record is to be written and it is not found now (new)
			if ((binStatus == REC_STATUS_DELETED) || (binStatus == REC_STATUS_MOVED) && (ifltab[zdssKeys.kreclaimLevel] > RECLAIM_NONE)) {
				//  Did we already find space?
			   if (ifltab[zdssKeys.kbinWithSpace] == 0) {
				   //  Is this spot big enough?
					if (pathSizeIn <= pathnameSize) {
						//  Save the first address where this is found
						//  (Note - this operation is only valid with the current pathname)
						ifltab[zdssKeys.kbinWithSpace] = binAddress;
					}
				}
			}
			//  Keep track of number of pathnames that share the same TABLE hash.
			ifltab[zdssKeys.kpathsThisHash]++;
			icount += zdssBinKeys.kbinSize + pathnameSize;
			//  Are we at the end of the pathname bin, and need to read the next
			//  (the next to the last word in the bin contains the address of the next for this table hash.)
			if (icount >= (isize-2))
				break;
		}
		if (boolFound) {
			break;
		}
		//  We've read through the bin and have not found our pathname.  Go to
		//  the next bin for this table hash and search it
		binAddress = pathnameBin[isize-1];
		if (binAddress == 0) {
			//  There is no next pathname bin for this hash, so we have exhausted
			//  our search and have not found the record.
			break;
		}
	}

	//  Are we dealing with an alias pathname?
	if ((boolFound && !zcompareRecordStatus((int)binStatus, statusWanted) &&		
		(ifltab[zdssKeys.kbinStatus] == REC_STATUS_ALIAS) && (statusWanted != REC_STATUS_ALIAS))) {
		//  The pathname bin status indicates that this is an alias pathname
		//  We need to save the alias address and walk down the aliases bins to the primary
		//  The address in this bin points either to the real (non-alias) info block or to another
		//  pathname bin location, with an address that points to another alias or the info block
		if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebug(ifltab, DSS_FUNCTION_zcheck_ID, "Alias Pathname found, searching for primary.  Pathname: ", pathname);
		}
		while (1) {
			//  The fourth [3] word points to the address, either the info block or another alias bin
			if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_USER_DIAG)) {
				zmessageDebugLong(ifltab, DSS_FUNCTION_zcheck_ID, "At pathname bin at address: ", ifltab[zdssKeys.kaddInfoLastPath]);
			}
			status = zget(ifltab, ifltab[zdssKeys.kaddInfoLastPath], (void *)binAlias, 8, 2);
			if (zisError(status)) {
				zmessage2(ifltab, "Reading info for alias for record ", pathname);
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcheck_ID);
			}
			//  Is this the primary's bin area, or a pathname bin that we are walking down?
			if (binAlias[0] == DSS_INFO_FLAG) {
				//  Yes, this is the (real) info block -
				//  ifltab[zdssKeys.kaddInfoLastPath] contains the address of the real info block
				if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_USER_DIAG)) {
					zmessageDebugLong(ifltab, DSS_FUNCTION_zcheck_ID, "Found primary pathname at info address: ", ifltab[zdssKeys.kaddInfoLastPath]);
				}
				break;
			}
			else {
				//  Nope, still walking down alias bins
				//  double check that we are still walking down alias bins
				if (binAlias[zdssBinKeys.kbinStatus] != REC_STATUS_ALIAS) {
					//  Uh-oh, shouldn't have this number - needs to be a "2"!
					return zerrorProcessing(ifltab, DSS_FUNCTION_zcheck_ID, zdssErrorCodes.INVALID_BIN_STATUS,
											(int)binAlias[zdssBinKeys.kbinStatus], ifltab[zdssKeys.kaddInfoLastPath],
											zdssErrorSeverity.CORRUPT_FILE, "", "");
				}
				//  Use this alias bins info address and read it's bin or info area
				ifltab[zdssKeys.kaddInfoLastPath] = binAlias[zdssBinKeys.kbinInfoAdd];
			}
		}
	 }

	ifltab[zdssKeys.kfound] = boolFound;
	ifltab[zdssKeys.klockCheckSet] = ifltab[zdssKeys.klocked];

	if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_USER_DIAG)) {
		if (boolFound) {
			zmessage2(ifltab, ZCHECK_FOUND, pathname);
		}
		else {
			zmessage2(ifltab, ZCHECK_NOT_FOUND, pathname);
		}
	}

	if (boolFound) {
		return STATUS_RECORD_FOUND;
	}
	else {
		return STATUS_RECORD_NOT_FOUND;
	}

}

