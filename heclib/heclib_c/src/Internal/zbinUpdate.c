#include <stdio.h>
#include <string.h>

#include "zdssKeys.h"
#include "heclib7.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"
#include "zerrorCodes.h"


/**
*  Function:	zbinUpdate
*
*  Use:			Private (internal)
*
*  Description:	Given a table hash code, locates an empty location in the bin(s) for this code
*					and writes the addresses for this record to that bin.
*
*  Declaration: int zbinUpdate(long long *ifltab, const char* pathname, long long infoAddress,
*							   int recordStatus, int dataType);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				const char* pathname
*					The pathname of the record to to store addresses for.
*
*				long long infoAddress
*					The address of the information for this record.  This address must have been assigned prior.
*
*				int recordStatus
*					The status of the record to store for this pathname.
*
*				int dataType
*					The data type to report for this record when checked or scanned.
*
*
*
*	Returns:	int status
*					STATUS_OKAY for successful operation.
*					errorCode for error
*
*
*	Note:		This function can use reclaimed space in the bin.  Often a record will
*				be deleted only to be written again; this will maximize space and speed based on this.
*				When reclaiming space, the pathname "size" (number int*8) must be the same as the
*				one replaced.
*				The new info area is usually at EOF, except on some renames
*
*				//  The pathname bin contains
*						pathname hash
*						status flag
*						pathname length (in bytes) and size (in 8 byte words)
*						address to info block (or primary or alias bin block, if alias)
*						Data type and catalog sort sequence number (when this recored was added)
*						For time Series: Julian start, end;  undefined (0) other types (2 singles in double)
*						Pathname
*						next pathname hash
*
*						address to next pathname bin with same table hash or 0, if no more
*						At end + 1, address of next set of pathname bin blocks
*
*
*
*	Called by:	zwriteNew and zbinNew
*
*	See Also:	zbinNew
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zbinUpdate(long long *ifltab, const char* pathname, long long infoAddress,
			   int recordStatus, int dataType)
{
	int status;
	int isize;
	int icount;
	int pathnameLength;
	int numberChars;
	int pathnameSize;
	int pathSizeInBin;
	int boolSame;
	int boolUseRecovered;
	int jsize;
	int julianFirst;
	int julianLast;
	int done;
	int temp;
	int boolReadBin;
	int boolDoubleCheckPath;
	long long *pathnameBin;
	long long *fileHeader;
	char buff[80];
	char path[MAX_PATHNAME_LENGTH];

	char messageString[100];


	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zbinUpdate_ID, "Enter;  Pathname: ", pathname);
	}

	status = STATUS_OKAY;
	done = 0;
	boolReadBin = 0;
	//  We have to double check full hash on writes.
	boolDoubleCheckPath = 1;
	isize = (int)fileHeader[zdssFileKeys.kbinSize];

	//  Get the pathname bin location.
	//  Check to see if we have reclaimed bin space that we can use
	if ((ifltab[zdssKeys.kbinWithSpace] > 0)  && (ifltab[zdssKeys.kreclaimLevel] > 0)){
		boolReadBin = 1;
		boolUseRecovered = 1;
		ifltab[zdssKeys.kbinAddCurrent] = ifltab[zdssKeys.kbinWithSpace];
	}
	else {
		boolUseRecovered = 0;
	}

	pathnameBin = (long long *)ifltab[zdssKeys.kpathBin];

	while (1) {
		//  Don't read if this is the first access from zbinUpdate;
		//  it will have either been read by zreadInfo, or will be
		//  blank from zbinNew.  If from zbinNew, then we cannot read
		//  it yet, as it has not been written to disk.
		if (boolReadBin) {
			if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%lld,  size: %d",
					ifltab[zdssKeys.kbinAddCurrent], isize);
				zmessageDebug(ifltab, DSS_FUNCTION_zbinUpdate_ID, "Read pathname bin at address: ", messageString);
			}
			status = zget(ifltab, ifltab[zdssKeys.kbinAddCurrent], (void *)pathnameBin, isize, 2);
			if (zisError(status)) {
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_zbinUpdate_ID);
			}
		}
		else {
			if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%lld,  size: %d",
					ifltab[zdssKeys.kbinAddCurrent], isize);
				zmessageDebug(ifltab, DSS_FUNCTION_zbinUpdate_ID, "Bypass pathname bin read for address: ", messageString);
			}
			boolReadBin = 1;
		}

		if ((pathnameBin[isize-1] != 0) && (!boolUseRecovered)) {
			ifltab[zdssKeys.kbinAddCurrent] = pathnameBin[isize-1];
			continue;
		}

		//  The pathname bin contains
		//  pathname hash
		//  status flag
		//  pathname length (in bytes) and size (in 8 byte words)
		//  address to info block (or primary or alias bin block, if alias)
		//  Data type and catalog sort sequence number (when this recored was added)
		//  For time Series: Julian start, end;  undefined (0) other types (2 singles in double)
		//  Pathname
		//  next pathname hash
		//
		//  address to next pathname bin with same table hash or 0, if no more
		//  At end + 1, address of next set of pathname bin blocks
		icount = 0;
		while (1) {
			if (boolUseRecovered) {
				//  Is this a deleted record with the same space allocated to store this pathname?
				if ((pathnameBin[icount+zdssBinKeys.kbinStatus] == 11) || (pathnameBin[icount+zdssBinKeys.kbinStatus] == 12)) {
					i8toi4(pathnameBin[icount+zdssBinKeys.kbinPathLen], &numberChars, &pathSizeInBin);
					pathnameSize = numberLongsInBytes((int)ifltab[zdssKeys.klenLastPath]);
					//  Path sizes (int *8) have to match!
					if (pathSizeInBin == pathnameSize) {
						//  zero out the hash so it will be used below
						pathnameBin[icount+zdssBinKeys.kbinHash] = 0;
						ifltab[zdssKeys.kbinWithSpace] = 0;
						fileHeader[zdssFileKeys.kreclaimedPaths]++;
						if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
							_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%lld,  count: %d",
								ifltab[zdssKeys.kbinAddCurrent], icount);
							zmessageDebug(ifltab, DSS_FUNCTION_zbinUpdate_ID, "Recovered bin space use at address: ", messageString);
						}
					}
				}
			}
			if ((pathnameBin[icount+zdssBinKeys.kbinHash] == 0) ||
				(pathnameBin[icount+zdssBinKeys.kbinHash] == ifltab[zdssKeys.kpathnameHash])) {
				//  Do we need to double check paths because of a collision?  (very rare)
				if (pathnameBin[icount+zdssBinKeys.kbinHash] == ifltab[zdssKeys.kpathnameHash]) {
					if ((fileHeader[zdssFileKeys.khashCollisions] > 0) || boolDoubleCheckPath) {
						//  Same hash - is this the same path?
						//  First check pathname lengths
						boolSame = 0;
						pathnameLength = (int)strlen(pathname);
						i8toi4(pathnameBin[icount+zdssBinKeys.kbinPathLen], &numberChars, &pathnameSize);
						if (pathnameLength == numberChars) {
							//  Same lengths... are these the same paths?
							if (zpathnameCompare(pathname, &pathnameBin[icount+zdssBinKeys.kbinPath], (size_t)pathnameLength)) {
								//  Yep.
								boolSame = 1;
							}
							else {
								//  Very rare
								if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
									charLong(&pathnameBin[icount + zdssBinKeys.kbinPath], path, numberChars, sizeof(path), 0, 0);
									_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%lld",
										ifltab[zdssKeys.kpathnameHash]);
									zmessageDebug(ifltab, DSS_FUNCTION_zbinUpdate_ID, "Pathname hash collision found.  Hash code: ", messageString);
									zmessageDebug(ifltab, DSS_FUNCTION_zbinUpdate_ID, "First Pathname: ", pathname);
									zmessageDebug(ifltab, DSS_FUNCTION_zbinUpdate_ID, "Second Pathname: ", path);
								}
							}
						}
						if (!boolSame) {
							//  Not the same, move on to the next one in the bin
							icount += zdssBinKeys.kbinSize + pathnameSize;
							continue;
						}
					}
				}
				//  Found an empty spot...  see if there is room for
				//  our pathname here.
				pathnameSize = numberLongsInBytes((int)ifltab[zdssKeys.klenLastPath]);
				jsize = zdssBinKeys.kbinSize + pathnameSize;
				jsize += icount + 1;
				//  Do we have enough space left in this bin?
				if (jsize >= isize) {
					//  No, create a new bin
					if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
						_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%lld,  count: %d",
							ifltab[zdssKeys.kbinAddCurrent], icount);
						zmessageDebug(ifltab, DSS_FUNCTION_zbinUpdate_ID, "Bin full for address: ", messageString);
					}
					zbinNew(ifltab);
					fileHeader[zdssFileKeys.kbinsOverflow]++;
					icount = 0;
				}
				//  Now there's space
				pathnameBin[icount+zdssBinKeys.kbinHash] = ifltab[zdssKeys.kpathnameHash];
				pathnameBin[icount+zdssBinKeys.kbinPathLen] = i4toi8((int)ifltab[zdssKeys.klenLastPath], pathnameSize);
				pathnameBin[icount+zdssBinKeys.kbinStatus] = recordStatus;
				pathnameBin[icount+zdssBinKeys.kbinInfoAdd] = infoAddress;
				pathnameBin[icount+zdssBinKeys.kbinTypeAndCatSort] = i4toi8(dataType, (int)fileHeader[zdssFileKeys.kcatSequenceNumber]);
				if (dataType > 50) {
					ifltab[zdssKeys.klastType] = dataType;
				}
				pathnameBin[icount+zdssBinKeys.kbinLastWrite] = ifltab[zdssKeys.kbinLastWrite];
				i8toi4(ifltab[zdssKeys.kdataFirstDate], &julianFirst, &temp);
				i8toi4(ifltab[zdssKeys.kdataLastDate],  &julianLast,  &temp);
				ifltab[zdssKeys.kbinDates] = i4toi8(julianFirst, julianLast);
				pathnameBin[icount+zdssBinKeys.kbinDates] = ifltab[zdssKeys.kbinDates];
				//  Store the pathname
				charLong((void *)pathname, (void *)&pathnameBin[icount+zdssBinKeys.kbinPath], (int)ifltab[zdssKeys.klenLastPath], (int)ifltab[zdssKeys.klenLastPath], 1, 1);
				ifltab[zdssKeys.kaddInfoLastPath] = infoAddress;
				ifltab[zdssKeys.kpathBinAddress] = ifltab[zdssKeys.kbinAddCurrent] + icount;
				ifltab[zdssKeys.kinfoAddInBin]   = ifltab[zdssKeys.kbinAddCurrent] + icount + zdssBinKeys.kbinInfoAdd;
				if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
					_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, "%lld,  Specific address: %lld, (count: %d)",
						ifltab[zdssKeys.kbinAddCurrent], ifltab[zdssKeys.kpathBinAddress], icount);
					zmessageDebug(ifltab, DSS_FUNCTION_zbinUpdate_ID, "Bin space available at address: ", messageString);
				}
				//  Write the bin to disk
				status = zput(ifltab, ifltab[zdssKeys.kbinAddCurrent], (int *)pathnameBin, isize, 2);
				if (zisError(status)) {
					return zerrorUpdate(ifltab, status, DSS_FUNCTION_zbinUpdate_ID);
				}
				done = 1;
				break;
			}
			else {
				//zdebugout7 (ifltab, pathnameBin, (long long)0, 20);
				i8toi4(pathnameBin[icount+zdssBinKeys.kbinPathLen], &pathnameLength, &pathnameSize);
				icount += zdssBinKeys.kbinSize + pathnameSize;
				if (icount >= (isize-1)) {
					//  hmm... shouldn't get here.  The pathname block size is larger than possible....
					if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_CRITICAL)) {
						_snprintf_s(buff, sizeof(buff), _TRUNCATE, "icount: %d, isize: %d, pathname length: %d, address: %lld",
							icount, isize, pathnameLength, ifltab[zdssKeys.kbinAddCurrent]);
						zmessage2(ifltab, "*****DSS--- ERROR zbinUpdate: ", buff);
					}
					return zerrorProcessing(ifltab, DSS_FUNCTION_zbinUpdate_ID, zdssErrorCodes.BIN_SIZE_CONFLICT,
										 icount, ifltab[zdssKeys.kbinAddCurrent], zdssErrorSeverity.CORRUPT_FILE, pathname, "");
				}
			}
		}
		if (done) break;
	}

	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zbinUpdate_ID, "Exit;   Pathname: ", pathname);
	}

	return status;
}

