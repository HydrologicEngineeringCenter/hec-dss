#include <string.h>
#include <stdio.h>

#include "zdssKeys.h"
#include "zdssVals.h"
#include "zprogress.h"
#include "zdssMessages.h"
#include "heclib.h"


//	***  Avoid use of this function
//		 It is not efficient
//  ONLY for compatibility with old Java functions
//  Use zcatalog instead

/*
*
*           Get all pathnames in a DSS file - return one at a time.
*            To start list, set filePos to 0.  At the end of the list
*            istatus will be set to 1 (with no pathname returned), otherwise
*            istatus will be 0 for a normal return with pathname, or <0 for
*            an (somewhat serious) error return.
*
*            CINSTR is a string with instructions on how to perform this
*            operation.  It can be either a selective catalog command
*            (e.g., 'B=NORTH FORK'), or specify what kind of records.
*            Valid entries include:
*            'DELETE'
*            'RENAME'
*          ****** LW not implemented yet *****
*            'LW>date time'  (Last Written later than DATE TIME, where date is
*            'LW<date time'   9 char mil style, and time may include seconds.
*            'LW=date time'   e.g., 'LW>04JUN2004 14:00:00')
*            'PR=program name' (program is the name stored with the record)
*            'S=string'  (Pathname contains this string)
*            'A=..., B=...' (Standard selective catalog)
*
*             These items cannot be combined (cannot have 'DELETE, LW>date')
*
*/



void zplist7 (long long *ifltab, const char *instr, int *filePos, char *pathname,
               int *nPathname, int *istatus, int len_instr, size_t sizeof_pathname)
{

	int icount;
	int jcount;
	int countTotal;
	int statusWanted;
	int binSize;
	int numberChars;
	int pathnameSize;
	int status;
	char path[MAX_PATHNAME_SIZE];
	char cbuff[60];
	long long binAddress;
	long long *pathnameBin;
	long long *fileHeader;
	int binPosition;

	int boolSelect;
	int boolMatch;
	int begPart[7];
	int endPart[7];
	int lenPart[7];


	statusWanted = 0;
	*nPathname = 0;

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	pathnameBin = (long long *)ifltab[zdssKeys.kpathBin];
	binSize = (int)fileHeader[zdssFileKeys.kbinSize];

	if (*filePos == 0) {
		binAddress = fileHeader[zdssFileKeys.kaddFirstBin];
		binPosition = 0;
		jcount = 0;
		ifltab[zdssKeys.kbinAddplist] = 0;
		ifltab[zdssKeys.kbinPosPlist] = 0;
		ifltab[zdssKeys.kbinCountplist] = 0;
	}
	else {
		binAddress = ifltab[zdssKeys.kbinAddplist];
		binPosition = (int)ifltab[zdssKeys.kbinPosPlist];
		jcount = (int)ifltab[zdssKeys.kbinCountplist];
	}

	if (len_instr > 1) {
		zsetca6_(instr, &boolSelect, len_instr);
	}
	else {
		boolSelect = 0;
	}

	while (1) {
		while (1) {
			status = zget(ifltab, binAddress, (int *)pathnameBin, binSize, 2);
			if (zisError(status)) {
				*istatus = zerrorUpdate(ifltab, status, DSS_FUNCTION_zcatalog_ID);
				return;
			}
			if (zmessageLevel(ifltab, MESS_METHOD_CATALOG_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
				_snprintf_s(cbuff, sizeof(cbuff), _TRUNCATE, "%lld, first value %lld", binAddress, pathnameBin[0]);
				zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "Read bin at address: ", cbuff);
			}

			icount = 0;
			if (pathnameBin[icount+zdssBinKeys.kbinHash] == 0) {
				//  No more, didn't find it in this block, continue on
					break;
			}
			while (1) {
				if (pathnameBin[icount+zdssBinKeys.kbinHash] == 0) {
					//  No more, didn't find it in this block, continue on
					break;
				}
				i8toi4(pathnameBin[icount+zdssBinKeys.kbinPathLen], &numberChars, &pathnameSize);
				if (binPosition <= icount) {
					if (zcompareRecordStatus((int)pathnameBin[icount+zdssBinKeys.kbinStatus], statusWanted)) {
						charLong(&pathnameBin[icount + zdssBinKeys.kbinPath], path, numberChars, sizeof(path), 0, 0);
						path[numberChars] = '\0';
						if (zmessageLevel(ifltab, MESS_METHOD_CATALOG_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
							zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "Pathname Found: ", path);
						}
						if (boolSelect) {
							zupath_(path, begPart, endPart, lenPart, &status, numberChars);
							zmatca_(path, begPart, endPart, lenPart, cbuff, cbuff, &boolMatch, numberChars, 10, 10);
							if (zmessageLevel(ifltab, MESS_METHOD_CATALOG_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
								zmessageDebugInt(ifltab, DSS_FUNCTION_zcatalog_ID, "Pathname match status: ", boolMatch);
							}
						}
						else {
							boolMatch = 1;
						}
						if (boolMatch) {
							//  Found one!
							stringCopy(pathname, sizeof_pathname, path,  numberChars);
							if (numberChars < (int)sizeof_pathname) {
								pathname[numberChars] = '\0';
							}
							*nPathname = numberChars;
							(*filePos)++;
							*istatus = 0;
							//  Remeber where we are and the next spot to check
							icount += zdssBinKeys.kbinSize + pathnameSize;
							ifltab[zdssKeys.kbinPosPlist] = (long)icount;
							ifltab[zdssKeys.kbinAddplist] = binAddress;
							ifltab[zdssKeys.kbinCountplist] = jcount;
							if (zmessageLevel(ifltab, MESS_METHOD_CATALOG_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
								zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "zplist7, Pathname found: ", pathname);
							}
							return;
						}
					}
				}
				icount += zdssBinKeys.kbinSize + pathnameSize;
				if (icount >= (binSize-2)) {
					break;
				}
				if (status < 0) {
					countTotal = status;
					break;
				}
			}
			binAddress += binSize;
			binPosition = 0;
			jcount++;
			if (jcount >= (int)fileHeader[zdssFileKeys.kbinsPerBlock]) {
				jcount = 0;
				break;
			}
		}
		if (status < 0) {
			countTotal = status;
			break;
		}
		status = zget(ifltab, binAddress, (int *)pathnameBin, 1, 2);
		if (status != 0) {
			countTotal = status;
			break;
		}
		binAddress = pathnameBin[0];
		if (binAddress == 0) {
			break;
		}
	}

	//  The only way to get here is to have gone through the entire file
	ifltab[zdssKeys.kbinPosPlist] = 0;
	ifltab[zdssKeys.kbinAddplist] = 0;
	*filePos = -1;
	*istatus = 1;
	return;
}

void zplist7_(long long *ifltab, const char *instr, int *filePos, char *pathname,
               int *nPathname, int *istatus, int len_instr, size_t sizeof_pathname)
{
	zplist7 (ifltab, instr, filePos, pathname, nPathname, istatus, len_instr, sizeof_pathname);
}

