#include <stdio.h>
#include <stdlib.h>
#include <stdlib.h>

#include "zdssKeys.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "zdssLocking.h"
#include "zprogress.h"
#include "hecdssInternal.h"
#include "hecdssFort.h"
#include "heclib.h"



/**
*  Function:	zanalyzeFile
*
*  Use:			Semi-Public
*
*  Description:	Searchs a DSS file for all record info blocks, then walks
*					the pathname bin to be sure there are addresses to that record.
*					Intent is to look for any damage within the DSS file
*					Generally zcheckFile is called instead
*
*  Declaration: int zanalyzeFile(long long *ifltab);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*	Returns:	int status
*					STATUS_OKAY for no errors.
*					> 0 for number of records not found (number of errors)
*					< 0 errorCode for severe error
*
*	See Also:	int zcheckFile(long long *ifltab);
*
*
*	Author:			Bill Charley
*	Date:			2010
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zanalyzeFile(long long *ifltab)
{
	int i;
	int pathLen;
	int numberInfo1;
	int numberInfo2;
	int numberNotFound;
	int numberFound;
	int status;
	int icase;
	int jcount;
	int icount;
	int numberChars;
	int pathnameSize;
	int binSize;
	int countTotal;
	int numberValid;
	int numberDeleted;
	int numberZeros = 0;
	long long binAddress;
	long long *pathnameBin;
	long long infoAddress;
	long long address;
	long long addressLoc;
	long long addressStart;
	long long addressCat;
	long long fileSize;
	int readSize = 4028;
	long long fileBuff[4028];
	long long recHeader[2];
	long long info1[100];
	long long info2[100];
	long long *fileHeader;
	char messageString[80];
	char pathname[MAX_PATHNAME_LENGTH];


	if (zgetVersion(ifltab) == 6) {
		zmessage(ifltab, "zanalyzeFile is for DSS Version 7 files only.  This file is version 6.");
		return -1;
	}

	if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcheckLinks_ID, "Enter, Handle: ", zhandle(ifltab));
	}

	status = zcheckKeys(ifltab);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcheckLinks_ID);
	}

	status = zpermRead(ifltab);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcheckLinks_ID);
	}

	numberValid = 0;
	numberDeleted = 0;
	countTotal = 0;

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	binSize = (int)fileHeader[zdssFileKeys.kbinSize];
	binAddress = fileHeader[zdssFileKeys.kaddFirstBin];
	pathnameBin = (long long *)ifltab[zdssKeys.kpathBin];

	zmessage(ifltab, "");
	zmessage(ifltab, "Pathname bin blocks.");
	zmessageLong(ifltab, "First pathname bin starts at address: ", binAddress);

	//  Walk pathname bins
	while (1) {
		zmessageLong(ifltab, "Start of block at address: ", binAddress);
		addressStart = binAddress;
		jcount = 0;
		while (jcount < (int)fileHeader[zdssFileKeys.kbinsPerBlock]) {
			status = zget(ifltab, binAddress, (int *)pathnameBin, binSize, 2);
			if (zisError(status)) {
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcheckLinks_ID);
			}

			icount = 0;
			if (pathnameBin[icount + zdssBinKeys.kbinHash] == 0) {
				//  No more, didn't find it in this block, continue on
				break;
			}
			while (1) {
				if (pathnameBin[icount + zdssBinKeys.kbinHash] == 0) {
					//  No more, didn't find it in this block, continue on
					break;
				}
				i8toi4(pathnameBin[icount + zdssBinKeys.kbinPathLen], &numberChars, &pathnameSize);
				if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
					charLong(&pathnameBin[icount + zdssBinKeys.kbinPath], pathname, numberChars, sizeof(pathname), 0, 0);
					zmessageDebug(ifltab, DSS_FUNCTION_zcheckLinks_ID, "Found pathname: ", pathname);
				}
				if (zcompareRecordStatus((int)pathnameBin[icount + zdssBinKeys.kbinStatus], 0)) {
					numberValid++;
				}
				else {
					numberDeleted++;
				}
				countTotal++;
				zprogress.currentNumber = countTotal;
				zprogress.handle = zhandle(ifltab);

				icount += zdssBinKeys.kbinSize + pathnameSize;
				if (icount >= (binSize - 2)) {
					break;
				}
			}
			binAddress += binSize;
			jcount++;
		}
		zmessageLong(ifltab, "End of block at address:   ", binAddress);
		address = binAddress - addressStart;
		zmessageLong(ifltab, "Length of block:           ", address);
		
		status = zget(ifltab, binAddress, (int *)pathnameBin, 1, 2);
		if (status != 0) {
			countTotal = status;
			break;
		}
		if (pathnameBin[0] == 0) {
			zmessageLong(ifltab, "End of all pathname bins at address: ", binAddress);
			break;
		}
		binAddress = pathnameBin[0];
	}
	zmessageInt(ifltab, "Total number of pathnames found in pathname bins: ", countTotal);
	zmessageInt(ifltab, "Number of valid pathnames found: ", numberValid);
	zmessageInt(ifltab, "Number of deleted / renamed pathnames found: ", numberDeleted);

	numberNotFound = 0;
	numberFound = 0;
	address = 0;
	fileSize = fileHeader[zdssFileKeys.kfileSize] + 1L; 
	zmessage(ifltab, "");
	zmessage(ifltab, "Key file locations.");
	while (address < fileSize) {
		if ((address + readSize)  > fileSize) {
			readSize = (int)(fileSize - address);
		}
		if (zprogress.interrupt) {
			return numberNotFound;
		}
		status = zget(ifltab, address, (int *)fileBuff, readSize, 2);
		if (status != 0) {
			return status;
		}
		for (i = 0; i<readSize; i++) {
			if (fileBuff[i] == 0L) {
				numberZeros++;
			}
			else {
				if (numberZeros > 3000) {
					zmessageInt(ifltab, "Excessive number of zeros (0) found: ", numberZeros);
					zmessageLong(ifltab, "Starting at address:                 ", addressStart);
					addressLoc = address + (long long)i;
					zmessageLong(ifltab, "Ending at address:                   ", addressLoc);
				}
				addressStart = address + (long long)i;
				numberZeros = 0;
			}
			if (fileBuff[i] == DSS_END_FILE_FLAG) {
				addressLoc = address + (long long)i;
				zmessageLong(ifltab, "End of file flag found at address: ", addressLoc);
			}
			else if (fileBuff[i] == DSS_START_CAT_SORT_FLAG) {
				addressLoc = address + (long long)i;
				addressCat = addressLoc;
				zmessageLong(ifltab, "Start of catalog sort area flag found at address: ", addressLoc);
			}
			else if (fileBuff[i] == DSS_END_CAT_SORT_FLAG) {
				addressLoc = address + (long long)i;
				zmessageLong(ifltab, "End of catalog sort area flag found at address:   ", addressLoc);
				addressCat = addressLoc - addressCat;
				zmessageLong(ifltab, "Size of catalog sort area                                 ", addressCat);
			}
			else if (fileBuff[i] == DSS_END_HEADER_FLAG) {
				addressLoc = address + (long long)i;
				zmessageLong(ifltab, "End of file header flag found at address: ", addressLoc);
			}
			else if (fileBuff[i] == DSS_INFO_FLAG) {
				//  Found a flag - check to be sure this is the flag and not a fluke
				//  Make sure the next word contains the status and the following the path length
				if ((i + 2) < readSize) {
					recHeader[0] = fileBuff[i + 1];
					recHeader[1] = fileBuff[i + 2];
				}
				else {
					infoAddress = address + (long long)i + 1;
					status = zget(ifltab, infoAddress, (int *)recHeader, 2, 2);
					if (status != 0) {
						return status;
					}
				}
				//  Check for a valid pathname length
				if ((recHeader[1] > 0) && (recHeader[1] < MAX_PATHNAME_LENGTH)) {
					//  Check the status flag
					//  Be sure this was not deleted or renamed....
					if ((recHeader[0] == REC_STATUS_PRIMARY) || (recHeader[0] == REC_STATUS_VALID)) {
						//  Good - get the pathname
						pathLen = (int)recHeader[1];
						infoAddress = address + (long long)i;
						numberInfo1 = zdssVals.infoSize + numberLongsInBytes(pathLen);
						status = zget(ifltab, infoAddress, (int *)info1, numberInfo1, 2);
						//  Get the pathname from the info block
						charInt((void *)&info1[zdssInfoKeys.kinfoPathname], (void *)pathname, pathLen, sizeof(pathname), 0, 1, 0);
						pathname[pathLen] = '\0';

						//  Now double check that it exists
						status = zreadInfoBlock(ifltab, pathname, 0, info2, sizeof(info2), &numberInfo2);
						if (zisError(status)) {
							return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcheckLinks_ID);
						}
						icase = 0;
						if (status == STATUS_RECORD_FOUND) {
							//  Double check that this is the same area for both records
							if (infoAddress != ifltab[zdssKeys.kaddInfoLastPath]) {
								status = STATUS_RECORD_NOT_FOUND;
								icase = 1;
							}
							else if (numberInfo1 != numberInfo2) {
								status = 0;
								icase = 2;
							}
						}
						if (status == STATUS_RECORD_NOT_FOUND) {
							if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_GENERAL)) {
								_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %lld, info address2 %lld",
									infoAddress, ifltab[zdssKeys.kaddInfoLastPath]);
								zmessage2(ifltab, "***  Unable to find record from check links at info address1 ", messageString);
								zmessage2(ifltab, "***  Pathname:  ", pathname);								
							}
							numberNotFound++;
							zprogress.numberErrors = numberNotFound;
							zprogress.handle = zhandle(ifltab);							
						}
						else {
							numberFound++;
							zprogress.currentNumber = numberFound;
							zprogress.handle = zhandle(ifltab);
							if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
								zmessageDebug(ifltab, DSS_FUNCTION_zcheckLinks_ID, "Record links check: ", pathname);
							}
						}
					}
				}
			}
		}
		address += readSize;
	}
	if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_GENERAL)) {
		if (numberNotFound == 0) {
			zmessage(ifltab, "zanalyzeFile check passed with no errors");
		}
		else {
			zmessageInt(ifltab, "zanalyzeFile check FAILED.  Number of errors: ", numberNotFound);
		}
	}
	return numberNotFound;
}

