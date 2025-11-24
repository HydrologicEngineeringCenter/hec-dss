#include "zdssKeys.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "zprogress.h"
#include "heclib.h"


/**
*  Function:	zcheckHashTable
*
*  Use:			Semi-Public
*
*  Description:	Check the hash table and addresses in a DSS file.
*					Check the info block for each record.
*					Intent is to look for any damage within the DSS file
*					Generally zcheckFile is called instead
*
*  Declaration: int zcheckHashTable(long long *ifltab);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*	Returns:	int status
*					STATUS_OKAY for no errors.
*					> 0 for number of errors in the file detected (some errors will generate multiple errors)
*					< 0 errorCode for severe error
*
*	See Also:	int zcheckFile(long long *ifltab);
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zcheckHashTable(long long *ifltab)
{
	int status = 0;
	int icount;
	int numberNotFound;
	int numberFound;
	int numberErrors;
	int binSize;
	int numberChars;
	int pathnameSize;
	int numberInfo;
	int maxInfo;
	char pathname[MAX_PATHNAME_LENGTH];
	long long binAddress;
	long long tableHash;
	long long infoAddress;
	long long *pathnameBin;
	long long *fileHeader;
	long long *info;


	if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcheckHashTable_ID, "Enter, Handle: ", zhandle(ifltab));
	}

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];

	status = zcheckKeys(ifltab);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcheckHashTable_ID);
	}

	zresetProgress(zhandle(ifltab), fileHeader[zdssFileKeys.knumberRecords]);
	zprogress.handle = zhandle(ifltab);

	//  We need to keep the pathname bin and info arrays as separate
	//  so that ifltab can be used for double checking
	binSize = (int)fileHeader[zdssFileKeys.kbinSize];
	pathnameBin = (long long*)calloc((size_t)binSize, LONG_SIZE);
	if (!pathnameBin) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zcheckHashTable_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, binSize, 0,
								zdssErrorSeverity.MEMORY_ERROR, "", "Allocating pathnameBin");
	}
	maxInfo = zdssVals.infoSize + numberLongsInBytes((int)fileHeader[zdssFileKeys.kmaxPath]) + 1;
	info = (long long*)calloc((size_t)maxInfo, LONG_SIZE);
	numberNotFound = 0;
	numberFound = 0;
	numberErrors = 0;

	tableHash = -1;
	ifltab[zdssKeys.khashTableBinAdd] = 0;

	//  Hash table level
	while (1) {
		//  Need to read next hash from the hash table
		// and then pathname bin
		tableHash++;
		if (tableHash == fileHeader[zdssFileKeys.kmaxHash]) {
			//  All done - no more pathnames in file
			break;
		}
		if (zprogress.interrupt) {
			free(pathnameBin);
			free(info);
			return (numberNotFound + numberErrors);
		}
		ifltab[zdssKeys.kaddTableHash] = tableHash + fileHeader[zdssFileKeys.kaddHashTableStart];
		status = zget(ifltab, ifltab[zdssKeys.kaddTableHash], (int *)&binAddress, 1, 2);
		if (zisError(status)) {
			free(pathnameBin);
			free(info);
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcheckHashTable_ID);
		}
		if (binAddress == 0) {
			continue;
		}

		//  Read all pathname bins for one hash
		//  Now that the hash table points to a pathname bin, read from that bin and subsequent with same hash
		while (1) {
			if (zprogress.interrupt) {
				free(pathnameBin);
				free(info);
				return (numberNotFound + numberErrors);
			}
			status = zget(ifltab, binAddress, (int *)pathnameBin, binSize, 2);
			if (zisError(status)) {
				free(pathnameBin);
				free(info);
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcheckHashTable_ID);
			}

			//  Loop through the bin, looking for records
			infoAddress = 0;
			icount = 0;
			while (1) {
				assert(icount+zdssBinKeys.kbinHash <= binSize);
				if (pathnameBin[icount+zdssBinKeys.kbinHash] == 0) {
					//  No more, didn't find it in this block, continue on
					break;
				}
				assert(icount+zdssBinKeys.kbinPathLen <= binSize);
				i8toi4(pathnameBin[icount+zdssBinKeys.kbinPathLen], &numberChars, &pathnameSize);
				if (zcompareRecordStatus((int)pathnameBin[icount+zdssBinKeys.kbinStatus], REC_STATUS_VALID)) {
					//  Found it!
					assert(icount+zdssBinKeys.kbinInfoAdd <= binSize);
					infoAddress = pathnameBin[icount+zdssBinKeys.kbinInfoAdd];
					charLong(&pathnameBin[icount + zdssBinKeys.kbinPath], pathname, numberChars, sizeof(pathname), 0, 0);
					status = zreadInfoBlock (ifltab, pathname, 0, info, maxInfo, &numberInfo);
					if (status == STATUS_OKAY) {
						numberErrors += zcheckInfo(ifltab, pathname, info, numberInfo);
					}
					else {
						numberErrors++;
					}
					zprogress.numberErrors += numberErrors;
					numberFound++;
					zprogress.currentNumber = numberFound;
					zprogress.handle = zhandle(ifltab);

				}
				icount += zdssBinKeys.kbinSize + pathnameSize;
				if (icount >= (binSize-2))
					break;
			}
			//  Get ready to read next bin
			binAddress = pathnameBin[binSize-1];
			if (binAddress == 0) {
				//  No more bins - read next hash index
				break;
			}
		}
	}

	if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcheckHashTable_ID, "Exit, Handle: ", zhandle(ifltab));
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcheckHashTable_ID, "Number errors: ", numberErrors);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcheckHashTable_ID, "Number found: ", numberFound);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcheckHashTable_ID, "Number NOT found: ", numberNotFound);
	}

	if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_GENERAL)) {
		if ((numberNotFound + numberErrors) == 0) {
			zmessage(ifltab, "zcheckHashTable check passed with no errors");
		}
		else {
			zmessage(ifltab, " ");
			zmessageInt(ifltab, "zcheckHashTable check FAILED.  Number of errors: ", (numberNotFound + numberErrors));
			zmessage(ifltab, " ");
		}
	}

	free(pathnameBin);
	free(info);
	return (numberNotFound + numberErrors);
}

