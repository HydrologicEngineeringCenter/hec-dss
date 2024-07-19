#include "zdssKeys.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "zprogress.h"
#include "heclib.h"


/**
*  Function:	zcheckPathnames
*
*  Use:			Semi-Public
*
*  Description:	Walks down the pathname bins and check that each pathname has a valid
*					info area, and there are addresses to that record.
*					Intent is to look for any damage within the DSS file
*					Generally zcheckFile is called instead
*
*  Declaration: int zcheckPathnames(long long *ifltab);
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
*	Remarks:	Similar to zcheckLinks, but this searches pathname bins whereas zcheckLinks
*					goes through the entire file
*					Quits after 100 errors.  All errors are written to the message log
*					if log level is at least MESS_LEVEL_GENERAL.
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zcheckPathnames(long long *ifltab)
{
	int istat = 0;
	int icount;
	int jcount;
	int numberErrors;
	int numberFound;
	int boolFound;
	int status;
	int binSize;
	int numberChars;
	int pathnameSize;
	int numberInfo;
	long long binAddress;
	long long *pathnameBin;
	long long info[300];
	long long *fileHeader;
	char messageString[80];
	char pathname[MAX_PATHNAME_LENGTH];

	if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcheckPathnames_ID, "Enter, Handle: ", zhandle(ifltab));
	}

	status = zcheckKeys(ifltab);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcheckPathnames_ID);
	}

	istat = zcheckKeys(ifltab);
	if (istat != 0) {
		return istat;
	}

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];

	//  We need to keep the pathname bin and info arrays as separate
	//  so that ifltab can be used for double checking
	binSize = (int)fileHeader[zdssFileKeys.kbinSize];
	pathnameBin = (long long*)calloc((size_t)binSize, LONG_SIZE);
	binAddress = fileHeader[zdssFileKeys.kaddFirstBin];
	numberErrors = 0;
	numberFound = 0;

	while (1) {
		jcount = 0;
		while (jcount < (int)fileHeader[zdssFileKeys.kbinsPerBlock]) {
			istat = zget(ifltab, binAddress, (int *)pathnameBin, binSize, 2);
			if (zisError(status)) {
				free(pathnameBin);
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcheckPathnames_ID);
			}
			icount = 0;
			while (1) {
				if (pathnameBin[icount+zdssBinKeys.kbinHash] == 0) {
					//  No more, didn't find it in this block, continue on
					break;
				}
				if (zprogress.interrupt) {
					free(pathnameBin);
					return numberErrors;
				}
				i8toi4(pathnameBin[icount+zdssBinKeys.kbinPathLen], &numberChars, &pathnameSize);
				if (zcompareRecordStatus((int)pathnameBin[icount+zdssBinKeys.kbinStatus], REC_STATUS_VALID)) {
					//  Found one!
					if (numberChars <=0 ) {
						if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_GENERAL)) {
							_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %lld, position %d", binAddress, icount);
							zmessage2(ifltab, "***  Zero length pathname in check pathnames at address ", messageString);
						}
						numberErrors++;
						zprogress.numberErrors = numberErrors;
						if (numberErrors >= zprogress.maxErrors) {
							free(pathnameBin);
							return numberErrors;
						}
					}
					charLong(&pathnameBin[icount + zdssBinKeys.kbinPath], pathname, numberChars, sizeof(pathname), 0, 0);
					//  Now double check that it exists
					status = zreadInfoBlock(ifltab, pathname, 0, info, sizeof(info), &numberInfo);
					if ((status != STATUS_RECORD_FOUND) && (status != STATUS_RECORD_NOT_FOUND)) {
						free(pathnameBin);
						return status;
					}
					if (status == STATUS_RECORD_FOUND) {
						//  Double check that the bin pathname is the same as the info pathname
						boolFound = zpathnameCompare(pathname, &pathnameBin[icount+zdssBinKeys.kbinPath], (size_t)numberChars);
					}
					else {
						boolFound = 0;
					}
					if (!boolFound) {
						if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
							_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %lld, position %d", binAddress, icount);
							zmessage2(ifltab, "***  Unable to find record from check pathnames at address ", messageString);
							zmessage2(ifltab, "***  Bin Pathname:  ", pathname);
							if (numberInfo > 0) {
								charLong(&pathnameBin[icount + zdssBinKeys.kbinPath], pathname, numberChars, sizeof(pathname), 0, 0);
								_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %lld", ifltab[zdssKeys.kaddInfoLastPath]);
								zmessage2(ifltab, "***  Info address:  ", messageString);
								charLong(&info[zdssInfoKeys.kinfoPathname], pathname, numberChars, sizeof(pathname), 0, 0);
								zmessage2(ifltab, "***  Info Pathname: ", pathname);
							}
						}
						numberErrors++;
						zprogress.numberErrors = numberErrors;
						if (numberErrors >= zprogress.maxErrors) {
							free(pathnameBin);
							return numberErrors;
						}
					}
					else {
						numberErrors += zcheckInfo(ifltab, pathname, info, numberInfo);
						numberFound++;
						zprogress.currentNumber = numberFound;
						if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_USER_DIAG)) {
							zmessage2(ifltab, "Record pathname checks: ", pathname);
						}
					}
				}
				icount += zdssBinKeys.kbinSize + pathnameSize;
				if (icount >= (binSize-2)) {
					break;
				}
			}
			binAddress += binSize;
			jcount++;
		}
		if (zprogress.interrupt) {
			free(pathnameBin);
			return numberErrors;
		}
		istat = zget(ifltab, binAddress, (int *)pathnameBin, 1, 2);
		if (istat != 0) {
			free(pathnameBin);
			return istat;
		}
		binAddress = pathnameBin[0];
		if (binAddress == 0) {
			break;
		}
	}
	if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcheckPathnames_ID, "Exit;  Handle: ",  zhandle(ifltab));
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcheckPathnames_ID, "Number not found: ", numberErrors);
	}
	if (numberErrors == 0) {
		if (fileHeader[zdssFileKeys.knumberRecords] != numberFound) {
			if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
				zmessage(ifltab, "zcheckPathnames, More pathnames found in file than recorded in file header.");
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d, Number in file header: %d",
					numberFound, (int)fileHeader[zdssFileKeys.knumberRecords]);
				zmessage2(ifltab, "Number counted:", messageString);
			}
			numberErrors = (int)fileHeader[zdssFileKeys.knumberRecords] - numberFound;
			if (numberErrors < 0) numberErrors = -numberErrors;
		}
	}
	free(pathnameBin);
	if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_GENERAL)) {
		if (numberErrors == 0) {
			zmessage(ifltab, "zcheckPathnames check passed with no errors");
		}
		else {
			zmessage(ifltab, " ");
			zmessageInt(ifltab, "zcheckPathnames check FAILED.  Number of errors: ", numberErrors);
			zmessage(ifltab, " ");
		}
	}
	return numberErrors;
}

void zcheckpathnames_(long long *ifltab, int *numberErrors)
{
	*numberErrors = zcheckPathnames(ifltab);
}
