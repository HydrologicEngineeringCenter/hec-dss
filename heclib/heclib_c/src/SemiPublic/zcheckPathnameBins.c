#include "zdssKeys.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "zprogress.h"
#include "heclib.h"


/**
*  Function:	zcheckPathnameBins
*
*  Use:			Semi-Public
*
*  Description:	Walks through a DSS file checking that all pathname bins
*					contain valid information.  (It does not check to see if
*					there is a link to each pathname; use zcheckPathnames instead.)
*					Intent is to look for any damage within the DSS file
*					Generally zcheckFile is called instead
*
*  Declaration: int zcheckPathnameBins(long long *ifltab);
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
*	Remarks:	All errors are written to the message log if log level is at least MESS_LEVEL_GENERAL.
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

int zcheckPathnameBins(long long *ifltab)
{
	int istat = 0;
	int icount;
	int jcount;
	int ncount;
	int numberErrors;
	int binSize;
	int numberChars;
	int pathnameSize;
	int boolShouldBeZero;
	int k;
	int errorOccurred;
	int dataType;
	int catSequenceNumber;
	int ich;
	int numberPathnames;
	int numberBins;
	long long binAddress;
	long long *pathnameBin;
	long long *fileHeader;
	char messageString[120];
	char pathname[MAX_PATHNAME_LENGTH];


	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcheckPathnameBins_ID, "Enter;  Handle: ",  zhandle(ifltab));
	}


	istat = zcheckKeys(ifltab);
	if (istat != 0) {
		return istat;
	}

	//  We need to keep the pathname bin and info arrays as separate
	//  so that ifltab can be used for double checking
	binSize = (int)fileHeader[zdssFileKeys.kbinSize];
	pathnameBin = (long long*)calloc((size_t)binSize, LONG_SIZE);
	binAddress = fileHeader[zdssFileKeys.kaddFirstBin];
	numberErrors = 0;
	errorOccurred = 0;
	boolShouldBeZero = 0;
	numberPathnames = 0;
	numberBins = 0;

	/*
	pathnameBin[icount+zdssBinKeys.kbinHash] = ifltab[zdssKeys.kpathnameHash];
	pathnameBin[icount+zdssBinKeys.kbinStatus] = recordStatus;
	pathnameBin[icount+zdssBinKeys.kbinPathLen] = i4toi8((int)ifltab[zdssKeys.klenLastPath], pathnameSize);
	pathnameBin[icount+zdssBinKeys.kbinInfoAdd] = infoAddress;
	pathnameBin[icount+zdssBinKeys.kbinTypeAndCatSort] = i4toi8(dataType, (int)fileHeader[zdssFileKeys.kcatSequenceNumber]);
	pathnameBin[icount+zdssBinKeys.kbinDates] = ifltab[zdssKeys.kbinDates];
	charLong((void *)pathname, (void *)&pathnameBin[icount+zdssBinKeys.kbinPath], (int)ifltab[zdssKeys.klenLastPath], (int)ifltab[zdssKeys.klenLastPath], 1, 1);
	*/

	while (1) {
		jcount = 0;
		while (jcount < (int)fileHeader[zdssFileKeys.kbinsPerBlock]) {
			istat = zget(ifltab, binAddress, (void *)pathnameBin, binSize, 2);
			if (istat != 0) {
				free(pathnameBin);
				return istat;
			}
			icount = 0;
			boolShouldBeZero = 0;
			ncount = 0;
			numberBins++;
			while (ncount < binSize) {
				if (boolShouldBeZero) {
					//  Last position in bin can be address to following bin
					if ((pathnameBin[ncount] != 0) && (ncount != (binSize-1))) {
						//  Error - Non zero value read, when zero expected
						if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
							_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %lld, position %d, value %lld",
								binAddress, ncount, pathnameBin[ncount]);
							zmessage2(ifltab, "***  Non-zero value (when zero expected) in check pathnames bins at address ", messageString);
						}
						numberErrors++;
						errorOccurred = 1;
					}
					ncount++;
				}
				else if (icount == 0) {
					//  Should be pathname hash (virtually any number)
					if (pathnameBin[ncount+zdssBinKeys.kbinHash] == 0) {
						//  End of pathnames in this block
						boolShouldBeZero = 1;
					}
				}
				else if (icount == 1) {
					//  Should be record status
					if ((pathnameBin[ncount+zdssBinKeys.kbinStatus] < 1) || (pathnameBin[ncount+zdssBinKeys.kbinStatus] > 15)) {
						//  Error - Invalid status
						if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
							_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %lld, position %d, value %lld",
								binAddress, ncount+zdssBinKeys.kbinStatus, pathnameBin[ncount+zdssBinKeys.kbinStatus]);
							zmessage2(ifltab, "***  Invalid status in check pathnames bins at address ", messageString);
						}
						numberErrors++;
						errorOccurred = 1;
					}
				}
				else if (icount == 2) {
					//  Should be pathname length (chars) and size (i8 words)
					i8toi4(pathnameBin[ncount+zdssBinKeys.kbinPathLen], &numberChars, &pathnameSize);
					if ((numberChars < 1) || (numberChars > 800)) {
						//  Invalid pathname length, in chars
						if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
							_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %lld, position %d, length %d",
								binAddress, ncount+zdssBinKeys.kbinPathLen, numberChars);
							zmessage2(ifltab, "***  Invalid pathname length in check pathnames bins at address ", messageString);
							numberErrors++;
							errorOccurred = 1;
						}
					}
					else if ((pathnameSize < 1) || (pathnameSize > 100)) {
						//  Invalid pathname size, in i8 words
						if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
							_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %lld, position %d, size %d",
								binAddress, ncount+zdssBinKeys.kbinPathLen, pathnameSize);
							zmessage2(ifltab, "***  Invalid pathname size in check pathnames bins at address ", messageString);
							numberErrors++;
							errorOccurred = 1;
						}
					}
				}
				else if (icount == 3) {
					//  Should be infoAddress
					if ((pathnameBin[ncount+zdssBinKeys.kbinInfoAdd] < 1) || (pathnameBin[ncount+zdssBinKeys.kbinInfoAdd] > fileHeader[zdssFileKeys.kfileSize])) {
						//  Error - Invalid address
						if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
							_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %lld, position %d, address %lld",
								binAddress, ncount+zdssBinKeys.kbinInfoAdd, pathnameBin[ncount+zdssBinKeys.kbinInfoAdd]);
							zmessage2(ifltab, "***  Invalid info address in check pathnames bins at address ", messageString);
							numberErrors++;
							errorOccurred = 1;
						}
					}
				}
				else if (icount == 4) {
					//  Should be dataType and catSequenceNumber
					i8toi4(pathnameBin[ncount+zdssBinKeys.kbinTypeAndCatSort], &dataType, &catSequenceNumber);
					if ((dataType < 0) || (dataType > 1000)) {
						//  Invalid data type
						if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
							_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %lld, position %d, type %d",
								binAddress, ncount+zdssBinKeys.kbinTypeAndCatSort, dataType);
							zmessage2(ifltab, "***  Invalid data type in check pathnames bins at address ", messageString);
							numberErrors++;
							errorOccurred = 1;
						}
					}
					if (catSequenceNumber < 0) { // || (catSequenceNumber > (ifltab[knumberRecords])) {
						//  Invalid catSequenceNumber
						if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
							_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %lld, position %d, number %d",
								binAddress, ncount+zdssBinKeys.kbinTypeAndCatSort, catSequenceNumber);
							zmessage2(ifltab, "***  Invalid catSequenceNumber in check pathnames bins at address ", messageString);
							numberErrors++;
							errorOccurred = 1;
						}
					}
				}
				else if (icount == 5) {
					//  Should be first and last Julian dates
					//  Not going to check
				}
				else if (icount == 6) {
					//  Should be pathname
					numberPathnames++;
					i8toi4(pathnameBin[ncount+zdssBinKeys.kbinPathLen], &numberChars, &pathnameSize);
					if ((numberChars > 0) && (numberChars <= MAX_PATHNAME_LENGTH)) {
						charLong(&pathnameBin[ncount + zdssBinKeys.kbinPath], pathname, numberChars, sizeof(pathname), 0, 0);
						for (k=0; k<numberChars; k++) {
							ich = pathname[k];
							if (!isValidChar(ich)) {
								if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
									_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %lld, position %d, dec char %d",
										binAddress, ncount+zdssBinKeys.kbinPathLen, ich);
									zmessage2(ifltab, "***  Invalid character in check pathnames bins at address ", messageString);
									numberErrors++;
									errorOccurred = 1;
								}
								break;
							}
						}
						if (errorOccurred) {
							if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
								zmessage2(ifltab, "***  Error Occurred in pathname ", pathname);
							}
							errorOccurred = 0;
						}
					}
					else {
						if (errorOccurred) {
							if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
								zmessage(ifltab, "***  Invalid pathname length prevents display of pathname");
							}
							errorOccurred = 0;
						}
					}
					//  restart
					icount = -1;
					ncount += zdssBinKeys.kbinSize + pathnameSize;
				}
				icount++;
				zprogress.numberErrors = numberErrors;
				if (numberErrors > zprogress.maxErrors) {
					free(pathnameBin);
					return numberErrors;
				}
			}
			binAddress += binSize;
			jcount++;
		}
		if (zprogress.interrupt) {
			free(pathnameBin);
			return numberErrors;
		}
		istat = zget(ifltab, binAddress, (void *)pathnameBin, 1, 2);
		if (istat != 0) {
			free(pathnameBin);
			return istat;
		}
		numberBins++;
		binAddress = pathnameBin[0];
		if (binAddress == 0) {
			break;
		}
	}

	if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, MESS_METHOD_FILE_CHECK_ID, "Exit zcheckPathnameBins; Number Errors :", numberErrors);
		zmessageDebugInt(ifltab, MESS_METHOD_FILE_CHECK_ID, "Number Pathnames checked :", numberPathnames);
		zmessageDebugInt(ifltab, MESS_METHOD_FILE_CHECK_ID, "Number Bins checked :", numberBins);
	}

	if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_GENERAL)) {
		if (numberErrors == 0) {
			zmessage(ifltab, "zcheckPathnameBins check passed with no errors");
		}
		else {
			zmessageInt(ifltab, "zcheckPathnameBins check FAILED.  Number of errors: ", numberErrors);
		}
	}

	free(pathnameBin);
	return numberErrors;
}


