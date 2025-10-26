#include "zdssKeys.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "zprogress.h"
#include "heclib.h"


/**
*  Function:	zcheckLinks
*
*  Use:			Semi-Public
*
*  Description:	Searchs a DSS file for all record info blocks, then walks
*					the pathname bin to be sure there are addresses to that record.
*					Intent is to look for any damage within the DSS file
*					Generally zcheckFile is called instead
*
*  Declaration: int zcheckLinks(long long *ifltab);
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
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zcheckLinks(long long *ifltab)
{
	int i;
	int pathLen;
	int numberInfo1;
	int numberInfo2;
	int numberNotFound;
	int numberFound;
	int status;
	int icase;
	long long infoAddress;
	long long address;
	long long fileSize;
	int readSize = 4028;
	long long fileBuff[4028];
	long long recHeader[2];
	long long info1[100];
	long long info2[100];
	long long *fileHeader;
	char messageString[80];
	char pathname[MAX_PATHNAME_LENGTH];


	if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcheckLinks_ID, "Enter, Handle: ", zhandle(ifltab));
	}

	status = zcheckKeys(ifltab);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcheckLinks_ID);
	}

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];

	numberNotFound = 0;
	numberFound = 0;
	address = 0;
	fileSize = fileHeader[zdssFileKeys.kfileSize];
	//  Brute force through the file, ignoring hash, addresses, etc.
	//  This will allow recovery of a damaged file
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
		for (i=0; i<readSize; i++) {
			if (fileBuff[i] == DSS_INFO_FLAG) {
				//  Found a flag - check to be sure this is the flag and not a fluke
				//  Make sure the next word contains the status and the following the path length
				if ((i+2) < readSize) {
					recHeader[0] = fileBuff[i+1];
					recHeader[1] = fileBuff[i+2];
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
					if ((recHeader[0] == REC_STATUS_PRIMARY) || (recHeader[0] == REC_STATUS_VALID)){
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
								if (icase == 1) {
									zmessage (ifltab, "***  Info addresses differ for same record");
								}
								else if (icase == 2) {
									//  I don't think this will happen very often...
									zmessage (ifltab, "***  Different Info addresses lengths for same record");;
								}
							}
							numberNotFound++;
							zprogress.numberErrors = numberNotFound;
							zprogress.handle = zhandle(ifltab);
							if (numberNotFound >= zprogress.maxErrors)
								return numberNotFound;
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
			zmessage(ifltab, "zcheckLinks check passed with no errors");
		}
		else {
			zmessageInt(ifltab, "zcheckLinks check FAILED.  Number of errors: ", numberNotFound);
		}
	}
	return numberNotFound;
}

void zchecklinks_(long long *ifltab, int *numberNotFound)
{
	*numberNotFound = zcheckLinks(ifltab);
}

