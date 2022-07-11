#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdlib.h>

#include "zdssKeys.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "zprogress.h"
#include "hecdss7.h"

int checkGrid(long long* ifltab, const char* pathname, int dataType);

/**
*  Function:	zcheckInfo
*
*  Use:			Public 
*
*  Description:	Check a record information block to look for any invalid numbers in it.
*					Intent is to look for any damage 
*
*  Declaration: int zcheckInfo(long long *ifltab, const char *pathname, long long *info, int numberInfo);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.  
*					This should be considered as a �handle� array and must be passed to any function that accesses that file.
*
*				const char *pathname
*					The pathname for the info block.  This is only used to indicate the record if an error is found (not for checking)
*
*				long long *info
*					The information block to check
*
*				int numberInfo
*					The length of the info block, in long (integer 8) words
*
*
*	Returns:	int status
*					STATUS_OKAY for no errors.
*					> 0 for number of errors in the info block
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
int zcheckInfo(long long *ifltab, const char *pathname, long long *info, int numberInfo)
{
	long long *fileHeader;
	long long endAddress;
	int internalHeader[20];
	int dataType;
	int version;
	int len;
	int status;
	char dateString[40];
	char timeString[20];
	char programName[20];
	int numberErrors = 0;


	if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcheckInfo_ID, "Enter, Handle: ", zhandle(ifltab));
		zmessageDebug(ifltab, DSS_FUNCTION_zcheckInfo_ID, "Pathname: ", pathname);
	}
	
	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];		

	if ((numberErrors == 0) && ((info[zdssInfoKeys.kinfoFlag] != DSS_INFO_FLAG))) {
		numberErrors++;
		if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
			zmessageLong(ifltab, "zcheckInfo, Invalid Info Flag: ", info[zdssInfoKeys.kinfoFlag]);
			zmessageInt(ifltab, "Should be: ", DSS_INFO_FLAG);
		}
	}

	if ((numberErrors == 0) && ((info[zdssInfoKeys.kinfoStatus] < 0) ||  (info[zdssInfoKeys.kinfoStatus] > 20))) {
		numberErrors++;
		if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
			zmessageLong(ifltab, "zcheckInfo, Invalid Info Status: ", info[zdssInfoKeys.kinfoStatus]);
		}
	}

	if ((numberErrors == 0) && ((info[zdssInfoKeys.kinfoPathnameLength] < 0) ||  (info[zdssInfoKeys.kinfoPathnameLength] > 391))) {
		numberErrors++;
		if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
			zmessageLong(ifltab, "zcheckInfo, Invalid Info Pathname Length: ", info[zdssInfoKeys.kinfoPathnameLength]);
		}		
	}

	if ((numberErrors == 0) && ((info[zdssInfoKeys.kinfoInternalHeadAddress] < 0) ||  (info[zdssInfoKeys.kinfoInternalHeadAddress] > fileHeader[zdssFileKeys.kfileSize]))) {
		numberErrors++;
		if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
			zmessageLong(ifltab, "zcheckInfo, Invalid Info Internal Header Address: ", info[zdssInfoKeys.kinfoInternalHeadAddress]);
		}
	}

	if ((numberErrors == 0) && ((info[zdssInfoKeys.kinfoInternalHeadAddress] > info[zdssInfoKeys.kinfoHeader2Address]))) {
		numberErrors++;
		if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
			zmessageLong(ifltab, "zcheckInfo, Incorrect Info internal header address (after second header): ", info[zdssInfoKeys.kinfoInternalHeadAddress]);
			zmessageLong(ifltab, "Second header address: ", info[zdssInfoKeys.kinfoHeader2Address]);
		}
	}

	endAddress = (info[zdssInfoKeys.kinfoInternalHeadNumber]/2) + info[zdssInfoKeys.kinfoInternalHeadAddress];
	if ((numberErrors == 0) && ((info[zdssInfoKeys.kinfoInternalHeadNumber] < 0) ||  (endAddress > fileHeader[zdssFileKeys.kfileSize]))) {
		numberErrors++;
		if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
			zmessageLong(ifltab, "zcheckInfo, Invalid Info Internal Header Length: ", info[zdssInfoKeys.kinfoInternalHeadNumber]);
		}
	}

	if ((numberErrors == 0) && ((info[zdssInfoKeys.kinfoHeader2Address] < 0) ||  (info[zdssInfoKeys.kinfoHeader2Address] > fileHeader[zdssFileKeys.kfileSize]))) {
		numberErrors++;
		if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
			zmessageLong(ifltab, "zcheckInfo, Invalid Info Header 2 Address: ", info[zdssInfoKeys.kinfoHeader2Address]);
		}
	}

	if ((numberErrors == 0) && ((info[zdssInfoKeys.kinfoHeader2Address] > info[zdssInfoKeys.kinfoUserHeadAddress]))) {
		numberErrors++;
		if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
			zmessageLong(ifltab, "zcheckInfo, Incorrect Info header 2 address (after user header): ", info[zdssInfoKeys.kinfoHeader2Address]);
			zmessageLong(ifltab, "User header address: ", info[zdssInfoKeys.kinfoUserHeadAddress]);
		}
	}

	endAddress = (info[zdssInfoKeys.kinfoHeader2Number]/2) + info[zdssInfoKeys.kinfoHeader2Address];
	if ((numberErrors == 0) && ((info[zdssInfoKeys.kinfoHeader2Number] < 0) ||   (endAddress > fileHeader[zdssFileKeys.kfileSize]))) {
		numberErrors++;
		if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
			zmessageLong(ifltab, "zcheckInfo, Invalid Info Header 2 Length: ", info[zdssInfoKeys.kinfoHeader2Number]);
		}
	}	

	if ((numberErrors == 0) && ((info[zdssInfoKeys.kinfoUserHeadAddress] < 0) ||  (info[zdssInfoKeys.kinfoUserHeadAddress] > fileHeader[zdssFileKeys.kfileSize]))) {
		numberErrors++;
		if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
			zmessageLong(ifltab, "zcheckInfo, Invalid Info User Header Address: ", info[zdssInfoKeys.kinfoUserHeadAddress]);
		}
	}

	if ((numberErrors == 0) && ((info[zdssInfoKeys.kinfoUserHeadAddress] > info[zdssInfoKeys.kinfoValues1Address]))) {
		numberErrors++;
		if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
			zmessageLong(ifltab, "zcheckInfo, Incorrect user header address (after first values address): ", info[zdssInfoKeys.kinfoUserHeadAddress]);
			zmessageLong(ifltab, "first values address: ", info[zdssInfoKeys.kinfoValues1Address]);
		}
	}

	endAddress = (info[zdssInfoKeys.kinfoUserHeadNumber]/2) + info[zdssInfoKeys.kinfoUserHeadAddress];
	if ((numberErrors == 0) && ((info[zdssInfoKeys.kinfoUserHeadNumber] < 0) ||  (endAddress > fileHeader[zdssFileKeys.kfileSize]))) {
		numberErrors++;
		if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
			zmessageLong(ifltab, "zcheckInfo, Invalid Info User Header Length: ", info[zdssInfoKeys.kinfoUserHeadNumber]);
		}
	}

	if ((numberErrors == 0) && ((info[zdssInfoKeys.kinfoValues1Address] < 0) ||  (info[zdssInfoKeys.kinfoValues1Address] > fileHeader[zdssFileKeys.kfileSize]))) {
		numberErrors++;
		if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
			zmessageLong(ifltab, "zcheckInfo, Invalid first values block address: ", info[zdssInfoKeys.kinfoValues1Address]);
		}
	}

	if ((numberErrors == 0) && ((info[zdssInfoKeys.kinfoValues1Address] > info[zdssInfoKeys.kinfoValues2Address]))) {
		numberErrors++;
		if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
			zmessageLong(ifltab, "zcheckInfo, Incorrect first values address (after second values address): ", info[zdssInfoKeys.kinfoValues1Address]);
			zmessageLong(ifltab, "second values address: ", info[zdssInfoKeys.kinfoValues2Address]);
		}
	}

	endAddress = (info[zdssInfoKeys.kinfoValues1Number] / 2) + info[zdssInfoKeys.kinfoValues1Address];
	if ((numberErrors == 0) && ((info[zdssInfoKeys.kinfoValues1Number] < 0) || (endAddress > fileHeader[zdssFileKeys.kfileSize]))) {
		numberErrors++;
		if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
			zmessageLong(ifltab, "zcheckInfo, Invalid first values block length: ", info[zdssInfoKeys.kinfoValues1Number]);
		}
	}

	if ((numberErrors == 0) && ((info[zdssInfoKeys.kinfoValues2Address] < 0) || (info[zdssInfoKeys.kinfoValues2Address] > fileHeader[zdssFileKeys.kfileSize]))) {
		numberErrors++;
		if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
			zmessageLong(ifltab, "zcheckInfo, Invalid second values block address: ", info[zdssInfoKeys.kinfoValues2Address]);
		}
	}

	endAddress = (info[zdssInfoKeys.kinfoValues2Number] / 2) + info[zdssInfoKeys.kinfoValues2Address];
	if ((numberErrors == 0) && ((info[zdssInfoKeys.kinfoValues2Number] < 0) || (endAddress > fileHeader[zdssFileKeys.kfileSize]))) {
		numberErrors++;
		if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
			zmessageLong(ifltab, "zcheckInfo, Invalid second values block length: ", info[zdssInfoKeys.kinfoValues2Number]);
		}
	}

	if ((numberErrors == 0) && ((info[zdssInfoKeys.kinfoValues3Address] < 0) || (info[zdssInfoKeys.kinfoValues3Address] > fileHeader[zdssFileKeys.kfileSize]))) {
		numberErrors++;
		if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
			zmessageLong(ifltab, "zcheckInfo, Invalid third values block address: ", info[zdssInfoKeys.kinfoValues3Address]);
		}
	}

	if ((numberErrors == 0) && ((info[zdssInfoKeys.kinfoValues2Address] > info[zdssInfoKeys.kinfoValues3Address]))) {
		numberErrors++;
		if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
			zmessageLong(ifltab, "zcheckInfo, Incorrect second values address (after third values address): ", info[zdssInfoKeys.kinfoValues2Address]);
			zmessageLong(ifltab, "third values address: ", info[zdssInfoKeys.kinfoValues3Address]);
		}
	}

	endAddress = (info[zdssInfoKeys.kinfoValues3Number] / 2) + info[zdssInfoKeys.kinfoValues3Address];
	if ((numberErrors == 0) && ((info[zdssInfoKeys.kinfoValues3Number] < 0) || (endAddress > fileHeader[zdssFileKeys.kfileSize]))) {
		numberErrors++;
		if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
			zmessageLong(ifltab, "zcheckInfo, Invalid third values block length: ", info[zdssInfoKeys.kinfoValues3Number]);
		}
	}

	endAddress = (info[zdssInfoKeys.kinfoAllocatedSize] / 2) + info[zdssInfoKeys.kinfoValues1Address];
	if ((numberErrors == 0) && ((info[zdssInfoKeys.kinfoAllocatedSize] < 0) || (endAddress > fileHeader[zdssFileKeys.kfileSize]))) {
		numberErrors++;
		if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
			zmessageLong(ifltab, "zcheckInfo, Invalid Info Allocated Length: ", info[zdssInfoKeys.kinfoAllocatedSize]);
		}
	}

	if ((numberErrors == 0) && (info[zdssInfoKeys.kinfoNumberData] < 0)) {
		numberErrors++;
		if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
			zmessageLong(ifltab, "zcheckInfo, Invalid Info Number of Data: ", info[zdssInfoKeys.kinfoNumberData]);
		}
	}

	if ((numberErrors == 0) && (info[zdssInfoKeys.kinfoLogicalNumber] < 0)) {
		numberErrors++;
		if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
			zmessageLong(ifltab, "zcheckInfo, Invalid Info Logical Number of Data: ", info[zdssInfoKeys.kinfoLogicalNumber]);
		}
	}

	if ((numberErrors == 0) && ((info[zdssInfoKeys.kinfoAliasesBinAddress] < 0) || (info[zdssInfoKeys.kinfoAliasesBinAddress] > fileHeader[zdssFileKeys.kfileSize]))) {
		numberErrors++;
		if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
			zmessageLong(ifltab, "zcheckInfo, Invalid Info Aliases Bin Address: ", info[zdssInfoKeys.kinfoAliasesBinAddress]);
		}
	}

	if (numberErrors == 0) {
		i8toi4(info[zdssInfoKeys.kinfoTypeVersion], &dataType, &version);
		if (version < 0) {
			numberErrors++;
			if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
				zmessageInt(ifltab, "zcheckInfo, Invalid version: ", version);
			}
		}
		if (dataType < 0) {
			numberErrors++;
			if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
				zmessageInt(ifltab, "zcheckInfo, Invalid data type: ", dataType);
			}
		}
		//  Check that we have a valid compression header, if required.
		if ((dataType >= 100) && (dataType < 200)) {
			status = zget(ifltab, info[zdssInfoKeys.kinfoInternalHeadAddress], internalHeader, 18, 1);
			if (zisError(status)) {
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcheckInfo_ID);
			}
			if (bigEndian()) {
				zswitchInts(internalHeader, 18);
			}
			if ((internalHeader[9] == 1) && (info[zdssInfoKeys.kinfoHeader2Number] == 0)) {
				numberErrors++;
				if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
					zmessageInt(ifltab, "zcheckInfo, Compression header required but missing.  Compression method: ", internalHeader[9]);
					zmessageLong(ifltab, "zcheckInfo, Compression header length: ", info[zdssInfoKeys.kinfoHeader2Number]);
				}
			}
		}
		// check gridstructversion
		if ((dataType >= 400) && (dataType < 450)) {
		
			numberErrors += checkGrid(ifltab, pathname,dataType);
		}

	}



	if (numberErrors > 0) {
		if (zmessageLevel(ifltab, MESS_METHOD_FILE_CHECK_ID, MESS_LEVEL_GENERAL)) {
			zmessage2(ifltab, "Pathname: ", pathname);
			zmessageLong(ifltab, "zcheckInfo, Pathname Bin Address: ", ifltab[zdssKeys.kpathBinAddress]);
			zmessageLong(ifltab, "zcheckInfo, Info Address: ", ifltab[zdssKeys.kaddInfoLastPath]);
			millsToDateTime(info[zdssInfoKeys.kinfoLastWriteTime], dateString, timeString, sizeof(dateString), sizeof(timeString));
			len = (int)strlen(dateString);
			dateString[len++] = ',';
			dateString[len++] = ' ';
			dateString[len] = '\0';
			stringCat(dateString, sizeof(dateString), timeString, strlen(timeString));
			zmessage2(ifltab, "Last written at: ", dateString);
			charLong(&info[zdssInfoKeys.kinfoProgram], programName, zdssVals.numberProgram, sizeof(programName), 0, 0);
			zmessage2(ifltab, "Written by program: ", programName);
		}
	}

	if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcheckInfo_ID, "Exit, Handle: ", zhandle(ifltab));
		zmessageDebug(ifltab, DSS_FUNCTION_zcheckInfo_ID, "Pathname: ", pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcheckInfo_ID, "Number of Errors: ", numberErrors);
	}
		
	return numberErrors;
}



/*
Check if DSS7 files should have gridstruct version = 100
		 DSS6 files should have gridstruct version = 0
*/
int checkGrid(long long* ifltab, const char* pathname, int dataType)
{
	int dssVersion = zgetVersion(ifltab);
	int gridStructVersion;
	int status;
	int errorCount;
	errorCount = 0;

	if (dataType >= DATA_TYPE_UGT && dataType < 450)
	{//grid data types
		status = zspatialGridRetrieveVersion(ifltab, pathname, &gridStructVersion);
		if (status != 0)
		{
			zmessageInt(ifltab, "Error code: ", status);
			zmessage2(ifltab, pathname, "Error reading grid. \n");
			zmessage(ifltab, pathname);
			errorCount++;
		}
		else if (dssVersion == 7 && gridStructVersion != -VERSION_100)
		{
			zmessage(ifltab, "Warning: unexpected gridStructVersion in DSS7 file");
			zmessage(ifltab, pathname);
			zmessageInt(ifltab, "gridStructVersion = ", gridStructVersion);
			errorCount++;
		}else 	if (dssVersion == 6 && gridStructVersion == -VERSION_100)
		{
			zmessage(ifltab, "Warning: unexpected gridStructVersion in DSS6 file");
			zmessage(ifltab, pathname);
			zmessageInt(ifltab, "gridStructVersion = ", gridStructVersion);
			errorCount++;
		}
	}

	return errorCount;
}