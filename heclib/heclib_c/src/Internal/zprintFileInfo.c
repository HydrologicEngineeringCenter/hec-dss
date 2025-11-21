
#include <stdio.h>

#include "heclib7.h"
#include "zdssMessages.h"
#include "zdssKeys.h"
#include "zdssVals.h"
#include "hecdssInternal.h"

/**
*  Function:	zprintFileInfo
*
*  Use:			Semi-Private
*
*  Description:	Prints information about a DSS file on closing
*
*  Declaration: void zprintFileInfo(long long *ifltab)
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*
*	Note:		Strings are defined in zdssMessages.h for portability
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/
void zprintFileInfo(long long *ifltab)
{
	long long kbytes;
	long long mbytes;
	long long *fileHeader;
	char messageString[200];


	if (zgetVersion(ifltab) != 7) {
		zmessage(ifltab, "Print file info called with incorrect version");
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
			"This function is for DSS version 7; it is being called with a %d version file.",  zgetVersion(ifltab));
		zmessage(ifltab, messageString);
		return;
	}

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	kbytes = fileHeader[zdssFileKeys.kfileSize] * 8 / 1024;
	mbytes = kbytes / 1024;

	_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, FILE_INFO_MESS_01, fileHeader[zdssFileKeys.knumberRecords]);
	zmessage(ifltab, messageString);
	_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, FILE_INFO_MESS_02, fileHeader[zdssFileKeys.kfileSize]);
	zmessage(ifltab, messageString);
	_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, FILE_INFO_MESS_03, kbytes, mbytes);
	zmessage(ifltab, messageString);
	_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, FILE_INFO_MESS_04, fileHeader[zdssFileKeys.kdead]);
	zmessage(ifltab, messageString);
	_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, FILE_INFO_MESS_05, fileHeader[zdssFileKeys.kmaxHash]);
	zmessage(ifltab, messageString);
	_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, FILE_INFO_MESS_06, fileHeader[zdssFileKeys.khashsUsed]);
	zmessage(ifltab, messageString);
	_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, FILE_INFO_MESS_07, fileHeader[zdssFileKeys.kmaxPathsOneHash]);
	zmessage(ifltab, messageString);
	_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, FILE_INFO_MESS_08, fileHeader[zdssFileKeys.kmaxPathsHashCode]);
	zmessage(ifltab, messageString);
	_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, FILE_INFO_MESS_09, fileHeader[zdssFileKeys.khashCollisions]);
	zmessage(ifltab, messageString);
	_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, FILE_INFO_MESS_10, fileHeader[zdssFileKeys.ktotalBins]);
	zmessage(ifltab, messageString);
	_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, FILE_INFO_MESS_11, fileHeader[zdssFileKeys.kbinsOverflow]);
	zmessage(ifltab, messageString);
	_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, FILE_INFO_MESS_12, ifltab[zdssKeys.knumberReads]);
	zmessage(ifltab, messageString);
	_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, FILE_INFO_MESS_13, ifltab[zdssKeys.knumberWrites]);
	zmessage(ifltab, messageString);
	_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, FILE_INFO_MESS_14, ifltab[zdssKeys.klocksDenied]);
	zmessage(ifltab, messageString);
}

