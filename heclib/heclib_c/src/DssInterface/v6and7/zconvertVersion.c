#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "heclib.h"

// If MacOS, use hec_zopen instead of stdio::zopen
#ifdef __APPLE__
#define zopen hec_zopen
#endif

/**
*  Function:	zconvertVersion
*
*  Use:			Public
*
*  Description:	Copies a DSS-7 file into a DSS-6 file, or a DSS-6 file into a DSS-7 file
*
*  Declaration: int zconvertVersion(const char* fileNameFrom, const char* fileNameTo);
*
*  Parameters:
*				char *fileNameFrom:  The DSS file name to convert from
*					The file has to exist.  It can be opened elsewhere, but usually is not.
*
*				char *fileNameTo:  The DSS file name to convert to
*					The file should not exist, as it will be created with the other version from fileNameFrom.
*					If the file does exist and is the same version as fileNameFrom, an error will be returned
*
*	Returns:	STATUS_OKAY if file successfully converted and copied
*				< 0 if an error occurred
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zconvertVersion(const char* fileNameFrom, const char* fileNameTo)
{
	long long ifltabFrom[600];
	long long ifltabTo[600];
	int status;
	int versionNumber;
	int nrecs;
	char cscrat[20];


	if (!fileNameFrom) {
		return zerrorProcessing(ifltabFrom, DSS_FUNCTION_zcopyFile_ID, zdssErrorCodes.NULL_FILENAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "");
	}
	if (!fileNameTo) {
		return zerrorProcessing(ifltabFrom, DSS_FUNCTION_zcopyFile_ID, zdssErrorCodes.NULL_FILENAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "");
	}

	versionNumber = zgetFileVersion(fileNameFrom);
	if (versionNumber == 0) {
		return zerrorProcessing(ifltabFrom, DSS_FUNCTION_zcopyFile_ID,
				zdssErrorCodes.FILE_DOES_NOT_EXIST, 0, 0,
				zdssErrorSeverity.WARNING_NO_FILE_ACCESS, "", fileNameTo);
	}

	status = zopen(ifltabFrom, fileNameFrom);
	if (zisError(status)) {
		printf("Error opening file %s\n", fileNameFrom);
		return status;
	}

	if (zgetFileVersion(fileNameTo) == 0) {
		if (versionNumber == 6) {
			zinqir_(ifltabFrom, "NREC", cscrat, &nrecs, (size_t)4, (size_t)0);
			status = zopenInternal(ifltabTo, fileNameTo, 0, nrecs, 0, 0, 0);
		}
		else if (versionNumber == 7) {
			zopen6_ (ifltabTo, fileNameTo, &status, strlen(fileNameTo));
		}
		else {
			status = -1;
		}
	}
	else {
		if (versionNumber == zgetFileVersion(fileNameTo)) {
			return zerrorProcessing(ifltabFrom, DSS_FUNCTION_zcopyFile_ID,
				zdssErrorCodes.FILE_EXISTS, 0, 0,
				zdssErrorSeverity.WARNING_NO_FILE_ACCESS, fileNameTo, "");
		}
		status = zopen(ifltabTo, fileNameTo);
	}

	if (status == 0) {
		status = zcopyFile(ifltabFrom, ifltabTo, 0);
		zclose (ifltabFrom);
		zclose (ifltabTo);
	}

	return status;
}

void zconvertversion_ (const char* fileNameFrom, const char* fileNameTo, int *status,
					   size_t fileNameFromLen, size_t fileNameToLen)
{
	char *fileFrom;
	char *fileTo;

	fileFrom = stringFortToC(fileNameFrom, fileNameFromLen);
	fileTo = stringFortToC(fileNameTo, fileNameToLen);

	*status = zconvertVersion(fileFrom, fileTo);

	free(fileFrom);
	free(fileTo);
}

