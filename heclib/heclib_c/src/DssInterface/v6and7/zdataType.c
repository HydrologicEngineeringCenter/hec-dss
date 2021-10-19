#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include "heclib6.h"
#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssVals.h"
#include "hecdssInternal.h"
#include "zdssMessages.h"
#include "hecdssFort.h"


/**
*  Function:	zdataType
*
*  Use:			Public
*
*  Description:	Obtains the data type (time series, paired, etc.) for a record
*					Does not guarantee that the record exists (use zcheck for that)
*
*  Declaration: int zdataType (long long *ifltab, const char* pathname);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				const char* pathname
*					The pathname of the record to check.
*
*
*	Returns:	int dataType
					> 0  The data type for the record
					See file zdssRecordDescriptions.h for data types and descriptions
*					STATUS_NOT_OKAY (=STATUS_RECORD_NOT_FOUND, =-1) if the data type can not be determined
*					< -1.   error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*	Remarks:	Version 6 and 7
*					If the type is not time series, then a positive dataType indicates the record does exist
*					If time series, then no guarantee that the record exists (uses the E part to identify)
*
*	See Also:	ztypeName - returns the name of the data type
*					const char *ztypeName(int recordType, int boolAbbreviation);
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*
*/

int zdataType (long long *ifltab, const char* pathname)
{
	int dataType;
	int status;
	int catSort;
	int interval;


	if (!pathname) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zreadInfo_ID, zdssErrorCodes.NULL_PATHNAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "pathname is null");
	}

	if (zmessageLevel(ifltab, MESS_METHOD_GENERAL_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zreadInfo_ID, "Enter zdataType;  Handle: ",  zhandle(ifltab));
		zmessageDebug(ifltab, DSS_FUNCTION_zreadInfo_ID, " Pathname: ",pathname);
	}

	if (zgetVersion(ifltab) == 6) {
		dataType = zdataType6(ifltab, pathname);
		return dataType;
	}


	status = zcheck(ifltab, pathname);
	if (zisError(status)) {
		return status;
	}

	if (status == STATUS_RECORD_FOUND) {
		i8toi4(ifltab[zdssKeys.kbinTypeAndCatSort], &dataType, &catSort);
	}
	else {
		//  If it is time series, the D part may not be exact...
		//  Check to see if a valid E (Interval) part
		interval = ztsGetInterval(zgetVersion(ifltab), pathname);
		if (interval > 0) {
			dataType = 100;
		}
		else if (interval < 0) {
			dataType = 110;
		}
		else {
			dataType = 0;
		}
	}

	if (zmessageLevel(ifltab, MESS_METHOD_GENERAL_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zreadInfo_ID, "zdataType, Found: ", status);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zreadInfo_ID, "zdataType, Data Type: ", dataType);
	}
	return dataType;
}

int zdataType6(long long *ifltab, const char* pathname)
{
	int ndata;
	int lfound;
	int dataType;
	char cdtype[10];
	char cpath[MAX_PATHNAME_SIZE];

	stringCToFort(cpath, sizeof(cpath), pathname);
	zdtype6_(ifltab, cpath, &ndata, &lfound, cdtype, &dataType,
		strlen(pathname), sizeof(cdtype));

	if (lfound) return dataType;
	return STATUS_RECORD_NOT_FOUND;

}


//  Fortran compatible interface
void zdatatype_(long long *ifltab, const char* pathname, int *dataType,
			 int *status, size_t lenPathname)
{
	char *path;
	path = stringFortToC(pathname, lenPathname);
	*dataType = zdataType(ifltab, path);
	if (*dataType < 0) *status = *dataType;
	free(path);
}


//CALL zdtype7 (IFLTAB, CPATH, NDATA, LFOUND, CDTYPE, IDTYPE)
//  Fortran compatible interface
int zdatatype7_(long long *ifltab, const char* pathname, int *numberData,
			 int *status, int *dataType, size_t lenPathname)
{
	 char *path;
	 zStructRecordBasics *recordBasics;

	path = stringFortToC(pathname, lenPathname);

	recordBasics = zstructRecordBasicsNew(path);
	free(path);

	*status = zgetRecordBasics(ifltab, recordBasics);
	if (zisError(*status)) {
		zstructFree(recordBasics);
		return *status;
	}

	*dataType = recordBasics->recordType;
	if (*status == STATUS_RECORD_NOT_FOUND) {
		//  Could be time series with a different "E" part
		*numberData = 0;
		zstructFree(recordBasics);
		return *dataType;
	}
	*numberData = recordBasics->numberValues;
	zstructFree(recordBasics);
	return STATUS_RECORD_FOUND;
}

