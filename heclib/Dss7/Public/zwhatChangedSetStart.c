#include <stdlib.h>

#include "zdssKeys.h"
#include "hecdss7.h"

/**
*  Function:	zwhatChangedSetStart
*
*  Use:			Public
*
*  Description:	Sets a catalog structure to use for compare later for records that have changed
*
*  Declaration: int zwhatChangedSetStart(long long *ifltab, zStructCatalog *catStruct, const char *pathWithWildChars, int boolUseCRC);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.*  Parameters:	const char *pathname1:  The first pathname to compare
*
*				zStructCatalog *catStruct (optional)
*					A catalog struct to be made before an operation (e.g., compute) to compare it with.
*					This can be null (0), if you are only doing one comparison and want DSS to hold the catalog struct internally.
*
*				const char *pathWithWildChars
*					Either null (for ignore) or a String that represents a pathname with wild characters represented
*					by a star (*) to match any string in the pathname part.  Wild characters can only be at the beginning or end of a part,
*					not inside of a string.  An example is a C part with "*Flow*" , which
*					will match all pathnames that have "Flow" anywhere in the C part, such as "Flow", "Inflow", "Outflow-Reg", etc.
*					A part such as "Flow*Reg" is not legal.  A null (//) will only match a null, where only a star (*) will match all.
*					catStructBefore should have been filled out by zcatalog using this same pathWithWildChars.
*
*				int boolUseCRC
*					Use cycle redundancy check to determine if the data values have changed, instead of a record just being re-written.
*					This must be included when catStructBefore was filled out (catStructBefore->boolGetCRCvalues = boolUseCRC;)
*					Set to 1 to use CRC and determine if data values have changed, or set to 0 to just check last written time
*					CAUTION:  Use of CRC is resource intensive; only use when you really need (such as transferring across network.)
*
*
*  Returns:		status => 0 for success, < 0 for error
*					If CRC is used, the CRC of all matching paths will be returned in catStruct or stored internally
*
*  Remarks:		DSS Version 7 only
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int zwhatChangedSetStart(long long *ifltab, zStructCatalog *catStruct, const char *pathWithWildChars, int boolUseCRC)
{
	int status;
	zStructCatalog *catStructLocal;
	long long *fileHeader;


	if (zgetVersion(ifltab) != 7) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
								zgetVersion(ifltab), 0, zdssErrorSeverity.WARNING, "", "");
	}

	if (zmessageLevel(ifltab, MESS_METHOD_CATALOG_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "Enter zwhatChangedSetStart, path with wild: ", pathWithWildChars);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcatalog_ID, "boolUseCRC: ", boolUseCRC);
	}

	//  Free any previous start point
	if (!catStruct) {
		catStructLocal = (zStructCatalog *)ifltab[zdssKeys.kcatStruct];
		if (catStructLocal) {
			zstructFree(catStructLocal);
			catStructLocal = (zStructCatalog *)0;
			ifltab[zdssKeys.kcatStruct] = 0;
		}
		catStructLocal = zstructCatalogNew();
		ifltab[zdssKeys.kcatStruct] = (long long)catStructLocal;
	}
	else {
		catStructLocal = catStruct;
	}

	catStructLocal->boolGetCRCvalues = boolUseCRC;
	if (boolUseCRC) {
		status = zcatalog(ifltab, pathWithWildChars, catStructLocal, 0);
	}
	else {
		//  If we are NOT using CRC, but just last written time, no need to do a full catalog
		//  Just save last write time and path with wild characters.
		status = zpermRead(ifltab);
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcatalog_ID);
		}
		fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
		catStructLocal->lastWriteTimeFile = fileHeader[zdssFileKeys.klastWriteTime];
		if (pathWithWildChars) {
			catStructLocal->pathWithWildChars = mallocAndCopy(pathWithWildChars);
		}
		if (zmessageLevel(ifltab, MESS_METHOD_CATALOG_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			zmessageDebugLong(ifltab, DSS_FUNCTION_zcatalog_ID, "zwhatChangedSetStart, lastWriteTimeFile: ", catStructLocal->lastWriteTimeFile);
		}
	}
	return status;
}

