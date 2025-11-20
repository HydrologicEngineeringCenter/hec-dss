#include "heclib.h"


/**
*  Function:	zcatalogToFile
*
*  Use:			Public
*
*  Description:	Writes a list of pathnames in a DSS file to an (external) file controlled by the calling program
*
*  Declaration: int zcatalogToFile(long long *ifltab, int catalogHandle, int fortranUnit, int boolSorted);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				int catalogHandle
*					A C handle (file descriptor) connected to a file opened by the calling function, usually with sopen or similar.
*					If the pathnames are to be written to a FORTRAN unit, then this should be zero.
*
*				int fortranUnit
*					A Fortran unit number connected to a file opened by the calling function.
*					If the pathnames are to be written to a C handle, then this should be zero.
*
*				int boolSorted
*					An int boolean flag set to "1" to indicate that the pathnames should be sorted, or "0" if they should not.
*					Sorting can take considerable more resources and time
*
*
*	Returns:	int numberCataloged
*					Number of pathnames in the file (a positive number)
*					or, if a negative number, then errorCode for error
*					errorCode may be decoded by function zerrorDecode()
*
*	Remarks:	For both DSS Version 6 and DSS Version 7 file.
*					This is very similar to function zcatalogFile, except here, you are responsible for opening
*					and closing your file.  This function is usually used when the calling function does
*					not have many resources.
*
*
*	See Also:	zcatalog(), zcatalogFile()
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/



int zcatalogToFile(long long *ifltab, int catalogHandle, int fortranUnit, int boolSorted)
{
	int numberPaths;


	if (!catalogHandle && !fortranUnit) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID, zdssErrorCodes.NULL_FILENAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "No handle or unit number provided");
	}

	if (zmessageLevel(ifltab, MESS_METHOD_CATALOG_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		if (catalogHandle) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_zcatalog_ID, "zcatalogFile, Catalog handle:  ", catalogHandle);
		}
		else {
			zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "zcatalogFile, No handle given ", "");
		}
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcatalog_ID, "Sort Catalog:  ", boolSorted);
		if (fortranUnit) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_zcatalog_ID, "zcatalogFile, Catalog unit:  ", fortranUnit);
		}
		else {
			zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "zcatalogFile, No unit given ", "");
		}
	}

	if (boolSorted) {
		numberPaths = zcatInternalSort (ifltab, (char *)0, (zStructCatalog *)0, catalogHandle, fortranUnit, 0);
	}
	else {
		numberPaths = zcatalogInternal (ifltab, (char *)0, (zStructCatalog *)0, catalogHandle, fortranUnit, 0, 0, 0);
	}
	return numberPaths;
}

