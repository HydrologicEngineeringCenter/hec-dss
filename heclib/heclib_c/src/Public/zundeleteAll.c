#include <string.h>

#include "heclib.h"

/**
*  Function:	zundeleteAll
*
*  Use:			Public
*
*  Description:	Undeletes all records previously deleted (before squeezing)
*
*  Declaration: int zundeleteAll(long long *ifltab;
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*	Note:		A deleted record cannot be undeleted in version 7 file that uses space reclamation.
*					If you really need the data, you can try turning reclamation off, then undelete.
*					If space has been reclaimed since deletion, then the data set may be bad.
*					A file squeeze will remove all deleted records (and cannot be undeleted after that).
*
*
*	See:	zdeleteInteral() and zundelete()  for more
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zundeleteAll(long long *ifltab)
{
	zStructCatalog *catStruct;
	int numberDeleted;
	int status;
	int i;


	if (zgetVersion(ifltab) == 6) {
		i = 0;
		zudall6_(ifltab, &i);
		return 0;
	}

	if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zundelete_ID, "Enter, Handle: ", zhandle(ifltab));
	}

	catStruct = zstructCatalogNew();
	if (!catStruct) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zundelete_ID,
						zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
						zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for catStruct");
	}

	catStruct->statusWanted = REC_STATUS_DELETED;
	numberDeleted = zcatalogInternal(ifltab, (char *)0, catStruct, 0, 0, 0, 0, 0);
	if (zisError(numberDeleted)) {
		zstructFree(catStruct);
		return zerrorUpdate(ifltab, numberDeleted, DSS_FUNCTION_zundelete_ID);
	}

	if (numberDeleted > 0) {
		for (i=0; i<catStruct->numberPathnames; i++) {
			//  zdeleteInternal also does undeletes
			status =  zdeleteInternal(ifltab, catStruct->pathnameList[i], 1);
			if (zisError(status)) {
				zstructFree(catStruct);
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_zundelete_ID);
			}
		}
		if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_zundelete_ID, "Number of records un-deleted: ", numberDeleted);
		}
	}
	else {
		if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_zundelete_ID, "No deleted records found in file, handle: ", zhandle(ifltab));
		}
	}
	zstructFree(catStruct);

	return numberDeleted;
}


int zudall_(long long *ifltab)
{
	return zundeleteAll(ifltab);
}

