#include <stdlib.h>

#include "zdssKeys.h"
#include "hecdss7.h"

/**
*  Function:	zwhatChanged
*
*  Use:			Public
*
*  Description:	Gets the pathnames for records that have changed since a previous point
*
*  Declaration: int zwhatChangedCompare(long long *ifltab, zStructCatalog *catStructBefore, zStructCatalog *catStructChanged,
*										 const char *pathWithWild, int boolUseCRC);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.*  Parameters:	const char *pathname1:  The first pathname to compare
*
*				zStructCatalog *catStructBefore:
*					A catalog struct made before an operation (e.g., compute) to compare it with.
*
*				zStructCatalog *catStructChanged:
*					An empty catalog struct that will contain the changed pathnames, etc.   Be sure to free
*					it after you are done.
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
*  Returns:		Number of records that have changed and a fill out catStructChanged.
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

int zwhatChangedCompare(long long *ifltab, zStructCatalog *catStructBefore, zStructCatalog *catStructChanged, const char *pathWithWild, int boolUseCRC)
{
	int status;
	int count;
	int i;
	int j;
	int *index;
	zStructCatalog *catStruct;


	if (zmessageLevel(ifltab, MESS_METHOD_CATALOG_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "Enter zwhatChangedCompare, path with wild: ", pathWithWild);
		if (pathWithWild) {
			zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "path with wild: ", pathWithWild);
		}
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcatalog_ID, "boolUseCRC: ", boolUseCRC);
		zmessageDebugLong(ifltab, DSS_FUNCTION_zcatalog_ID, "catStructBefore->lastWriteTimeFile: ", catStructBefore->lastWriteTimeFile);
	}

	if (!catStructBefore->crcValues) boolUseCRC = 0;

	if (!boolUseCRC) {
		catStructChanged->lastWriteTimeSearch = catStructBefore->lastWriteTimeFile;
		catStructChanged->lastWriteTimeSearchFlag = 2;
		status = zcatalog(ifltab, pathWithWild, catStructChanged, catStructBefore->boolSorted);
		if (status < 0) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcatalog_ID);
		}
		return status;
	}

	//  The following code always uses CRC values
	catStruct = zstructCatalogNew();
	if (!catStruct) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID,
						zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
						zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for catStruct");
	}

	catStruct->boolGetCRCvalues = boolUseCRC;
	catStruct->lastWriteTimeSearch = catStructBefore->lastWriteTimeFile;
	catStruct->lastWriteTimeSearchFlag = 2;
	catStruct->boolIncludeDates = catStructBefore->boolIncludeDates;
	catStruct->statusWanted = catStructBefore->statusWanted;
	catStruct->typeWantedStart = catStructBefore->typeWantedStart;
	catStruct->typeWantedEnd = catStructBefore->typeWantedEnd;
	status = zcatalog(ifltab, pathWithWild, catStruct, catStructBefore->boolSorted);
	if (status < 0) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcatalog_ID);
	}

	//  If a (new) record is in the file now, that is considered a change
	//  If a record is not (it's been deleted), that is not considered a change
	//  (It is, but generally is not what a user wants)
	if (catStruct->numberPathnames == 0) {
		catStructChanged->numberPathnames = 0;
		zstructFree(catStruct);
		return 0;
	}

	index = (int *)calloc((size_t)catStruct->numberPathnames, WORD_SIZE);
	if (!index) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID,
						zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
						zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for index");
	}

	count = 0;
	for (i=0; i<catStruct->numberPathnames; i++) {
		for (j=0; j<catStructBefore->numberPathnames; j++) {
			if (catStruct->pathnameHash[i] == catStructBefore->pathnameHash[j]) {
				//  We have an updated record.  Has the data changed?
				if (catStruct->crcValues[i] != catStructBefore->crcValues[j]) {
					//  Found One.  The CRC value has changed, so the data set has too.
					index[count++] = i;
				}
			}
		}
	}

	if (count == 0) {
		catStructChanged->numberPathnames = 0;
		zstructFree(catStruct);
		free(index);
		return 0;
	}

	//  We have our changed records - build catStructChanged
	catStructChanged->numberPathnames = count;
	catStructChanged->listSize = count;
	catStructChanged->structType = catStruct->structType;
	catStructChanged->statusWanted = catStruct->statusWanted;
	catStructChanged->typeWantedStart = catStruct->typeWantedStart;
	catStructChanged->typeWantedEnd = catStruct->typeWantedEnd;
	catStructChanged->boolSorted = catStruct->boolSorted;
	catStructChanged->boolIncludeDates = catStruct->boolIncludeDates;
	catStructChanged->lastWriteTimeFile = catStruct->lastWriteTimeFile;

	catStructChanged->pathnameList = (char **)calloc((size_t)catStructChanged->listSize, WORD_SIZE);
	if (!catStructChanged->pathnameList) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID,
						zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, catStructChanged->listSize, 0,
						zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for catStructChanged->pathnameList");
	}
	catStructChanged->allocated[zSTRUCT_pathname] = 1;
	if (catStructChanged->boolIncludeDates) {
		catStructChanged->startDates = (int *)calloc((size_t)catStructChanged->listSize, WORD_SIZE);
		catStructChanged->endDates   = (int *)calloc((size_t)catStructChanged->listSize, WORD_SIZE);
		if (!catStructChanged->startDates || !catStructChanged->endDates) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID,
							zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, catStructChanged->listSize, 0,
							zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for startDates / endDates");
		}
	}
	catStructChanged->recordType = (int*)calloc((size_t)catStructChanged->listSize, WORD_SIZE);
	if (!catStructChanged->recordType) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID,
			zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, catStructChanged->listSize, 0,
			zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for catStructChanged->recordType");
	}
	catStructChanged->pathnameHash = (long long*)calloc((size_t)catStructChanged->listSize, LONG_SIZE);
	if (!catStructChanged->pathnameHash) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID,
						zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, catStructChanged->listSize, 0,
						zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for catStructChanged->pathnameHash");
	}
	catStructChanged->lastWriteTimeRecord = (long long*)calloc((size_t)catStructChanged->listSize, LONG_SIZE);
	if (!catStructChanged->lastWriteTimeRecord) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID,
						zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, catStructChanged->listSize, 0,
						zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for catStructChanged->lastWriteTimeRecord");
	}
	catStructChanged->crcValues = (unsigned int*)calloc((size_t)catStructChanged->listSize, sizeof(unsigned int));
	if (!catStructChanged->crcValues) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID,
						zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, catStructChanged->listSize, 0,
						zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for catStructChanged->crcValues");
	}

	for (i=0; i<count; i++) {
		j = index[i];
		catStructChanged->pathnameList[i] = mallocAndCopy(catStruct->pathnameList[j]);
		catStructChanged->pathnameHash[i] = catStruct->pathnameHash[j];
		catStructChanged->lastWriteTimeRecord[i] = catStruct->lastWriteTimeRecord[j];
		catStructChanged->crcValues[i] = catStruct->crcValues[j];
		if (catStructChanged->startDates) {
			catStructChanged->startDates[i] = catStruct->startDates[j];
		}
		if (catStructChanged->endDates) {
			catStructChanged->endDates[i] = catStruct->endDates[j];
		}
	}

	zstructFree(catStruct);
	free(index);
	return count;
}

