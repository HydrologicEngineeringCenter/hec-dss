#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <stdio.h>
#ifndef _MSC_VER
#include <unistd.h>
#endif

#include "zdssKeys.h"
#include "zdssLocking.h"
#include "zdssVals.h"
#include "zprogress.h"
#include "zdssMessages.h"
#include "heclib.h"
#include "zerrorCodes.h"



/**
*  Function:	zcatInternalSort
*
*  Use:			Private
*
*  Description:	Obtains pathnames from zcatalogInternal and sorts
*
*  Declaration: int zcatInternalSort(long long *ifltab, const char *pathWithWild, zStructCatalog *sortedStruct,
*									 int catalogHandle, int fortranUnit, int boolCollection);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				const char *pathWithWildChars
*					Either null (for ignore) or a String that represents a pathname with wild characters represented
*					by a star (*) to match any string in the pathname part.  Wild characters can only be at the beginning or end of a part,
*					not inside of a string.  An example is a C part with "*Flow*" , which
*					will match all pathnames that have "Flow" anywhere in the C part, such as "Flow", "Inflow", "Outflow-Reg", etc.
*					A part such as "Flow*Reg" is not legal.  A null (//) will only match a null, where only a star (*) will match all.
*
*				zStructCatalog *catStruct
*					Either a struct created by function zstructCatalogNew(); or null.  This struct will hold a list of all
*					pathnames returned by this call.  See zStructCatalog.h for definition.  If null, then pathnames are
*					expected to be written to a file using either the catalogHandle or fortranUnit.
*					Be sure to call function zstructFree(struct) when finished.
*
*				int catalogHandle
*					A C handle (file descriptor) connected to a file opened by the calling function, usually with sopen or similar.
*					If the pathnames are not to be written to a file, then this should be zero.
*
*				int fortranUnit
*					A Fortran unit number connected to a file opened by the calling function.
*					If the pathnames are not to be written to a file, then this should be zero.
*
*				int boolCollection
*					Set this to 1 if you want a collection set, and the wild path is a collection path
*					Otherwise, set to 0 for non-collection
*
*
*
*	Returns:	int numberCataloged
*					Number of pathnames in the file (a positive number)
*					or, if a negative number, then errorCode for error
*					errorCode may be decoded by function zerrorDecode()
*
*	Remarks:	For both DSS Version 6 and DSS Version 7 files.
*
*
*	See Also:	zcatalog(), zcatalogInternal()
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int zcatInternalSort(long long *ifltab, const char *pathWithWild, zStructCatalog *sortedStruct,
					 int catalogHandle, int fortranUnit, int boolCollection)
{
	int i;
	int j;
	int sequence;
	int unsortedHandle;
	int numberPathnames;
	int status;
	int sortListStatus;
	int zero;
	int one;
	int icount;
	long long* sortAddresses;
	long long *fileHeader;
	char cline[400];
	char *tempUnsorted;
	char *tempSorted;
	zStructCatalog *nonSortedStruct=0;
	FILE *sortedFP;


	nonSortedStruct = zstructCatalogNew();
	if (!nonSortedStruct) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
								zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for nonSortedStruct");
	}

	//  Get current status of sort list stored in DSS file
		//  0 = None (no sort list available)
		//  1 = List fully sorted and ready to use (without further sorting)
		//  2 = Updates have been made and catalog will have to be further sorted
		//  3 = Significant changes and sort list should not be used - do a full re-sort.
	if (zgetVersion(ifltab) == 6) {
		sortListStatus = 0;
		if (sortedStruct) {
			sortedStruct->boolIncludeDates = 0;
		}
	}
	else {
		fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
		if ((pathWithWild && ((int)strlen(pathWithWild) > 1)) || (boolCollection)) {
			sortListStatus = 0;
		}
		else {
			sortListStatus = (int)fileHeader[zdssFileKeys.kcatSortStatus];
		}
	}

	if (sortedStruct) {
		nonSortedStruct->boolIncludeDates = sortedStruct->boolIncludeDates;
		nonSortedStruct->statusWanted = sortedStruct->statusWanted;
		nonSortedStruct->typeWantedStart = sortedStruct->typeWantedStart;
		nonSortedStruct->typeWantedEnd = sortedStruct->typeWantedEnd;
	}
	sortAddresses = 0;

	//  In this function, we will treat 0, 2 and 3 the same - do a full resort
	//  (status 2 is used by Java)
	//  If status == 1, then we do not do a sort, but convert the nonSortedStruct into the sorted struct
	//  by using the sort list.

	if (sortListStatus == 1) {
		//  Catalog has full sort list
		//  Get unsorted struct, sort list, then reorder for user
		numberPathnames = zcatalogInternal (ifltab, pathWithWild, nonSortedStruct, 0, 0, 0, 0, 0);
		if (numberPathnames <= 0) {
			if (nonSortedStruct) zstructFree(nonSortedStruct);
			nonSortedStruct = 0;
			return zerrorUpdate(ifltab, numberPathnames, DSS_FUNCTION_zcatalog_ID);
		}
		//  DSS-7 code only
		sortAddresses = (long long*)calloc((size_t)numberPathnames, LONG_SIZE);
		if (!sortAddresses) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID,
									zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, numberPathnames, 0,
									zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for sortAddresses");
		}
		status = zgetCatalogSortAddresses(ifltab, sortAddresses, numberPathnames);
		if (status < 0) {
			//  Very rare
			sortListStatus = 0;
			free(sortAddresses);
			sortAddresses = 0;
		}
		else {
			if ((catalogHandle > 0) || (fortranUnit > 0)) {
				icount = 0;
				for (i=0; i<nonSortedStruct->numberPathnames; i++) {
					for (j=0; j<numberPathnames; j++) {
						if (sortAddresses[i] == nonSortedStruct->sortAddresses[j]) {
							if (catalogHandle) {
								status = writeBytes(catalogHandle, nonSortedStruct->pathnameList[j], (unsigned int)strlen(nonSortedStruct->pathnameList[j]));
								if (status < 0) {
									perror("Attempting to write pathname ");
									if (nonSortedStruct) zstructFree(nonSortedStruct);
									nonSortedStruct = 0;
									return status;
								}
								status = writeBytes(catalogHandle, "\n", (size_t)1);
							}
							icount++;
							break;
						}
					}
				}
			}
			if (sortedStruct) {
				sortedStruct->numberPathnames = nonSortedStruct->numberPathnames;
				sortedStruct->listSize = nonSortedStruct->numberPathnames;
				sortedStruct->pathnameList = (char **)calloc((size_t)sortedStruct->listSize, WORD_SIZE);
				if (!sortedStruct->pathnameList) {
					return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID,
									zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, sortedStruct->listSize, 0,
									zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for sortedStruct->pathnameList");
				}
				sortedStruct->allocated[zSTRUCT_pathname] = 1;
				if (sortedStruct->boolIncludeDates) {
					sortedStruct->startDates = (int *)calloc((size_t)sortedStruct->listSize, WORD_SIZE);
					sortedStruct->endDates   = (int *)calloc((size_t)sortedStruct->listSize, WORD_SIZE);
					if (!sortedStruct->startDates || !sortedStruct->endDates) {
						return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID,
										zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, sortedStruct->listSize, 0,
										zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for startDates / endDates");
					}
				}
				icount = 0;
				for (i=0; i<nonSortedStruct->numberPathnames; i++) {
					for (j=0; j<numberPathnames; j++) {
						if (sortAddresses[i] == nonSortedStruct->sortAddresses[j]) {
							sortedStruct->pathnameList[icount] = nonSortedStruct->pathnameList[j];
							nonSortedStruct->pathnameList[j] = 0;
							if (sortedStruct->boolIncludeDates) {
								sortedStruct->startDates[icount] = nonSortedStruct->startDates[j];
								sortedStruct->endDates[icount]   = nonSortedStruct->endDates[j];
							}
							icount++;
							break;
						}
					}
				}
			}
			if (sortAddresses) {
				free(sortAddresses);
				sortAddresses = 0;
			}
			if (nonSortedStruct) zstructFree(nonSortedStruct);
			nonSortedStruct = 0;
			return numberPathnames;
		}
	}


	//  Need to do a full sort.  Use temp disk files and sort those
	//  open temp files
#ifdef _MSC_VER
	tempUnsorted = _tempnam( "", "cat_unsorted" );
	tempSorted = _tempnam("", "cat_sorted");
#else
	tempUnsorted=(char*)malloc(32*sizeof(char));
	memset( tempUnsorted, 0, sizeof(tempUnsorted));
	strcpy( tempUnsorted, "/tmp/cat_unsorted-XXXXXX");
	int fileId = mkstemp(tempUnsorted);
	if (fileId<1)
	{
		if (nonSortedStruct) zstructFree(nonSortedStruct);
		nonSortedStruct = 0;
		free(tempUnsorted);
		return -1;
	}
	tempSorted=(char*)malloc(32*sizeof(char));
	memset( tempSorted, 0, sizeof(tempSorted));
	strcpy( tempSorted, "/tmp/cat_sorted-XXXXXX");
	fileId = mkstemp(tempSorted);
	if (fileId<1)
	{
		if (nonSortedStruct) zstructFree(nonSortedStruct);
		nonSortedStruct = 0;
		free(tempSorted);
		return -1;
	}
#endif
	if (!tempUnsorted) {
		if (nonSortedStruct) zstructFree(nonSortedStruct);
		nonSortedStruct = 0;
		return -1;
	}
	if (!tempSorted) {
		free(tempUnsorted);
		if (nonSortedStruct) zstructFree(nonSortedStruct);
		nonSortedStruct = 0;
		return -1;
	}
	//printf("%s\n",tempUnsorted);
	//printf("%s\n",tempSorted);

	status = zopenDisk(tempUnsorted, &unsortedHandle, 10, 1);
	if (unsortedHandle < 1) {
		free(tempUnsorted);
		free(tempSorted);
		if (nonSortedStruct) zstructFree(nonSortedStruct);
		nonSortedStruct = 0;
		return unsortedHandle;
	}
	//  For both DSS-6 and DSS-7
	numberPathnames = zcatalogInternal (ifltab, pathWithWild, nonSortedStruct, unsortedHandle, 0, 0, boolCollection, 1);
	if (numberPathnames < 0) {
		return numberPathnames;
	}
	closeFile(unsortedHandle);
	unsortedHandle = 0;
	if (numberPathnames < 0) {
#ifdef _MSC_VER
		_unlink(tempUnsorted);
#else
		unlink(tempUnsorted);
#endif
		free(tempUnsorted);
		free(tempSorted);
		if (nonSortedStruct) zstructFree(nonSortedStruct);
		nonSortedStruct = 0;
		return status;
	}

	status = sortfiles (tempUnsorted, tempSorted);

#ifdef _MSC_VER
	_unlink(tempUnsorted);
	free(tempUnsorted);
	if (status < 0) {
		_unlink(tempSorted);
		free(tempSorted);
		if (nonSortedStruct) zstructFree(nonSortedStruct);
		nonSortedStruct = 0;
		return status;
	}
#else
	unlink(tempUnsorted);
	free(tempUnsorted);
	if (status < 0) {
		unlink(tempSorted);
		free(tempSorted);
		if (nonSortedStruct) zstructFree(nonSortedStruct);
		nonSortedStruct = 0;
		return status;
	}
#endif


	if (sortedStruct) {
		sortedStruct->numberPathnames = numberPathnames;
		sortedStruct->listSize = numberPathnames;
		sortedStruct->pathnameList = (char **)calloc((size_t)sortedStruct->listSize, WORD_SIZE);
		if (!sortedStruct->pathnameList) {
				return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID,
							zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, sortedStruct->listSize, 0,
							zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for sortedStruct->pathnameList");
		}
		sortedStruct->allocated[zSTRUCT_pathname] = 1;
		if ((sortedStruct->boolIncludeDates) && (nonSortedStruct->startDates)) {
			sortedStruct->startDates = (int *)calloc((size_t)sortedStruct->listSize, WORD_SIZE);
			sortedStruct->endDates   = (int *)calloc((size_t)sortedStruct->listSize, WORD_SIZE);
			if (!sortedStruct->startDates || !sortedStruct->endDates) {
				return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, sortedStruct->listSize, 0,
								zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for startDates / endDates");
			}
		}
		if (zgetVersion(ifltab) == 7) {
			if ((numberPathnames == (int)fileHeader[zdssFileKeys.knumberRecords]) && (nonSortedStruct->sortAddresses)){
				sortedStruct->sortAddresses = (long long*)calloc((size_t)sortedStruct->listSize, LONG_SIZE);
				if (!sortedStruct->sortAddresses) {
					return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID,
									zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, sortedStruct->listSize, 0,
									zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for sortedStruct->sortAddresses");
				}
			}
		}
		else {
			sortedStruct->sortAddresses = 0;
		}
	}
#ifdef _MSC_VER
	status = fopen_s(&sortedFP, tempSorted, "r");
	if (status != 0) {
		_unlink(tempSorted);
		free(tempSorted);
		if (nonSortedStruct) zstructFree(nonSortedStruct);
		nonSortedStruct = 0;
		return -1;
	}
#else
	sortedFP = fopen(tempSorted, "r");
	if (!sortedFP) {
		unlink(tempSorted);
		free(tempSorted);
		if (nonSortedStruct) zstructFree(nonSortedStruct);
		nonSortedStruct = 0;
		return -1;
	}
#endif

	for (i=0; i<numberPathnames; i++) {
		if (!fgets(cline, sizeof(cline), sortedFP)) {
			fclose(sortedFP);
#ifdef _MSC_VER
			_unlink(tempSorted);
#else
			unlink(tempSorted);
#endif
			free(tempSorted);
			if (nonSortedStruct) zstructFree(nonSortedStruct);
			nonSortedStruct = 0;
			return -1;
		}
		sequence = getLastInt(cline);
		if (sequence > -1) {
			if (catalogHandle) {
				status = writeBytes(catalogHandle, nonSortedStruct->pathnameList[sequence], (unsigned int)strlen(nonSortedStruct->pathnameList[sequence]));
				if (status < 0) {
					perror("Attempting to write pathname ");
					if (nonSortedStruct) zstructFree(nonSortedStruct);
					nonSortedStruct = 0;
					return status;
				}
				status = writeBytes(catalogHandle, "\n", (size_t)1);
			}

			if (sortedStruct) {
				sortedStruct->pathnameList[i] = mallocAndCopy(nonSortedStruct->pathnameList[sequence]);
				if (!sortedStruct->pathnameList[i]) {
					return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID,
									zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
									zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for pathname");
				}
				if ((sortedStruct->startDates) && (sortedStruct->endDates) &&
					(nonSortedStruct->startDates) && (nonSortedStruct->endDates)) {
					sortedStruct->startDates[i] = nonSortedStruct->startDates[sequence];
					sortedStruct->endDates[i]   = nonSortedStruct->endDates[sequence];
				}
				if (sortedStruct->attributes) {}
				if (sortedStruct->sortAddresses && nonSortedStruct->sortAddresses) {
					sortedStruct->sortAddresses[i] = nonSortedStruct->sortAddresses[sequence];
				}
			}
		}
	}

	fclose(sortedFP);
#ifdef _MSC_VER
	_unlink(tempSorted);
#else
	unlink(tempSorted);
#endif
	free(tempSorted);
	if ((zgetVersion(ifltab) == 7) && sortedStruct && sortedStruct->sortAddresses){
		if ((sortedStruct->sortAddresses) && (sortedStruct->numberPathnames == fileHeader[zdssFileKeys.knumberRecords])){
			zsetCatalogSortAddresses(ifltab, sortedStruct->sortAddresses, sortedStruct->numberPathnames);
		}
	}

	if (nonSortedStruct) zstructFree(nonSortedStruct);
	nonSortedStruct = 0;
	return numberPathnames;
}
