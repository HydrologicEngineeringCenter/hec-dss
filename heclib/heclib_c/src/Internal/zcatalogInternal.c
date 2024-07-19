#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <stdio.h>

#include "zdssKeys.h"
#include "zdssVals.h"
#include "zprogress.h"
#include "zdssMessages.h"
#include "heclib.h"
#include "zerrorCodes.h"


/**
*  Function:	zcatalogInternal
*
*  Use:			Private - use zcatalog instead
*
*  Description:	Searches the DSS file for pathnames to catalog, as per input parameters
*
*  Declaration: int zcatalogInternal (long long *ifltab, const char *pathWithWild, zStructCatalog *catStruct,
*					  int catalogHandle, int ifortUnit, int numberWanted, int boolCollection, int boolForSort);
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
*				int numberWanted
*					The number of pathnames to limit the list to.  To retrieve all pathnames, set this to 0.
*					Sometimes only the first matching pathname is wanted, then this would be set to 1 (or however many).
*
*				int boolCollection
*					To search for all pathnames in a collection, pass one of the collections path in pathWithWild
*					and set this to 1.  You cannot directly search for both wild and collections (because pathWithWild
*					has to be a valid seed.  You can accomplish a similar search by passing a wild in the F part,
*					e.g. .../*|Run A/
*
*				int boolForSort
*					An int boolean flag set to "1" to indicate that this list is in preparation for sorting.
*					Sorting can take considerable more resources and time
*
*
*	Returns:	int numberCataloged
*					Number of pathnames in the file (a positive number)
*					or, if a negative number, then errorCode for error
*					errorCode may be decoded by function zerrorDecode()
*
*	Remarks:	This calls the appropriate DSS version 6 function when an ifltab for a version 6 file is passed in.
*
*
*	See Also:	zcatalog()
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zcatalogInternal (long long *ifltab, const char *pathWithWild, zStructCatalog *catStruct,
					  int catalogHandle, int ifortUnit, int numberWanted, int boolCollection, int boolForSort)
{

	int icount;
	int jcount;
	int countRecs;
	int countAdded;
	int countTotal;
	int status;
	int binSize;
	int numberChars;
	int julianFirst;
	int julianLast;
	int boolFound;
	int pathnameSize;
	int dataType;
	int catSort;
	int one;
	int i;
	int statusWanted;
	int partMax[6];

	char apart[MAX_PART_SIZE];
	char fpart[MAX_PART_SIZE];
	char bpart[MAX_PART_SIZE];
	char cpart[MAX_PART_SIZE];
	char dpart[MAX_PART_SIZE];
	char epart[MAX_PART_SIZE];
	int partAction[6];
	int lengths[6];
	int boolCompareWild;
	
	long long *info;
	char cbuff[60];
	long long binAddress;
	long long *pathnameBin;
	long long *fileHeader;
	char messageString[80];
	char dateString[40];
	char timeString[20];

	char *pathnameMalloc;
	char pathname[MAX_PATHNAME_LENGTH];



	if (zmessageLevel(ifltab, MESS_METHOD_CATALOG_ID, MESS_LEVEL_USER_DIAG)) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, ZCATALOG_NEW, zhandle(ifltab));
		zmessage(ifltab, messageString);
	}

	if (zmessageLevel(ifltab, MESS_METHOD_CATALOG_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcatalog_ID, "Catalog C Handle:  ", catalogHandle);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcatalog_ID, "Fortran Unit:  ", ifortUnit);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcatalog_ID, "Retrieve Collection:  ", boolCollection);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcatalog_ID, "Sort Catalog:  ", boolForSort);
		if (pathWithWild) {
			zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "Pathname with wild chars:  ", pathWithWild);
		}
		else {
			zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "No Pathname with wild chars:  ", "");
		}
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcatalog_ID, "lastWriteTimeSearchFlag: ", catStruct->lastWriteTimeSearchFlag);
		zmessageDebugLong(ifltab, DSS_FUNCTION_zcatalog_ID, "lastWriteTimeSearch:  ", catStruct->lastWriteTimeSearch);
	}

	//  Check for correct DSS Version
	if (zgetVersion(ifltab) < 0) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID, zdssErrorCodes.NOT_OPENED,
										0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "");
	}
	if (zgetVersion(ifltab) != 7) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
								zgetVersion((void *)ifltab), 0, zdssErrorSeverity.WARNING, "", "");
	}

	status = zcheckKeys(ifltab);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcatalog_ID);
	}

	//  Lock file to keep from being updated while cataloging
	//zlockActive(ifltab, LOCKING_LEVEL_HIGH, LOCKING_LOCK_ON, LOCKING_FLUSH_ON);
	status = zpermRead(ifltab);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcatalog_ID);
	}

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	binSize = (int)fileHeader[zdssFileKeys.kbinSize];
	binAddress = fileHeader[zdssFileKeys.kaddFirstBin];
	pathnameBin = (long long *)ifltab[zdssKeys.kpathBin];


	if (catStruct) {
		catStruct->lastWriteTimeFile = fileHeader[zdssFileKeys.klastWriteTime];
		statusWanted = catStruct->statusWanted;
		catStruct->listSize = (int)fileHeader[zdssFileKeys.knumberRecords]	+ (int)fileHeader[zdssFileKeys.knumberAliases]
				+ (int)fileHeader[zdssFileKeys.knumberRenames] + (int)fileHeader[zdssFileKeys.knumberDeletes]
				+ (int)fileHeader[zdssFileKeys.knumberAliasDeletes];
		if ((numberWanted > 0) && (numberWanted < catStruct->listSize)) {
			catStruct->listSize = numberWanted;
		}
		catStruct->pathnameList = (char **)calloc((size_t)catStruct->listSize, WORD_SIZE);
		if (!catStruct->pathnameList) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID,
							zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, catStruct->listSize, 0,
							zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for catStruct->pathnameList");
		}
		catStruct->allocated[zSTRUCT_pathname] = 1;
		catStruct->sortAddresses = (long long *)calloc((size_t)catStruct->listSize, LONG_SIZE);
		if (!catStruct->sortAddresses) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID,
							zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, catStruct->listSize, 0,
							zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for catStruct->sortAddresses");
		}
		if (catStruct->boolIncludeDates) {
			catStruct->startDates = (int *)calloc((size_t)catStruct->listSize, WORD_SIZE);
			catStruct->endDates   = (int *)calloc((size_t)catStruct->listSize, WORD_SIZE);
			if (!catStruct->startDates || !catStruct->endDates) {
				return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, catStruct->listSize, 0,
								zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for startDates / endDates");
			}
		}
		catStruct->recordType = (int*)calloc((size_t)catStruct->listSize, WORD_SIZE);
		if (!catStruct->recordType) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID,
				zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, catStruct->listSize, 0,
				zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for catStruct->recordType");
		}
		catStruct->pathnameHash = (long long*)calloc((size_t)catStruct->listSize, LONG_SIZE);
		if (!catStruct->pathnameHash) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID,
							zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, catStruct->listSize, 0,
							zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for catStruct->pathnameHash");
		}
		catStruct->lastWriteTimeRecord = (long long*)calloc((size_t)catStruct->listSize, LONG_SIZE);
		if (!catStruct->lastWriteTimeRecord) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID,
							zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, catStruct->listSize, 0,
							zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for catStruct->lastWriteTimeRecord");
		}
		if (pathWithWild) {
			catStruct->pathWithWildChars = mallocAndCopy(pathWithWild);
		}
		catStruct->boolIsCollection = boolCollection;
	}
	else {
		statusWanted = 0;
	}

	 if (pathWithWild) {
		 boolCompareWild = zcatParsePath(pathWithWild, partAction, lengths,
			 apart, sizeof(apart),
			 bpart, sizeof(bpart),
			 cpart, sizeof(cpart),
			 dpart, sizeof(dpart),
			 epart, sizeof(epart),
			 fpart, sizeof(fpart));
	 }
	 else {
		 boolCompareWild = 0;
	 }

	 //  Save the maximum length of each part for use in sorting
	partMax[0] = (int)fileHeader[zdssFileKeys.kmaxA];
	partMax[1] = (int)fileHeader[zdssFileKeys.kmaxB];
	partMax[2] = (int)fileHeader[zdssFileKeys.kmaxC];
	partMax[3] = (int)fileHeader[zdssFileKeys.kmaxF];
	partMax[4] = (int)fileHeader[zdssFileKeys.kmaxE];
	partMax[5] = (int)fileHeader[zdssFileKeys.kmaxD];

	countRecs = 0;
	countTotal = 0;
	countAdded = (int)fileHeader[zdssFileKeys.knumberRecords] + (int)fileHeader[zdssFileKeys.knumberAliases] -1;
	zresetProgress(zhandle(ifltab), countAdded);

	while (1) {
		jcount = 0;
		while (jcount < (int)fileHeader[zdssFileKeys.kbinsPerBlock]) {
			status = zget(ifltab, binAddress, (int *)pathnameBin, binSize, 2);
			if (zisError(status)) {
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcatalog_ID);
			}
			if (zmessageLevel(ifltab, MESS_METHOD_CATALOG_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
				_snprintf_s(cbuff, sizeof(cbuff), _TRUNCATE, "%lld, first value %lld", binAddress, pathnameBin[0]);
				zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "Read bin at address: ", cbuff);
			}

			icount = 0;
			if (pathnameBin[icount+zdssBinKeys.kbinHash] == 0) {
				//  No more, didn't find it in this block, continue on
					break;
			}
			while (1) {
				if (pathnameBin[icount+zdssBinKeys.kbinHash] == 0) {
					//  No more, didn't find it in this block, continue on
					break;
				}
				i8toi4(pathnameBin[icount+zdssBinKeys.kbinPathLen], &numberChars, &pathnameSize);
				// charLong(void *from, void *to, int numberBytes, int maxBytesTo, int boolToLong, int zeroEndFlag)
				charLong(&pathnameBin[icount + zdssBinKeys.kbinPath], pathname, numberChars, sizeof(pathname), 0, 0);
				if (zmessageLevel(ifltab, MESS_METHOD_CATALOG_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {					
					zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "Found pathname: ", pathname);					
				}
				if (zcompareRecordStatus((int)pathnameBin[icount+zdssBinKeys.kbinStatus], statusWanted)) {
					i8toi4(pathnameBin[icount+zdssBinKeys.kbinTypeAndCatSort], &dataType, &catSort);
					if ((boolCollection && !boolCompareWild) && pathWithWild) {
						boolFound = zpathnameCompareCollection(pathname, pathWithWild, (size_t)numberChars);
					}
					else if (boolCompareWild) {
						boolFound = zcatComparePath(pathname, partAction, lengths,
							apart, bpart, cpart, dpart, epart, fpart, boolCollection);
					}
					else {
						//  Found one!
						boolFound = 1;
					} /*
					if (boolCollection || boolCompareWild) {
						boolFound = zcatComparePath(pathname, partAction, lengths,
							apart, bpart, cpart, dpart, epart, fpart, boolCollection);
					}
					else {
						//  Found one!
						boolFound = 1;
					} */
					if (catStruct && boolFound) {
						if ((catStruct->typeWantedStart == 0) || ((dataType >= catStruct->typeWantedStart) && (dataType <= catStruct->typeWantedEnd))) {
							if (catStruct->lastWriteTimeSearch > 0) {
								boolFound = 0;
								if (catStruct->lastWriteTimeSearchFlag == -2) {
									if (pathnameBin[icount+zdssBinKeys.kbinLastWrite] < catStruct->lastWriteTimeSearch) boolFound = 1;
								}
								else if (catStruct->lastWriteTimeSearchFlag == -1) {
									if (pathnameBin[icount+zdssBinKeys.kbinLastWrite] <= catStruct->lastWriteTimeSearch) boolFound = 1;
								}
								else if (catStruct->lastWriteTimeSearchFlag == 0) {
									if (pathnameBin[icount+zdssBinKeys.kbinLastWrite] == catStruct->lastWriteTimeSearch) boolFound = 1;
								}
								else if (catStruct->lastWriteTimeSearchFlag == 1) {
									if (pathnameBin[icount+zdssBinKeys.kbinLastWrite] >= catStruct->lastWriteTimeSearch) boolFound = 1;
								}
								else if (catStruct->lastWriteTimeSearchFlag == 2) {
									if (pathnameBin[icount+zdssBinKeys.kbinLastWrite] > catStruct->lastWriteTimeSearch) boolFound = 1;
								}
								if (boolFound && zmessageLevel(ifltab, MESS_METHOD_CATALOG_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
									zmessageDebugInt(ifltab, DSS_FUNCTION_zcatalog_ID, "Found path for  lastWriteTimeSearchFlag = ", catStruct->lastWriteTimeSearchFlag);
									zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "Pathname: ", pathname);
									zmessageDebugLong(ifltab, DSS_FUNCTION_zcatalog_ID, "lastWriteTimeSearch = ", catStruct->lastWriteTimeSearch);
									zmessageDebugLong(ifltab, DSS_FUNCTION_zcatalog_ID, "pathnameBin kbinLastWrite = ", pathnameBin[icount + zdssBinKeys.kbinLastWrite]);
									millsToDateTime(catStruct->lastWriteTimeSearch, dateString, timeString, sizeof(dateString), sizeof(timeString));
									zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "lastWriteTimeSearch date: ", dateString);
									zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "lastWriteTimeSearch time: ", timeString);
									millsToDateTime(pathnameBin[icount + zdssBinKeys.kbinLastWrite], dateString, timeString, sizeof(dateString), sizeof(timeString));
									zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "kbinLastWrite date: ", dateString);
									zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "kbinLastWrite time: ", timeString);
								}
							}
						}
					}
					//  If it is a deleted record, be sure the info area is still there
					if (boolFound && (statusWanted == REC_STATUS_DELETED) && (ifltab[zdssKeys.kreclaimLevel] > RECLAIM_NONE)) {
						status = zget(ifltab, pathnameBin[icount+zdssBinKeys.kbinInfoAdd], (int *)ifltab[zdssKeys.kinfo], 10, 2);
						if (zisError(status)) {
							return zerrorUpdate(ifltab, status, DSS_FUNCTION_zcatalog_ID);
						}
						info = (long long *)ifltab[zdssKeys.kinfo];
						if (info[zdssInfoKeys.kinfoStatus] != REC_STATUS_DELETED) {
							boolFound = 0;
						}
					}

					if (boolFound) {
						if (zmessageLevel(ifltab, MESS_METHOD_CATALOG_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
							zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "Pathname passes requests ", "");
						}
						if (catStruct) {
							if ((catStruct->listSize > 0) && (countTotal >= catStruct->listSize)) {
								//  Not enough space!
								//  This is okay, as someone might be writing new pathnames while we are cataloging....
								/*status = zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID,
												zdssErrorCodes.ARRAY_SPACE_EXHAUSTED, catStruct->listSize, (long long)countTotal,
												zdssErrorSeverity.MEMORY_ERROR, "", "Catalog array length");  */
								break;
							}
						}
						//  If the number of characters in the pathname is not a multiple
						//  of 8, then it is guaranteed to be zero terminated
						//  If a multiple of 8, it will not be zero terminated
						//  len = pathnameSize * 8;
						if (catStruct && (catStruct->listSize > 0)) {
							pathnameMalloc = stringFortToC(pathname,  (size_t)numberChars);
							if (pathnameMalloc == NULL) {
								//  Memory error
								status = zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID,
												zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, numberChars, 0,
												zdssErrorSeverity.MEMORY_ERROR, "", "Allocating String for pathname");
								return status;
							}
							catStruct->pathnameList[countTotal] = pathnameMalloc;
							catStruct->sortAddresses[countTotal] = binAddress + icount + zdssBinKeys.kbinHash;
							if (catStruct->startDates && catStruct->endDates) {
								if ((dataType >= DATA_TYPE_RTS) && (dataType < DATA_TYPE_PD)) {
									i8toi4 (pathnameBin[icount+zdssBinKeys.kbinDates], &julianFirst, &julianLast);
								}
								else {
									julianFirst = 0;
									julianLast = 0;
								}
								catStruct->startDates[countTotal] = julianFirst;
								catStruct->endDates[countTotal] = julianLast;
							}
							catStruct->recordType[countTotal] = dataType;
							catStruct->pathnameHash[countTotal] = pathnameBin[icount+zdssBinKeys.kbinHash];
							catStruct->lastWriteTimeRecord[countTotal] = pathnameBin[icount+zdssBinKeys.kbinLastWrite];
						}
						if (catalogHandle > 0) {
							if (boolForSort) {
								status = zcatSortPath(catalogHandle, pathname, (size_t)numberChars, dataType, partMax, countTotal);
							}
							else {
								status = writeBytes(catalogHandle, pathname, (size_t)numberChars);
							}
							if (status < 0) {
								perror("Attempting to write pathname ");
							}
							status = writeBytes(catalogHandle, "\n", (size_t)1);
						}
						if (zmessageLevel(ifltab, MESS_METHOD_CATALOG_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
							zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "Pathname: ", pathname);
							_snprintf_s(cbuff, sizeof(cbuff), _TRUNCATE, " %d, added count: %d", countTotal, countAdded);
							zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "total count: ", cbuff);
						}
						countTotal++;
						zprogress.currentNumber = countTotal;
						zprogress.handle = zhandle(ifltab);
						if ((numberWanted > 0) && (countTotal >= numberWanted)) {
							break;
						}
						if (status < 0) {
							break;
						}
						if (catStruct) {
							if (countTotal > catStruct->listSize) {
								countTotal--;
								break;
							}
						}
					}
					if (status < 0)
						break;
				}
				else {
					if (zmessageLevel(ifltab, MESS_METHOD_CATALOG_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
						zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "Deleted pathname: ", pathname);
					}
				}
				icount += zdssBinKeys.kbinSize + pathnameSize;
				if (icount >= (binSize-2)) {
					break;
				}
				if (status < 0) {
					countTotal = status;
					break;
				}
			}
			if (status < 0) {
				countTotal = status;
				break;
			}
			if ((numberWanted > 0) && (countTotal >= numberWanted)) {
				break;
			}
			binAddress += binSize;
			jcount++;
		}
		if ((numberWanted > 0) && (countTotal >= numberWanted)) {
			break;
		}
		if (status < 0) {
			countTotal = status;
			break;
		}
		status = zget(ifltab, binAddress, (int *)pathnameBin, 1, 2);
		if (status != 0) {
			countTotal = status;
			break;
		}
		binAddress = pathnameBin[0];
		if (binAddress == 0) {
			break;
		}

	}

	//  Does the user want CRC values?
	//  CAUTION - Resource Intensive
	if ((catStruct) && (countTotal > 0)) {
		if (catStruct->boolGetCRCvalues) {
			catStruct->crcValues = (unsigned int*)calloc((size_t)countTotal, sizeof(unsigned int));
			if (!catStruct->crcValues) {
				return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, catStruct->listSize, 0,
								zdssErrorSeverity.MEMORY_ERROR, "", "Allocating space for catStruct->crcValues");
			}
			for (i=0; i<countTotal; i++) {
				catStruct->crcValues[i] =  zgetDataCRC(ifltab, catStruct->pathnameList[i], 0);
			}
		}
	}

	//zlockActive(ifltab, LOCKING_LEVEL_HIGH, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);

	if (zmessageLevel(ifltab, MESS_METHOD_CATALOG_ID, MESS_LEVEL_USER_DIAG)) {
		_snprintf_s(cbuff, sizeof(cbuff), _TRUNCATE, "%d", countTotal);
		zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "Exit; total number: ", cbuff);
	}
	if (catStruct) {
		catStruct->numberPathnames = countTotal;
		catStruct->listSize = countTotal;
	}
    return countTotal;
}
