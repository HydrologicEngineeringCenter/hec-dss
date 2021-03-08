#include <string.h>
#include <stdlib.h>
#include <stdio.h>


#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "zStructTsTimeWindow.h"
#include "heclib.h"
#include "hecdss7.h"

/**
*  Function:	ztsGetFirstLastPathnames
*
*  Use:			Semi-Public (use ztsGetDateRange instead)
*
*  Description:	Finds the first and last pathnames in a time series data set from a seed pathname.
*				For example, if you have a path like "/A/B/C/01JAN1988/1DAY/F/", or "/A/B/C//1DAY/F/"
*				this will find the earliest and latest pathnames for that dataset,
*				(For example "/A/B/C/01JAN1932/1DAY/F/" and "/A/B/C/01JAN2012/1DAY/F/")
*
*  Declaration: int ztsGetFirstLastPathnames(long long *ifltab, const char *pathnameSeed,
*											char *firstPath, size_t sizeofFirstPath,
*											char *lastPath, size_t sizeofLastPath);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				const char *pathnameSeed
*					One pathname from the dataset.  Although the pathname doesn't have to have a D (date) part,
*					it is much faster to provide a vaild D part.  (zcheck calls can be used with a valid D part)
*
*				char *firstPath (output)
*					Returns the earliest pathname in the dataset
*
*				size_t sizeofFirstPath
*					The size of firstPath (recommended MAX_PATHNAME_LENGTH)
*
*				char *lastPath (output)
*					Returns the latest pathname in the dataset
*
*				size_t sizeofLastPath
*					The size of lastPath (recommended MAX_PATHNAME_LENGTH)
*
*
*
*	Returns:	int status
*					STATUS_RECORD_FOUND (0) for valid time series pathname
*					STATUS_RECORD_NOT_FOUND (1) Cannot find the dataset
*					< 0 for error
*
*	Remarks:		Generally function ztsGetDateRange is intended to be used instead.
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int ztsGetFirstLastPathnames(long long *ifltab, const char *pathnameSeed,
							char *firstPath, size_t sizeofFirstPath,
							char *lastPath, size_t sizeofLastPath)
{
	int i;
	int status;
	int boolFoundValid;
	int number;
	int intervalSeconds;
	int julian;
	int blockSize;
	int version;
	int boolIrreg;
	int flag;
	int originalJulian;
	int firstJulian;
	int lastJulian;
	int blockJulian= 0;
	char ePart[MAX_PART_SIZE];
	char dPart[MAX_PART_SIZE];
	char path[MAX_PATHNAME_LENGTH];

	ztsTimeWindow *timeWindow;
	zStructCatalog *tsPaths;

	int numberNotFound;
	//int searchRange = 20;
	int searchRange = 0;



	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_other_ID, "Enter ztsGetFirstLastPathnames, Pathname: ",pathnameSeed);
	}
	if (zgetVersion(ifltab) == 6) {
		stringFill (firstPath, ' ', sizeofFirstPath);
		stringFill (lastPath, ' ', sizeofLastPath);
		ztsrange_(ifltab, pathnameSeed, &searchRange, firstPath, lastPath, &number,
			strlen(pathnameSeed), sizeofFirstPath, sizeofLastPath);
		if (number > 0) {
			stringLastNonBlank (firstPath, sizeofFirstPath);
			stringLastNonBlank (lastPath, sizeofLastPath);
			return STATUS_RECORD_FOUND;
		}
		if (number == 0) return STATUS_RECORD_NOT_FOUND;
		return number;
	}

	//  Clean the pathname and place into a char array that we can change
	zpathnameClean(path, sizeof(path), pathnameSeed);

	//  Firstly, do we have a valid pathname?
	status = zcheck(ifltab, path);
	if (zisError(status)) {
		return status;
	}

	if (status == STATUS_RECORD_FOUND) {
		boolFoundValid = 1;
	}
	else {
		boolFoundValid = 0;
	}

	timeWindow = 0;
	if (!boolFoundValid) {
		//  Could have a date range (/12JUL2002 - 13SEP2004/),
		//  or the wrong date or no date
		//  Make sure the E part is standard, and
		//  Get the time window specified in the D part of the pathname
		timeWindow = zstructTsNewTimeWindow();
		status = ztsGetPathTimeWindow(zgetVersion(ifltab), path, sizeof(path), timeWindow);
		if (status == STATUS_NOT_OKAY) {
			// Okay for now....
		}
		//  Time series, let's change the D part to a correct date
		if (timeWindow->blockSize != 0) {
			julianToDate(timeWindow->startBlockJulian, 4, dPart, sizeof(dPart));
			zpathnameSetPart (path, sizeof(path), dPart, 4);
		}
		//  See if it exsits...
		status = zcheck(ifltab, path);
		if (zisError(status)) {
			if (timeWindow != 0) free(timeWindow);
			return status;
		}
		if (status == STATUS_RECORD_FOUND) {
			boolFoundValid = 1;
		}
	}

	//  At this point, we should have either found a valid path
	//  or the D part is not specified.  The E part should be standard now.
	//  Go into the catalog and find a path that matches, if it exists!
	if (!boolFoundValid || (searchRange == 0)) {
		//  Exhaustive search
		zpathnameSetPart(path, sizeof(path), "*", 4);
		tsPaths = zstructCatalogNew();
		tsPaths->typeWantedStart = 100;
		tsPaths->typeWantedEnd = 199;
		number = zcatalogInternal(ifltab, path, tsPaths, 0, 0, 0, 0, 0);
		if (number <= 0) {			
			firstPath[0] = '\0';
			lastPath[0] = '\0';
			if (timeWindow != 0) free(timeWindow);
			zstructFree(tsPaths);
			tsPaths = 0;
			return STATUS_RECORD_NOT_FOUND;
		}
		zpathnameGetPart(tsPaths->pathnameList[0], 4, dPart, sizeof(dPart));
		julian = dateToJulian(dPart);
		firstJulian = julian;
		lastJulian = julian;
		for (i = 1; i < tsPaths->numberPathnames; i++) {
			zpathnameGetPart(tsPaths->pathnameList[i], 4, dPart, sizeof(dPart));
			julian = dateToJulian(dPart);
			if (julian < firstJulian) firstJulian = julian;
			if (julian > lastJulian) lastJulian = julian;
		}
		stringCopy(path, sizeof(path), tsPaths->pathnameList[0], strlen(tsPaths->pathnameList[0]));
		julianToDate(firstJulian, 4, dPart, sizeof(dPart));
		zpathnameSetPart(path, sizeof(path), dPart, 4);
		zpathnameClean(firstPath, sizeofFirstPath, path);
		julianToDate(lastJulian, 4, dPart, sizeof(dPart));
		zpathnameSetPart(path, sizeof(path), dPart, 4);
		zpathnameClean(lastPath, sizeofLastPath, path);
		zstructFree(tsPaths);
		if (timeWindow != 0) free(timeWindow);
		return STATUS_RECORD_FOUND;
	}
	if (!boolFoundValid || (searchRange == 0)) {
		zpathnameSetPart (path, sizeof(path), "*", 4);
		tsPaths =  zstructCatalogNew();
		tsPaths->typeWantedStart = 100;
		tsPaths->typeWantedEnd = 199;
		number = zcatalogInternal (ifltab, path, tsPaths, 0, 0, 1, 0, 0);
		if (number > 0) {
			stringCopy (path, sizeof(path), tsPaths->pathnameList[0], strlen(tsPaths->pathnameList[0]));
			//  See if it exsits...
			status = zcheck(ifltab, path);
			if (zisError(status)) {
				if (timeWindow != 0) free(timeWindow);
				return status;
			}
			if (status == STATUS_RECORD_FOUND) {
				boolFoundValid = 1;
			}
		}
		zstructFree(tsPaths);
	}

	//  Do we have a valid path yet?
	if (!boolFoundValid) {
		firstPath[0] = '\0';
		lastPath[0] = '\0';
		if (timeWindow != 0) free(timeWindow);
		return STATUS_RECORD_NOT_FOUND;
	}

	//  We have a valid checked pathname
	 zpathnameGetPart (path, 4, dPart, sizeof(dPart));
	 originalJulian = dateToJulian(dPart);
	 firstJulian = originalJulian;
	 lastJulian = originalJulian;

	//  Get the block size (for the interval)
	zpathnameGetPart (path, 5, ePart, sizeof(ePart));
	flag = 0;
	version = zgetVersion(ifltab);
	status = ztsGetStandardInterval(version, &intervalSeconds, ePart, sizeof(ePart), &flag);
	 if (status == 0) {
		 boolIrreg = 0;
		 ztsRegGetBlockStart(originalJulian, intervalSeconds, &blockSize);
	 }
	 else if (status == 1) {
		 //  Irregular Interval - block size sent back in interval, as negative
		 blockSize = -intervalSeconds;
		 boolIrreg = 1;
	 }

	 //  Walk backwards in time, checking paths, until we find no more
	 julian = originalJulian;
	 numberNotFound = 0;
	 while (numberNotFound < searchRange) {
		 julian = ztsIncrementBlock(julian, -blockSize);
		 julianToDate(julian, 4, dPart, sizeof(dPart));
		 zpathnameSetPart (path, sizeof(path), dPart, 4);
		 status = zcheck(ifltab, path);
		 if (zisError(status)) {
			if (timeWindow != 0) free(timeWindow);
			return status;
		 }
		 if (status == STATUS_RECORD_FOUND) {
			firstJulian = julian;
			numberNotFound = 0;
		 }
		 else {
			numberNotFound++;
		 }
	 }

	 //  Walk forwards in time, checking paths, until we find no more
	 julian = originalJulian;
	 numberNotFound = 0;
	 while (numberNotFound < searchRange) {
		 julian = ztsIncrementBlock(julian, blockSize);
		 julianToDate(julian, 4, dPart, sizeof(dPart));
		 zpathnameSetPart (path, sizeof(path), dPart, 4);
		 status = zcheck(ifltab, path);
		 if (zisError(status)) {
		 if (timeWindow != 0) free(timeWindow);
			return status;
		 }
		 if (status == STATUS_RECORD_FOUND) {
			lastJulian = julian;
			numberNotFound = 0;
		 }
		 else {
			numberNotFound++;
		 }
	 }

	 julianToDate(firstJulian, 4, dPart, sizeof(dPart));
	 zpathnameSetPart (path, sizeof(path), dPart, 4);
	 zpathnameClean(firstPath, sizeofFirstPath, path);

	 julianToDate(lastJulian, 4, dPart, sizeof(dPart));
	 zpathnameSetPart (path, sizeof(path), dPart, 4);
	 zpathnameClean(lastPath, sizeofLastPath, path);

	 if (timeWindow != 0) free(timeWindow);
	 return STATUS_RECORD_FOUND;
}


