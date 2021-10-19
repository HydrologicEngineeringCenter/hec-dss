#include <stdlib.h>
#include <string.h>

#include "heclib7.h"

/**
*  Function:	zstructTsClone
*
*  Use:			Public
*
*  Description:	Creates a new time series struct from an existing, not including internal flags and times
*
*  Declaration: zStructTimeSeries* zstructTsClone(const char* pathname);
*
*  Parameters:	const char* pathname
*					The pathname for this struct.  Must be a valid pathname.
*					A copy of the pathname is used in struct.
*
*
*
*	Returns:	zStructTimeSeries*
*					An address to the time series struct created.
*
*   Note:		ALWAYS call function zstructFree(struct) when finished using.
*
*	Remarks:	.
*
*	See:		ztsStore and ztsRetrieve for definition of zStructTimeSeries
*
*	See Also:	zstructTsNewTimes()
*				zstructTsNewRegFloats()
*				zstructTsNewRegDoubles()
*				zstructTsNewIrregFloats()
*				zstructTsNewIrregDoubles()
*
*
*	Author:			Bill Charley
*	Date:			2019
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


zStructTimeSeries* zstructTsClone(zStructTimeSeries *tss, const char* pathname)
{
	zStructTimeSeries *tssClone;
	int i;
	int number;
	
	tssClone = zstructTsNew(pathname);

	tssClone->structType = tss->structType;

	//  Time related
	tssClone->julianBaseDate =			tss->julianBaseDate;
	tssClone->startJulianDate =			tss->startJulianDate;
	tssClone->startTimeSeconds =		tss->startTimeSeconds;
	tssClone->endJulianDate =			tss->endJulianDate;
	tssClone->endTimeSeconds =			tss->endTimeSeconds;
	tssClone->timeGranularitySeconds =	tss->timeGranularitySeconds;
	tssClone->timeIntervalSeconds =		tss->timeIntervalSeconds;
	tssClone->timeOffsetSeconds =		tss->timeOffsetSeconds;
	tssClone->boolRetrieveAllTimes =	tss->boolRetrieveAllTimes;
	tssClone->boolPattern =				tss->boolPattern;

	//  Data related
	tssClone->numberValues = tss->numberValues;
	tssClone->sizeEachValueRead = tss->sizeEachValueRead;
	tssClone->precision = tss->precision;

	if (tss->units) {
		tssClone->units = mallocAndCopy(tss->units);
		if (!tssClone->units) return (zStructTimeSeries*)0;
		tssClone->allocated[zSTRUCT_TS_units] = 1;
	}
	if (tss->type) {
		tssClone->type = mallocAndCopy(tss->type);
		if (!tssClone->type) return (zStructTimeSeries*)0;
		tssClone->allocated[zSTRUCT_TS_type] = 1;
	}

	if (tss->times && (tss->numberValues > 0)) {
		tssClone->times = calloc(tssClone->numberValues, 4);
		if (!tssClone->times) return (zStructTimeSeries*) 0;
		tssClone->allocated[zSTRUCT_TS_times] = 1;
		for (i = 0; i < tssClone->numberValues; i++) {
			tssClone->times[i] = tss->times[i];
		}
	}

	if (tss->floatValues && (tss->numberValues > 0)) {
		tssClone->floatValues = calloc(tssClone->numberValues, 4);
		if (!tssClone->floatValues) return (zStructTimeSeries*)0;
		tssClone->allocated[zSTRUCT_TS_floatValues] = 1;
		for (i = 0; i < tssClone->numberValues; i++) {
			tssClone->floatValues[i] = tss->floatValues[i];
		}
	}

	if (tss->doubleValues && (tss->numberValues > 0)) {
		tssClone->doubleValues = calloc(tssClone->numberValues, 8);
		if (!tssClone->doubleValues) return (zStructTimeSeries*)0;
		tssClone->allocated[zSTRUCT_TS_doubleValues] = 1;
		for (i = 0; i < tssClone->numberValues; i++) {
			tssClone->doubleValues[i] = tss->doubleValues[i];
		}
	}

	//  Profile
	tssClone->profileDepthsNumber =	tss->profileDepthsNumber;

	//  Units
	if (tss->unitsProfileDepths) {
		tssClone->unitsProfileDepths = mallocAndCopy(tss->unitsProfileDepths);
		if (!tssClone->unitsProfileDepths) return (zStructTimeSeries*)0;
		tssClone->allocated[zSTRUCT_TS_profileUnitsDepths] = 1;
	}
	if (tss->unitsProfileValues) {
		tssClone->unitsProfileValues = mallocAndCopy(tss->unitsProfileValues);
		if (!tssClone->unitsProfileValues) return (zStructTimeSeries*)0;
		tssClone->allocated[zSTRUCT_TS_profileUnitsValues] = 1;
	}
	
	//  Floats
	if (tss->floatProfileDepths && (tss->profileDepthsNumber > 0)) {
		tssClone->floatProfileDepths = calloc(tssClone->profileDepthsNumber, 4);
		if (!tssClone->floatProfileDepths) return (zStructTimeSeries*)0;
		tssClone->allocated[zSTRUCT_TS_profileFloatDepths] = 1;
		for (i = 0; i<tssClone->profileDepthsNumber; i++) {
			tssClone->floatProfileDepths[i] = tss->floatProfileDepths[i];
		}
	}

	if (tss->floatProfileValues && (tss->profileDepthsNumber > 0) && (tss->numberValues > 0)) {
		number = tss->profileDepthsNumber * tss->numberValues;
		tssClone->floatProfileValues = calloc(number, 4);
		if (!tssClone->floatProfileValues) return (zStructTimeSeries*)0;
		tssClone->allocated[zSTRUCT_TS_profileFloatValues] = 1;
		for (i = 0; i<number; i++) {
			tssClone->floatProfileValues[i] = tss->floatProfileValues[i];
		}
	}

	//  Doubles
	if (tss->doubleProfileDepths && (tss->profileDepthsNumber > 0)) {
		tssClone->doubleProfileDepths = calloc(tssClone->profileDepthsNumber, 8);
		if (!tssClone->doubleProfileDepths) return (zStructTimeSeries*)0;
		tssClone->allocated[zSTRUCT_TS_profileDoubleDepths] = 1;
		for (i = 0; i<tssClone->profileDepthsNumber; i++) {
			tssClone->doubleProfileDepths[i] = tss->doubleProfileDepths[i];
		}
	}

	if (tss->doubleProfileValues && (tss->profileDepthsNumber > 0) && (tss->numberValues > 0)) {
		number = tss->profileDepthsNumber * tss->numberValues;
		tssClone->doubleProfileValues = calloc(number, 8);
		if (!tssClone->doubleProfileValues) return (zStructTimeSeries*)0;
		tssClone->allocated[zSTRUCT_TS_profileDoubleValues] = 1;
		for (i = 0; i<number; i++) {
			tssClone->doubleProfileValues[i] = tss->doubleProfileValues[i];
		}
	}
	
	if (tss->timeZoneName) {
		tssClone->timeZoneName = mallocAndCopy(tss->timeZoneName);
		if (!tssClone->timeZoneName) return (zStructTimeSeries*)0;
		tssClone->allocated[zSTRUCT_timeZoneName] = 1;
	}


	tssClone->qualityElementSize = tss->qualityElementSize;
	if (tss->quality && (tss->qualityElementSize > 0) && (tss->numberValues > 0)) {
		number = tss->qualityElementSize * tss->numberValues;
		tssClone->qualityArraySize = number;
		tssClone->quality = calloc(number, 4);
		if (!tssClone->quality) return (zStructTimeSeries*)0;
		tssClone->allocated[zSTRUCT_TS_quality] = 1;
		for (i = 0; i<number; i++) {
			tssClone->quality[i] = tss->quality[i];
		}
	}

	tssClone->inoteElementSize = tss->inoteElementSize;
	if (tss->inotes && (tss->inoteElementSize > 0) && (tss->numberValues > 0)) {
		number = tss->inoteElementSize * tss->numberValues;
		tssClone->inotesArraySize = number;
		tssClone->inotes = calloc(number, 4);
		if (!tssClone->inotes) return (zStructTimeSeries*)0;
		tssClone->allocated[zSTRUCT_TS_inotes] = 1;
		for (i = 0; i<number; i++) {
			tssClone->inotes[i] = tss->inotes[i];
		}
	}

	tssClone->cnotesLengthTotal = tss->cnotesLengthTotal;
	if (tss->cnotesLengthTotal > 0) {
		tssClone->cnotesSize = tss->cnotesSize;
		tssClone->cnotes = calloc(tss->cnotesLengthTotal, 1);
		if (!tssClone->cnotes) return (zStructTimeSeries*)0;
		tssClone->allocated[zSTRUCT_TS_cnotes] = 1;
		for (i = 0; i<tss->cnotesLengthTotal; i++) {
			tssClone->cnotes[i] = tss->cnotes[i];
		}
	}

	tssClone->userHeaderNumber = tss->userHeaderNumber;
	if (tss->userHeader && (tss->userHeaderNumber > 0)) {
		tssClone->userHeaderSize = tss->userHeaderNumber;  //  Note - number, not size (we are allocating)
		tssClone->userHeader = calloc(tss->userHeaderNumber, 4);
		if (!tssClone->userHeader) return (zStructTimeSeries*)0;
		tssClone->allocated[zSTRUCT_userHeader] = 1;
		for (i = 0; i<tss->userHeaderNumber; i++) {
			tssClone->userHeader[i] = tss->userHeader[i];
		}
	}

	tssClone->dataType =			tss->dataType;
	tssClone->dateOfFirstRecFound = tss->dateOfFirstRecFound;
	tssClone->lastWrittenTime =		tss->lastWrittenTime;
	tssClone->fileLastWrittenTime = tss->fileLastWrittenTime;

	if (tss->programName) {
		if (strlen(tss->programName) > 0) {
			stringCopy(tssClone->programName, sizeof(tssClone->programName), tss->programName, strlen(tss->programName));
		}
	}

	/*
	NEED zStructLocation *locationStruct;
	
	*/


	return tssClone;
}

