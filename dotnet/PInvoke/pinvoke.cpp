#include "pch.h"
#include "pinvoke.h"

int ZOpen(long long* ifltab, const char* dssFilename) {
	return zopen(ifltab, dssFilename);
}

int ZSet(const char* parameter, const char* charVal, int integerValue)
{
	return zset(parameter, charVal, integerValue);
}

void ZSetMessageLevel(int methodId, int levelId) {
	zsetMessageLevel(methodId, levelId);
}

int ZGetVersion(long long* ifltab)
{
	return zgetVersion(ifltab);
}

int ZClose(long long* ifltab)
{
	return zclose(ifltab);
}

int ZTsStore(long long* ifltab, zStructTimeSeries* tss, int storageFlag)
{
	return ztsStore(ifltab, tss, storageFlag);
}

zStructTimeSeries* ZStructTsNewRegDoubles(const char* pathName, double* dArray, int numberValues, const char* startDate, const char* startTime, const char* units, const char* type)
{
	zStructTimeSeries* ts = zstructTsNewRegDoubles(pathName, dArray, numberValues, startDate, startTime, units, type);
	ts->allocated[zSTRUCT_TS_doubleValues] = 1;
	return ts;
}

zStructTimeSeries* ZStructTsNewRegFloats(const char* pathName, float* floatValues, int numberValues, const char* startDate, const char* startTime, const char* units, const char* type)
{
	zStructTimeSeries* ts = zstructTsNewRegFloats(pathName, floatValues, numberValues, startDate, startTime, units, type);
	ts->allocated[zSTRUCT_TS_doubleValues] = 1;
	return ts;
}

zStructTimeSeries* ZStructTsNewIrregDoubles(const char* pathName, double* doubleValues, int numberValues, int* itimes, int timeGranularitySeconds, const char* startDateBase, const char* units, const char* type)
{
	zStructTimeSeries* ts = zstructTsNewIrregDoubles(pathName, doubleValues, numberValues, itimes, timeGranularitySeconds, startDateBase, units, type);
	ts->allocated[zSTRUCT_TS_doubleValues] = 1;
	ts->allocated[zSTRUCT_TS_times] = 1;
	return ts;
}

zStructTimeSeries* ZStructTsNewIrregFloats(const char* pathName, float* floatValues, int numberValues, int* itimes, int timeGranularitySeconds, const char* startDateBase, const char* units, const char* type)
{
	zStructTimeSeries* ts = zstructTsNewIrregFloats(pathName, floatValues, numberValues, itimes, timeGranularitySeconds, startDateBase, units, type);
	ts->allocated[zSTRUCT_TS_floatValues] = 1;
	ts->allocated[zSTRUCT_TS_times] = 1;
	return ts;
}

int ZTsRetrieve(long long* ifltab, zStructTimeSeries* tss, int retrieveFlag, int retrieveDoublesFlag, int boolRetrieveQualityNotes)
{

	return ztsRetrieve(ifltab, tss, retrieveFlag, retrieveDoublesFlag, boolRetrieveQualityNotes);
}

zStructTimeSeries* ZStructTsNewTimes(const char* pathName, const char* startDate, const char* startTime, const char* endDate, const char* endTime)
{
	return zstructTsNewTimes(pathName, startDate, startTime, endDate, endTime);
}

zStructTimeSeries* ZStructTsNew(const char* pathName)
{
	return zstructTsNew(pathName);
}

zStructCatalog* ZStructCatalogNew()
{
	return zstructCatalogNew();
}

int ZCatalog(long long* ifltab, const char* pathWithWild, zStructCatalog* cat, int boolSorted)
{
	return zcatalog(ifltab, pathWithWild, cat, boolSorted);
}

int ZTsRetrieveEmpty(long long* ifltab, zStructTimeSeries* tss)
{
	int version = zgetVersion(ifltab);
	zStructTransfer* transfer;
	transfer = zstructTransferNew(tss->pathname, 0);
	transfer->internalHeaderMode = 1;
	int status = zread(ifltab, transfer);
	if (status == 0)
	{
		if (version == 7)
		{
			int intervalType = ztsProcessTimes(ifltab, tss, 0);
			status = ztsInternalHeaderUnpack(tss, transfer->internalHeader, transfer->internalHeaderNumber);
		}
		else if (version == 6)
		{
			int v6header_units = 1;
			int v6header_type = 3;
			tss->units = getStringFromHeader(&transfer->internalHeader[v6header_units]);
			tss->type = getStringFromHeader(&transfer->internalHeader[v6header_type]);

		}
	}

	zstructFree(transfer);
	return status;
}

zStructLocation* ZLocationRetrieve(long long* ifltab, const char* pathName)
{
	int ver = zgetVersion(ifltab);

	zStructLocation* loc = zstructLocationNew(pathName);
	loc->xOrdinate = -9999;
	loc->yOrdinate = -9999;
	loc->zOrdinate = -9999;

	if (ver > 6)
		zlocationRetrieve(ifltab, loc);

	return loc;
}

int ZDataType(long long* ifltab, const char* pathName)
{
	return zdataType(ifltab, pathName);
}

int ZSqueeze(const char* fileName)
{
	return zsqueeze(fileName);
}

int ZSqueeze7(long long* ifltab, int boolOnlyIfNeeded, int boolInPlace)
{
	return zsqueeze7(ifltab, boolOnlyIfNeeded, boolInPlace);
}

int GetDateAndTime(int timeMinOrSec, int timeGranularitySeconds, int julianBaseDate, char* dateString, int sizeOfDateString, char* hoursMins, int sizeOfHoursMins)
{
	return getDateAndTime(timeMinOrSec, timeGranularitySeconds, julianBaseDate, dateString, sizeOfDateString, hoursMins, sizeOfHoursMins);
}

int ZCheck(long long* ifltab, const char* pathName)
{
	return zcheck(ifltab, pathName);
}

int ZDelete(long long* ifltab, const char* pathName)
{
	return zdelete(ifltab, pathName);
}

int ZSqueezeNeeded(long long* ifltab)
{
	return zsqueezeNeeded(ifltab);
}

float ZMissingFlag()
{
	return zmissingFlagFloat();
}

zStructLocation* ZStructLocationNew(const char* pathName)
{
	return zstructLocationNew(pathName);
}

int ZLocationStore(long long* ifltab, zStructLocation* loc, int storageFlag)
{
	return zlocationStore(ifltab, loc, storageFlag);
}

int ZPathNameForm(const char* aPart, const char* bPart, const char* cPart, const char* dPart, const char* ePart, const char* fPart, char* pathName, size_t sizeOfPathName)
{
	return zpathnameForm(aPart, bPart, cPart, dPart, ePart, fPart, pathName, sizeOfPathName);
}

int JulianToYearMonthDay(int julian, int* year, int* month, int* day)
{
	return julianToYearMonthDay(julian, year, month, day);
}

int TimeStringToSeconds(const char* timeString)
{
	return timeStringToSeconds(timeString);
}

int DateToJulian(const char* dateString)
{
	return dateToJulian(dateString);
}

zStructPairedData* ZStructPdNew(const char* pathName)
{
	return zstructPdNew(pathName);
}

zStructPairedData* ZStructPdNewDoubles(const char* pathname, double* doubleOrdinates, double* doubleValues, int numberOrdinates, int numberCurves, const char* unitsIndependent, const char* typeIndependent, const char* unitsDependent, const char* typeDependent)
{
	zStructPairedData* pd = zstructPdNewDoubles(pathname, doubleOrdinates, doubleValues, numberOrdinates, numberCurves, unitsIndependent, typeIndependent, unitsDependent, typeDependent);
	pd->allocated[zSTRUCT_PD_doubleValues] = 1;
	pd->allocated[zSTRUCT_PD_doubleOridnates] = 1;
	return pd;
}

zStructPairedData* ZStructPdNewFloats(const char* pathname, float* floatOrdinates, float* floatValues, int numberOrdinates, int numberCurves, const char* unitsIndependent, const char* typeIndependent, const char* unitsDependent, const char* typeDependent)
{
	zStructPairedData* pd = zstructPdNewFloats(pathname, floatOrdinates, floatValues, numberOrdinates, numberCurves, unitsIndependent, typeIndependent, unitsDependent, typeDependent);
	pd->allocated[zSTRUCT_PD_floatValues] = 1;
	pd->allocated[zSTRUCT_PD_floatOrdinates] = 1;
	return pd;
}

int ZPdRetrieve(long long* ifltab, zStructPairedData* pd, int retrieveDoubleFlag)
{
	return zpdRetrieve(ifltab, pd, retrieveDoubleFlag);
}

int ZPdStore(long long* ifltab, zStructPairedData* pd, int storageFlag)
{
	return zpdStore(ifltab, pd, storageFlag);
}

zStructSpatialGrid* ZStructSpatialGridNew(const char* pathName)
{
	return zstructSpatialGridNew(pathName);
}

int ZSpatialGridRetrieve(long long* ifltab, zStructSpatialGrid* grid, bool retrieveData)
{
	return DSSGrid::RetrieveGriddedData(ifltab, grid, retrieveData);
}

int ZSpatialGridStore(long long* ifltab, zStructSpatialGrid* grid)
{
	return zspatialGridStore(ifltab, grid);
}

int DateToYearMonthDay(const char* date, int* year, int* month, int* day)
{
	return dateToYearMonthDay(date, year, month, day);
}

bool IsTimeDefined(int julianDate, int timeSeconds)
{
	return isTimeDefined(julianDate, timeSeconds) == 1;
}

BSTR AlbersSRS()
{
	return ANSItoBSTR(SHG_SRC_DEFINITION);
}

int YearMonthDayToJulian(int year, int month, int day)
{
	return yearMonthDayToJulian(year, month, day);
}


