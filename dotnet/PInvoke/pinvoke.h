#pragma once

#include "DSSGrid.h"
#include "pinvokeTools.h"

extern "C" {
	__declspec(dllexport) int ZOpen(long long* ifltab, const char* dssFilename);
	__declspec(dllexport) int ZSet(const char* parameter, const char* charVal, int integerValue);
	__declspec(dllexport) void ZSetMessageLevel(int methodId, int levelId);
	__declspec(dllexport) int ZGetVersion(long long* ifltab);
	__declspec(dllexport) int ZClose(long long* ifltab);
	__declspec(dllexport) int ZTsStore(long long* ifltab, zStructTimeSeries* tss, int storageFlag);
	__declspec(dllexport) zStructTimeSeries* ZStructTsNewRegDoubles(const char* pathName, double* dArray, int numberValues, const char* startDate, const char* startTime, const char* units, const char* type);
	__declspec(dllexport) zStructTimeSeries* ZStructTsNewRegFloats(const char* pathName, float* floatValues, int numberValues, const char* startDate, const char* startTime, const char* units, const char* type);
	__declspec(dllexport) zStructTimeSeries* ZStructTsNewIrregDoubles(const char* pathName, double* doubleValues, int numberValues, int* itimes, int timeGranularitySeconds, const char* startDateBase, const char* units, const char* type);
	__declspec(dllexport) zStructTimeSeries* ZStructTsNewIrregFloats(const char* pathName, float* floatValues, int numberValues, int* itimes, int timeGranularitySeconds, const char* startDateBase, const char* units, const char* type);
	__declspec(dllexport) int ZTsRetrieve(long long* ifltab, zStructTimeSeries* tss, int retrieveFlag, int retrieveDoublesFlag, int boolRetrieveQualityNotes);
	__declspec(dllexport) zStructTimeSeries* ZStructTsNewTimes(const char* pathName, const char* startDate, const char* startTime, const char* endDate, const char* endTime);
	__declspec(dllexport) zStructTimeSeries* ZStructTsNew(const char* pathName);
	__declspec(dllexport) zStructCatalog* ZStructCatalogNew();
	__declspec(dllexport) int ZCatalog(long long* ifltab, const char* pathWithWild, zStructCatalog* cat, int boolSorted);
	__declspec(dllexport) int ZTsRetrieveEmpty(long long* ifltab, zStructTimeSeries* tss);
	__declspec(dllexport) zStructLocation* ZLocationRetrieve(long long* ifltab, const char* pathName);
	__declspec(dllexport) int ZDataType(long long* ifltab, const char* pathName);
	__declspec(dllexport) int ZSqueeze(const char* fileName);
	__declspec(dllexport) int ZSqueeze7(long long* ifltab, int boolOnlyIfNeeded, int boolInPlace);
	__declspec(dllexport) int GetDateAndTime(int timeMinOrSec, int timeGranularitySeconds, int julianBaseDate, char* dateString, int sizeOfDateString, char* hoursMins, int sizeOfHoursMins);
	__declspec(dllexport) int ZCheck(long long* ifltab, const char* pathName);
	__declspec(dllexport) int ZDelete(long long* ifltab, const char* pathName);
	__declspec(dllexport) int ZSqueezeNeeded(long long* ifltab);
	__declspec(dllexport) float ZMissingFlag();
	__declspec(dllexport) zStructLocation* ZStructLocationNew(const char* pathName);
	__declspec(dllexport) int ZLocationStore(long long* ifltab, zStructLocation* loc, int storageFlag);
	__declspec(dllexport) int ZPathNameForm(const char* aPart, const char* bPart, const char* cPart, const char* dPart, const char* ePart, const char* fPart, char* pathName, size_t sizeOfPathName);
	__declspec(dllexport) int JulianToYearMonthDay(int julian, int* year, int* month, int* day);
	__declspec(dllexport) int TimeStringToSeconds(const char* timeString);
	__declspec(dllexport) int DateToJulian(const char* dateString);
	__declspec(dllexport) zStructPairedData* ZStructPdNew(const char* pathName);
	__declspec(dllexport) zStructPairedData* ZStructPdNewDoubles(const char* pathname, double* doubleOrdinates, double* doubleValues, int numberOrdinates, int numberCurves, const char* unitsIndependent, const char* typeIndependent, const char* unitsDependent, const char* typeDependent);
	__declspec(dllexport) zStructPairedData* ZStructPdNewFloats(const char* pathname, float* floatOrdinates, float* floatValues, int numberOrdinates, int numberCurves, const char* unitsIndependent, const char* typeIndependent, const char* unitsDependent, const char* typeDependent);
	__declspec(dllexport) int ZPdRetrieve(long long* ifltab, zStructPairedData* pd, int retrieveDoubleFlag);
	__declspec(dllexport) int ZPdStore(long long* ifltab, zStructPairedData* pd, int storageFlag);

}