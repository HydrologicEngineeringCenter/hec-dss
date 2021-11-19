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
}