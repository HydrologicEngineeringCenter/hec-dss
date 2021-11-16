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


