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


