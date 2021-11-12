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


