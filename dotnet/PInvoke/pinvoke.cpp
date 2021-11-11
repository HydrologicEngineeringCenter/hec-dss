#include "pch.h"
#include "pinvoke.h"

int ZOpen(long long* ifltab, const char* dssFilename) {
	return zopen(ifltab, dssFilename);
}

int ZSet(const char* parameter, const char* charVal, int integerValue)
{
	return zset(parameter, charVal, integerValue);
}
