#include "pch.h"
#include "NativeTimeSeriesWrapper.h"

void SetQuality(zStructTimeSeries* ts, int* quality, int arraySize)
{
	if (ts->quality)
		free(ts->quality);

	ts->qualityArraySize = arraySize;
	ts->quality = (int*)malloc(sizeof(int) * arraySize);
	if (ts->quality) {
		std::copy(quality, quality + arraySize, ts->quality);
		ts->allocated[zSTRUCT_TS_quality] = 1;
	}
}

void SetQualityElementSize(zStructTimeSeries* ts, int elementSize)
{
	ts->qualityElementSize = elementSize;
}

BSTR GetPathName(zStructTimeSeries* ts)
{
	return ANSItoBSTR(ts->pathname);
}

BSTR GetUnits(zStructTimeSeries* ts)
{
	return ANSItoBSTR(ts->units);
}

BSTR GetType(zStructTimeSeries* ts)
{
	return ANSItoBSTR(ts->type);
}

BSTR GetProgramName(zStructTimeSeries* ts)
{
	return ANSItoBSTR(ts->programName);
}

int* GetQuality(zStructTimeSeries* ts)
{
	return ts->quality;
}

double* GetDoubleValues(zStructTimeSeries* ts)
{
	return ts->doubleValues;
}

double GetTimeGranularitySeconds(zStructTimeSeries* ts)
{
	return ts->timeGranularitySeconds;
}

double GetJulianBaseDate(zStructTimeSeries* ts)
{
	return ts->julianBaseDate;
}

int* GetTimes(zStructTimeSeries* ts)
{
	return ts->times;
}

double* GetDoubleProfileDepths(zStructTimeSeries* ts)
{
	return ts->doubleProfileDepths;
}

int GetProfileDepthsNumber(zStructTimeSeries* ts)
{
	return ts->profileDepthsNumber;
}

double* GetDoubleProfileValues(zStructTimeSeries* ts)
{
	return ts->doubleProfileValues;
}

int GetNumberValues(zStructTimeSeries* ts)
{
	return ts->numberValues;
}

zStructLocation* GetLocation(zStructTimeSeries* ts)
{
	return ts->locationStruct;
}
