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

int GetTimeGranularitySeconds(zStructTimeSeries* ts)
{
	return ts->timeGranularitySeconds;
}

void SetTimeGranularitySeconds(zStructTimeSeries* ts, int value)
{
	ts->timeGranularitySeconds = value;
}

int GetJulianBaseDate(zStructTimeSeries* ts)
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

void SetDoubleProfileDepths(zStructTimeSeries* ts, double* values, int arrayLength)
{
	if (ts->doubleProfileDepths)
		free(ts->doubleProfileDepths);

	ts->doubleProfileDepths = (double*)malloc(sizeof(double) * arrayLength);
	if (ts->doubleProfileDepths)
		memcpy(ts->doubleProfileDepths, values, sizeof(double) * arrayLength);
}

void SetProfileDepthsNumber(zStructTimeSeries* ts, int value)
{
	ts->profileDepthsNumber = value;
}

void SetDoubleProfileValues(zStructTimeSeries* ts, double* values, int arrayLength)
{
	if (ts->doubleProfileValues)
		free(ts->doubleProfileValues);

	ts->doubleProfileValues = (double*)malloc(sizeof(double) * arrayLength);
	if (ts->doubleProfileValues)
		memcpy(ts->doubleProfileValues, values, sizeof(double) * arrayLength);
}

void SetNumberValues(zStructTimeSeries* ts, int value)
{
	ts->numberValues = value;
}

void SetUnitsProfileValues(zStructTimeSeries* ts, const char* value)
{
	if (ts->unitsProfileValues)
		free(ts->unitsProfileValues);

	ts->unitsProfileValues = _strdup(value);
}

void SetUnitsProfileDepths(zStructTimeSeries* ts, const char* value)
{
	if (ts->unitsProfileDepths)
		free(ts->unitsProfileDepths);

	ts->unitsProfileDepths = _strdup(value);
}

void SetFloatProfileDepths(zStructTimeSeries* ts, float* values, int arrayLength)
{
	if (ts->floatProfileDepths)
		free(ts->floatProfileDepths);

	ts->floatProfileDepths = (float*)malloc(sizeof(float) * arrayLength);
	if (ts->floatProfileDepths)
		memcpy(ts->floatProfileDepths, values, sizeof(float) * arrayLength);
}

void SetFloatProfileValues(zStructTimeSeries* ts, float* values, int arrayLength)
{
	if (ts->floatProfileValues)
		free(ts->floatProfileValues);

	ts->floatProfileValues = (float*)malloc(sizeof(float) * arrayLength);
	if (ts->floatProfileValues)
		memcpy(ts->floatProfileValues, values, sizeof(float) * arrayLength);
}

int GetStartJulianDate(zStructTimeSeries* ts)
{
	return ts->startJulianDate;
}

void SetStartJulianDate(zStructTimeSeries* ts, int value)
{
	ts->startJulianDate = value;
}

int GetStartTimeSeconds(zStructTimeSeries* ts)
{
	return ts->startTimeSeconds;
}

void SetStartTimeSeconds(zStructTimeSeries* ts, int value)
{
	ts->startTimeSeconds = value;
}

float* GetFloatValues(zStructTimeSeries* ts)
{
	return ts->floatValues;
}

void SetFloatValues(zStructTimeSeries* ts, float* value, int arrayLength)
{
	if (ts->floatValues)
		free(ts->floatValues);

	ts->floatValues = (float*)malloc(sizeof(float) * arrayLength);
	if (ts->floatValues)
		memcpy(ts->floatValues, value, sizeof(float) * arrayLength);
}

int GetEndJulianDate(zStructTimeSeries* ts)
{
	return ts->endJulianDate;
}

void SetEndJulianDate(zStructTimeSeries* ts, int value)
{
	ts->endJulianDate = value;
}

int GetEndTimeSeconds(zStructTimeSeries* ts)
{
	return ts->endTimeSeconds;
}

void SetEndTimeSeconds(zStructTimeSeries* ts, int value)
{
	ts->endTimeSeconds = value;
}

int GetTimeIntervalSeconds(zStructTimeSeries* ts)
{
	return ts->timeIntervalSeconds;
}

void SetTimeIntervalSeconds(zStructTimeSeries* ts, int value)
{
	ts->timeIntervalSeconds = value;
}

char* GetCNotes(zStructTimeSeries* ts)
{
	return ts->cnotes;
}

void SetCNotes(zStructTimeSeries* ts, const char* value, int arrayLength)
{
	if (ts->cnotes)
		free(ts->cnotes);

	ts->cnotes = (char*)malloc(sizeof(char) * arrayLength);
	if (ts->cnotes)
		memcpy(ts->cnotes, value, sizeof(char) * arrayLength);
}

int GetCNotesSize(zStructTimeSeries* ts)
{
	return ts->cnotesSize;
}

void SetCNotesSize(zStructTimeSeries* ts, int value)
{
	ts->cnotesSize = value;
}

int GetCNotesLengthTotal(zStructTimeSeries* ts)
{
	return ts->cnotesLengthTotal;
}

void SetCNotesLengthTotal(zStructTimeSeries* ts, int value)
{
	ts->cnotesLengthTotal = value;
}
