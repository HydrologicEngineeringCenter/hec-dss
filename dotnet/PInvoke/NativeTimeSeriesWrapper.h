#pragma once

#include "DSSGrid.h"
#include "pinvokeTools.h"

extern "C" {
	__declspec(dllexport) void SetQuality(zStructTimeSeries* ts, int* quality, int arraySize);
	__declspec(dllexport) void SetQualityElementSize(zStructTimeSeries* ts, int elementSize);
	__declspec(dllexport) BSTR GetPathName(zStructTimeSeries* ts);
	__declspec(dllexport) BSTR GetUnits(zStructTimeSeries* ts);
	__declspec(dllexport) BSTR GetType(zStructTimeSeries* ts);
	__declspec(dllexport) BSTR GetProgramName(zStructTimeSeries* ts);
	__declspec(dllexport) int* GetQuality(zStructTimeSeries* ts);
	__declspec(dllexport) double* GetDoubleValues(zStructTimeSeries* ts);
	__declspec(dllexport) int GetTimeGranularitySeconds(zStructTimeSeries* ts);
	__declspec(dllexport) int GetJulianBaseDate(zStructTimeSeries* ts);
	__declspec(dllexport) int* GetTimes(zStructTimeSeries* ts);
	__declspec(dllexport) double* GetDoubleProfileDepths(zStructTimeSeries* ts);
	__declspec(dllexport) int GetProfileDepthsNumber(zStructTimeSeries* ts);
	__declspec(dllexport) double* GetDoubleProfileValues(zStructTimeSeries* ts);
	__declspec(dllexport) int GetNumberValues(zStructTimeSeries* ts);
	__declspec(dllexport) zStructLocation* GetLocation(zStructTimeSeries* ts);
	__declspec(dllexport) void SetDoubleProfileDepths(zStructTimeSeries* ts, double* values, int arrayLength);
	__declspec(dllexport) void SetProfileDepthsNumber(zStructTimeSeries* ts, int value);
	__declspec(dllexport) void SetDoubleProfileValues(zStructTimeSeries* ts, double* values, int arrayLength);
	__declspec(dllexport) void SetNumberValues(zStructTimeSeries* ts, int value);
	__declspec(dllexport) void SetUnitsProfileValues(zStructTimeSeries* ts, const char* value);
	__declspec(dllexport) void SetUnitsProfileDepths(zStructTimeSeries* ts, const char* value);
	__declspec(dllexport) void SetFloatProfileDepths(zStructTimeSeries* ts, float* values, int arrayLength);
	__declspec(dllexport) void SetFloatProfileValues(zStructTimeSeries* ts, float* values, int arrayLength);
	__declspec(dllexport) int GetStartJulianDate(zStructTimeSeries* ts);
	__declspec(dllexport) void SetStartJulianDate(zStructTimeSeries* ts, int value);
	__declspec(dllexport) int GetStartTimeSeconds(zStructTimeSeries* ts);
	__declspec(dllexport) void SetStartTimeSeconds(zStructTimeSeries* ts, int value);
	__declspec(dllexport) float* GetFloatValues(zStructTimeSeries* ts);
	__declspec(dllexport) void SetFloatValues(zStructTimeSeries* ts, float* value);
	__declspec(dllexport) int GetEndJulianDate(zStructTimeSeries* ts);
	__declspec(dllexport) void SetEndJulianDate(zStructTimeSeries* ts, int value);
	__declspec(dllexport) int GetEndTimeSeconds(zStructTimeSeries* ts);
	__declspec(dllexport) void SetEndTimeSeconds(zStructTimeSeries* ts, int value);
	__declspec(dllexport) int GetTimeIntervalSeconds(zStructTimeSeries* ts);
	__declspec(dllexport) void SetTimeIntervalSeconds(zStructTimeSeries* ts, int value);
	__declspec(dllexport) char* GetCNotes(zStructTimeSeries* ts);
	__declspec(dllexport) void SetCNotes(zStructTimeSeries* ts, const char* value);
	__declspec(dllexport) int GetCNotesLengthTotal(zStructTimeSeries* ts);
	__declspec(dllexport) void SetCNotesLengthTotal(zStructTimeSeries* ts, int value);

}