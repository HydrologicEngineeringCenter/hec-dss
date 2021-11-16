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
	__declspec(dllexport) double GetTimeGranularitySeconds(zStructTimeSeries* ts);
	__declspec(dllexport) double GetJulianBaseDate(zStructTimeSeries* ts);
	__declspec(dllexport) int* GetTimes(zStructTimeSeries* ts);
	__declspec(dllexport) double* GetDoubleProfileDepths(zStructTimeSeries* ts);
	__declspec(dllexport) int GetProfileDepthsNumber(zStructTimeSeries* ts);
	__declspec(dllexport) double* GetDoubleProfileValues(zStructTimeSeries* ts);
	__declspec(dllexport) int GetNumberValues(zStructTimeSeries* ts);
	__declspec(dllexport) zStructLocation* GetLocation(zStructTimeSeries* ts);
}