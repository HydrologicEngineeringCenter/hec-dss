#pragma once

#include "DSSGrid.h"

extern "C" {
	__declspec(dllexport) void SetQuality(zStructTimeSeries* ts, int* quality, int arraySize);
	__declspec(dllexport) void SetQualityElementSize(zStructTimeSeries* ts, int elementSize);
}