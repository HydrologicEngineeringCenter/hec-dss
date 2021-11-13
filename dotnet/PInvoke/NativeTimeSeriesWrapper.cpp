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
	}
}

void SetQualityElementSize(zStructTimeSeries* ts, int elementSize)
{
	ts->qualityElementSize = elementSize;
}
