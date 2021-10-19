#include <string.h>

#include "heclib.h"
#include "hecdssInternal.h"

#include "zStructTimeSeries.h"

/*
	Description


	ztsTimeWindow *ztimeWindow;

*/

ztsTimeWindow* zstructTsNewTimeWindow()
{
	ztsTimeWindow *ztimeWindow;
	int len;

	len = sizeof(ztsTimeWindow);
	ztimeWindow = (ztsTimeWindow*)calloc((size_t)len, BYTE_SIZE);
	if (!ztimeWindow) {
//		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsProcessTimes_ID,
//								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, len, 0,
//								zdssErrorSeverity.MEMORY_ERROR, "", "time window array");
	}

	ztimeWindow->blockSize = 0;
	ztimeWindow->intervalSeconds = 0;
	ztimeWindow->numberValues = 0;
	ztimeWindow->startJulian = UNDEFINED_TIME;
	ztimeWindow->startTimeSeconds = UNDEFINED_TIME;
	ztimeWindow->endJulian = UNDEFINED_TIME;
	ztimeWindow->endTimeSeconds = UNDEFINED_TIME;

	return ztimeWindow;
}

