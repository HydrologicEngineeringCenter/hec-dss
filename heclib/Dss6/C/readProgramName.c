#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "heclib.h"
#include "zdssKeys.h"
#include "zdssVals.h"

/// <summary>
/// This function gets the program name for a DSS version 6 file and assigns it to the program name property of the time series struct.
/// </summary>
/// <param name="ifltab"></param>
/// <param name="tss"></param>
/// <param name="status"></param>
void readProgramName(long long* ifltab, zStructTimeSeries* tss, int status)
{

	return; // may be causing Solaris issue (return for now)

	if (zgetVersion(ifltab) != 6) {
		return;
	}
	if (status == STATUS_RECORD_FOUND || status == STATUS_NO_OP)
	{
		long long* info = (long long*)ifltab[zdssKeys.kinfo];
		int ibuff[20];
		int istat = 0;
		zgetinfo6_(ifltab, tss->pathname, ibuff, &istat, strlen(tss->pathname));
		charLong(&ibuff[8], tss->programName, zdssVals.numberProgram, zdssVals.numberProgram, 0, 0);
	}
}