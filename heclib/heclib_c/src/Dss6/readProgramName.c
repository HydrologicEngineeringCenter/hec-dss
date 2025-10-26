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
}