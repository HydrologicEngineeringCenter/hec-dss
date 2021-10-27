
#include "zdssMessages.h"
#include "hecdss7.h"

/**
*  Function:	ztsGetDateTimeRange
*
*  Use:			Public
*
*  Description:	Returns the Julian date of the first valid value in a time series dataset or record,
*					and the last valid value.  Will work with either a single record or a full dataset
*					(multiple records)
*
*  Declaration: int ztsGetDateTimeRange(long long *ifltab, const char *pathname, int boolFullSet,
*										int *firstValidJulian, int *firstSeconds,
*										int *lastValidJulian, int *lastSeconds);
*  Parameters:	long long *ifltab
*					The file table array, similar to a handle number
*
*				const char* pathname
*					The pathname or seed pathname of the dataset.
*
*				int boolFullSet
*					A boolean flag set to 0 to return dates of the single record,
*					or set to 1 to return the dates of the entire dataset (multiple records)
*
*				int *firstJulian
*					Returns the Julian date of the first valid value
*
*				int *firstSeconds
*					Returns the time of day in seconds (for firstJulian) of the first valid value
*
*				int *lastJulian
*					Returns the Julian date of the last valid value
*
*				int *lastSeconds
*					Returns the time of day in seconds of the last valid value
*
*
*
*	Returns:	int status
*					STATUS_OKAY
*					error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*					Returns STATUS_NOT_OKAY (-1) if not time series
*
*	See Also:	ztsGetFirstLastRecordTimes
*				ztsGetDateRange
*
*	Remarks:	Works for both regular and irregular interval data
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int ztsGetDateTimeRange(long long *ifltab, const char *pathname, int boolFullSet,
	int *firstValidJulian, int *firstSeconds, int *lastValidJulian, int *lastSeconds)
{
	int status;
	int seconds;
	int dummy;

	char pathFirst[MAX_PATHNAME_LENGTH];
	char pathLast[MAX_PATHNAME_LENGTH];


	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_USER_DIAG) ||
		zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_other_ID, "Enter ztsGetDateTimeRange, Pathname: ", pathname);
	}

	if (boolFullSet) {
		status = ztsGetFirstLastPathnames(ifltab, pathname,
								pathFirst, sizeof(pathFirst),
								pathLast, sizeof(pathLast));
		if (status != STATUS_RECORD_FOUND) {
			return status;
		}
		status =  ztsGetFirstLastRecordTimes(ifltab, pathFirst,
								firstValidJulian, firstSeconds,
								&dummy, &seconds, 1);
		if (status != STATUS_RECORD_FOUND) {
			return status;
		}
		status =  ztsGetFirstLastRecordTimes(ifltab, pathLast,
								&dummy, &seconds,
								lastValidJulian, lastSeconds, 1);
	}
	else {
		status =  ztsGetFirstLastRecordTimes(ifltab, pathname,
								firstValidJulian, firstSeconds,
								lastValidJulian, lastSeconds, 1);
	}
	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_USER_DIAG) ||
		zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "Exit ztsGetDateTimeRange, First Pathname: ", pathFirst);
		zmessageDebug(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "Exit ztsGetDateTimeRange, Last Pathname:  ", pathLast);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "ztsGetDateTimeRange, First Valid Julian: ", *firstValidJulian);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "ztsGetDateTimeRange, First Valid seconds: ", *firstSeconds);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "ztsGetDateTimeRange, Last Valid Julian:  ", *lastValidJulian);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "ztsGetDateTimeRange, Last Valid seconds: ", *lastSeconds);
	}
	return status;

}

