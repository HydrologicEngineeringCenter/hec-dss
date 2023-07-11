
#include "zdssMessages.h"
#include "hecdss7.h"

/**
*  Function:	ztsGetDateRange
*
*  Use:			Public
*
*  Description:	Returns the Julian date of the first valid value in a time series dataset or record,
*					and the last valid value.  Will work with either a single record or a full dataset
*					(multiple records).  See ztsGetDateTimeRange to get both dates and times.
*
*  Declaration: int ztsGetDateRange(long long *ifltab, const char *pathname, int boolFullSet,
*									int *firstValidJulian, int *lastValidJulian);
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
*				int *lastJulian
*					Returns the Julian date of the last valid value
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
*				ztsGetDateTimeRange
*
*	Remarks:	Works for both regular and irregular interval data
*				This is much faster than ztsGetFirstLastRecordTimes as it does not need to get
*				the time also.  (Dates are store with pathname, whereas data has to be
*				read to get times.)
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int ztsGetDateRange(long long *ifltab, const char *pathname, int boolFullSet,
	int *firstValidJulian, int *lastValidJulian)
{
	int status;
	int seconds;
	int dummy;
	char dPart[MAX_PART_SIZE];
	char ePart[MAX_PART_SIZE];
	int operation;
	int intervalSeconds;
	int len;
	int blockSize;

	char pathFirst[MAX_PATHNAME_LENGTH];
	char pathLast[MAX_PATHNAME_LENGTH];


	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_USER_DIAG) ||
		zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_other_ID, "Enter ztsGetDateRange, Pathname: ", pathname);
	}

	//  Be sure we have a valid d part
	if (!boolFullSet) {
		len = zpathnameGetPart(pathname, 4, dPart, sizeof(dPart));
		//  Be sure we have a D part
		if (len < 7) boolFullSet = 1;
	}

	if (boolFullSet) {
		status = ztsGetFirstLastPathnames(ifltab, pathname,
								pathFirst, sizeof(pathFirst),
								pathLast, sizeof(pathLast));
		if (status != STATUS_RECORD_FOUND) {
			return status;
		}
		status =  ztsGetFirstLastRecordTimes(ifltab, pathFirst,
								firstValidJulian, &seconds,
								&dummy, &seconds, 0);
		if (status != STATUS_RECORD_FOUND) {
			len = zpathnameGetPart(pathFirst, 4, dPart, sizeof(dPart));
			//  Get the Julian date
			if (len > 7) {
				*firstValidJulian = dateToJulian(dPart);
				if (*firstValidJulian == UNDEFINED_TIME) return STATUS_NOT_OKAY;
			}
		}
		status =  ztsGetFirstLastRecordTimes(ifltab, pathLast,
								&dummy, &seconds,
								lastValidJulian, &seconds, 0);
		if (status != STATUS_RECORD_FOUND) {
			len = zpathnameGetPart(pathLast, 4, dPart, sizeof(dPart));
			//  Get the Julian date
			if (len > 7) {
				*lastValidJulian = dateToJulian(dPart);
				if (*lastValidJulian == UNDEFINED_TIME) return STATUS_NOT_OKAY;
				//  Get the time interval
				len = zpathnameGetPart(pathLast, 5, ePart, sizeof(ePart));
				if (len < 4) {
					//  Not a valid length for a time series interval
					return STATUS_NOT_OKAY;
				}
				operation = EPART_TO_SECONDS_TO_EPART;
				ztsGetStandardInterval(zgetVersion(ifltab), &intervalSeconds, ePart, sizeof(ePart), &operation);
				ztsRegGetBlockStart(*lastValidJulian, intervalSeconds, &blockSize);
				*lastValidJulian = ztsIncrementBlock(*lastValidJulian, blockSize) - 1;
				status = STATUS_RECORD_FOUND;
			}
		}
	}
	else {
		status =  ztsGetFirstLastRecordTimes(ifltab, pathname,
								firstValidJulian, &seconds,
								lastValidJulian, &seconds, 0);
		if (status != STATUS_RECORD_FOUND) {
			len = zpathnameGetPart(pathname, 4, dPart, sizeof(dPart));
			//  Get the Julian date
			if (len > 7) {
				*firstValidJulian = dateToJulian(dPart);
				if (*firstValidJulian == UNDEFINED_TIME) return STATUS_NOT_OKAY;
				//  Get the time interval
				len = zpathnameGetPart(pathname, 5, ePart, sizeof(ePart));
				if (len < 4) {
					//  Not a valid length for a time series interval
					return STATUS_NOT_OKAY;
				}
				operation = EPART_TO_SECONDS_TO_EPART;
				ztsGetStandardInterval(zgetVersion(ifltab), &intervalSeconds, ePart, sizeof(ePart), &operation);
				ztsRegGetBlockStart(*firstValidJulian, intervalSeconds, &blockSize);
				*lastValidJulian = ztsIncrementBlock(*firstValidJulian, blockSize) - 1;
				status = STATUS_RECORD_FOUND;
			}
		}
	}
	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_USER_DIAG) ||
		zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "Exit ztsGetDateRange, First Pathname: ", pathFirst);
		zmessageDebug(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "Exit ztsGetDateRange, Last Pathname:  ", pathLast);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "ztsGetDateRange, First Valid Julian: ", *firstValidJulian);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "ztsGetDateRange, Last Valid Julian:  ", *lastValidJulian);
	}
	return status;

}

