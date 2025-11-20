#include <string.h>

#include "hecdssInternal.h"
#include "zdssKeys.h"
#include "heclib.h"
#include "zStructTsTimeWindow.h"
#include "zStructTimeSeries.h"

/**
*  Function:	ztsRetrievePattern
*
*  Use:			Semi-Public
*
*  Description:	Retrieve time series pattern data (e.g., unit hydrographs, daily average temperatures, etc.)
*					Use ztsRetrieve instead (it will call this function.)
*					Note:  Pattern data has a D part of "TS-Pattern", instead of a date
*
*  Declaration: int ztsRetrievePattern(long long *ifltab, zStructTimeSeries *tss, int retrieveDoublesFlag);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [100] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*					See zopen() for more information.
*
*				zStructTimeSeries *tss
*					A struct that will contain all data and information read.
*					This struct must be created by the following TS Struct new method:
*						zStructTimeSeries* zstructTsNew(const char* pathname);
*						What the struct needs to contain is defined below.
*					When the read is complete, the struct must be freed by a call to
*						void zstructFree(zStructTimeSeries *zstruct)
*					NEVER REUSE A zStructTimeSeries, always free and create a new on.
*
*				int retrieveDoublesFlag
*					A flag indicating if floats or doubles should be returned.  This is independent of
*					what is actually stored on disk!  Values will be converted to the requested type.
*						0:  Return data as stored.  If missing, will return as doubles.
*						1:  Return floats
*						2:  Return doubles

*
*	Returns:	int status
*					STATUS_OKAY
*					error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*
*
*  zStructTimeSeries parameters used in this call:
*
*	Required:
*
*				const char* pathname
*					The pathname of the record to store.  The pathname must be exact, but is case insensitive
*					(i.e., "Apart" is the same as "APART"), and have a correct E (interval) part.
*					The full single record specified by the D (date part) will be read if the start date is not given.
*					If the D part contains a date range ("20July2002 - 13May2010") and the start date is not given,
*					then that time window will be used to read the data.
*

*
*	Returns:
*
*				int numberValues
*					The number of values to store (and the number of values in the data array and other optional arrays (e.g., quality))
*
*				int timeGranularitySeconds
*					The number of seconds a unit in *itimes represents, usually
*					MINUTE_GRANULARITY (60) or SECOND_GRANULARITY (1)
*
*				float *floatValues
*					The float array containing the data read, if retrieveDoublesFlag was set to one.
*			or
*				double *doubleValues
*					The double array containing the data read, if retrieveDoublesFlag was set to two.
*
*				int sizeEachValueRead
*					The size of values on disk, 1 = float, 2 = double
*
*				int precision
*					The data precision stored with the data.
*					-1 = not set, otherwise decimal places for each value (e.g., 2 is 0.01)
*
*				const char *units
*					The units of the data, such as "CFS".
*
*				const char *type
*					The type of data.  Valid values include "PER-AVER", "PER-CUM", "INST-VAL", and "INST-CUM".
*
*				int startJulianDate
*					The julian date of the first data value.
*
*				int startTimeSeconds
*					The time of the first data value, in seconds after startJulianDate.
*					Note:  code uses seconds, regardless of granularity
*
*				int endJulianDate
*					The julian date of the last data value.
*
*				int startTimeSeconds
*					The time of the last data value, in seconds after endJulianDate.
*
*				char *timeZoneName
*					The time zone name stored with the data.  Note - this is not necessarily the same
*					time zone stored with the location ID.  (This may be UTC and the location may be EST)
*
*	Regular-interval:
*
*				int timeOffsetSeconds
*					The time offset from the standard (EOP) time, in seconds
*
*	Irregular-interval:
*
*				int *itimes
*					An integer array containing the time of each data value read.  The time can be
*					either in minutes (default) or seconds from the base date.  The default base date
*					is 01Jan1900 at 0000.
*
*
*	Optional:
*
*		Profiles:
*				int profileDepthsNumber
*					The number of values for each time.  For example, if you had measurements
*					at 0, 5, 10, 15 meters, this would be 4, and you have to have 4 values for every time.
*
*				*floatProfileDepths or
*				*doubleProfileDepths
*					An array containing the depths (independent variable) for each time.  The same depths
*					are applied to each time.  For example, this array might be 0.0, 5.0, 10.0, 15.0.
*
*				*floatProfileValues or
*				*doubleProfileValues
*					An array containing the values or measurements (dependent variable) for each time.
*					This would normally be a double dimensioned array, but C doesn't handle those well,
*					so this contains the values for the first time, followed by the values for
*					the second time, and so forth.
*					For the above example, this array might be 72.1, 71.5, 70.2, 69.7, 72.3, 71.7, 70.5, 69.9, ...
*					numberValues = number times.
*
*		Quality:
*				int *quality
*					The array that contains quality or other additional information.  A single quality value
*					may be an int or multiple of an int.  For example, you might have 3 quality ints for
*					each value (time).  In this case, qualityElementSize would be 3, and then have each set follow in the array.
*
*				int qualityElementSize
*					The int size of each quality value read, normally 1.  In the above example, this is 3,
*					indicating that the first three ints belong to the first value, the second three ints
*					belong the the second value, and so forth.
*
*				int qualityArraySize (required)
*					A flag, or the size of the quality array available to be read.
*					If quality is not to be read, set to zero.  If space is to be allocated and all
*					quality is to be read, set this to one (1).  Must be freed afterwards with zstructFree(zStructTimeSeries *zstruct).
*					If larger than 1, then this is the array size and no more than that number will be read.
*
*		Notes:
*			Notes can be either as a string for each value or a specific number of ints
*			(essentially a second quality array).  Notes can either be ints or strings, not both.
*
*				char *cnotes
*					A character array containing a null terminated string for each value (time period).
*					For example, if you were had 5 values, you might have a cnote array as follows:
*						"\0\0Gage failed to report\0Gage repaired\0\0"
*
*				int cnotesLengthTotal
*					The length (dimension) of the cnotes array in characters (bytes).
*
*				int cnotesLengthTotal (required)
*					A flag, or the size of the string array available to be read.
*					If string notes are not to be read, set to zero.  If space is to be allocated and all
*					string notes  is to be read, set this to one (1).  Must be freed afterwards with zstructFree(zStructTimeSeries *zstruct).
*					If larger than 1, then this is the array size, and no more than that number of bytes will be read.
*
*				int *inotes
*					An array that contains additional information to store with each value.
*					The inotes array is similar to the quality array, and each inotes value
*					may be an int or multiple of an int.  For example, you might have 6 ints for
*					each value (time).  inoteElementSize would be 6, and then have each set follow in the array.
*					You cannot use inotes if you use cnotes, as they use the same storage space.
*
*				int inoteElementSize
*					The int size of each inote value.
*
*				int inotesArraySize
*					A flag, or the size of the inotes array available to be read.
*					If inotes are not to be read, set to zero.  If space is to be allocated and all
*					inotes are to be read, set this to one (1).  Must be freed afterwards with zstructFree(zStructTimeSeries *zstruct).
*					If larger than 1, then this is the array size and no more than that number will be read.
*
*				int *userHeader
*					An int array to hold any additional information for the record, usually from the user.
*
*				int userHeaderSize
*					A flag, or the size of the userHeader array available to be read.
*					If userHeader is not to be read, set to zero.  If space is to be allocated to read the
*					user header from the first record, set this to one (1).  Must be freed afterwards with zstructFree(zStructTimeSeries *zstruct).
*					If larger than 1, then this is the array size and no more than that number will be read.
*
*				int userHeaderNumber
*					The number of ints read into the user header.
*
*
*	Additional (Informational only)
*
*				int dataType
*					The DSS data type for this data set
*
*				int dateOfFirstRecFound
*					The julian date of the first record found for this data set
*
*				long long lastWrittenTime
*					The last write time of the first record found, in seconds sine Jan 01, 1970
*
*				char programName[PARAMETER_NAME_SIZE]
*					The name of the program that last wrote this record.
*

	In DSS Version 7, irregular data is stored as: time1 from base, time2 from base, ... value1, value2...
*
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int ztsRetrievePattern(long long *ifltab, zStructTimeSeries *tss, int retrieveDoublesFlag)
{

	int status;
	int julian;
	int seconds;
	long long *info;
	zStructTransfer* ztransfer;

	//  Messages and debug
	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "Enter ztsRetrievePattern, Pathname: ", tss->pathname);
	}

	//  Determine if regular interval or irregular interval
	if (tss->timeWindow == 0) {
		ztsProcessTimes(ifltab, tss, 0);
	}
	else if (tss->timeWindow->intervalSeconds == 0) {
		ztsProcessTimes(ifltab, tss, 0);
	}
	if ((tss->timeWindow == 0) || ((tss->timeWindow->intervalSeconds == 0) && (tss->timeWindow->blockSize == 0))) {
		// error out
		return -1;
	}
	if (tss->pathnameInternal == 0) {
		//  This shouldn't happen - supposed to be done in ztsProcessTimes
		tss->pathnameInternal = mallocAndCopy(tss->pathname);
		tss->allocated[zSTRUCT_pathnameInternal] = 1;
	}

	//  We don't have to mess with times (much) for pattern data, like
	//  we do with standard time series.  We just return the array if irregular
	ztransfer = zstructTransferNew(tss->pathnameInternal, 1);
	if (!ztransfer) {
		//  error out
		return -1;
	}
	status = zread(ifltab, ztransfer);
	if (status != STATUS_RECORD_FOUND) {
		zstructFree(ztransfer);
		return status;
	}

		tss->numberValues = ztransfer->numberValues;
		ztsInternalHeaderUnpack(tss, ztransfer->internalHeader, ztransfer->internalHeaderNumber);
		info = (long long *)ifltab[zdssKeys.kinfo];
		tss->lastWrittenTime = info[zdssInfoKeys.kinfoLastWriteTime];
		tss->fileLastWrittenTime = ztransfer->fileLastWrittenTime;
		charLong(&info[zdssInfoKeys.kinfoProgram], tss->programName, zdssVals.numberProgram, zdssVals.numberProgram, 0, 1);


	tss->dataType = ztransfer->dataType;
	if (ztransfer->dataType == DATA_TYPE_RTS_PATTERN) {
		tss->sizeEachValueRead = 1;
		if ((retrieveDoublesFlag == 0) || (retrieveDoublesFlag == 1)) {
			tss->floatValues = (float *)ztransfer->values1;
			tss->allocated[zSTRUCT_TS_floatValues] = 1;
			ztransfer->allocated[zSTRUCT_TRANS_values1] = 0;
		}
		else {
			tss->doubleValues = (double *)malloc(tss->numberValues * 8);
			tss->allocated[zSTRUCT_TS_doubleValues] = 1;
			convertDataArray(ztransfer->values1, (int *)tss->doubleValues, tss->numberValues, 1, 2);
		}
	}
	else if (ztransfer->dataType == DATA_TYPE_RTD_PATTERN) {
		tss->sizeEachValueRead = 2;
		if ((retrieveDoublesFlag == 0) || (retrieveDoublesFlag == 2)) {
			tss->doubleValues = (double *)malloc(tss->numberValues * 8);
			tss->allocated[zSTRUCT_TS_doubleValues] = 1;
			convertDataArray(ztransfer->values1, (int *)tss->doubleValues, tss->numberValues, 2, 2);
		}
		else {
			tss->floatValues = (float *)malloc(tss->numberValues * 4);
			tss->allocated[zSTRUCT_TS_floatValues] = 1;
			convertDataArray(ztransfer->values1, (int *)tss->floatValues, tss->numberValues, 2, 1);
		}
	}
	else if ((ztransfer->dataType == DATA_TYPE_ITS_PATTERN) ||
	(ztransfer->dataType == DATA_TYPE_ITD_PATTERN)) {
		tss->times = (int *)malloc(tss->numberValues * 4);
		tss->allocated[zSTRUCT_TS_times] = 1;
		convertDataArray(ztransfer->values1, (int *)tss->times, tss->numberValues, 1, 1);
		if (ztransfer->dataType == DATA_TYPE_ITS_PATTERN) {
			tss->sizeEachValueRead = 1;
			if ((retrieveDoublesFlag == 0) || (retrieveDoublesFlag == 1)) {
				tss->floatValues = (float *)malloc(tss->numberValues * 4);
				tss->allocated[zSTRUCT_TS_floatValues] = 1;
				convertDataArray(&ztransfer->values1[tss->numberValues], (int *)tss->floatValues, tss->numberValues, 1, 1);
			}
			else {
				tss->doubleValues = (double *)malloc(tss->numberValues * 8);
				tss->allocated[zSTRUCT_TS_doubleValues] = 1;
				convertDataArray(&ztransfer->values1[tss->numberValues], (int *)tss->doubleValues, tss->numberValues, 1, 2);
			}
		}
		else if (ztransfer->dataType == DATA_TYPE_ITD_PATTERN) {
			tss->sizeEachValueRead = 2;
			if ((retrieveDoublesFlag == 0) || (retrieveDoublesFlag == 2)) {
				tss->doubleValues = (double *)malloc(tss->numberValues * 8);
				tss->allocated[zSTRUCT_TS_doubleValues] = 1;
				convertDataArray(&ztransfer->values1[tss->numberValues], (int *)tss->doubleValues, tss->numberValues, 2, 2);
			}
			else {
				tss->floatValues = (float *)malloc(tss->numberValues * 4);
				tss->allocated[zSTRUCT_TS_floatValues] = 1;
				convertDataArray(&ztransfer->values1[tss->numberValues], (int *)tss->floatValues, tss->numberValues, 2, 1);
			}
		}
	}


	if (status == STATUS_OKAY) {
		//  By convention, we use a start of Jan 01, 3000 for patterns,
		//  in case the data is plotted or tabulated.
		tss->startJulianDate = dateToJulian("01Jan3000");
		tss->startTimeSeconds = 0;  //  This puts it at the end of the previous interval
		tss->julianBaseDate = 0;
		incrementTime(tss->timeIntervalSeconds, 1, tss->startJulianDate, tss->startTimeSeconds,
			&julian, &seconds);
		if (tss->timeOffsetSeconds != 0) {
			ztsOffsetAdjustToOffset(tss->timeOffsetSeconds, tss->timeIntervalSeconds, &julian, &seconds);
		}
		tss->startJulianDate = julian;
		tss->startTimeSeconds = seconds;
		incrementTime(tss->timeIntervalSeconds, tss->numberValues, tss->startJulianDate, tss->startTimeSeconds,
			&tss->endJulianDate, &tss->endTimeSeconds);
	}

	if (tss->timeWindow) {
		free(tss->timeWindow);
		tss->timeWindow = 0;
		tss->allocated[zSTRUCT_TS_timeWindow] = 0;
	}
	zstructFree(ztransfer);

	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztsRetrieve_ID,  "Exit; Pathname: ", tss->pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "status: ", status);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "numberValues: ", tss->numberValues);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "sizeEachValueRead: ", tss->sizeEachValueRead);
	}

	return status;
}


