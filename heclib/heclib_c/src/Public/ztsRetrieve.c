#include <string.h>

#include "hecdssInternal.h"
#include "heclib.h"
#include "zStructTsTimeWindow.h"
#include "zStructTimeSeries.h"
#include "verticalDatum.h"

/**
*  Function:	ztsRetrieve
*
*  Use:			Public
*
*  Description:	Primary function to retrieve time series data (all types)
*
*  Declaration: int ztsRetrieve(long long *ifltab, zStructTimeSeries *tss,
*								int retrieveFlag, int retrieveDoublesFlag, int boolRetrieveQualityNotes);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a �handle� array and must be passed to any function that accesses that file.
*					See zopen() for more information.
*
*				zStructTimeSeries *tss
*					A struct that will contain all data and information read.
*					This struct must be created by one of the following TS Struct new methods:
*						zStructTimeSeries* zstructTsNew(const char* pathname);
*						zStructTimeSeries* zstructTsNewTimes(const char* pathname, const char* startDate, const char* startTime,
															 const char* endDate, const char* endTime);
*						What the struct needs to contain is defined below.
*					When the read is complete, the struct must be freed by a call to
*						void zstructFree(zStructTimeSeries *zstruct)
*					NEVER REUSE A zStructTimeSeries, always free and create a new on.
*
*				int retrieveFlag
*					A flag indicating how to read and trim data.
*					For regular interval data:
*						retrieveFlag = 0:  Adhere to time window provided and generate the time array.
*						retrieveFlag = -1: Trim data.  Remove missing values at the beginning and end of data set (not inside),
*											 and generate the time array.
*						retrieveFlag = -2: Adhere to time window provided but do not include time array.
*						retrieveFlag = -3: Trim data.  Remove missing values at the beginning and end of data set (not inside),
*											 no time array.
*					For irregular interval data:
*						retrieveFlag = 0:  Adhere to time window provided.
*						retrieveFlag = 1:  Retrieve one value previous to start of time window
*						retrieveFlag = 2:  Retrieve one value after end of time window
*						retrieveFlag = 3:  Retrieve one value before and one value after time window
*
*				int retrieveDoublesFlag
*					A flag indicating if floats or doubles should be returned.  This is independent of
*					what is actually stored on disk!  Values will be converted to the requested type.
*						0:  Return data as stored.  If missing, will return as doubles.
*						1:  Return floats
*						2:  Return doubles
*
*				int boolRetrieveQualityNotes
*					A flag indicating if you want quality and notes read also, if they exist.
*					If you are not going to use them, it is much more efficient not to read them.
*					(If they don't exist, they will not be returned, regardless.)
*						0:  Do not read quality or note arrays
*						1:  Read and return any quality or notes
*
*	Returns:	int status
*					STATUS_OKAY
*					error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*
*	Remarks:
*				Coordinates and other supplemental information is stored using the location functions
*					Start and end dates and times are optional, if a correct D pathname part is supplied.
*					If the start and end is not given, then the full data set will be returned,
*					unless a date range is given in the pathname ("20July2002 - 13May2010")
*
*
*  zStructTimeSeries parameters used in this call:
*
*	Required:
*
*				const char* pathname
*					The pathname of the record to retrieve.  The pathname must be exact, but is case insensitive
*					(i.e., "Apart" is the same as "APART"), and have a correct E (interval) part.
*					The full single record specified by the D (date part) will be read if the start date is not given.
*					If the D part contains a date range ("20July2002 - 13May2010") and the start date is not given,
*					then that time window will be used to read the data.
*
*	Optional:
*
*				const char* startDate
*					The starting date of the time window to read.  A variety of date formats will be accepted, but
*					the standard 9 character military style date will always work, such as "04JUL1776".
*
*				const char* startTime
*					The starting time for the date of the time window to read.  This should be given as 24 hour clock
*					style date, such as "1400", "08:30", or "17:23:15".
*
*				const char* endDate
*					The ending date of the time window to read.
*
*				const char* endTime
*					The ending time for the date of the time window to read.
*
*	Returns:
*
*				int numberValues
*					The number of values to store (and the number of values in the data array and other optional arrays (e.g., quality))
*
*				int timeGranularitySeconds
*					The number of seconds a unit in *itimes represents, usually MINUTE_GRANULARITY (60) or SECOND_GRANULARITY (1)
*					This is also usually the user specified granularity for time that data sets were stored with
*					(but is independent on how times are actually stored.)
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
*					An integer array containing the time of each data value read.  The time is in timeGranularitySeconds,
*					usually either in minutes (default) or seconds from the base date.  The default base date
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
*				char programName[17]
*					The name of the program that last wrote this record.
*
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int ztsRetrieve(long long *ifltab, zStructTimeSeries *tss,
				int retrieveFlag, int retrieveDoublesFlag, int boolRetrieveQualityNotes)
{

	int status;
	int intervalType;
	int version;



	if (!tss) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieve_ID, zdssErrorCodes.NULL_ARGUMENT,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "zStructTimeSeries is null");
	}
	if (!tss->pathname) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieve_ID, zdssErrorCodes.NULL_PATHNAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "zStructTimeSeries pathname is null");
	}

	//  Messages and debug
	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessage(ifltab, " ");
		zmessageDebug(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "Enter, Pathname: ", tss->pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "retrieveFlag: ", retrieveFlag);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "retrieveDoublesFlag: ", retrieveDoublesFlag);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "boolRetrieveQualityNotes: ", boolRetrieveQualityNotes);
		zmessageDebug(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "Time window prior to processing: ", "");
		ztsMessTimeWindow(ifltab, DSS_FUNCTION_ztsRetrieve_ID, tss);
	}

	//  Determine if regular interval or irregular interval
	//  and get time window information
	//  Returns -1 if error, 0 if regular interval, 1 if irregular interval
	intervalType = ztsProcessTimes(ifltab, tss, 0);
	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "Time window after processing: ", "");
		ztsMessTimeWindow(ifltab, DSS_FUNCTION_ztsRetrieve_ID, tss);
	}
	if (zisError(intervalType)) {
		return zerrorUpdate(ifltab, intervalType, DSS_FUNCTION_ztsRetrieve_ID);
	}
	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessage(ifltab, " ");
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "intervalType: ", intervalType);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "retrieveFlag: ", retrieveFlag);
		if (intervalType == 0) {
			if (retrieveFlag == 0) zmessageDebug(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "retrieveFlag = 0:  Adhere to time window provided.", "");
			if (retrieveFlag == -1) zmessageDebug(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "retrieveFlag = -1: Trim data.  Remove missing values at the beginning and end.", "");
		}
		else if (intervalType == 1) {
			if (retrieveFlag == 0) zmessageDebug(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "retrieveFlag = 0:  Adhere to time window provided.", "");
			if (retrieveFlag == 0) zmessageDebug(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "retrieveFlag = 1:  Retrieve one value previous to start of time window.", "");
			if (retrieveFlag == 0) zmessageDebug(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "retrieveFlag = 2:  Retrieve one value after end of time window.", "");
			if (retrieveFlag == 0) zmessageDebug(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "retrieveFlag = 3:  Retrieve one value before and one value after time window.", "");
		}
	}

	if (tss->boolPattern) {
		status = ztsRetrievePattern(ifltab, tss, retrieveDoublesFlag);
		if ((status == STATUS_OKAY) && (tss->timeIntervalSeconds > 0)) {
			ztsRegAddTimes(tss);
		}
/////////////////////////
//////////  FIX ME - Location???
		return status;
	}

	if (intervalType == STATUS_NOT_OKAY) {
		//  If no time window supplied, but a valid E part,
		//  then search for all time within the file.
		if (tss->timeWindow && (tss->timeWindow->intervalSeconds != 0)) {
			tss->boolRetrieveAllTimes = 1;
			intervalType = ztsProcessTimes(ifltab, tss, 0);
			if (zisError(intervalType)) {
				return zerrorUpdate(ifltab, intervalType, DSS_FUNCTION_ztsRetrieve_ID);
			}
		}
	}


	if (intervalType < 0) {
		//  No valid time window!
		status = zerrorProcessing(ifltab, DSS_FUNCTION_ztsRetrieve_ID, zdssErrorCodes.NO_TIME_WINDOW,
							    intervalType, 0, zdssErrorSeverity.WARNING, tss->pathname, "");
		if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_USER_DIAG)) {
			zmessage2(ifltab, "Pathname: ", tss->pathname);
			ztsDateMessage(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "Start: ", tss->startJulianDate, tss->startTimeSeconds);
			ztsDateMessage(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "End:   ",   tss->endJulianDate,   tss->endTimeSeconds);
		}
		return status;
	}

	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessage(ifltab, " ");
		zmessageDebug(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "Time window after processing: ", "");
		ztsMessTimeWindow(ifltab, DSS_FUNCTION_ztsRetrieve_ID, tss);
	}

	version = zgetVersion(ifltab);
	if (version == 6) {
		if (intervalType == 0)
		{
			status = ztsRetrieveReg6(ifltab, tss,
									retrieveFlag, retrieveDoublesFlag, boolRetrieveQualityNotes);
		}
		else {
			status = ztsRetrieveIrreg6(ifltab, tss,
									retrieveFlag, retrieveDoublesFlag, boolRetrieveQualityNotes);
		}
	}
	else {
		if (intervalType == 0) {
			status = ztsRetrieveReg7(ifltab, tss,
									retrieveFlag, retrieveDoublesFlag, boolRetrieveQualityNotes);
		}
		else {
			status = ztsRetrieveIrreg7(ifltab, tss,
									retrieveFlag, retrieveDoublesFlag, boolRetrieveQualityNotes);
		}
		//  Do we need to get location information?
		if ((status == STATUS_OKAY) && tss->locationStruct) {
			zlocationRetrieve(ifltab, tss->locationStruct);
		}
		//--------------------------------------------------//
		// convert to requested vertical datum if necessary //
		//--------------------------------------------------//
		char cPart[65];
		zpathnameGetPart(tss->pathname, 3, cPart, sizeof(cPart));
		if (!strncasecmp(cPart, "ELEV", 4)) {
			//------------------------//
			// parameter is elevation //
			//------------------------//
			char cvertical_datum[CVERTICAL_DATUM_SIZE];
			int  ivertical_datum = -1;
			verticalDatumInfo _vdi;
			verticalDatumInfo *vdi = NULL;
			char *vdiStr;
			char errmsg[1024];
			zquery("VDTM", cvertical_datum, sizeof(cvertical_datum), &ivertical_datum);
			if (ivertical_datum != IVERTICAL_DATUM_UNSET) {
				//-----------------------------------//
				// specific vertical datum requested //
				//-----------------------------------//
				vdi = extractVerticalDatumInfoFromUserHeader(tss->userHeader, tss->userHeaderNumber);
				if (!vdi) {
					if (tss->locationStruct && tss->locationStruct->supplemental) {
						vdiStr = extractFromDelimitedString(
							&tss->locationStruct->supplemental,
							VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
							":",
							TRUE,
							FALSE,
							'\n');
						if (vdiStr) {
							char *msg = stringToVerticalDatumInfo(&_vdi, vdiStr);
							if(msg == NULL) {
								vdi = &_vdi;
							}
							free(vdiStr);
						}
					}
				}
				if (vdi == NULL) {
					sprintf(
						errmsg,
						"Cannot convert from native vertical datum of '%s' to '%s'.\n"
						"Record has no conversion information.\n"
						"Values not converted.",
						tss->locationStruct->verticalDatum == 0 ? "UNSET"   :
						tss->locationStruct->verticalDatum == 1 ? "NAVD-88" :
						tss->locationStruct->verticalDatum == 2 ? "NGVD-29" : "OTHER",
						cvertical_datum);
					status = zerrorProcessing(
						ifltab,
						DSS_FUNCTION_ztsRetrieve_ID,
						zdssErrorCodes.INVALID_HEADER_PARAMETER,
						intervalType,
						0,
						zdssErrorSeverity.WARNING,
						tss->pathname,
						errmsg);
					return status;
				}
				else {
					//---------------------------------------------------------------//
					// ensure the vertical datum info is returned in the user header //
					//---------------------------------------------------------------//
					verticalDatumInfoToString(&vdiStr, vdi, TRUE);
					char *headerString;
					if (tss->userHeader) {
						headerString = userHeaderToString(tss->userHeader, tss->userHeaderNumber);
						if (!strstr(headerString, VERTICAL_DATUM_INFO_USER_HEADER_PARAM)) {
							headerString = (char *)realloc(
								headerString,
								strlen(headerString)
								+ VERTICAL_DATUM_INFO_USER_HEADER_PARAM_LEN
								+ strlen(vdiStr)
								+ 3);
							sprintf(
								headerString+strlen(headerString),
								";%s:%s",
								VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
								vdiStr);
							free(tss->userHeader);
						}
					}
					else {
						headerString = (char *)malloc(
							VERTICAL_DATUM_INFO_USER_HEADER_PARAM_LEN
							+ strlen(vdiStr)
							+ 2);
						sprintf(
							headerString,
							"%s:%s",
							VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
							vdiStr);
					}
					free(vdiStr);
					//--------------------------------------------//
					// add the requested datum to the user header //
					//--------------------------------------------//
					headerString = (char *)realloc(
						headerString,
						strlen(headerString)
						+ VERTICAL_DATUM_USER_HEADER_PARAM_LEN
						+ strlen(cvertical_datum)
						+ 3);
					sprintf(
						headerString+strlen(headerString),
						";%s:%s",
						VERTICAL_DATUM_USER_HEADER_PARAM,
						cvertical_datum);
					tss->userHeader = stringToUserHeader(headerString, &tss->userHeaderNumber);
					tss->allocated[zSTRUCT_userHeader] = TRUE;
					free(headerString);
					//-----------------------------//
					// determine the offset to use //
					//-----------------------------//
					double offset;
					switch(ivertical_datum) {
						case IVERTICAL_DATUM_NAVD88 :
							offset = vdi->offsetToNavd88;
							break;
						case IVERTICAL_DATUM_NGVD29 :
							offset = vdi->offsetToNgvd29;
							break;
						default :
							if(!strcmp(cvertical_datum, vdi->nativeDatum) || !strcmp(cvertical_datum, CVERTICAL_DATUM_OTHER)) {
								offset = 0;
							}
							else {
								offset = UNDEFINED_VERTICAL_DATUM_VALUE;
							}
							break;
					}
					if (offset != 0.) {
						offset = getOffset(offset, vdi->unit, tss->units);
						if (offset == UNDEFINED_VERTICAL_DATUM_VALUE) {
							sprintf(
								errmsg,
								"\nData unit (%s) and/or offset unit (%s) is invalid for vertical datum conversion.\n"
								"Conversion to datum '%s' could not be performed.\n"
								"Values not converted.",
								tss->units,
								vdi->unit,
								cvertical_datum);
							status = zerrorProcessing(
								ifltab,
								DSS_FUNCTION_ztsRetrieve_ID,
								zdssErrorCodes.INVALID_HEADER_PARAMETER,
								intervalType,
								0,
								zdssErrorSeverity.WARNING,
								tss->pathname,
								errmsg);
							return status;
						}
						else {
							//------------------------------//
							// add the offset to the values //
							//------------------------------//
							for (int i = 0; i < tss->numberValues; ++i) {
								if (tss->floatValues) {
									if (tss->floatValues[i] != UNDEFINED_FLOAT) {
										tss->floatValues[i] += offset;
									}
								}
								else {
									if (tss->doubleValues[i] != UNDEFINED_DOUBLE) {
										tss->doubleValues[i] += offset;
									}
								}
							}
						}
					}
				}
			}
			if (vdi && vdi != &_vdi) {
				free(vdi);
			}
		}
	}
	//  Do we need to trim the data?
	if (status == STATUS_OKAY) {
		if ((retrieveFlag == -1) || (retrieveFlag == -3)) {
			if (tss->timeIntervalSeconds > 0) {
				//  Only trim regular interval data
				status = ztsTrim(ifltab, tss);
			}
		}
	}
	//  Add the time array for regular interval?
	if (status == STATUS_OKAY) {
		if (tss->timeIntervalSeconds > 0) {
			//  Be sure start time reflects actual data
			if (tss->timeOffsetSeconds < 0) tss->timeOffsetSeconds = 0;
			ztsOffsetAdjustToOffset(tss->timeOffsetSeconds, tss->timeIntervalSeconds, &tss->startJulianDate, &tss->startTimeSeconds);
			ztsOffsetAdjustToOffset(tss->timeOffsetSeconds, tss->timeIntervalSeconds, &tss->endJulianDate, &tss->endTimeSeconds);
			if ((retrieveFlag == 0) || (retrieveFlag == -1)) {
				ztsRegAddTimes(tss);
			}
		}
	}

	if ((tss->pathnameInternal) && (tss->allocated[zSTRUCT_pathnameInternal])) {
		free(tss->pathnameInternal);
		tss->pathnameInternal = 0;
		tss->allocated[zSTRUCT_pathnameInternal] = 0;
	}

	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessage(ifltab, " ");
		zmessageDebug(ifltab, DSS_FUNCTION_ztsRetrieve_ID,  "Exit; Pathname: ", tss->pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "status: ", status);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "numberValues: ", tss->numberValues);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsRetrieve_ID, "sizeEachValueRead: ", tss->sizeEachValueRead);
	}
	return status;
}
