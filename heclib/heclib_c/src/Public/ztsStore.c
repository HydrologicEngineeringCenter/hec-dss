#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "heclib7.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "zStructTimeSeries.h"
#include "verticalDatum.h"


/**
*  Function:	ztsStore
*
*  Use:			Public
*
*  Description:	Primary function to store time series data (all types)
*
*  Declaration: int ztsStore(long long *ifltab, zStructTimeSeries *tss, int storageFlag);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*					See zopen() for more information.
*
*				zStructTimeSeries *tss
*					A struct that needs to contain all data and information related to this data set to store.
*					This struct must be created by one of the TS Struct new methods, such as:
*						zStructTimeSeries* zstructTsNew(const char* pathname);
*						zStructTimeSeries* zstructTsNewRegFloats(const char* pathname, float *floatValues, int numberValues,
*												const char *startDate, const char *startTime, const char *units, const char *type);
*						What the struct needs to contain is defined below.
*					When the store is complete, the struct must be freed by a call to
*						void zstructFree(zStructTimeSeries *zstruct)
*					NEVER REUSE A zStructTimeSeries, always free and create a new on.
*
*				int storageFlag
*					A flag indicating how to handle existing data on disk.  For regular interval data:
*						storageFlag = 0  Always replace data.
*						storageFlag = 1  Only replace missing data.
*						storageFlag = 2  Write regardless, even if all missing data (write a missing record)
*						storageFlag = 3  If a record is all missing, do not write it
*							and delete it from disk if it exists.
*						storageFlag = 4  Do not allow a missing input data to
*							replace a valid data piece.
*
*					For irregular interval data, this is a flag to indicate whether to replace or merge new data
*						or merge new data with old data. Replace for editing/changing data, merge for adding data.
*						 storageFlag = 0  merge
*						 storageFlag = 1  replace
*
*	Returns:	int status
*					STATUS_OKAY
*					error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*
*	Remarks:
*				Coordinates and other supplemental information is stored using the location functions
*
*	Functions to use to create zStructTimeSeries:
*
*				zStructTimeSeries* zstructTsNew(const char* pathname);
*
*		Regular-interval data
*				zStructTimeSeries* zstructTsNewRegFloats(const char* pathname, float *floatValues, int numberValues,
*														 const char *startDate, const char *startTime,
*														 const char *units, const char *type);
*				zStructTimeSeries* zstructTsNewRegDoubles(const char* pathname, double *doubleValues, int numberValues,
*														 const char *startDate, const char *startTime,
*														 const char *units, const char *type);
*
*		Irregular-interval data
*				zStructTimeSeries* zstructTsNewIrregFloats(const char* pathname, double *doubleValues, int numberValues,
*														   int *itimes, int timeGranularitySeconds, const char* startDateBase,
*														   const char *units, const char *type);
*				zStructTimeSeries* zstructTsNewIrregDoubles(const char* pathname, float *floatValues, int numberValues,
*														   int *itimes, int timeGranularitySeconds, const char* startDateBase,
*														   const char *units, const char *type);
*
*	Function to free zStructTimeSeries
*				void zstructFree(zStructTimeSeries *tss);  //  All DSS structs are freed by this call.
*
*
*  zStructTimeSeries parameters used in this call:
*
*	Required:
*
*				const char* pathname
*					The pathname of the record to store.  The pathname must be exact, but is case insensitive
*					(i.e., "Apart" is the same as "APART"), and have a correct E (interval) part.
*
*				const char *units
*					The units of the data, such as "CFS".
*					The number of characters in units is not limited, but generally should not be greater than 25.
*
*				const char *type
*					The type of data.  Valid values include "PER-AVER", "PER-CUM", "INST-VAL", and "INST-CUM".
*					The number of characters in units is not limited, but generally should not be greater than 25.
*
*				int numberValues
*					The number of values to store (and the number of values in the data array and other optional arrays (e.g., quality))
*
*				float *floatValues
*					The float array containing the data to store if storing floats.  If storing doubles, this must be zero.
*			or
*				double *doubleValues
*					The double array containing the data to store if storing double.  If storing floats, this must be zero.
*
*	Regular-interval Required:
*
*				const char *startDate
*					The date of the first data value, such as "23MAR1985".  Multiple date formats are supported.
*
*				const char *startTime
*					The time of the first data value, such as "0700" or "07:00:00".
*
*	Irregular-interval Required:
*
*				int *itimes
*					An integer array containing the time of each data value to be stored.  The time can be
*					either in minutes (default) or seconds from the base date.  The default base date
*					is 01Jan1900 at 0000.  If times are given in seconds, you must use a base date closer to
*					the data set, as a large second value may be to big to store in an int.
*
*				int timeGranularitySeconds
*					The number of seconds a unit in *itimes represents, usually
*					MINUTE_GRANULARITY (60) or SECOND_GRANULARITY (1)
*					Note:  Irregular interval data conventions for DSS-7 stores time in seconds, except for century blocks.
*
*				const char* startDateBase (int julianBaseDate in struct)
*					A date that times in itimes is offset from, if not the default of 01Jan1900.
*					For example, if you had data that started 05Apr1986, you might choose 01Apr1986 as the base date
*					and then the first time in itimes would be something like (4 days * 1440 mins/day)
*					If you use the default base date of 01Jan1900, you can set this to either "" or zero.
*					The time is not given in the base date, but is assumed to be 0000.
*					If your base date was 01Apr1986 and itimes[i] was 60, that value would be at 01:00 on 01Apr1986.
*
*
*	Optional (You need to add to the struct):
*
*				int precision
*					An int that represents the precision, or number of decimal places, for all values.
*					This is the number of digits past the decimal.
*					For example, 0 gives a number like 123.  1 gives a number like 123.4, 2 1234.56, etc.
*					-1 indicates that this has not been set.
*
*				char *timeZoneName
*					The time zone name for this data set.  This is not necessarily the same as the location time zone,
*					although it usually is.  The name is generally that used in Java and includes a variety of types.
*					For example, you might have a data set recorded with GTM, although the location is PST.
*
*		Profiles:
*				int profileDepthsNumber
*					The number of values to store for each time.  For example, if you had measurements
*					at 0, 5, 10, 15 meters, this would be 4, and you have to have 4 values for every time.
*					If you are not storing profiles, set this to zero.
*
*				*floatProfileDepths or
*				*doubleProfileDepths
*					An array containing the depths (independent variable) for each time.  The same depths
*					are applied to each time; you cannot store varying depths under this convention.
*					You can store a missing flag in the values array, where an value is not used.
*					For example, this array might be 0.0, 5.0, 10.0, 15.0.
*					Dimension to floatProfileDepths[profileDepthsNumber].
*					Set the array that is not used to zero.
*
*				*floatProfileValues or
*				*doubleProfileValues
*					An array containing the values or measurements (dependent variable) for each time.
*					This would normally be a double dimensioned array, but C doesn't handle those well,
*					so this should contain the values for the first time, followed by the values for
*					the second time, and so forth.
*					For the above example, this array might be 72.1, 71.5, 70.2, 69.7, 72.3, 71.7, 70.5, 69.9, ...
*					Dimension to floatProfileValues[profileDepthsNumber][numberValues].  numberValues = number times.
*					Set the array that is not used to zero.
*
*		Quality:
*				int *quality
*					The array that contains quality or other additional information.  A single quality value
*					may be an int or multiple of an int.  For example, you might store 3 quality ints for
*					each value (time).  Set qualityElementSize to 3, and then have each set follow in the array.
*					Dimension to quality[qualityElementSize][numberValues].  numberValues = number times.
*					Set the array to zero if not used.
*
*				int qualityElementSize
*					The int size of each quality value to store, normally 1.  In the above example, this is 3,
*					indicating that the first three ints belong to the first value, the second three ints
*					belong the the second value, and so forth.
*					Set to zero if quality is not used.
*
*		Notes:
*			Notes can be stored either as a string for each value, or a specific number of ints
*			(essentially a second quality array).  You can store one or the other, not both.
*
*				char *cnotes
*					A character array containing a null terminated string for each value (time period).
*					For example, if you were storing 5 values, you might have a cnote array as follows:
*						"\0\0Gage failed to report\0Gage repaired\0\0"
*					You must have the same number of null terminated strings as values.
*					Dimension to cnotes[cnotesLengthTotal].  Set array to zero if not used.
*
*				int cnotesLengthTotal
*					The length (dimension) of the cnotes array in characters (bytes).  Set to zero if not used.
*
*				int *inotes
*					An array that contains additional information to store with each value.
*					The inotes array is similar to the quality array, and each inotes value
*					may be an int or multiple of an int.  For example, you might store 6 ints for
*					each value (time).  Set inoteElementSize to 6, and then have each set follow in the array.
*					Dimension to inotes[inoteElementSize][numberValues].  numberValues = number times.
*					Set the array to zero if not used.
*					You cannot use inotes if you use cnotes, as they use the same storage space,
*
*				int inoteElementSize
*					The int size of each inote value to store.
*					Set to zero if inotes are not used.
*
*
*				int *userHeader
*					An int array to hold any additional information for the record, usually from the user.
*
*				int userHeaderNumber
*					The number of int words to be stored in the userHeader
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int ztsStore(long long *ifltab, zStructTimeSeries *tss, int storageFlag)
{
	int intervalType;
	int version;
	int status;


	if (!tss) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStore_ID, zdssErrorCodes.NULL_ARGUMENT,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "zStructTimeSeries is null");
	}
	//  Some basic error checking
	if (!tss->pathname) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStore_ID,
			zdssErrorCodes.NULL_PATHNAME, 0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "Null pathname");
	}
	if (strlen(tss->pathname) < 10) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStore_ID,
			zdssErrorCodes.INVALID_PATHNAME, 0, 0, zdssErrorSeverity.INVALID_ARGUMENT, tss->pathname, "Invalid pathname");
	}
	if (tss->profileDepthsNumber == 0) {
		if (!tss->units) {
			tss->units = '\0';
		}
		if (!tss->type) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStore_ID,
				zdssErrorCodes.NULL_ARGUMENT, 0, 0, zdssErrorSeverity.WARNING, tss->pathname, "Null type");
		}
		if (tss->numberValues < 1) {
			status = zerrorProcessing(ifltab, DSS_FUNCTION_ztsStore_ID, zdssErrorCodes.NO_DATA_GIVEN,
				tss->numberValues, 0, zdssErrorSeverity.WARNING, tss->pathname, "");
		}
		if (!tss->floatValues && !tss->doubleValues && !tss->floatProfileValues && !tss->doubleProfileValues) {
			status = zerrorProcessing(ifltab, DSS_FUNCTION_ztsStore_ID, zdssErrorCodes.NO_DATA_GIVEN,
				tss->numberValues, 0, zdssErrorSeverity.WARNING, tss->pathname, "");
		}
	}

	//  Messages and debug
	if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessage(ifltab, " ");
		zmessageDebug(ifltab, DSS_FUNCTION_ztsStore_ID, "Enter ztsStore; Pathname: ", tss->pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsStore_ID, "storageFlag: ", storageFlag);
		zmessageDebug(ifltab, DSS_FUNCTION_ztsStore_ID, "Time window prior to processing: ", "");
		ztsMessTimeWindow(ifltab, DSS_FUNCTION_ztsStore_ID, tss);		
	}

	//  Determine if regular interval or irregular interval
	//  and get time window information
	//  Returns -1 if error, 0 if regular interval, 1 if irregular interval
	intervalType =  ztsProcessTimes(ifltab, tss, 1);

	if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_USER_DIAG) || zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessage2(ifltab, "Time window specified for record: ", tss->pathname);
		ztsMessTimeWindow(ifltab, DSS_FUNCTION_ztsProcessTimes_ID, tss);
	}

	if (intervalType < 0) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStore_ID,
				zdssErrorCodes.INVALID_INTERVAL, intervalType,
				0, zdssErrorSeverity.WARNING, tss->pathname, "");
	}

	if (tss->boolPattern) {
		status = ztsStorePattern(ifltab, tss);
		/////////////////////////
		//////////  FIX ME - Location???
		return status;
	}

	if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessage(ifltab, " ");
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsStore_ID, "intervalType: ", intervalType);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsStore_ID, "storageFlag: ", storageFlag);
		if (intervalType == 0) {
			if (storageFlag == 0) zmessageDebug(ifltab, DSS_FUNCTION_ztsStore_ID, "storageFlag = 0:  Always replace data.", "");
			if (storageFlag == 1) zmessageDebug(ifltab, DSS_FUNCTION_ztsStore_ID, "storageFlag = 1:  Only replace missing data.", "");
			if (storageFlag == 2) zmessageDebug(ifltab, DSS_FUNCTION_ztsStore_ID, "storageFlag = 2  Write regardless, even if all missing data (write a missing record).", "");
			if (storageFlag == 3) zmessageDebug(ifltab, DSS_FUNCTION_ztsStore_ID, "storageFlag = 3  If a record is all missing, do not write it and delete it.", "");
			if (storageFlag == 4) zmessageDebug(ifltab, DSS_FUNCTION_ztsStore_ID, "storageFlag = 4  Do not allow a missing input data to replace a valid data.", "");
		}
		else if (intervalType == 1) {
			if (storageFlag == 0) zmessageDebug(ifltab, DSS_FUNCTION_ztsStore_ID, "storageFlag = 0  merge datasets.", "");
			if (storageFlag == 1) zmessageDebug(ifltab, DSS_FUNCTION_ztsStore_ID, "storageFlag = 1  replace datasets.", "");
		}
		if (tss->floatValues) {
			zmessageDebug(ifltab, DSS_FUNCTION_ztsStore_ID, "Float values provided.", "");
		}
		else if (tss->doubleValues) {
			zmessageDebug(ifltab, DSS_FUNCTION_ztsStore_ID, "Double values provided.", ""); 
		}
		else if (tss->floatProfileDepths) {
			zmessageDebug(ifltab, DSS_FUNCTION_ztsStore_ID, "Float profiles values provided.", ""); 
		}
		else if (tss->doubleProfileDepths) {
			zmessageDebug(ifltab, DSS_FUNCTION_ztsStore_ID, "Double profiles values provided.", "");
		}
		else {
			zmessageDebug(ifltab, DSS_FUNCTION_ztsStore_ID, "NO values provided.", "  Will cause an error shortly.");
		}
	}


	version = zgetVersion(ifltab);
	if (version == 6) {
		if (tss->profileDepthsNumber > 0) {
			//  Profiles are not implemented in version 6
			return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStore_ID,
				zdssErrorCodes.INCOMPATIBLE_CALL, 0,
				0, zdssErrorSeverity.WARNING, tss->pathname,
				"Profiles are not implemented in version 6");
		}
		if (intervalType == 0) {
			status = ztsStoreReg6(ifltab, tss, storageFlag);
		}
		else {
			status = ztsStoreIrreg6(ifltab, tss, storageFlag);
		}
	}
	else {
		int allowOverwriteLocationVerticalDatum;
		if (!zinquire(ifltab, "write")) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStore_ID,
				zdssErrorCodes.WRITE_ON_READ_ONLY, 0, 0,
				zdssErrorSeverity.WARNING_NO_WRITE_ACCESS, tss->pathname, "");
		}
		if (tss->profileDepthsNumber > 0) {
			if (tss->floatValues || tss->doubleValues) {
				//  Error out
				//  Programming error - profile values must only be in profile struct
				//  values detected in time series struct.
				return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStore_ID,
				zdssErrorCodes.INCOMPATIBLE_CALL, 0,
				0, zdssErrorSeverity.WARNING, tss->pathname,
				"Either doubles or floats may be stored, not both");
			}
		}
		//------------------------------------//
		// START OF VERTICAL DATUM PROCESSING //
		//------------------------------------//
		float  *tmpFloatVals = NULL;
		float  *origFloatVals = NULL;
		double *tmpDoubleVals = NULL;
		double *origDoubleVals = NULL;
		if (tss->floatValues || tss->doubleValues) {
			//--------------------------------------------------------//
			// see if we have one or more vertical_datum_info objects //
			//--------------------------------------------------------//
			vertical_datum_info *vdi;
			vertical_datum_info *vdiTs  = NULL;
			vertical_datum_info *vdiLoc = NULL;
			vertical_datum_info _vdiTs;
			vertical_datum_info _vdiLoc;
			//------------------------------------------------------//
			// get the info from the time series struct user header //
			//------------------------------------------------------//
			vdiTs = vertical_datum_info_from_user_header(tss->userHeader, tss->userHeaderSize);
			if (tss->locationStruct && tss->locationStruct->supplemental) {
				//------------------------------------------------------------------------------//
				// none in the user header, see if any is passed in in embedded location struct //
				//------------------------------------------------------------------------------//
				char *vdiStr = extract_from_delimited_string(
					&tss->locationStruct->supplemental,
					VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
					":",
					TRUE,
					FALSE,
					'\n');
				if (vdiStr) {
					vertical_datum_info_from_string(&_vdiTs, vdiStr);
					vdiTs = &_vdiTs;
					free(vdiStr);
				}
			}
			//-----------------------------------------------//
			// get the info from the location struct on disk //
			//-----------------------------------------------//
			zStructLocation *ls = zstructLocationNew(tss->pathname);
			zlocationRetrieve(ifltab, ls);
			if (ls && ls->supplemental) {
				char *vdiStr = extract_from_delimited_string(
					&ls->supplemental,
					VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
					":",
					TRUE,
					FALSE,
					'\n');
				if (vdiStr) {
					vertical_datum_info_from_string(&_vdiLoc, vdiStr);
					vdiLoc = &_vdiLoc;
					free(vdiStr);
				}
			}
			if (vdiTs && vdiLoc) {
				zquery("VDOW", "", 0, &allowOverwriteLocationVerticalDatum);
				if (allowOverwriteLocationVerticalDatum) {
					vdi = vdiTs;
				}
				else {
					//-------------------------------------------------------------------//
					// We have 2 sources of information, abort if they're not equivalent //
					//-------------------------------------------------------------------//
					char errmsg[1024];
					//-----------------------//
					// compare native datums //
					//-----------------------//
					if (strcmp(vdiTs->native_datum, vdiLoc->native_datum)) {
						sprintf(
							errmsg, 
							"\nIncoming native vertical datum of '%s' conflicts with location's native datum of '%s'.\n"
							"Call 'zset(\"VDOW\", \"\", 1)' to allow overwriting the location's vertical datum information.\n"
							"Conversion to datum '%s' was not performed.\n"
							"No data stored.", 
							vdiTs->native_datum, vdiLoc->native_datum, vdiLoc->native_datum);
						return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStore_ID,
							zdssErrorCodes.INCOMPATIBLE_CALL, 0,
							0, zdssErrorSeverity.WARNING, tss->pathname,
							errmsg);
					}
					//--------------------//
					// compare elevations //
					//--------------------//
					if (vdiTs->elevation != vdiLoc->elevation) {
						sprintf(
							errmsg, 
							"\nIncoming elevation of %f %s conflicts with location's elevation of %f %s.\n"
							"Call 'zset(\"VDOW\", \"\", 1)' to allow overwriting the location's vertical datum information.\n"
							"Conversion to datum '%s' was not performed.\n"
							"No data stored.", 
							vdiTs->elevation, vdiTs->unit, vdiLoc->elevation, vdiLoc->unit);
						return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStore_ID,
							zdssErrorCodes.INCOMPATIBLE_CALL, 0,
							0, zdssErrorSeverity.WARNING, tss->pathname,
							errmsg);
					}
					//----------------------//
					// compare offset units //
					//----------------------//
					if (strcmp(vdiTs->unit, vdiLoc->unit)) {
						sprintf(
							errmsg, 
							"\nIncoming vertical datum offset unit of %s conflicts with location's unit of %s.\n"
							"Call 'zset(\"VDOW\", \"\", 1)' to allow overwriting the location's vertical datum information.\n"
							"Conversion to datum '%s' was not performed.\n"
							"No data stored.", 
							vdiTs->unit, vdiLoc->unit);
						return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStore_ID,
							zdssErrorCodes.INCOMPATIBLE_CALL, 0,
							0, zdssErrorSeverity.WARNING, tss->pathname,
							errmsg);
					}
					//------------------------//
					// compare NAVD-88 offset //
					//------------------------//
					if (vdiTs->offset_to_navd_88 != vdiLoc->offset_to_navd_88) {
						sprintf(
							errmsg, 
							"\nIncoming offset to NAVD-88 of %f %s conflicts with location's offset of %f %s.\n"
							"Call 'zset(\"VDOW\", \"\", 1)' to allow overwriting the location's vertical datum information.\n"
							"Conversion to datum '%s' was not performed.\n"
							"No data stored.", 
							vdiTs->offset_to_navd_88, vdiTs->unit, vdiLoc->offset_to_navd_88, vdiLoc->unit);
						return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStore_ID,
							zdssErrorCodes.INCOMPATIBLE_CALL, 0,
							0, zdssErrorSeverity.WARNING, tss->pathname,
							errmsg);
					}
					//------------------------//
					// compare NGVD-29 offset //
					//------------------------//
					if (vdiTs->offset_to_ngvd_29 != vdiLoc->offset_to_ngvd_29) {
						sprintf(
							errmsg, 
							"\nIncoming offset to NGVD_29 of %f %s conflicts with existing offset of %f %s.\n"
							"Call 'zset(\"VDOW\", \"\", 1)' to allow overwriting the location's vertical datum information.\n"
							"Conversion to datum '%s' was not performed.\n"
							"No data stored.", 
							vdiTs->offset_to_ngvd_29, vdiTs->unit, vdiLoc->offset_to_ngvd_29, vdiLoc->unit);
						return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStore_ID,
							zdssErrorCodes.INCOMPATIBLE_CALL, 0,
							0, zdssErrorSeverity.WARNING, tss->pathname,
							errmsg);
					}
					vdi = vdiLoc;
				}
			}
			else {
				//--------------------------//
				// use the only one we have //
				//--------------------------//
				vdi = vdiTs ? vdiTs : vdiLoc;
			}
			if (vdi) {
				double offset = 0.;
				//----------------------------------------//
				// get the default vertical datum, if any //
				//----------------------------------------//
				char cvertical_datum[17];
				int  ivertical_datum = -1;
				int  ivertical_datum2;
				zquery("VDTM", cvertical_datum, sizeof(cvertical_datum), &ivertical_datum);
				ivertical_datum2 = ivertical_datum;
				//-------------------------------------------------------------------------//
				// override with the vertical datum specified in user header datum, if any //
				//-------------------------------------------------------------------------//
				char *userHeaderString = string_from_user_header(tss->userHeader, tss->userHeaderSize);
				if (userHeaderString) {
					char *headerDatum = extract_from_delimited_string(
						&userHeaderString, 
						VERTICAL_DATUM_USER_HEADER_PARAM, 
						":",
						TRUE, 
						FALSE,
						';');
					if (headerDatum) {
						if (!strcmp(headerDatum, CVERTICAL_DATUM_NAVD88)) {
							ivertical_datum = IVERTICAL_DATUM_NAVD88;
						}
						else if (!strcmp(headerDatum, CVERTICAL_DATUM_NGVD29)) {
							ivertical_datum = IVERTICAL_DATUM_NGVD29;
						}
						else {
							ivertical_datum = IVERTICAL_DATUM_OTHER;
						}
						strcpy(cvertical_datum, headerDatum);
						free(headerDatum);
						//------------------------------------------------------------//
						// remove the datum from the user header since it's transient //
						//------------------------------------------------------------//
						free(extract_from_delimited_string(
							&userHeaderString, 
							VERTICAL_DATUM_USER_HEADER_PARAM, 
							":",
							TRUE, 
							TRUE,
							';'));
						free(headerDatum);
						int newHeaderSize;
						int *newHeader = string_to_user_header(userHeaderString, &newHeaderSize);
						free(tss->userHeader);
						tss->userHeader = newHeader;
						tss->userHeaderNumber = tss->userHeaderSize = newHeaderSize;
					}
					free(userHeaderString);
				}
				//-----------------------------------------------------------------//
				// override with the vertical datum specified in unit spec, if any //
				//-----------------------------------------------------------------//
				if (strchr(tss->units, '|')) {
					char  strtok_buf[256];
					char *value;
					char *unit = NULL;
					char *vertical_datum = NULL;
					char *unitSpec = mallocAndCopy(tss->units);
					char *key = strtok_r(unitSpec, "|=", (char **)&strtok_buf);
					while (key) {
						value = strtok_r(NULL, "|=", (char **)&strtok_buf);
						if (!strcasecmp(key, "U")) {
							unit = value;
						}
						else if (!strcasecmp(key, "V")) {
							vertical_datum = value;
						}
						key = strtok_r(NULL, "|=", (char **)&strtok_buf);
					}
					if (unit) {
						free(tss->units);
						tss->units = mallocAndCopy(unit);
					}
					if (vertical_datum) {
						if (!strcasecmp(vertical_datum, CVERTICAL_DATUM_NAVD88)) {
							ivertical_datum = IVERTICAL_DATUM_NAVD88;
						}
						else if (!strcasecmp(vertical_datum, CVERTICAL_DATUM_NGVD29)) {
							ivertical_datum = IVERTICAL_DATUM_NGVD29;
						}
						else {
							ivertical_datum = IVERTICAL_DATUM_OTHER;
						}
						strcpy(cvertical_datum, vertical_datum);
					}
					free(unitSpec);
				}
				//-------------------------------------------------------//
				// now that we have a datum, determine the offset to use //
				//-------------------------------------------------------//
				switch(ivertical_datum) {
					case IVERTICAL_DATUM_NAVD88 :
						offset = vdi->offset_to_navd_88;
						break;
					case IVERTICAL_DATUM_NGVD29 :
						offset = vdi->offset_to_ngvd_29;
						break;
					case IVERTICAL_DATUM_OTHER :
						if (strcasecmp(cvertical_datum, vdi->native_datum)) {
							offset = UNDEFINED_VERTICAL_DATUM_VALUE;
						}
						else {
							offset = 0;
						}
						break;
					default :
						offset = 0;
						break;
				}
				if (offset != 0.) {
					if (offset == UNDEFINED_VERTICAL_DATUM_VALUE) {
						char errmsg[256];
						sprintf(
							errmsg, 
							"\nVertical datum offset is undefined for datum '%s'.\n"
							"Datum conversion could not be performed.\n"
							"No data stored.", 
							cvertical_datum);
						return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStore_ID,
							zdssErrorCodes.INCOMPATIBLE_CALL, 0,
							0, zdssErrorSeverity.WARNING, tss->pathname,
							errmsg);
					}
					offset = get_offset(offset, vdi->unit, tss->units);
					if (offset == UNDEFINED_VERTICAL_DATUM_VALUE) {
						char errmsg[256];
						sprintf(
							errmsg, 
							"\nData unit (%s) and/or offset unit (%s) is invalid for vertical datum conversion.\n"
							"Conversion to datum '%s' could not be performed.\n"
							"No data stored.", 
							tss->units, vdi->unit, cvertical_datum);
						return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStore_ID,
							zdssErrorCodes.INCOMPATIBLE_CALL, 0,
							0, zdssErrorSeverity.WARNING, tss->pathname,
							errmsg);
					}
					//--------------------------------------------------------------------------//
					// use the offset to put the values back to the native datum before storing //
					//--------------------------------------------------------------------------//
					// use temporary arrays so the values stored to disk are modified but the   //
					// values in the zstructTs object aren't modified after the call            //
					//--------------------------------------------------------------------------//
					if (offset != 0.) {
						if (tss->floatValues) {
							tmpFloatVals = (float *)calloc(tss->numberValues, sizeof(float));
							for (int i = 0; i < tss->numberValues; ++i) {
								tmpFloatVals[i] = tss->floatValues[i] - offset;
							}
							origFloatVals = tss->floatValues;
							tss->floatValues = tmpFloatVals;
						}
						else if (tss->doubleValues) {
							tmpDoubleVals = (double *)calloc(tss->numberValues, sizeof(double));
							for (int i = 0; i < tss->numberValues; ++i) {
								tmpDoubleVals[i] = tss->doubleValues[i] - offset;
							}
							origDoubleVals = tss->doubleValues;
							tss->doubleValues = tmpDoubleVals;
						}
					}
				}
				if (vdi == vdiTs) {
					//----------------------------------------------------------------------------//
					// move the vertical datum info into the time series struct embedded location //
					//----------------------------------------------------------------------------//
					if (!tss->locationStruct) {
						tss->locationStruct = zstructLocationNew(tss->pathname);
					}
					tss->locationStruct->verticalUnits = unit_is_feet(vdi->unit) ? 1 : 2;
					if (!strcmp(vdi->native_datum, CVERTICAL_DATUM_NAVD88)) {
						tss->locationStruct->verticalDatum = IVERTICAL_DATUM_NAVD88;
					}
					else if (!strcmp(vdi->native_datum, CVERTICAL_DATUM_NGVD29)) {
						tss->locationStruct->verticalDatum = IVERTICAL_DATUM_NGVD29;
					}
					else {
						tss->locationStruct->verticalDatum = IVERTICAL_DATUM_OTHER;
					}
					char *compressed = NULL;
					vertical_datum_info_to_string(&compressed, vdi, TRUE);
					char errmsg[256];
					if (compressed) { // should never be NULL
						if (tss->locationStruct->supplemental) {
							status = insert_into_delimited_string(
								&tss->locationStruct->supplemental,
								strlen(tss->locationStruct->supplemental),
								VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
								compressed,
								":",
								TRUE,
								'\n');
							if (status) { // not enough space to insert
								int newLen = 
									strlen(tss->locationStruct->supplemental) +
									VERTICAL_DATUM_INFO_USER_HEADER_PARAM_LEN + 
									strlen(compressed) +
									3;
								tss->locationStruct->supplemental = (char *)realloc(
									tss->locationStruct->supplemental,
									newLen);
								status = insert_into_delimited_string(
									&tss->locationStruct->supplemental,
									newLen,
									VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
									compressed,
									":",
									TRUE,
									'\n');
								if (status) { // unexpected error
									if (vdiTs != &_vdiTs) {
										free(vdiTs);
									}
									sprintf(
										errmsg, 
										"\nVertical datum information could not be assigned to location record.\n"
										"No data stored.");
									return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStore_ID,
										zdssErrorCodes.INCOMPATIBLE_CALL, 0,
										0, zdssErrorSeverity.WARNING, tss->pathname,
										errmsg);
								}
							}
						}
						else {
							int len = 
								VERTICAL_DATUM_INFO_USER_HEADER_PARAM_LEN + 
								strlen(compressed) +
								3;
							tss->locationStruct->supplemental = (char *)malloc(len);
							memset(tss->locationStruct->supplemental, 0, len);
							status = insert_into_delimited_string(
								&tss->locationStruct->supplemental,
								len,
								VERTICAL_DATUM_INFO_USER_HEADER_PARAM,
								compressed,
								":",
								TRUE,
								'\n');
							if (status) { // unexpected error
								if (vdiTs != &_vdiTs) {
									free(vdiTs);
								}
								sprintf(
									errmsg, 
									"\nVertical datum information could not be assigned to location record.\n"
									"No data stored.");
								return zerrorProcessing(ifltab, DSS_FUNCTION_ztsStore_ID,
									zdssErrorCodes.INCOMPATIBLE_CALL, 0,
									0, zdssErrorSeverity.WARNING, tss->pathname,
									errmsg);
							}
						}
					}
				}
				if (vdiTs != &_vdiTs) {
					//--------------------------------------------------------------------//
					// remove the vertical datum info from the user header before storing //
					//--------------------------------------------------------------------//
					char *userHeaderString = string_from_user_header(tss->userHeader, tss->userHeaderSize);
					if (userHeaderString) {
						char *vdiStr = extract_from_delimited_string(
							&userHeaderString, 
							VERTICAL_DATUM_INFO_USER_HEADER_PARAM, 
							":",
							TRUE, 
							TRUE,
							';');
						if (vdiStr) {
							free(vdiStr);
							int newHeaderSize;
							int *newHeader = string_to_user_header(userHeaderString, &newHeaderSize);
							free(tss->userHeader);
							tss->userHeader = newHeader;
							tss->userHeaderNumber = tss->userHeaderSize = newHeaderSize;
							// don't free newHeader - the zstructFree() call will get it.
						}
						free(userHeaderString);
					}
					free(vdiTs);
				}
			}
		}
		//----------------------------------//
		// END OF VERTICAL DATUM PROCESSING //
		//----------------------------------//
		if (intervalType == 0) {
			status = ztsStoreReg7(ifltab, tss, storageFlag);
		}
		else {
			status = ztsStoreIrreg7(ifltab, tss, storageFlag);
		}
		//----------------------------------------------//
		// restore the original values to the structure //
		//----------------------------------------------//
		if (tmpFloatVals != NULL) {
			tss->floatValues = origFloatVals;
			free(tmpFloatVals);
		}
		if (tmpDoubleVals != NULL) {
			tss->doubleValues = origDoubleVals;
			free(tmpFloatVals);
		}
		if ((tss->locationStruct) && (status == STATUS_OKAY)) {
			if (tss->locationStruct->pathnameInternal && tss->locationStruct->allocated[zSTRUCT_locationPathInternal]) {
				free(tss->locationStruct->pathnameInternal);
			}
			tss->locationStruct->pathnameInternal = mallocAndCopy(tss->pathname);
			tss->locationStruct->allocated[zSTRUCT_locationPathInternal] = 1;
			zlocationStore(ifltab, tss->locationStruct, allowOverwriteLocationVerticalDatum);
		}
	}


	if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztsStore_ID, "Exit ztsStore; Pathname: ", tss->pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsStore_ID, "Status: ", status);
		zmessage(ifltab, " ");
	}

	return status;
}


