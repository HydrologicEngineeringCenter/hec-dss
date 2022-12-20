#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "fortran_sting_len_size.h"

/**
*  Function:	zlocationStore
*
*  Use:			Semi-Public
*
*  Description:	Store location information for a dataset
*
*  Declaration: int zlocationStore(long long *ifltab, zStructLocation *locationStruct, int storageFlag)
*
*
*  Parameters:	long long *ifltab
*					The ifltab array used with zopen
*
*				zStructLocation *locationStruct
*					The location struct being stored.
*
*				int storageFlag
*					0 = Do not write over an existing location record (no update)
*					1 = Overwrite an existing location record (update)
*
*
*	Returns:	int status
*					STATUS_OKAY
*					error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*
*  Remarks:		Location information belongs to a point that several datasets can share.
*					A location pathname is in the form:
*						/A part/B part/Location Info////
*
*
*  zStructLocation:
*
*				public double xOrdinate - Longitude, Easting or decimal degrees (negative for Western Hemisphere)
*
*				public double yOrdinate - Latitude, Northing or decimal degrees
*
*				public double zOrdinate � Elevation
*
*				public int coordinateSystem � int representing coordinate system:
*					0 - no coordinates set
*					1 � Latitude / Longitude
*					2 - State Plane, FIPS
*					3 - State Plane, ADS
*					4 � UTM
*					5 - Local (other)
*
*				public int coordinateID - UTM zone #, or FIPS SPCS # ADS SPCS #
*
*				public int horizontalUnits � int representing the horizontal units:
*					0 - unset
*					1 � Feet
*					2 - Meters
*					3 - Decimal Degrees
*					4 � Degrees / Minutes / Seconds
*					5 - Local (other)
*
*				public int horizontalDatum � int representing the horizontal datum:
*					0 - unset
*					1 � NAD83
*					2 - NAD27
*					3 - WGS84
*					4 � WGS72
*					5 - Local (other)
*
*				public int verticalUnits � int representing the vertical units:
*					0 - unset
*					1 � feet
*					2 - meters
*
*				public int verticalDatum � int representing the vertical datum:
*					0 - unset
*					1 � NAVD88
*					2 � NGVD29
*					3 - Local (other)
*
*				public String locationTimezone � The (Java) time zone name at the location (not the data).
*					"America/New York" is preferred, but "EST" may be acceptable.
*					Daylight savings time is a component of a time zone, not a time zone itself,
*					so "EDT" would be rejected by Java (although you can save anything here.)
*
*				public String supplementalInfo � Any additional information that describes the location.
*					When stored, supplementalInfo will be null terminated.
*					You can separate segments within supplementalInfo using a new line character (\n),
*					or you can use an XML format, or you can use a "Keyword:Item;" format,
*					where the keyword is separated from the item by a colon (:), and the item is
*					terminated by a semi-colon (;).  Keyword1:Item1;Keyword2:Item2;Keyword3:Item3;
*					For example:
*						Offset:1.2;Shift:3.4;Datum:5.6;Transform:LogLog;
*
*
*
*
*  See Also:	zlocationRetrieve
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int zlocationStore(long long *ifltab, zStructLocation *locationStruct, int storageFlag)
{
	int len;
	int status;
	int nwords;
	double coordinates[3];
	int values1[30];
	int values1Size = 30;
	int values1Number;
	char *locationPathname;
	char *path;

	zStructTransfer* ztransfer;


	if (!locationStruct) return STATUS_NOT_OKAY;

	if (zgetVersion(ifltab) != 7) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zlocationStore_ID,
			zdssErrorCodes.INCOMPATIBLE_VERSION,
			zgetVersion((void *)ifltab), 0, zdssErrorSeverity.WARNING, "", "");
	}

	//   See if there is anything to store....
	if (zlocationStructValid(locationStruct) == STATUS_NOT_OKAY) {
		//  Nope, nothing to store
		return STATUS_OKAY;
	}

	//  We do have data to store
	if (locationStruct->pathname) {
		path = locationStruct->pathname;
	}
	else if (locationStruct->pathnameInternal) {
		path = locationStruct->pathnameInternal;
	}
	else {
		// error out
		return zerrorProcessing(ifltab, DSS_FUNCTION_zlocationStore_ID,
				zdssErrorCodes.INVALID_PATHNAME, 0, 0,
				zdssErrorSeverity.WARNING, "No pathname given", "");
	}

	locationPathname = zlocationPath(path);

	//  Messages and debug
	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zlocationStore_ID, "Enter; Original pathname: ", path);
		zmessageDebug(ifltab, DSS_FUNCTION_zlocationStore_ID, "Location pathname: ", locationPathname);
		if (storageFlag == 0) {
			zmessageDebug(ifltab, DSS_FUNCTION_zlocationStore_ID, "Storage flag set to not over write existing record", "");
		}
		else if (storageFlag == 1) {
			zmessageDebug(ifltab, DSS_FUNCTION_zlocationStore_ID, "Storage flag set to over write existing record", "");
		}
		else {
			zmessageDebug(ifltab, DSS_FUNCTION_zlocationStore_ID, "*** Invalid Storage flag", "");
		}
	}

	if (storageFlag == 0) {
		//  Don't update if the data set already exists
		status = zcheck(ifltab, locationPathname);
		if (status == STATUS_RECORD_FOUND) {
				if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
					zmessageDebug(ifltab, DSS_FUNCTION_zlocationStore_ID, "Record already exists and storage flag set to not overwrite", "");
					zmessageDebug(ifltab, DSS_FUNCTION_zlocationStore_ID, "Location pathname: ", locationPathname);
				}
				free(locationPathname);
			return status;
		}
	}

	ztransfer = zstructTransferNew(locationPathname, 0);
	free(locationPathname);
	if (!ztransfer) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zlocationStore_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
								zdssErrorSeverity.MEMORY_ERROR,
								path, "Allocating ztransfer struct");
	}


	values1Number = 0;

	//  X, Y, Z coordinates
	coordinates[0] = locationStruct->xOrdinate;
	coordinates[1] = locationStruct->yOrdinate;
	coordinates[2] = locationStruct->zOrdinate;
	convertDataType((void *)coordinates, &values1[values1Number], 6, 6);
	values1Number += 6;

	values1[values1Number++] = locationStruct->coordinateSystem;
	values1[values1Number++] = locationStruct->coordinateID;
	values1[values1Number++] = locationStruct->horizontalUnits;
	values1[values1Number++] = locationStruct->horizontalDatum;
	values1[values1Number++] = locationStruct->verticalUnits;
	values1[values1Number++] = locationStruct->verticalDatum;


	//  Time zone name
	if (values1Size > 14) {
		if (locationStruct->timeZoneName) {
			len = (int)strlen(locationStruct->timeZoneName);
			if (len > 0) {
				nwords = numberIntsInBytes(len);
				if ((nwords + values1Number) <= values1Size) {
					charInt(locationStruct->timeZoneName, (void *)&values1[values1Number], len, nwords*4, 1, 1, isOdd(values1Number));
					values1Number += nwords;
				}
			}
		}
	}

	if (locationStruct->supplemental) {
		len = (int)strlen(locationStruct->supplemental);
		ztransfer->values2Number = numberIntsInBytes(len+1);
		ztransfer->values2 = (int *)calloc(ztransfer->values2Number, 4);
		charInt(locationStruct->supplemental, (void *)ztransfer->values2, len, ztransfer->values2Number * 4, 1, 0, 0);
	}
	else {
		ztransfer->values2Number  = 0;
	}


	ztransfer->dataType = DATA_TYPE_LOCATION;
	ztransfer->numberValues = values1Number;;
	ztransfer->values1 = values1;
	ztransfer->values1Number = values1Number;

	//  Update perm section, if needed
	zlocationUpdatePerm(ifltab, locationStruct);

	status = zwrite(ifltab, ztransfer);

	ztransfer->values1 = 0;
	if (ztransfer->values2) {
		free(ztransfer->values2);
		ztransfer->values2 = 0;
	}

	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zlocationStore_ID, "Exit; pathname: ", ztransfer->pathname);
	}

	zstructFree(ztransfer);
	return status;
}

//  FORTRAN Interface

void zlocationstore_(long long *ifltab, const char *path,
					double coordinates[3], int coordinateDescription[6],
					char *timeZoneName, char *supplemental, int *status,
					slen_t pathLen, slen_t timeZoneLen, slen_t supplementalLen)
{
	char *pathname;
	zStructLocation* locationStruct;


	pathname = stringFortToC(path, pathLen);
	locationStruct = zstructLocationNew(pathname);
	free(pathname);
	if (!locationStruct) {
		//  error out
		zstructFree(locationStruct);
		*status = zerrorProcessing(ifltab, DSS_FUNCTION_zlocationStore_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
								zdssErrorSeverity.MEMORY_ERROR,
								path, "Allocating ztransfer struct");
		return;
	}

	locationStruct->xOrdinate = coordinates[0];
	locationStruct->yOrdinate = coordinates[1];
	locationStruct->zOrdinate = coordinates[2];

	locationStruct->coordinateSystem = coordinateDescription[0];
	locationStruct->coordinateID = coordinateDescription[1];
	locationStruct->horizontalUnits = coordinateDescription[2];
	locationStruct->horizontalDatum = coordinateDescription[3];
	locationStruct->verticalUnits = coordinateDescription[4];
	locationStruct->verticalDatum = coordinateDescription[5];

	if (timeZoneLen > 0) {
		locationStruct->timeZoneName = stringFortToC(timeZoneName, timeZoneLen);
		locationStruct->allocated[zSTRUCT_timeZoneName] = 1;
	}
	if (supplementalLen > 0) {
		locationStruct->supplemental = stringFortToC(supplemental, supplementalLen);
		locationStruct->allocated[zSTRUCT_otherInformation] = 1;
	}

	*status = zlocationStore(ifltab, locationStruct, 1);

	zstructFree(locationStruct);

}

