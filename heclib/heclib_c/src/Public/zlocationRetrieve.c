#include <string.h>
#include <stdlib.h>

#include "heclib7.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"


/**
*  Function:	zlocationRetrieve
*
*  Use:			Semi-Public
*
*  Description:	Retrieves location information for a dataset
*
*  Declaration: int zlocationRetrieve(long long *ifltab, zStructLocation *locationStruct)
*
*
*  Parameters:	long long *ifltab
*					The ifltab array used with zopen
*
*				zStructLocation *locationStruct
*					A location struct that will contain location information
*
*
*	Returns:	int status
*					STATUS_RECORD_FOUND
*					STATUS_RECORD_NOT_FOUND
*					STATUS_NOT_OKAY
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
*					You can separate segments within supplemental Info using a new line character (\n),
*					or you can use an XML format, or you can use a "Keyword:Item;" format,
*					where the keyword is separated from the item by a colon (:), and the item is
*					terminated by a semi-colon (;).  Keyword1:Item1;Keyword2:Item2;Keyword3:Item3;
*					For example:
*						Offset:1.2;Shift:3.4;Datum:5.6;Transform:LogLog;
*
*
*
*
*  See Also:	zlocationStore
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zlocationRetrieve(long long *ifltab, zStructLocation *locationStruct)
{

	int status;
	int nwords;
	int count;
	int len;
	char *path;
	double coordinates[3];

	char *locationPathname;

	zStructTransfer* ztransfer;



	if (!locationStruct) return STATUS_NOT_OKAY;

	if (zgetVersion(ifltab) != 7) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zlocationRetrieve_ID,
			zdssErrorCodes.INCOMPATIBLE_VERSION,
			zgetVersion((void *)ifltab), 0, zdssErrorSeverity.WARNING, "", "");
	}

	if (locationStruct->pathname) {
		path = locationStruct->pathname;
	}
	else if (locationStruct->pathnameInternal) {
		path = locationStruct->pathnameInternal;
	}
	else {
		// error out
		return zerrorProcessing(ifltab, DSS_FUNCTION_zlocationRetrieve_ID,
				zdssErrorCodes.INVALID_PATHNAME, 0, 0,
				zdssErrorSeverity.WARNING, "No pathname given", "");
	}

	locationPathname = zlocationPath(path);

	//  Messages and debug
	if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zlocationRetrieve_ID, "Enter; Original pathname: ", path);
		zmessageDebug(ifltab, DSS_FUNCTION_zlocationRetrieve_ID, "Location pathname: ", locationPathname);
	}

	ztransfer = zstructTransferNew(locationPathname, 1);
	free(locationPathname);
	if (!ztransfer) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zlocationRetrieve_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
								zdssErrorSeverity.MEMORY_ERROR,
								path, "Allocating ztransfer struct");
	}

	status = zread(ifltab, ztransfer);
	if (status != STATUS_RECORD_FOUND) {
		zstructFree(ztransfer);
		return status;
	}

	if (ztransfer->values1Number < 6) {
		//  no data, error out.
		zstructFree(ztransfer);
		return STATUS_NOT_OKAY;
	}


	count = 0;

	//  X, Y, Z coordinates
	if (ztransfer->values1Number >= 6) {
		convertDataType(&ztransfer->values1[count], (void *)coordinates, 6, 6);
		locationStruct->xOrdinate = coordinates[0];
		locationStruct->yOrdinate = coordinates[1];
		locationStruct->zOrdinate = coordinates[2];
		count += 6;
	}

	if (ztransfer->values1Number >= 12) {
		locationStruct->coordinateSystem = ztransfer->values1[count++];
		locationStruct->coordinateID = ztransfer->values1[count++];
		locationStruct->horizontalUnits = ztransfer->values1[count++];
		locationStruct->horizontalDatum = ztransfer->values1[count++];
		locationStruct->verticalUnits = ztransfer->values1[count++];
		locationStruct->verticalDatum = ztransfer->values1[count++];
	}


	//  Time zone name
	if (ztransfer->values1Number > 12) {
		nwords = ztransfer->values1Number - count;
		len = nwords * 4;
		if (len > 0) {
			locationStruct->timeZoneName = (char *)malloc(len + 8);
			locationStruct->allocated[zSTRUCT_timeZoneName] = 1;
			charInt ((void *)&ztransfer->values1[count], locationStruct->timeZoneName, len, len, 0, 1, 0);
		}
	}

	//  Supplemental information
	if (ztransfer->values2Number > 0) {
		len = ztransfer->values2Number * 4;
		locationStruct->supplemental = (char *)malloc(len + 8);
		locationStruct->allocated[zSTRUCT_otherInformation] = 1;
		charInt ((void *)ztransfer->values2, locationStruct->supplemental, len, len, 0, 1, 0);
	}

	if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zlocationRetrieve_ID, "Exit; pathname: ", ztransfer->pathname);
	}

	zstructFree(ztransfer);
	return status;
}
