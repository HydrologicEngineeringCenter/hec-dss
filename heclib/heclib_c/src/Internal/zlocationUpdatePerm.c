#include "heclib7.h"
#include "hecdssInternal.h"
#include "zdssKeys.h"

#include "zStructLocation.h"

/**
*  Function:	zlocationUpdatePerm
*
*  Use:			Private
*
*  Description:	When storing a location, and decimal degrees lat/long is used, update
*					the min / max location info in the file header
*
*  Declaration: void zlocationUpdatePerm(long long *ifltab, zStructLocation *locationStruct)
*
*
*  Parameters:	long long *ifltab
*					The ifltab array used with zopen
*
*				zStructLocation *locationStruct
*					The location struct being stored.
*
*  Returns:		None
*
*  Remarks:		The file header contains the min and max lat/long for all locations
*					within the file, so one can quickly determine the geographic extents
*					for data within this file.  If the location coordinates is not
*					lat/long in decimal degrees, this operation is ignored (the calling
*					function does not need to check for this.)
*
*
*  See Also:	zlocationStore and zlocationRetrieve
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/



void zlocationUpdatePerm(long long *ifltab, zStructLocation *locationStruct)
{

	float latitude;
	float longitude;
	float elevationMin;
	float elevationMax;
	long long latLong;
	long long *fileHeader;

	if ((locationStruct->coordinateSystem != 1) ||
		(locationStruct->horizontalUnits != 3)) {
			return;
	}


	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	if (locationStruct->yOrdinate != 0) {
		if (fileHeader[zdssFileKeys.klocBoundLR] == 0) {
			latitude = (float)locationStruct->yOrdinate;
			longitude = (float)locationStruct->xOrdinate;
			latLong = f4toi8(latitude, longitude);
			fileHeader[zdssFileKeys.klocBoundLR] = latLong;
			fileHeader[zdssFileKeys.klocBoundLL] = latLong;
			fileHeader[zdssFileKeys.klocBoundUR] = latLong;
			fileHeader[zdssFileKeys.klocBoundUL] = latLong;
		}
		else {
			i8tof4(fileHeader[zdssFileKeys.klocBoundLR], &latitude, &longitude);
			if ((float)locationStruct->xOrdinate > longitude) {
				longitude = (float)locationStruct->xOrdinate;
				fileHeader[zdssFileKeys.klocBoundLR] = f4toi8(latitude, longitude);
			}
			if ((float)locationStruct->yOrdinate < latitude) {
				latitude = (float)locationStruct->yOrdinate;
				fileHeader[zdssFileKeys.klocBoundLR] = f4toi8(latitude, longitude);
			}
			i8tof4(fileHeader[zdssFileKeys.klocBoundLL], &latitude, &longitude);
			if ((float)locationStruct->xOrdinate < longitude) {
				longitude = (float)locationStruct->xOrdinate;
				fileHeader[zdssFileKeys.klocBoundLL] = f4toi8(latitude, longitude);
			}
			if ((float)locationStruct->yOrdinate < latitude) {
				latitude = (float)locationStruct->yOrdinate;
				fileHeader[zdssFileKeys.klocBoundLL] = f4toi8(latitude, longitude);
			}
			i8tof4(fileHeader[zdssFileKeys.klocBoundUR], &latitude, &longitude);
			if ((float)locationStruct->xOrdinate > longitude) {
				longitude = (float)locationStruct->xOrdinate;
				fileHeader[zdssFileKeys.klocBoundUR] = f4toi8(latitude, longitude);
			}
			if ((float)locationStruct->yOrdinate > latitude) {
				latitude = (float)locationStruct->yOrdinate;
				fileHeader[zdssFileKeys.klocBoundUR] = f4toi8(latitude, longitude);
			}
			i8tof4(fileHeader[zdssFileKeys.klocBoundUL], &latitude, &longitude);
			if ((float)locationStruct->xOrdinate < longitude) {
				longitude = (float)locationStruct->xOrdinate;
				fileHeader[zdssFileKeys.klocBoundUL] = f4toi8(latitude, longitude);
			}
			if ((float)locationStruct->yOrdinate > latitude) {
				latitude = (float)locationStruct->yOrdinate;
				fileHeader[zdssFileKeys.klocBoundUL] = f4toi8(latitude, longitude);
			}
		}
	}

	if (locationStruct->zOrdinate != 0) {
		if (fileHeader[zdssFileKeys.klocBoundElev] == 0) {
			elevationMax = (float)locationStruct->zOrdinate;
			fileHeader[zdssFileKeys.klocBoundElev] = f4toi8(elevationMax, elevationMax);
		}
		else {
			i8tof4(fileHeader[zdssFileKeys.klocBoundElev], &elevationMin, &elevationMax);
			if ((float)locationStruct->zOrdinate < elevationMin) {
				elevationMin = (float)locationStruct->zOrdinate;
				fileHeader[zdssFileKeys.klocBoundElev] = f4toi8(elevationMin, elevationMax);
			}
			if ((float)locationStruct->zOrdinate > elevationMax) {
				elevationMax = (float)locationStruct->zOrdinate;
				fileHeader[zdssFileKeys.klocBoundElev] = f4toi8(elevationMin, elevationMax);
			}
		}
	}


}

