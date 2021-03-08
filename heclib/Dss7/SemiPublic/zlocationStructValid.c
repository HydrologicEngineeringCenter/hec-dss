#include <string.h>

#include "hecdss7.h"

/**
*  Function:	zlocationStructValid
*
*  Use:			Semi-Public
*
*  Description:	Utility functions to determine if a location struct has data in it
*
*  Declaration: int zlocationStructValid(zStructLocation *locationStruct)
*
*
*  Parameters:	zStructLocation *locationStruct
*					The location struct to check
*
*  Returns:		STATUS_OKAY if struct contains information
*				STATUS_NOT_OKAY if there is no data in struct
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

int zlocationStructValid(zStructLocation *locationStruct)
{

	//  Is there any valid data in this location struct?
	if (locationStruct->xOrdinate != 0.0) {
		return STATUS_OKAY;
	}
	if (locationStruct->yOrdinate != 0.0) {
		return STATUS_OKAY;
	}
	if (locationStruct->zOrdinate != 0.0) {
		return STATUS_OKAY;
	}
	if (locationStruct->horizontalDatum != 0.0) {
		return STATUS_OKAY;
	}
	if (locationStruct->verticalDatum != 0.0) {
		return STATUS_OKAY;
	}
	if (locationStruct->supplemental) {
		return STATUS_OKAY;
	}
	if ((locationStruct->timeZoneName) && (strlen(locationStruct->timeZoneName) > 0)) {
		return STATUS_OKAY;
	}


	return STATUS_NOT_OKAY;
}

