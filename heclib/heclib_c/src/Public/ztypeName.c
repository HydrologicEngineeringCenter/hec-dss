#include "zdataTypeDescriptions.h"
#include "hecdssInternal.h"

/**
*  Function:	ztypeName
*
*  Use:			Public
*
*  Description:	Short utility function to return the character name of a data type
*
*  Declaration: const char *ztypeName(int recordType, int boolAbbreviation);
*
*  Parameters:	int recordType
*					The data type, such as 110 for irregular-interval time series
*
*				int boolAbbreviation
*					Set to 1 to get the 3 character abbreviation instead, 0 for full name
*
*
*	Returns:
*				const char *dataTypeName
*					The character name of the data type
*
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*/

const char *ztypeName(int recordType, int boolAbbreviation)
{
	if (recordType == DATA_TYPE_LOCATION) {
		if (boolAbbreviation) return DATA_TYPE_ABBR_20;
		return DATA_TYPE_20;
	}
	else if (recordType == DATA_TYPE_ARRAY) {
		if (boolAbbreviation) return DATA_TYPE_ABBR_90;
		return DATA_TYPE_90;
	}
	else if (recordType == DATA_TYPE_RTS) {
		if (boolAbbreviation) return DATA_TYPE_ABBR_100;
		return DATA_TYPE_100;
	}
	else if (recordType == DATA_TYPE_RTS_PATTERN) {
		if (boolAbbreviation) return DATA_TYPE_ABBR_101;
		return DATA_TYPE_101;
	}
	else if (recordType == DATA_TYPE_RTS_PROFILE) {
		if (boolAbbreviation) return DATA_TYPE_ABBR_102;
		return DATA_TYPE_102;
	}
	else if (recordType == DATA_TYPE_RTD) {
		if (boolAbbreviation) return DATA_TYPE_ABBR_105;
		return DATA_TYPE_105;
	}
	else if (recordType == DATA_TYPE_RTD_PROFILE) {
		if (boolAbbreviation) return DATA_TYPE_ABBR_107;
		return DATA_TYPE_107;
	}
	else if (recordType == DATA_TYPE_ITS) {
		if (boolAbbreviation) return DATA_TYPE_ABBR_110;
		return DATA_TYPE_110;
	}
	else if (recordType == DATA_TYPE_ITS_PROFILE) {
		if (boolAbbreviation) return DATA_TYPE_ABBR_112;
		return DATA_TYPE_112;
	}
	else if (recordType == DATA_TYPE_ITD) {
		if (boolAbbreviation) return DATA_TYPE_ABBR_115;
		return DATA_TYPE_115;
	}
	else if (recordType == DATA_TYPE_ITD_PROFILE) {
		if (boolAbbreviation) return DATA_TYPE_ABBR_117;
		return DATA_TYPE_117;
	}
	else if (recordType == DATA_TYPE_PD) {
		if (boolAbbreviation) return DATA_TYPE_ABBR_200;
		return DATA_TYPE_200;
	}
	else if (recordType == DATA_TYPE_PDD) {
		if (boolAbbreviation) return DATA_TYPE_ABBR_205;
		return DATA_TYPE_205;
	}
	else if (recordType == DATA_TYPE_TEXT) {
		if (boolAbbreviation) return DATA_TYPE_ABBR_300;
		return DATA_TYPE_300;
	}
	else if (recordType == DATA_TYPE_TEXT_TABLE) {
		if (boolAbbreviation) return DATA_TYPE_ABBR_310;
		return DATA_TYPE_310;
	}
	else if (recordType == DATA_TYPE_UGT) {
		if (boolAbbreviation) return DATA_TYPE_ABBR_400;
		return DATA_TYPE_400;
	}
	else if (recordType == DATA_TYPE_UG) {
		if (boolAbbreviation) return DATA_TYPE_ABBR_401;
		return DATA_TYPE_401;
	}
	else if (recordType == DATA_TYPE_HGT) {
		if (boolAbbreviation) return DATA_TYPE_ABBR_410;
		return DATA_TYPE_410;
	}
	else if (recordType == DATA_TYPE_HG) {
		if (boolAbbreviation) return DATA_TYPE_ABBR_411;
		return DATA_TYPE_411;
	}
	else if (recordType == DATA_TYPE_AGT) {
		if (boolAbbreviation) return DATA_TYPE_ABBR_420;
		return DATA_TYPE_420;
	}
	else if (recordType == DATA_TYPE_AG) {
		if (boolAbbreviation) return DATA_TYPE_ABBR_421;
		return DATA_TYPE_421;
	}
	else if (recordType == DATA_TYPE_SGT) {
		if (boolAbbreviation) return DATA_TYPE_ABBR_430;
		return DATA_TYPE_430;
	}
	else if (recordType == DATA_TYPE_SG) {
		if (boolAbbreviation) return DATA_TYPE_ABBR_431;
		return DATA_TYPE_431;
	}
	else if (recordType == DATA_TYPE_SPATIAL_TIN) {
		if (boolAbbreviation) return DATA_TYPE_ABBR_450;
		return DATA_TYPE_450;
	}
	else if (recordType == DATA_TYPE_FILE) {
		if (boolAbbreviation) return DATA_TYPE_ABBR_600;
		return DATA_TYPE_600;
	}
	else if (recordType == DATA_TYPE_IMAGE) {
		if (boolAbbreviation) return DATA_TYPE_ABBR_610;
		return DATA_TYPE_610;
	}
	
	if (boolAbbreviation) return DATA_TYPE_ABBR_UNDEFINED;
	return DATA_TYPE_UNDEFINED;
}

