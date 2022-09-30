#include <stdlib.h>
#include <string.h>

#include "heclib.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"

/**
*  Function:	zstructFree
*
*  Use:			Public
*
*  Description:	 Frees allocated space for standard DSS data structs (allocated by a zstructNew... function)
*
*  Declaration: void zstructFree(void *zstruct);
*
*  Parameters:
*				void *zstruct - One of the standard DSS data structs
*
*  	structType:
*					0:  Invalid
*					1:  zStructTransfer
*					2:  zStructRecordSize
*					10:  zStructCatalog
*					20 (DATA_TYPE_LOCATION): zStructLocation
*					90:   zStructArray
*					100:  zStructTimeSeries
*					200:  zStructPairedData
*					300:  Text
*					310 (DATA_TYPE_TEXT_TABLE):  zStructTextTable
*					450 (DATA_TYPE_SPATIAL_TIN): zStructSpatialTin
*
*  Remarks:		Only frees space allocated by DSS functions; does not
*					free any space allocated outside of it.
*				For use with all standard DSS data structs (see above)
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


//  Private declarations; do not access
void ztransferStructFree(zStructTransfer *zstruct);
void zrecordSizeStructFree(zStructRecordSize* zstruct);
void zrecordBasicsStructFree(zStructRecordBasics* zstruct);
void zlocationStructFree(zStructLocation* zstruct);
void ztsStructFree(zStructTimeSeries *zstruct);
void zpdStructFree(zStructPairedData *zstruct);
void ztextStructFree(zStructText *zstruct);
void zspatialTinStructFree(zStructSpatialTin *zstruct);
void zcatalogStructFree(zStructCatalog *zstruct);
void zarrayStructFree(zStructArray *zstruct);
void zspatialGridStructFree(zStructSpatialGrid *zstruct);


void zstructFree(void *zstruct)
{
	int structType;

	structType = zstructGetType(zstruct);

	if (structType == STRUCT_TYPE_TRANSFER) {
		ztransferStructFree((zStructTransfer *)zstruct);
	}
	else if (structType == STRUCT_TYPE_RECORD_SIZES) {
		zrecordSizeStructFree((zStructRecordSize *)zstruct);
	}
	else if (structType == STRUCT_TYPE_RECORD_BASICS) {
		zrecordBasicsStructFree((zStructRecordBasics *)zstruct);
	}
	else if (structType == STRUCT_TYPE_RECORD_ADDRESSES) {
		//  Nothing to free
	}
	else if (structType == STRUCT_TYPE_CATALOG) {
		zcatalogStructFree((zStructCatalog *)zstruct);
	}
	else if (structType == DATA_TYPE_LOCATION) {
		zlocationStructFree((zStructLocation *)zstruct);
	}
	else if (structType == STRUCT_TYPE_ARRAY) {
		zarrayStructFree((zStructArray *)zstruct);
	}
	else if (structType == DATA_TYPE_RTS) {
		ztsStructFree((zStructTimeSeries *)zstruct);
	}
	else if (structType == DATA_TYPE_PD) {
		zpdStructFree((zStructPairedData *)zstruct);
	}
	else if (structType == DATA_TYPE_TEXT) {
		ztextStructFree((zStructText *)zstruct);
	}
	else if (structType == DATA_TYPE_SPATIAL_TIN) {
		zspatialTinStructFree((zStructSpatialTin *)zstruct);
	}
	else if (structType >= DATA_TYPE_UGT && structType <= DATA_TYPE_SG) {
		zspatialGridStructFree((zStructSpatialGrid*)zstruct);
	}
}

void ztransferStructFree(zStructTransfer *zstruct)
{
	if (!zstruct) return;

	if ((zstruct->pathname) && (zstruct->allocated[zSTRUCT_pathname])) {
		free(zstruct->pathname);
		zstruct->pathname = 0;
	}

	if ((zstruct->internalHeader) && (zstruct->allocated[zSTRUCT_TRANS_internalHeader])) {
		free(zstruct->internalHeader);
		zstruct->internalHeader = 0;
	}
	if ((zstruct->header2) && (zstruct->allocated[zSTRUCT_TRANS_header2])) {
		free(zstruct->header2);
		zstruct->header2 = 0;
	}
	if ((zstruct->values3) && (zstruct->allocated[zSTRUCT_TRANS_values3])) {
		free(zstruct->values3);
		zstruct->values3 = 0;
	}
	if ((zstruct->userHeader) && (zstruct->allocated[zSTRUCT_userHeader])) {
		free(zstruct->userHeader);
		zstruct->userHeader = 0;
	}
	if ((zstruct->values1) && (zstruct->allocated[zSTRUCT_TRANS_values1])) {
		free(zstruct->values1);
		zstruct->values1 = 0;
	}
	if ((zstruct->values2) && (zstruct->allocated[zSTRUCT_TRANS_values2])) {
		free(zstruct->values2);
		zstruct->values2 = 0;
	}


	free(zstruct);

}

void zrecordSizeStructFree(zStructRecordSize* zstruct)
{
	if (zstruct->allocated[zSTRUCT_pathname]) {
		free(zstruct->pathname);
	}
	free(zstruct);
}

void zrecordBasicsStructFree(zStructRecordBasics* zstruct)
{
	if (zstruct->allocated[zSTRUCT_pathname]) {
		free(zstruct->pathname);
	}
	free(zstruct);
}



void zlocationStructFree(zStructLocation* zstruct)
{
	if ((zstruct->pathname) && zstruct->allocated[zSTRUCT_pathname]) {
		free(zstruct->pathname);
	}
	if ((zstruct->pathnameInternal) && zstruct->allocated[zSTRUCT_locationPathInternal]) {
		free(zstruct->pathnameInternal);
	}
	if ((zstruct->timeZoneName) && zstruct->allocated[zSTRUCT_timeZoneName]) {
		free(zstruct->timeZoneName);
	}
	if ((zstruct->supplemental) && zstruct->allocated[zSTRUCT_otherInformation]) {
		free(zstruct->supplemental);
	}

	free(zstruct);

}

void ztsStructFree(zStructTimeSeries *zstruct)
{
	int i;

	if (!zstruct) return;

	if ((zstruct->pathname) && (zstruct->allocated[zSTRUCT_pathname])) free(zstruct->pathname);
	if ((zstruct->pathnameInternal) && (zstruct->allocated[zSTRUCT_pathnameInternal])) free(zstruct->pathnameInternal);

	if ((zstruct->userHeader) && (zstruct->allocated[zSTRUCT_userHeader])) free(zstruct->userHeader);

	if ((zstruct->units) && (zstruct->allocated[zSTRUCT_TS_units])) free(zstruct->units);
	if ((zstruct->type) && (zstruct->allocated[zSTRUCT_TS_type])) free(zstruct->type);
	if ((zstruct->quality) && (zstruct->allocated[zSTRUCT_TS_quality])) free(zstruct->quality);
	if ((zstruct->inotes) && (zstruct->allocated[zSTRUCT_TS_inotes])) free(zstruct->inotes);
	if ((zstruct->cnotes) && (zstruct->allocated[zSTRUCT_TS_cnotes])) free(zstruct->cnotes);

	if ((zstruct->times) && (zstruct->allocated[zSTRUCT_TS_times])) free(zstruct->times);
	if ((zstruct->floatValues) && (zstruct->allocated[zSTRUCT_TS_floatValues])) free(zstruct->floatValues);
	if ((zstruct->doubleValues) && (zstruct->allocated[zSTRUCT_TS_doubleValues])) free(zstruct->doubleValues);

	if ((zstruct->floatProfileDepths) && (zstruct->allocated[zSTRUCT_TS_profileFloatDepths])) free(zstruct->floatProfileDepths);
	if ((zstruct->floatProfileValues) && (zstruct->allocated[zSTRUCT_TS_profileFloatValues])) free(zstruct->floatProfileValues);
	if ((zstruct->doubleProfileDepths) && (zstruct->allocated[zSTRUCT_TS_profileDoubleDepths])) free(zstruct->doubleProfileDepths);
	if ((zstruct->doubleProfileValues) && (zstruct->allocated[zSTRUCT_TS_profileDoubleValues])) free(zstruct->doubleProfileValues);
	if ((zstruct->unitsProfileDepths) && (zstruct->allocated[zSTRUCT_TS_profileUnitsDepths])) free(zstruct->unitsProfileDepths);
	if ((zstruct->unitsProfileValues) && (zstruct->allocated[zSTRUCT_TS_profileUnitsValues])) free(zstruct->unitsProfileValues);


	if ((zstruct->timeZoneName) && (zstruct->allocated[zSTRUCT_timeZoneName])) free(zstruct->timeZoneName);
	if ((zstruct->timeWindow) && (zstruct->allocated[zSTRUCT_TS_timeWindow])) {
		free(zstruct->timeWindow);
	}

	if ((zstruct->locationStruct) && (zstruct->allocated[zSTRUCT_TS_locationStruct])) {
		zlocationStructFree(zstruct->locationStruct);
		zstruct->locationStruct = 0;
	}

	for (i=0; i<zSTRUCT_length; i++) {
		zstruct->allocated[i] = 0;
	}

	free(zstruct);
}


void zpdStructFree(zStructPairedData *zstruct)
{
	int i;

	if (!zstruct) return;

	if ((zstruct->pathname) && (zstruct->allocated[zSTRUCT_pathname])) {
		free(zstruct->pathname);
	}
	if ((zstruct->timeZoneName) && (zstruct->allocated[zSTRUCT_timeZoneName])) {
		free(zstruct->timeZoneName);
	}
	if ((zstruct->otherInfo) && (zstruct->allocated[zSTRUCT_otherInformation])) {
		free(zstruct->otherInfo);
	}
	if ((zstruct->userHeader) && (zstruct->allocated[zSTRUCT_userHeader])) {
		free(zstruct->userHeader);
	}

	if ((zstruct->unitsIndependent) && (zstruct->allocated[zSTRUCT_unitsIndependent])) {
		free(zstruct->unitsIndependent);
	}
	if ((zstruct->typeIndependent) && (zstruct->allocated[zSTRUCT_typeIndependent])) {
		free(zstruct->typeIndependent);
	}
	if ((zstruct->unitsDependent) && (zstruct->allocated[zSTRUCT_unitsDependent])) {
		free(zstruct->unitsDependent);
	}
	if ((zstruct->typeDependent) && (zstruct->allocated[zSTRUCT_typeDependent])) {
		free(zstruct->typeDependent);
	}

	if ((zstruct->labels) && (zstruct->allocated[zSTRUCT_PD_labels])) {
		free(zstruct->labels);
	}

	if ((zstruct->floatOrdinates) && (zstruct->allocated[zSTRUCT_PD_floatOrdinates])) {
		free(zstruct->floatOrdinates);
	}
	if ((zstruct->floatValues) && (zstruct->allocated[zSTRUCT_PD_floatValues])) {
		free(zstruct->floatValues);
	}
	if ((zstruct->doubleOrdinates) && (zstruct->allocated[zSTRUCT_PD_doubleOridnates])) {
		free(zstruct->doubleOrdinates);
	}
	if ((zstruct->doubleValues) && (zstruct->allocated[zSTRUCT_PD_doubleValues])) {
		free(zstruct->doubleValues);
	}

	if ((zstruct->locationStruct) && (zstruct->allocated[zSTRUCT_PD_locationStruct])) {
		zlocationStructFree(zstruct->locationStruct);
		zstruct->locationStruct = 0;
	}

	for (i=0; i<zSTRUCT_length; i++) {
		zstruct->allocated[i] = 0;
	}

	free(zstruct);
}


void ztextStructFree(zStructText *zstruct)
{
	int i;

	if (zstruct == 0) return;

	if ((zstruct->pathname) && (zstruct->allocated[zSTRUCT_pathname])) free(zstruct->pathname);
	if ((zstruct->textString) && (zstruct->allocated[zSTRUCT_TX_textString])) free(zstruct->textString);
	if ((zstruct->textTable) && (zstruct->allocated[zSTRUCT_TX_textTable])) free(zstruct->textTable);
	if ((zstruct->labels) && (zstruct->allocated[zSTRUCT_TX_labels])) free(zstruct->labels);

	for (i=0; i<zSTRUCT_length; i++) {
		zstruct->allocated[i] = 0;
	}

	free(zstruct);
}

void zarrayStructFree(zStructArray *zstruct)
{
	int i;

	if (zstruct == 0) return;

	if ((zstruct->pathname) && (zstruct->allocated[zSTRUCT_pathname])) free(zstruct->pathname);

	if ((zstruct->intArray) && (zstruct->allocated[zSTRUCT_ARRAY_INT])) free(zstruct->intArray);
	if ((zstruct->floatArray) && (zstruct->allocated[zSTRUCT_ARRAY_FLOAT])) free(zstruct->floatArray);
	if ((zstruct->doubleArray) && (zstruct->allocated[zSTRUCT_ARRAY_DOUBLE])) free(zstruct->doubleArray);


	for (i=0; i<zSTRUCT_length; i++) {
		zstruct->allocated[i] = 0;
	}

	free(zstruct);
}

void zspatialTinStructFree(zStructSpatialTin *zstruct)
{
	int i;

	if (zstruct == 0) return;

	if ((zstruct->pathname) && (zstruct->allocated[zSTRUCT_pathname])) free(zstruct->pathname);

	if ((zstruct->xCoordinate) && (zstruct->allocated[zSTRUCT_TIN_xCoordinate])) free(zstruct->xCoordinate);
	if ((zstruct->yCoordinate) && (zstruct->allocated[zSTRUCT_TIN_yCoordinate])) free(zstruct->yCoordinate);
	if ((zstruct->value) && (zstruct->allocated[zSTRUCT_TIN_value])) free(zstruct->value);
	if ((zstruct->pointType) && (zstruct->allocated[zSTRUCT_TIN_pointType])) free(zstruct->pointType);
	if ((zstruct->numberConnections) && (zstruct->allocated[zSTRUCT_TIN_numberConnections])) free(zstruct->numberConnections);
	if ((zstruct->SpatialReferenceSystem) && (zstruct->allocated[zSTRUCT_TIN_SpatialReferenceSystem])) free(zstruct->SpatialReferenceSystem);
	if ((zstruct->SRSName) && (zstruct->allocated[zSTRUCT_TIN_SRSName])) free(zstruct->SRSName);
	if ((zstruct->SRSUnits) && (zstruct->allocated[zSTRUCT_TIN_SRSUnits])) free(zstruct->SRSUnits);
	if ((zstruct->units) && (zstruct->allocated[zSTRUCT_TIN_units])) free(zstruct->units);
	if ((zstruct->type) && (zstruct->allocated[zSTRUCT_TIN_type])) free(zstruct->type);
	if ((zstruct->timeZoneName) && (zstruct->allocated[zSTRUCT_TIN_timeZoneName])) free(zstruct->timeZoneName);
	if ((zstruct->connectTo) && (zstruct->allocated[zSTRUCT_TIN_connection])) free(zstruct->connectTo);
	if ((zstruct->pointLabel) && (zstruct->allocated[zSTRUCT_TIN_label])) free(zstruct->pointLabel);

	for (i=0; i<zSTRUCT_length; i++) {
		zstruct->allocated[i] = 0;
	}

	free(zstruct);
}


void zcatalogStructFree(zStructCatalog *zstruct)
{
	int i;

	if ((zstruct->pathnameList) && (zstruct->allocated[zSTRUCT_pathname])) {
		for (i=0; i<zstruct->listSize; i++) {
			if (zstruct->pathnameList[i]) {
				free(zstruct->pathnameList[i]);
			}
		}
		free(zstruct->pathnameList);
		zstruct->pathnameList = 0;
	}

	if (zstruct->startDates) {
		free(zstruct->startDates);
		zstruct->startDates = 0;
	}
	if (zstruct->endDates) {
		free(zstruct->endDates);
		zstruct->endDates = 0;
	}
	if (zstruct->sortAddresses) {
		free(zstruct->sortAddresses);
		zstruct->sortAddresses = 0;
	}
	if (zstruct->recordType) {
		free(zstruct->recordType);
		zstruct->recordType = 0;
	}
	if (zstruct->pathnameHash) {
		free(zstruct->pathnameHash);
		zstruct->pathnameHash = 0;
	}
	if (zstruct->lastWriteTimeRecord) {
		free(zstruct->lastWriteTimeRecord);
		zstruct->lastWriteTimeRecord = 0;
	}
	if (zstruct->crcValues) {
		free(zstruct->crcValues);
		zstruct->crcValues = 0;
	}
	if (zstruct->pathWithWildChars) {
		free(zstruct->pathWithWildChars);
		zstruct->pathWithWildChars = 0;
	}


	zstruct->numberPathnames = 0;
	zstruct->listSize = 0;


	for (i=0; i<zSTRUCT_length; i++) {
		zstruct->allocated[i] = 0;
	}

	free(zstruct);
}

void zspatialGridStructFree(zStructSpatialGrid *zstruct)
{
	if (zstruct->pathname) free(zstruct->pathname);
	if (zstruct->_dataUnits) free(zstruct->_dataUnits);
	if (zstruct->_dataSource) free(zstruct->_dataSource);
	if (zstruct->_compressionParameters) free(zstruct->_compressionParameters);
	if (zstruct->_rangeLimitTable) free(zstruct->_rangeLimitTable);
	if (zstruct->_numberEqualOrExceedingRangeLimit) free(zstruct->_numberEqualOrExceedingRangeLimit);
	if (zstruct->_srsName) free(zstruct->_srsName);
	if (zstruct->_srsDefinition) free(zstruct->_srsDefinition);
	if (zstruct->_timeZoneID) free(zstruct->_timeZoneID);
	if (zstruct->_data) free(zstruct->_data);
	if (zstruct->_maxDataValue) free(zstruct->_maxDataValue);
	if (zstruct->_minDataValue) free(zstruct->_minDataValue);
	if (zstruct->_meanDataValue) free(zstruct->_meanDataValue);

	// free *void floats.
	memset(&(zstruct[0]), 0, sizeof(zStructSpatialGrid));
	free(zstruct);
}

