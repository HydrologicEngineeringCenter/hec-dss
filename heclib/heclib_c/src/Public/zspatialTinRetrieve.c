#include <string.h>
#include <stdlib.h>

#include "heclib.h"


/**
*  Function:	zspatialTinRetrieve
*
*  Use:			Public
*
*  Description:	Retrieve a TIN record
*
*  Declaration: int zspatialTinRetrieve(long long *ifltab, zStructSpatialTin *tinStruct, int boolRetrieveData);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				zStructSpatialTin *tinStruct
*					A TIN struct that will contain data for a single spatial TIN record.
*					This struct is created by the following method:
*						zStructSpatialTin* zstructSpatialTinNew(const char* pathname);
*					with pathname being an valid existing pathname
*					When you are finished with the struct, the struct must be freed by a call to
*						void zstructFree(zStructText *textStruct)
*					NEVER REUSE A zStructSpatialTin, always free and create a new on.
*
*				int boolRetrieveData
*					A flag indicating if both data and meta-data (header info) should be retrieve
*					= 0, Just retrieve meta-data
*					= 1, Retrieve both meta-data and tin arrays
*
*
*	Returns:	int status
*					STATUS_OKAY
*					error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*	Comments:
*
*
*
*	Author:			Tom Evans
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*
*/

int zspatialTinRetrieve(long long *ifltab, zStructSpatialTin *tinStruct, int boolRetrieveData)
{

	int status;
	size_t count;
	size_t len;
	char *str;
	char messageString[80];

	zStructTransfer* ztransfer;


	if (!tinStruct) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialTinRetrieve_ID, zdssErrorCodes.NULL_ARGUMENT,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "tinStruct is null");
	}
	if (!tinStruct->pathname) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialTinRetrieve_ID, zdssErrorCodes.NULL_PATHNAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "tinStruct pathname is null");
	}


	//  Messages and debug
	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessage(ifltab, " ");
		zmessageDebug(ifltab, DSS_FUNCTION_zspatialTinRetrieve_ID, "Enter ; Pathname: ", tinStruct->pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zspatialTinRetrieve_ID, "Handle:  ", zhandle(ifltab));
		zmessageDebugInt(ifltab, DSS_FUNCTION_zspatialTinRetrieve_ID, "boolRetrieveData flag:  ", boolRetrieveData);
	}

	//  Check for correct DSS Version
	if (zgetVersion(ifltab) != 7) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialTinRetrieve_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
			zgetVersion((void *)ifltab), 0, zdssErrorSeverity.WARNING, "", "");
	}


	ztransfer = zstructTransferNew(tinStruct->pathname, 1);
	if (!ztransfer) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialTinRetrieve_ID,
			zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
			zdssErrorSeverity.MEMORY_ERROR,
			tinStruct->pathname, "Allocating ztransfer struct");
	}

	if (!boolRetrieveData) {
		//  Do not retrieve the data, just the meta data in the header
		ztransfer->userHeaderMode = 0;
		ztransfer->values1Mode = 0;
		ztransfer->values2Mode = 0;
		ztransfer->values3Mode = 0;
	}

	status = zread(ifltab, ztransfer);
	if (zisError(status)) {
		//  An error code
		status = zerrorUpdate(ifltab, status, DSS_FUNCTION_zspatialTinRetrieve_ID);
		zstructFree(ztransfer);
		return status;
	}
	if (status != STATUS_RECORD_FOUND) {
		if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_GENERAL)) {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, ZREAD_NOT_EXIST, zhandle(ifltab));
			zmessage2(ifltab, messageString, ztransfer->pathname);
		}
		zstructFree(ztransfer);
		return status;
	}
	if (ztransfer->dataType != DATA_TYPE_SPATIAL_TIN) {
		status = zerrorProcessing(ifltab, DSS_FUNCTION_zspatialTinRetrieve_ID,
			zdssErrorCodes.WRONG_RECORD_TYPE, DATA_TYPE_SPATIAL_TIN,
			(long long)ztransfer->dataType,
			zdssErrorSeverity.WARNING, ztransfer->pathname, "");
		zstructFree(ztransfer);
		return status;
	}

	if (bigEndian()) {
		zswitchInts(ztransfer->internalHeader, ztransfer->internalHeaderNumber);
	}


	//  Header 1, basic info
	tinStruct->numberPoints = ztransfer->internalHeader[0];
	tinStruct->connectTableLen = ztransfer->internalHeader[1];
	tinStruct->pointLabelLen = ztransfer->internalHeader[2];
	tinStruct->SRSType = ztransfer->internalHeader[3];
	convertDataType(&ztransfer->internalHeader[4], (int *)&tinStruct->slendernessRatio, 1, 1);
	convertDataType(&ztransfer->internalHeader[5], (int *)&tinStruct->minXCoordinate, 1, 1);
	convertDataType(&ztransfer->internalHeader[6], (int *)&tinStruct->minYCoordinate, 1, 1);
	convertDataType(&ztransfer->internalHeader[7], (int *)&tinStruct->maxXCoordinate, 1, 1);
	convertDataType(&ztransfer->internalHeader[8], (int *)&tinStruct->maxYCoordinate, 1, 1);
	convertDataType(&ztransfer->internalHeader[9], (int *)&tinStruct->minValue, 1, 1);
	convertDataType(&ztransfer->internalHeader[10], (int *)&tinStruct->meanValue, 1, 1);
	convertDataType(&ztransfer->internalHeader[11], (int *)&tinStruct->maxValue, 1, 1);

	//  Header 2, character information
	str = (char *)calloc(ztransfer->header2Number + 2, 4);
	charInt(ztransfer->header2, str, (ztransfer->header2Number * 4), (ztransfer->header2Number * 4), 0, 0, 0);

	count = 0;
	tinStruct->SpatialReferenceSystem = mallocAndCopy(&str[count]);
	len = strlen(tinStruct->SpatialReferenceSystem);
	count += len;
	count++;	//  null terminator
	tinStruct->SRSName = mallocAndCopy(&str[count]);
	len = strlen(tinStruct->SRSName);
	count += len;
	count++;
	tinStruct->SRSUnits = mallocAndCopy(&str[count]);
	len = strlen(tinStruct->SRSUnits);
	count += len;
	count++;
	tinStruct->units = mallocAndCopy(&str[count]);
	len = strlen(tinStruct->units);
	count += len;
	count++;
	tinStruct->type = mallocAndCopy(&str[count]);
	len = strlen(tinStruct->type);
	count += len;
	count++;
	tinStruct->timeZoneName = mallocAndCopy(&str[count]);
	len = strlen(tinStruct->timeZoneName);
	count += len;
	count++;

	free(str);
	tinStruct->allocated[zSTRUCT_TIN_SpatialReferenceSystem] = 1;
	tinStruct->allocated[zSTRUCT_TIN_SRSName] = 1;
	tinStruct->allocated[zSTRUCT_TIN_SRSUnits] = 1;
	tinStruct->allocated[zSTRUCT_TIN_units] = 1;
	tinStruct->allocated[zSTRUCT_TIN_type] = 1;
	tinStruct->allocated[zSTRUCT_TIN_timeZoneName] = 1;

	//  Labels (char) array
	if (ztransfer->userHeader && (ztransfer->userHeaderNumber > 0)) {
		if (tinStruct->pointLabelLen > 0) {
			if (ztransfer->userHeaderNumber >= numberIntsInBytes(tinStruct->pointLabelLen)) {
				tinStruct->pointLabel = (char *)calloc(tinStruct->pointLabelLen + 8, 1);
				tinStruct->allocated[zSTRUCT_TIN_label] = 1;
				//  Need to be sure to copy char into in array, not cast and assign to header
				charInt(ztransfer->userHeader, tinStruct->pointLabel, tinStruct->pointLabelLen, (ztransfer->userHeaderNumber * 4), 0, 0, 0);
			}
		}
	}

	if (bigEndian()) {
		zswitchInts(ztransfer->values1, ztransfer->values1Number);
	}
	//  Values 1, x, y, value
	if (ztransfer->values1 && (ztransfer->values1Number > 0)) {
		tinStruct->xCoordinate = (float *)calloc(tinStruct->numberPoints, 4);
		tinStruct->yCoordinate = (float *)calloc(tinStruct->numberPoints, 4);
		tinStruct->value = (float *)calloc(tinStruct->numberPoints, 4);
		convertDataArray(&ztransfer->values1[0], (int *)tinStruct->xCoordinate, tinStruct->numberPoints, 1, 1);
		convertDataArray(&ztransfer->values1[tinStruct->numberPoints], (int *)tinStruct->yCoordinate, tinStruct->numberPoints, 1, 1);
		convertDataArray(&ztransfer->values1[(tinStruct->numberPoints * 2)], (int *)tinStruct->value, tinStruct->numberPoints, 1, 1);
		tinStruct->allocated[zSTRUCT_TIN_xCoordinate] = 1;
		tinStruct->allocated[zSTRUCT_TIN_yCoordinate] = 1;
		tinStruct->allocated[zSTRUCT_TIN_value] = 1;
	}
	if (bigEndian()) {
		zswitchInts(ztransfer->values2, ztransfer->values2Number);
	}
	//  Values 2, other arrays
	if (ztransfer->values2 && (ztransfer->values2Number > 0)) {
		tinStruct->pointType = (int *)calloc(tinStruct->numberPoints, 4);
		tinStruct->numberConnections = (int *)calloc(tinStruct->numberPoints, 4);
		convertDataArray(&ztransfer->values2[0], (int *)tinStruct->pointType, tinStruct->numberPoints, 1, 1);
		convertDataArray(&ztransfer->values2[(tinStruct->numberPoints * 2)], (int *)tinStruct->numberConnections, tinStruct->numberPoints, 1, 1);
		tinStruct->allocated[zSTRUCT_TIN_pointType] = 1;
		tinStruct->allocated[zSTRUCT_TIN_numberConnections] = 1;
	}
	if (bigEndian()) {
		zswitchInts(ztransfer->values3, ztransfer->values3Number);
	}
	//  Value 3, connections  tinStruct->connection
	if (ztransfer->values3 && (ztransfer->values3Number > 0)) {
		if (tinStruct->connectTableLen > 0) {
			tinStruct->connectTo = (int *)calloc(tinStruct->connectTableLen, 4);
			convertDataArray(ztransfer->values3, tinStruct->connectTo, tinStruct->connectTableLen, 1, 1);
			tinStruct->allocated[zSTRUCT_TIN_connection] = 1;
		}
	}

	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zspatialTinRetrieve_ID, "Exit; status:  ", status);
	}

	return status;
}

