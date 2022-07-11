#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "hecdssInternal.h"
#include "zdssKeys.h"


/**
*  Function:	zspatialTinStore
*
*  Use:			Public
*
*  Description:	Store a TIN record
*
*  Declaration: int zspatialTinStore(long long *ifltab, zStructSpatialTin *tinStruct);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				zStructSpatialTin *tinStruct
*					A TIN struct that contains data for a single spatial TIN record.
*					This struct is created by the following method:
*						zStructSpatialTin* zstructSpatialTinNew(const char* pathname);
*					with pathname being an valid existing pathname
*					When you are finished with the struct, the struct must be freed by a call to
*						void zstructFree(zStructText *textStruct)
*					NEVER REUSE A zStructSpatialTin, always free and create a new on.
*
*	Returns:	int status
*					STATUS_OKAY
*					error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*	Comments:
*
*
*	Author:			Tom Evans
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*
*/

int zspatialTinStore(long long *ifltab, zStructSpatialTin *tinStruct)
{

	int status;
	size_t count;
	size_t len;
	size_t total;
	char *str;
	int julianFirstValue, secondsFirstValue;
	int julianLastValue, secondsLastValue;
	int dateStatus;
	char dPart[MAX_PART_SIZE];
	char ePart[MAX_PART_SIZE];

	zStructTransfer* ztransfer;


	if (!tinStruct) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialTinStore_ID, zdssErrorCodes.NULL_ARGUMENT,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "tinStruct is null");
	}
	if (!tinStruct->pathname) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialTinStore_ID, zdssErrorCodes.NULL_PATHNAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "tinStruct pathname is null");
	}

	//  Messages and debug
	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessage(ifltab, " ");
		zmessageDebug(ifltab, DSS_FUNCTION_zspatialTinStore_ID, "Enter; Pathname: ", tinStruct->pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zspatialTinStore_ID, "Handle:  ", zhandle(ifltab));
		zmessageDebugInt(ifltab, DSS_FUNCTION_zspatialTinStore_ID, "Number of points:  ", tinStruct->numberPoints);
	}

	//  Check for correct DSS Version
	if (zgetVersion(ifltab) != 7) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialTinStore_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
								zgetVersion((void *)ifltab), 0, zdssErrorSeverity.WARNING, "", "");
	}


	ztransfer = zstructTransferNew(tinStruct->pathname, 0);
	if (!ztransfer) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialTinStore_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
								zdssErrorSeverity.MEMORY_ERROR,
								tinStruct->pathname, "Allocating ztransfer struct");
	}
	ztransfer->dataType = DATA_TYPE_SPATIAL_TIN;

	//  Header 1, basic info
	ztransfer->internalHeaderNumber = 12;
	ztransfer->internalHeader = (int *)calloc(ztransfer->internalHeaderNumber, 4);
	ztransfer->internalHeaderMode = 1;
	ztransfer->internalHeader[0] = tinStruct->numberPoints;
	ztransfer->internalHeader[1] = tinStruct->connectTableLen;
	ztransfer->internalHeader[2] = tinStruct->pointLabelLen;
	ztransfer->internalHeader[3] = tinStruct->SRSType;
	convertDataType((int *)&tinStruct->slendernessRatio, &ztransfer->internalHeader[4], 1, 1);
	convertDataType((int *)&tinStruct->minXCoordinate, &ztransfer->internalHeader[5], 1, 1);
	convertDataType((int *)&tinStruct->minYCoordinate, &ztransfer->internalHeader[6], 1, 1);
	convertDataType((int *)&tinStruct->maxXCoordinate, &ztransfer->internalHeader[7], 1, 1);
	convertDataType((int *)&tinStruct->maxYCoordinate, &ztransfer->internalHeader[8], 1, 1);
	convertDataType((int *)&tinStruct->minValue, &ztransfer->internalHeader[9], 1, 1);
	convertDataType((int *)&tinStruct->meanValue, &ztransfer->internalHeader[10], 1, 1);
	convertDataType((int *)&tinStruct->maxValue, &ztransfer->internalHeader[11], 1, 1);

	//  Header 2, character information
	//  Count the number of characters (including nulls) in struct strings
	total = 0;
	if (tinStruct->SpatialReferenceSystem) {
		len = strlen(tinStruct->SpatialReferenceSystem);
		total += len;
	}
	total++;	//  null terminator
	if (tinStruct->SRSName) {
		len = strlen(tinStruct->SRSName);
		total += len;
	}
	total++;
	if (tinStruct->SRSUnits) {
		len = strlen(tinStruct->SRSUnits);
		total += len;
	}
	total++;
	if (tinStruct->units) {
		len = strlen(tinStruct->units);
		total += len;
	}
	total++;
	if (tinStruct->type) {
		len = strlen(tinStruct->type);
		total += len;
	}
	total++;
	if (tinStruct->timeZoneName) {
		len = strlen(tinStruct->timeZoneName);
		total += len;
	}
	total++;
	//  Copy strings into one array
	str = (char *)calloc(total, 1);
	count = 0;
	if (tinStruct->SpatialReferenceSystem) {
		len = strlen(tinStruct->SpatialReferenceSystem);
		stringCopy(&str[count], total, tinStruct->SpatialReferenceSystem, len);
		count += len;
	}
	str[count++] = '\0';
	if (tinStruct->SRSName) {
		len = strlen(tinStruct->SRSName);
		stringCopy(&str[count], total, tinStruct->SRSName, len);
		count += len;
	}
	str[count++] = '\0';
	if (tinStruct->SRSUnits) {
		len = strlen(tinStruct->SRSUnits);
		stringCopy(&str[count], total, tinStruct->SRSUnits, len);
		count += len;
	}
	str[count++] = '\0';
	if (tinStruct->units) {
		len = strlen(tinStruct->units);
		stringCopy(&str[count], total, tinStruct->units, len);
		count += len;
	}
	str[count++] = '\0';
	if (tinStruct->type) {
		len = strlen(tinStruct->type);
		stringCopy(&str[count], total, tinStruct->type, len);
		count += len;
	}
	str[count++] = '\0';
	if (tinStruct->timeZoneName) {
		len = strlen(tinStruct->timeZoneName);
		stringCopy(&str[count], total, tinStruct->timeZoneName, len);
		count += len;
	}
	str[count++] = '\0';
	ztransfer->header2Number = numberIntsInBytes((int)total);
	ztransfer->header2 = (int *)calloc(ztransfer->header2Number + 2, 4);
	ztransfer->header2Mode = 1;
	//  Need to be sure to copy char into in array, not cast and assign to header
	charInt (str, ztransfer->header2, (int)total, (ztransfer->header2Number * 4), 1, 1, 0);
	free(str);

	//  Labels (char) array
	ztransfer->userHeaderNumber = numberIntsInBytes(tinStruct->pointLabelLen);
	ztransfer->userHeader = (int *)calloc(ztransfer->userHeaderNumber + 2, 4);
	ztransfer->header2Mode = 1;
	//  Need to be sure to copy char into in array, not cast and assign to header
	 charInt (tinStruct->pointLabel, ztransfer->userHeader, tinStruct->pointLabelLen, (ztransfer->userHeaderNumber * 4), 1, 1, 0);

	 //  Values 1, x, y, value
	 ztransfer->values1Number = 3 * tinStruct->numberPoints;
	 ztransfer->values1 = (int *)calloc(ztransfer->values1Number, 4);
	 ztransfer->values1Mode = 1;
	 convertDataArray((int *)tinStruct->xCoordinate, &ztransfer->values1[0], tinStruct->numberPoints, 1, 1);
	 convertDataArray((int *)tinStruct->yCoordinate, &ztransfer->values1[tinStruct->numberPoints], tinStruct->numberPoints, 1, 1);
	 convertDataArray((int *)tinStruct->value, &ztransfer->values1[(tinStruct->numberPoints * 2)], tinStruct->numberPoints, 1, 1);

	  //  Values 2, other arrays
	 ztransfer->values2Number = 3 * tinStruct->numberPoints;
	 ztransfer->values2 = (int *)calloc(ztransfer->values2Number, 4);
	 ztransfer->values2Mode = 1;
	 convertDataArray((int *)tinStruct->pointType, &ztransfer->values2[0], tinStruct->numberPoints, 1, 1);
	 convertDataArray((int *)tinStruct->numberConnections, &ztransfer->values2[(tinStruct->numberPoints * 2)], tinStruct->numberPoints, 1, 1);

	 //  Value 3, connections  tinStruct->connection
	 if (tinStruct->connectTableLen > 0) {
		 ztransfer->values3Number = tinStruct->connectTableLen;
		 ztransfer->values3 = (int *)calloc(ztransfer->values3Number, 4);
		 ztransfer->values3Mode = 1;
		 convertDataArray((int *)tinStruct->connectTo, ztransfer->values3, tinStruct->connectTableLen, 1, 1);
	 }

	 //  Save the date!
	 len = zpathnameGetPart (ztransfer->pathname, 4, dPart, sizeof(dPart));
	 if (len > 12) {
		dateStatus = spatialDateTime(dPart, &julianFirstValue, &secondsFirstValue);
		if (dateStatus == STATUS_OKAY) {
			len = zpathnameGetPart (ztransfer->pathname, 5, ePart, sizeof(ePart));
			if (len < 5) {
				//  An empty E part indicating an instantaneous dataset.
				//  Set end to start
				julianLastValue = julianFirstValue;
				secondsLastValue = secondsFirstValue;
			}
			else {
				dateStatus = spatialDateTime(ePart, &julianLastValue, &secondsLastValue);
			}
			if (dateStatus == STATUS_OKAY) {
				ifltab[zdssKeys.kdataFirstDate] = i4toi8(julianFirstValue, secondsFirstValue);
				ifltab[zdssKeys.kdataLastDate]  = i4toi8(julianLastValue, secondsLastValue);
			}
		}
	}

	 if (bigEndian()) {
		 zswitchInts(ztransfer->internalHeader, ztransfer->internalHeaderNumber);
		 zswitchInts(ztransfer->values1, ztransfer->values1Number);
		 zswitchInts(ztransfer->values2, ztransfer->values2Number);
		 zswitchInts(ztransfer->values3, ztransfer->values3Number);
	 }

	 //  Transfer struct is ready to go.   Write it!
	 status = zwrite(ifltab, ztransfer);
	 zstructFree(ztransfer);

	 if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zspatialTinStore_ID, "Exit; status:  ", status);
	 }

	 if (zisError(status)) {
		//  An error code
		status = zerrorUpdate(ifltab, status, DSS_FUNCTION_zspatialTinStore_ID);
	 }
	 return status;

}

