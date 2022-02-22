#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "hecdssInternal.h"
#include "zdssKeys.h"

/**
*  Function:	zspatialGridStore
*
*  Use:			Public
*
*  Description:	Store a GRID record
*
*  Declaration: int zspatialGridStore(long long *ifltab, zStructSpatialGrid *gridStruct);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				zStructSpatialGrid *gridStruct
*					A GRID struct that contains data for a single spatial GRID record.
*					This struct is created by the following method:
*						zStructSpatialGrid* zstructSpatialGridNew(const char* pathname);
*					with pathname being an valid existing pathname
*					When you are finished with the struct, the struct must be freed by a call to
*						void zstructFree(zStructSpatialGrid *gridStruct)
*					NEVER REUSE A zStructSpatialGrid, always free and create a new on.
*
*	Returns:	int status
*					STATUS_OKAY
*					error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*	Comments:
*
*
*	Author:			Prasad Vemulapati
*	Date:			2018
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*
*/


int zspatialGridStore(long long *ifltab, zStructSpatialGrid *gridStruct) {

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
	void *buffer;
	int bufsize;
	int *internalHeader;
	if (!gridStruct) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialGridStore_ID, zdssErrorCodes.NULL_ARGUMENT,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "gridStruct is null");
	}
	if (!gridStruct->pathname) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialGridStore_ID, zdssErrorCodes.NULL_PATHNAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "gridStruct pathname is null");
	}

	//  Messages and debug
	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessage(ifltab, " ");
		zmessageDebug(ifltab, DSS_FUNCTION_zspatialGridStore_ID, "Enter; Pathname: ", gridStruct->pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zspatialGridStore_ID, "Handle:  ", zhandle(ifltab));
		zmessageDebugInt(ifltab, DSS_FUNCTION_zspatialGridStore_ID, "Number of points:  ", gridStruct->_numberOfCellsX * gridStruct->_numberOfCellsY);
	}

	//  Check for correct DSS Version
	if (zgetVersion(ifltab) != 7) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialGridStore_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
			zgetVersion((void *)ifltab), 0, zdssErrorSeverity.WARNING, "", "");
	}


	ztransfer = zstructTransferNew(gridStruct->pathname, 0);
	if (!ztransfer) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialGridStore_ID,
			zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
			zdssErrorSeverity.MEMORY_ERROR,
			gridStruct->pathname, "Allocating ztransfer struct");
	}
	ztransfer->dataType = gridStruct->_type;

	//  Header 1, basic info
	ztransfer->internalHeaderNumber = INT_HEAD_grid_internalHeaderNumber;


	ztransfer->internalHeaderMode = 1;
	internalHeader = (int *)calloc(ztransfer->internalHeaderNumber, 4);
	internalHeader[INT_HEAD_grid_structVersion] = gridStruct->_structVersion;
	internalHeader[INT_HEAD_grid_type] = gridStruct->_type;
	internalHeader[INT_HEAD_grid_version] = gridStruct->_version;
	internalHeader[INT_HEAD_grid_dataType] = gridStruct->_dataType;
	internalHeader[INT_HEAD_grid_lowerLeftCellX] = gridStruct->_lowerLeftCellX;
	internalHeader[INT_HEAD_grid_lowerLeftCellY] = gridStruct->_lowerLeftCellY;
	internalHeader[INT_HEAD_grid_numberOfCellsX] = gridStruct->_numberOfCellsX;
	internalHeader[INT_HEAD_grid_numberOfCellsY] = gridStruct->_numberOfCellsY;
	internalHeader[INT_HEAD_grid_compressionMethod] = gridStruct->_compressionMethod;
	internalHeader[INT_HEAD_grid_numberOfRanges] = gridStruct->_numberOfRanges;
	internalHeader[INT_HEAD_grid_srsDefinitionType] = gridStruct->_srsDefinitionType;
	internalHeader[INT_HEAD_grid_timeZoneRawOffset] = gridStruct->_timeZoneRawOffset;
	internalHeader[INT_HEAD_grid_isInterval] = gridStruct->_isInterval;
	internalHeader[INT_HEAD_grid_isTimeStamped] = gridStruct->_isTimeStamped;
	internalHeader[INT_HEAD_grid_storageDataType] = gridStruct->_storageDataType;



	//  Header 2, character information
	//  Count the number of characters (including nulls) in struct strings
	total = 0;
	if (gridStruct->_dataUnits) {
		len = strlen(gridStruct->_dataUnits);
		total += len;
	}
	total++; //  null terminator
	if (gridStruct->_dataSource) {
		len = strlen(gridStruct->_dataSource);
		total += len;
	}
	total++;
	if (gridStruct->_srsName) {
		len = strlen(gridStruct->_srsName);
		total += len;
	}
	total++;
	if (gridStruct->_srsDefinition) {
		len = strlen(gridStruct->_srsDefinition);
		total += len;
	}
	total++;
	if (gridStruct->_timeZoneID) {
		len = strlen(gridStruct->_timeZoneID);
		total += len;
	}
	total++;


	//  Copy strings into one array
	str = (char *)calloc(total, 1);
	count = 0;
	if (gridStruct->_dataUnits) {
		len = strlen(gridStruct->_dataUnits);
		stringCopy(&str[count], total, gridStruct->_dataUnits, len);
		count += len;
	}
	str[count++] = '\0';
	if (gridStruct->_dataSource) {
		len = strlen(gridStruct->_dataSource);
		stringCopy(&str[count], total, gridStruct->_dataSource, len);
		count += len;
	}
	str[count++] = '\0';
	if (gridStruct->_srsName) {
		len = strlen(gridStruct->_srsName);
		stringCopy(&str[count], total, gridStruct->_srsName, len);
		count += len;
	}
	str[count++] = '\0';
	if (gridStruct->_srsDefinition) {
		len = strlen(gridStruct->_srsDefinition);
		stringCopy(&str[count], total, gridStruct->_srsDefinition, len);
		count += len;
	}
	str[count++] = '\0';
	if (gridStruct->_timeZoneID) {
		len = strlen(gridStruct->_timeZoneID);
		stringCopy(&str[count], total, gridStruct->_timeZoneID, len);
		count += len;
	}
	str[count++] = '\0';


	ztransfer->header2Number = numberIntsInBytes((int)total);
	ztransfer->header2 = (int *)calloc(ztransfer->header2Number + 2, 4);
	ztransfer->header2Mode = 1;
	//  Need to be sure to copy char into in array, not cast and assign to header
	charInt(str, ztransfer->header2, (int)total, (ztransfer->header2Number * 4), 1, 1, 0);
	free(str);

	//  Store floats: _cellSize,_xCoordOfGridCellZero,_yCoordOfGridCellZero,_nullValue
	ztransfer->userHeaderNumber = (4*2)+3;
	ztransfer->userHeader = (int *)calloc(ztransfer->userHeaderNumber, 4);
	ztransfer->userHeaderMode = 1;
	memset(ztransfer->userHeader, 0, ztransfer->userHeaderNumber * 4);
	memcpy(&ztransfer->userHeader[0], &gridStruct->_cellSize, 4);
	memcpy(&ztransfer->userHeader[1], &gridStruct->_xCoordOfGridCellZero, 4);
	memcpy(&ztransfer->userHeader[2], &gridStruct->_yCoordOfGridCellZero, 4);
	memcpy(&ztransfer->userHeader[3], &gridStruct->_nullValue, 4);
	if (getEndian()) {
		zswitchInts(ztransfer->userHeader, ztransfer->userHeaderNumber);
	}

	//  Store additional compression parameters (for future use)
	ztransfer->values2Number = 0;
	ztransfer->values2 = 0;

	if (gridStruct->_storageDataType == GRID_FLOAT) {
		//  Range Limits and min/max/mean values
		ztransfer->values1Number = gridStruct->_numberOfRanges * 2 + 3;
		ztransfer->values1 = (int *)calloc(ztransfer->values1Number, 4);
		ztransfer->values1Mode = 1;
		convertDataArray((int *)(gridStruct->_minDataValue), &ztransfer->values1[0], 1, 1, 1);
		convertDataArray((int *)(gridStruct->_maxDataValue), &ztransfer->values1[1], 1, 1, 1);
		convertDataArray((int *)(gridStruct->_meanDataValue), &ztransfer->values1[2], 1, 1, 1);
		convertDataArray((int *)(gridStruct->_rangeLimitTable), &ztransfer->values1[3], gridStruct->_numberOfRanges, 1, 1);
		convertDataArray((int *)(&gridStruct->_numberEqualOrExceedingRangeLimit[0]), &ztransfer->values1[gridStruct->_numberOfRanges + 3], gridStruct->_numberOfRanges, 1, 1);
		int dataSize = (gridStruct->_numberOfCellsX * gridStruct->_numberOfCellsY);
		int numLongs = ((dataSize - 1) / 2) + 1;
		int *dataValues = calloc(numLongs, 8);
		memcpy(dataValues, gridStruct->_data, dataSize * 4);
		switch (gridStruct->_compressionMethod) {
		case NO_COMPRESSION:
			gridStruct->_sizeofCompressedElements = dataSize * 4;
			internalHeader[INT_HEAD_grid_sizeofCompressedElements] = gridStruct->_sizeofCompressedElements;
			ztransfer->values3Number = numLongs * 2;
			ztransfer->values3 = (int *)dataValues;
			if (getEndian()) {
				zswitchInts(ztransfer->values3, ztransfer->values3Number);
			}


			break;
		case ZLIB_COMPRESSION:
#if 0
		{
			printf("XXX: Before Values: ");
			for (int i = 0; i < numLongs * 2; i++)
				printf("%x,", dataValues[i]);
			printf("\n");
		}
#endif
		if (getEndian()) {
			zswap((long long *)dataValues, numLongs * 2);
			zswitchInts(dataValues, numLongs * 2);
		}



		bufsize = compress_zlib(dataValues, numLongs * 2 * 4, &buffer);
		/*if (compress_zfp(gridStruct->data, gridStruct->_numberOfCellsX, gridStruct->_numberOfCellsY, (float)1.0e-6, &buffer, &bufsize, 0)) */
		if (bufsize <= 0) {
			free(dataValues);
			zstructFree(ztransfer);
			return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialGridStore_ID, zdssErrorCodes.WRITE_ERROR,
				0, 0, zdssErrorSeverity.WRITE_ERROR, "", "gridStruct error in zlib compression");
		}
#if 0
		{
			char *cdata = buffer;
			printf("XXX: Compressed Buffer: ");
			for (int i = 0; i < bufsize; i++)
				printf("%x,", (int)cdata[i]);
			printf("\n");
			printf("XXX: Values: ");
			for (int i = 0; i < numLongs * 2; i++)
				printf("%x,", dataValues[i]);
			printf("\n");
		}
#endif
		gridStruct->_sizeofCompressedElements = bufsize;
		internalHeader[INT_HEAD_grid_sizeofCompressedElements] = gridStruct->_sizeofCompressedElements;
		ztransfer->values3Number = numberIntsInBytes(bufsize);
		ztransfer->values3 = (int *)calloc((size_t)ztransfer->values3Number + 2, 4);
		charInt((void *)buffer, (void *)ztransfer->values3, bufsize, (ztransfer->values3Number * 4), 1, 1, 0);
		free(buffer);
		free(dataValues);
		break;
		default:
			zstructFree(ztransfer);
			return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialGridStore_ID, zdssErrorCodes.WRITE_ERROR,
				0, 0, zdssErrorSeverity.WRITE_ERROR, "", "gridStruct: Unsupported Compression");

		}
		if (getEndian()) {
			zswitchInts(ztransfer->values1, ztransfer->values1Number);
		}
	}
	else {
		zstructFree(ztransfer);
		return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialGridStore_ID, zdssErrorCodes.WRITE_ERROR,
			0, 0, zdssErrorSeverity.WRITE_ERROR, "", "gridStruct: Unsupported storage data type");
	}



	//  Save the date!
	len = zpathnameGetPart(ztransfer->pathname, 4, dPart, sizeof(dPart));
	if (len > 12) {
		dateStatus = spatialDateTime(dPart, &julianFirstValue, &secondsFirstValue);
		if (dateStatus == STATUS_OKAY) {
			len = zpathnameGetPart(ztransfer->pathname, 5, ePart, sizeof(ePart));
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
				ifltab[zdssKeys.kdataLastDate] = i4toi8(julianLastValue, secondsLastValue);
			}
		}
	}
	ztransfer->internalHeader = internalHeader;
	if (getEndian()) {
		zswitchInts(ztransfer->internalHeader, ztransfer->internalHeaderNumber);
	}

	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		printGridStruct(ifltab, DSS_FUNCTION_zspatialGridStore_ID, gridStruct);
	}
	//  Transfer struct is ready to go.   Write it!
	status = zwrite(ifltab, ztransfer);
	zstructFree(ztransfer);

	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zspatialGridStore_ID, "Exit; status:  ", status);
	}

	if (zisError(status)) {
		//  An error code
		status = zerrorUpdate(ifltab, status, DSS_FUNCTION_zspatialGridStore_ID);
	}
	return status;

}
