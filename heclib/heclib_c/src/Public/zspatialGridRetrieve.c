#include <string.h>
#include <stdlib.h>

#include "heclib.h"

/**
*  Function:	zspatialGridRetrieveVersion
*
*  Use:			Public
*
*  Description:	Retrieve gridded struct version. A return of '0' indicates DSS 6 type grid.
*
*  Declaration: int zspatialGridRetrieveVersion(long long *ifltab, const char *cpath, int* gridStructVersion)
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				const char *cpath
*					Path name
*
*				int* gridStructVersion
*					Grid structure version is returned in this
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
*	Author:			Prasad Vemulapati
*	Date:			2018
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*
*/



int zspatialGridRetrieveVersion(long long *ifltab, const char *cpath, int* gridStructVersion) {
	int status;
	char messageString[80];

	zStructTransfer* ztransfer;

	if (!cpath) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialGridRetrieve_ID, zdssErrorCodes.NULL_PATHNAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "gridStruct pathname is null");
	}


	//  Messages and debug
	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessage(ifltab, " ");
		zmessageDebug(ifltab, DSS_FUNCTION_zspatialGridRetrieve_ID, "Enter ; Pathname: ", cpath);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zspatialGridRetrieve_ID, "Handle:  ", zhandle(ifltab));
	}



	ztransfer = zstructTransferNew(cpath, 1);
	if (!ztransfer) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialGridRetrieve_ID,
			zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
			zdssErrorSeverity.MEMORY_ERROR,
			cpath, "Allocating ztransfer struct");
	}


	//  Do not retrieve the data, just the meta data in the header
	ztransfer->header2Mode = 0;
	ztransfer->userHeaderMode = 0;
	ztransfer->values1Mode = 0;
	ztransfer->values2Mode = 0;
	ztransfer->values3Mode = 0;


	status = zread(ifltab, ztransfer);

	if (zisError(status)) {
		//  An error code
		status = zerrorUpdate(ifltab, status, DSS_FUNCTION_zspatialGridRetrieve_ID);
		zstructFree(ztransfer);
		return status;
	}


	if (status != STATUS_RECORD_FOUND) {
		if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_GENERAL)) {
			snprintf(messageString, sizeof(messageString), ZREAD_NOT_EXIST, zhandle(ifltab));
			zmessage2(ifltab, messageString, ztransfer->pathname);
		}
		zstructFree(ztransfer);
		return status;
	}

	if (bigEndian()) {
		zswitchInts(ztransfer->internalHeader, ztransfer->internalHeaderNumber);
	}

	if (ztransfer->internalHeader[0] < 0)
		*gridStructVersion = -ztransfer->internalHeader[0];
	else
		*gridStructVersion = 0;

	zstructFree(ztransfer);
	return status;
}

/**
*  Function:	zspatialGridRetrieve
*
*  Use:			Public
*
*  Description:	Retrieve a GRID record
*
*  Declaration: int zspatialGridRetrieve(long long *ifltab, zStructSpatialGrid *gridStruct, int boolRetrieveData);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				zStructSpatialGrid *gridStruct
*					A GRID struct that will contain data for a single spatial GRID record.
*					This struct is created by the following method:
*						zStructSpatialGrid* zstructSpatialGridNew(const char* pathname);
*					with pathname being an valid existing pathname
*					When you are finished with the struct, the struct must be freed by a call to
*						void zstructFree(zStructText *textStruct)
*					NEVER REUSE A zStructSpatialGrid, always free and create a new on.
*
*				int boolRetrieveData
*					A flag indicating if both data and meta-data (header info) should be retrieve
*					= 0, Just retrieve meta-data
*					= 1, Retrieve both meta-data and GRID arrays
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
*	Author:			Prasad Vemulapati
*	Date:			2018
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*
*/

int zspatialGridRetrieve(long long *ifltab, zStructSpatialGrid *gridStruct, int boolRetrieveData) {

	int status;
	size_t count;
	size_t len;
	char *str;
	char messageString[80];

	zStructTransfer* ztransfer;
	int ret;
	int *values;
	void *buffer;

	if (!gridStruct) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialGridRetrieve_ID, zdssErrorCodes.NULL_ARGUMENT,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "gridStruct is null");
	}
	if (!gridStruct->pathname) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialGridRetrieve_ID, zdssErrorCodes.NULL_PATHNAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "gridStruct pathname is null");
	}


	//  Messages and debug
	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessage(ifltab, " ");
		zmessageDebug(ifltab, DSS_FUNCTION_zspatialGridRetrieve_ID, "Enter ; Pathname: ", gridStruct->pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zspatialGridRetrieve_ID, "Handle:  ", zhandle(ifltab));
		zmessageDebugInt(ifltab, DSS_FUNCTION_zspatialGridRetrieve_ID, "boolRetrieveData flag:  ", boolRetrieveData);
	}

	//  Check for correct DSS Version
	if (zgetVersion(ifltab) != 7) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialGridRetrieve_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
			zgetVersion((void *)ifltab), 0, zdssErrorSeverity.WARNING, "", "");
	}


	ztransfer = zstructTransferNew(gridStruct->pathname, 1);
	if (!ztransfer) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialGridRetrieve_ID,
			zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
			zdssErrorSeverity.MEMORY_ERROR,
			gridStruct->pathname, "Allocating ztransfer struct");
	}

	if (!boolRetrieveData) {
		//  Do not retrieve the data, just the meta data in the header
		ztransfer->userHeaderMode = 1;
		ztransfer->values3Mode = 0;
	}

	status = zread(ifltab, ztransfer);
	if (zisError(status)) {
		//  An error code
		status = zerrorUpdate(ifltab, status, DSS_FUNCTION_zspatialGridRetrieve_ID);
		zstructFree(ztransfer);
		return status;
	}


	if (status != STATUS_RECORD_FOUND) {
		if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_GENERAL)) {
			snprintf(messageString, sizeof(messageString), ZREAD_NOT_EXIST, zhandle(ifltab));
			zmessage2(ifltab, messageString, ztransfer->pathname);
		}
		zstructFree(ztransfer);
		return status;
	}


	if (ztransfer->dataType < DATA_TYPE_UGT || ztransfer->dataType > DATA_TYPE_SG) {
		status = zerrorProcessing(ifltab, DSS_FUNCTION_zspatialGridRetrieve_ID,
			zdssErrorCodes.WRONG_RECORD_TYPE, ztransfer->dataType,
			(long long)ztransfer->dataType,
			zdssErrorSeverity.WARNING, ztransfer->pathname, "");
		zstructFree(ztransfer);
		return status;
	}

	if (bigEndian()) {
		zswitchInts(ztransfer->internalHeader, ztransfer->internalHeaderNumber);
	}

	//  Header 1, basic info
	gridStruct->_structVersion = ztransfer->internalHeader[INT_HEAD_grid_structVersion];
	// Can only handle version 1 records 
	if (gridStruct->_structVersion == VERSION_100) {
		gridStruct->_type = ztransfer->internalHeader[INT_HEAD_grid_type];
		gridStruct->_dataType = ztransfer->internalHeader[INT_HEAD_grid_dataType];
		gridStruct->_lowerLeftCellX = ztransfer->internalHeader[INT_HEAD_grid_lowerLeftCellX];
		gridStruct->_lowerLeftCellY = ztransfer->internalHeader[INT_HEAD_grid_lowerLeftCellY];
		gridStruct->_numberOfCellsX = ztransfer->internalHeader[INT_HEAD_grid_numberOfCellsX];
		gridStruct->_numberOfCellsY = ztransfer->internalHeader[INT_HEAD_grid_numberOfCellsY];
		gridStruct->_compressionMethod = ztransfer->internalHeader[INT_HEAD_grid_compressionMethod];
		gridStruct->_sizeofCompressedElements = ztransfer->internalHeader[INT_HEAD_grid_sizeofCompressedElements];
		gridStruct->_numberOfRanges = ztransfer->internalHeader[INT_HEAD_grid_numberOfRanges];
		gridStruct->_srsDefinitionType = ztransfer->internalHeader[INT_HEAD_grid_srsDefinitionType];
		gridStruct->_timeZoneRawOffset = ztransfer->internalHeader[INT_HEAD_grid_timeZoneRawOffset];
		gridStruct->_isInterval = ztransfer->internalHeader[INT_HEAD_grid_isInterval];
		gridStruct->_isTimeStamped = ztransfer->internalHeader[INT_HEAD_grid_isTimeStamped];
		gridStruct->_storageDataType = ztransfer->internalHeader[INT_HEAD_grid_storageDataType];



		//  Header 2, character information
		str = (char *)calloc(ztransfer->header2Number + 2, 4);
		charInt(ztransfer->header2, str, (ztransfer->header2Number * 4), (ztransfer->header2Number * 4), 0, 0, 0);

		count = 0;
		gridStruct->_dataUnits = mallocAndCopy(&str[count]);
		len = strlen(gridStruct->_dataUnits);
		count += len;
		count++; //  null terminator
		gridStruct->_dataSource = mallocAndCopy(&str[count]);
		len = strlen(gridStruct->_dataSource);
		count += len;
		count++;
		gridStruct->_srsName = mallocAndCopy(&str[count]);
		len = strlen(gridStruct->_srsName);
		count += len;
		count++;
		gridStruct->_srsDefinition = mallocAndCopy(&str[count]);
		len = strlen(gridStruct->_srsDefinition);
		count += len;
		count++;
		gridStruct->_timeZoneID = mallocAndCopy(&str[count]);
		len = strlen(gridStruct->_timeZoneID);
		count += len;
		count++;


		free(str);

		//  Retrieve floats: _cellSize,_xCoordOfGridCellZero,_yCoordOfGridCellZero,_nullValue
		if (ztransfer->userHeader && (ztransfer->userHeaderNumber > 0)) {
			if (bigEndian()) {
				zswitchInts(ztransfer->userHeader, ztransfer->userHeaderNumber);
			}
			memcpy(&gridStruct->_cellSize,             &ztransfer->userHeader[0], 4);
			memcpy(&gridStruct->_xCoordOfGridCellZero, &ztransfer->userHeader[1], 4);
			memcpy(&gridStruct->_yCoordOfGridCellZero, &ztransfer->userHeader[2], 4);
			memcpy(&gridStruct->_nullValue,            &ztransfer->userHeader[3], 4);
		}

		if (gridStruct->_storageDataType == GRID_FLOAT) {
			if (bigEndian()) {
				zswitchInts(ztransfer->values1, ztransfer->values1Number);
			}
			//  Values 1, x, y, value
			if (ztransfer->values1 && (ztransfer->values1Number > 0)) {
				gridStruct->_maxDataValue = calloc(1, 4);
				gridStruct->_minDataValue = calloc(1, 4);
				gridStruct->_meanDataValue = calloc(1, 4);
				convertDataArray(&ztransfer->values1[0], (int *)gridStruct->_minDataValue, 1, 1, 1);
				convertDataArray(&ztransfer->values1[1], (int *)gridStruct->_maxDataValue, 1, 1, 1);
				convertDataArray(&ztransfer->values1[2], (int *)gridStruct->_meanDataValue, 1, 1, 1);
				gridStruct->_rangeLimitTable = (float *)calloc(gridStruct->_numberOfRanges, 4);
				gridStruct->_numberEqualOrExceedingRangeLimit = (int *)calloc(gridStruct->_numberOfRanges, 4);
				convertDataArray(&ztransfer->values1[3], (int *)gridStruct->_rangeLimitTable, gridStruct->_numberOfRanges, 1, 1);
				convertDataArray(&ztransfer->values1[gridStruct->_numberOfRanges + 3], (int *)gridStruct->_numberEqualOrExceedingRangeLimit, gridStruct->_numberOfRanges, 1, 1);
			}

			//  No values2 to retrieve

			// Actual Data 
			if (ztransfer->values3 && (ztransfer->values3Number > 0)) {
				int dataSize = (gridStruct->_numberOfCellsX * gridStruct->_numberOfCellsY);
				int numLongs = ((dataSize - 1) / 2) + 1;
				switch (gridStruct->_compressionMethod) {
				case NO_COMPRESSION:
					if (bigEndian()) {
						zswitchInts(ztransfer->values3, ztransfer->values3Number);
					}
					gridStruct->_data = (int *)calloc(dataSize, 4);
					convertDataArray(&ztransfer->values3[0], (int *)gridStruct->_data, dataSize, 1, 1);
					break;
				case ZLIB_COMPRESSION:
					buffer = (void *)calloc(ztransfer->values3Number + 2, 4);
					charInt((void *)ztransfer->values3, buffer, ztransfer->values3Number * 4, ztransfer->values3Number * 4, 0, 0, 0);
					// Uncompress data
					values = (int *)calloc(numLongs * 2, 4);
					ret = uncompress_zlib(buffer, gridStruct->_sizeofCompressedElements, values, numLongs * 2 * 4);
					/*if (compress_zfp(gridStruct->data, gridStruct->_numberOfCellsX, gridStruct->_numberOfCellsY, (float)1.0e-6, &buffer, &(gridStruct->_sizeofCompressedElements), 1))*/
					if (ret < 0) {
						free(buffer);
						free(values);
						zstructFree(ztransfer);
						return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialGridRetrieve_ID, zdssErrorCodes.READ_ERROR,
							0, 0, zdssErrorSeverity.READ_ERROR, "", "gridStruct error in decompression");
					}
					else {
#if 0
						printf("XXX: Values: ");
						for (int i = 0; i < numLongs * 2; i++)
							printf("%x,", values[i]);
						printf("\n");
#endif
						gridStruct->_data = values;

						if (bigEndian()) {
							zswap(gridStruct->_data, numLongs * 2);
							zswitchInts(gridStruct->_data, numLongs * 2);
						}
#if 0
						{
							char *cdata = buffer;
							printf("XXX: Compressed Buffer: ");
							for (int i = 0; i < gridStruct->_sizeofCompressedElements; i++)
								printf("%x,", (int)cdata[i]);
							printf("\n");
							printf("XXX: Values: ");
							for (int i = 0; i < numLongs * 2; i++)
								printf("%x,", values[i]);
							printf("\n");
						}
#endif
					}
					free(buffer);
				}

			}
		}
		else {
			zstructFree(ztransfer);
			return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialGridStore_ID, zdssErrorCodes.READ_ERROR,
				0, 0, zdssErrorSeverity.READ_ERROR, "", "gridStruct retrieve error: unsupported data type");
		}
		if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			printGridStruct(ifltab, DSS_FUNCTION_zspatialGridRetrieve_ID, gridStruct);
		}
		if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_zspatialGridRetrieve_ID, "Exit; status:  ", status);
		}

		zstructFree(ztransfer);
		return status;
	}
	else {
		zstructFree(ztransfer);
		return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialGridStore_ID, zdssErrorCodes.READ_ERROR,
			0, 0, zdssErrorSeverity.READ_ERROR, "", "gridStruct retrieve error: unsupported gridstruct version");
	}

}

