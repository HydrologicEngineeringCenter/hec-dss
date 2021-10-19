#include <stdlib.h>
#include <string.h>

#include "heclib.h"

/**
*  Function:	zstructSpatialGridNew
*
*  Use:			Public
*
*  Description:	Creates a new spatial GRID struct
*
*  Declaration: zStructSpatialGrid* zstructSpatialGridNew(const char* pathname);
*
*  Parameters:	const char* pathname
*					The pathname for this struct.  Must be a valid pathname.
*					A copy of the pathname is used in struct.
*
*
*
*	Returns:	zStructSpatialGrid*
*					An address to the struct created.
*
*   Note:		ALWAYS call function zstructFree(struct) when finished using.
*
*	See:		zspatialGridStore and zspatialGridRetrieve for more information
*
*
*
*	Author:			????
*	Date:			2018
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


zStructSpatialGrid* zstructSpatialGridNew(const char* pathname) {
	zStructSpatialGrid *gridStruct;
	int len;

	len = sizeof(zStructSpatialGrid);
	gridStruct = (zStructSpatialGrid*)calloc((size_t)len, BYTE_SIZE);
	if (!gridStruct) {
		return gridStruct;
	}
	if (pathname) {
		gridStruct->pathname = mallocAndCopyPath(pathname);
	}
	gridStruct->_structVersion = VERSION_100;
	return gridStruct;
}

void printGridStruct(long long *ifltab, int fid, zStructSpatialGrid *gridStruct) {
	char *messageString, *arrayString;
	arrayString = 0;

	messageString = (char *)calloc(16, 1);

	zmessageDebug(ifltab, fid, "******** Printing GRID STRUCT ********", "");
	zmessageDebugInt(ifltab, fid, "Struct Type :", gridStruct->structType);
	zmessageDebugInt(ifltab, fid, "GridStruct Version :", gridStruct->_structVersion);
	zmessageDebug(ifltab, fid, "Path :", gridStruct->pathname);
	zmessageDebugInt(ifltab, fid, "GridType :", gridStruct->_type);
	zmessageDebugInt(ifltab, fid, "Version : ", gridStruct->_version);
	zmessageDebug(ifltab, fid, "Data Units : ", gridStruct->_dataUnits);
	zmessageDebugInt(ifltab, fid, "Data Type : ", gridStruct->_dataType);
	zmessageDebug(ifltab, fid, "Data Source : ", gridStruct->_dataSource);
	zmessageDebugInt(ifltab, fid, "LowerLeftCellX : ", gridStruct->_lowerLeftCellX);
	zmessageDebugInt(ifltab, fid, "LowerLeftCellY : ", gridStruct->_lowerLeftCellY);
	zmessageDebugInt(ifltab, fid, "NumberOfCellsX : ", gridStruct->_numberOfCellsX);
	zmessageDebugInt(ifltab, fid, "NumberOfCellsY : ", gridStruct->_numberOfCellsY);
	sprintf(messageString, "%5.5f", gridStruct->_cellSize);
	zmessageDebug(ifltab, fid, "CellSize : ", messageString);
	zmessageDebugInt(ifltab, fid, "CompressionMethod : ", gridStruct->_compressionMethod);
	zmessageDebugInt(ifltab, fid, "SizeofCompressedElements : ", gridStruct->_sizeofCompressedElements);

	zmessageDebugInt(ifltab, fid, "NumberOfRanges : ", gridStruct->_numberOfRanges);
	zmessageDebug(ifltab, fid, "SrsName : ", gridStruct->_srsName);
	zmessageDebugInt(ifltab, fid, "SrsDefinitionType : ", gridStruct->_srsDefinitionType);
	zmessageDebug(ifltab, fid, "_srsDefinition : ", gridStruct->_srsDefinition);
	sprintf(messageString, "%5.5f", gridStruct->_xCoordOfGridCellZero);
	zmessageDebug(ifltab, fid, "XCoordOfGridCellZero : ", messageString);
	sprintf(messageString, "%5.5f", gridStruct->_yCoordOfGridCellZero);
	zmessageDebug(ifltab, fid, "YCoordOfGridCellZero : ", messageString);
	sprintf(messageString, "%5.5f", gridStruct->_nullValue);
	zmessageDebug(ifltab, fid, "NullValue : ", messageString);
	zmessageDebug(ifltab, fid, "TimeZoneID : ", gridStruct->_timeZoneID);
	zmessageDebugInt(ifltab, fid, "timeZoneRawOffset : ", gridStruct->_timeZoneRawOffset);
	zmessageDebugInt(ifltab, fid, "IsInterval : ", gridStruct->_isInterval);
	zmessageDebugInt(ifltab, fid, "IsTimeStamped : ", gridStruct->_isTimeStamped);
	zmessageDebugInt(ifltab, fid, "_storageDataType : ", gridStruct->_storageDataType);
	zmessageDebug(ifltab, fid, "Range Limit Table: ", "");

	if (gridStruct->_storageDataType == GRID_FLOAT) {
		sprintf(messageString, "%5.5f", *((float *)gridStruct->_maxDataValue));
		zmessageDebug(ifltab, fid, "Max Data Value : ", messageString);
		sprintf(messageString, "%5.5f", *((float *)gridStruct->_minDataValue));
		zmessageDebug(ifltab, fid, "Min Data Value : ", messageString);
		sprintf(messageString, "%5.5f", *((float *)gridStruct->_meanDataValue));
		zmessageDebug(ifltab, fid, "Mean Data Value : ", messageString);
		if (gridStruct->_numberOfRanges > 0) {
			arrayString = calloc(gridStruct->_numberOfRanges * 16, 1);
			for (int i = 0; i < gridStruct->_numberOfRanges && arrayString !=0; i++) {
				snprintf(messageString, 16,"%5.5f,", *((float*)gridStruct->_rangeLimitTable + i));
				strncat(arrayString, messageString, strlen(messageString));
			}
		}
		if (arrayString != 0) {
			zmessageDebug(ifltab, fid, "Range Limit Table: ", arrayString);
			arrayString[0] = 0;
			for (int i = 0; i < gridStruct->_numberOfRanges; i++) {
				snprintf(messageString, 16, "%5d,", gridStruct->_numberEqualOrExceedingRangeLimit[i]);
				strncat(arrayString, messageString, strlen(messageString));
			}
			zmessageDebug(ifltab, fid, "Histo Table: ", arrayString);
			free(arrayString);
		}
	}

	free(messageString);

}
