#include "pch.h"
#include "NativeSpatialGridWrapper.h"

BSTR GetPathName(zStructSpatialGrid* grid)
{
	return ANSItoBSTR(grid->pathname);
}

void SetPathName(zStructSpatialGrid* grid, const char* value)
{
	if (grid->pathname)
		free(grid->pathname);

	grid->pathname = strdup(value);
}

int GetGridType(zStructSpatialGrid* grid)
{
	return grid->_type;
}

BSTR GetDataUnits(zStructSpatialGrid* grid)
{
	return ANSItoBSTR(grid->_dataUnits);
}

void SetDataUnits(zStructSpatialGrid* grid, const char* value)
{
	if (grid->_dataUnits)
		free(grid->_dataUnits);

	grid->_dataUnits = strdup(value);
}

int GetDataType(zStructSpatialGrid* grid)
{
	return grid->_dataType;
}

void SetDataType(zStructSpatialGrid* grid, int value)
{
	grid->_dataType = value;
}

int GetLowerLeftCellX(zStructSpatialGrid* grid)
{
	return grid->_lowerLeftCellX;
}

void SetLowerLeftCellX(zStructSpatialGrid* grid, int value)
{
	grid->_lowerLeftCellX = value;
}

int GetLowerLeftCellY(zStructSpatialGrid* grid)
{
	return grid->_lowerLeftCellY;
}

void SetLowerLeftCellY(zStructSpatialGrid* grid, int value)
{
	grid->_lowerLeftCellY = value;
}

int GetNumberOfCellsX(zStructSpatialGrid* grid)
{
	return grid->_numberOfCellsX;
}

void SetNumberOfCellsX(zStructSpatialGrid* grid, int value)
{
	grid->_numberOfCellsX = value;
}

int GetNumberOfCellsY(zStructSpatialGrid* grid)
{
	return grid->_numberOfCellsY;
}

void SetNumberOfCellsY(zStructSpatialGrid* grid, int value)
{
	grid->_numberOfCellsY = value;
}

float GetCellSize(zStructSpatialGrid* grid)
{
	return grid->_cellSize;
}

void SetCellSize(zStructSpatialGrid* grid, float value)
{
	grid->_cellSize = value;
}

BSTR GetSRSName(zStructSpatialGrid* grid)
{
	return ANSItoBSTR(grid->_srsName);
}

void SetSRSName(zStructSpatialGrid* grid, const char* value)
{
	if (grid->_srsName)
		free(grid->_srsName);

	grid->_srsName = strdup(value);
}

int GetSRSDefinitionType(zStructSpatialGrid* grid)
{
	return grid->_srsDefinitionType;
}

void SetSRSDefinitionType(zStructSpatialGrid* grid, int value)
{
	grid->_srsDefinitionType = value;
}

BSTR GetSRSDefinition(zStructSpatialGrid* grid)
{
	return ANSItoBSTR(grid->_srsDefinition);
}

void SetSRSDefinition(zStructSpatialGrid* grid, const char* value)
{
	if (grid->_srsDefinition)
		free(grid->_srsDefinition);

	grid->_srsDefinition = strdup(value);
}

float GetXCoordOfGridCellZero(zStructSpatialGrid* grid)
{
	return grid->_xCoordOfGridCellZero;
}

void SetXCoordOfGridCellZero(zStructSpatialGrid* grid, float value)
{
	grid->_xCoordOfGridCellZero = value;
}

float GetYCoordOfGridCellZero(zStructSpatialGrid* grid)
{
	return grid->_yCoordOfGridCellZero;
}

void SetYCoordOfGridCellZero(zStructSpatialGrid* grid, float value)
{
	grid->_yCoordOfGridCellZero = value;
}

int GetStructVersion(zStructSpatialGrid* grid)
{
	return grid->_structVersion;
}

int GetVersion(zStructSpatialGrid* grid)
{
	return grid->_version;
}

int GetStorageDataType(zStructSpatialGrid* grid)
{
	return grid->_storageDataType;
}

int GetCompressionMethod(zStructSpatialGrid* grid)
{
	return grid->_compressionMethod;
}

int GetSizeOfCompressedElements(zStructSpatialGrid* grid)
{
	return grid->_sizeofCompressedElements;
}

int GetNumberOfRanges(zStructSpatialGrid* grid)
{
	return grid->_numberOfRanges;
}

void SetNumberOfRanges(zStructSpatialGrid* grid, int value)
{
	grid->_numberOfRanges = value;
}

float GetNullValue(zStructSpatialGrid* grid)
{
	return grid->_nullValue;
}

void SetNullValue(zStructSpatialGrid* grid, float value)
{
	grid->_nullValue = value;
}

BSTR GetTimeZoneID(zStructSpatialGrid* grid)
{
	return ANSItoBSTR(grid->_timeZoneID);
}

void SetTimeZoneID(zStructSpatialGrid* grid, const char* value)
{
	if (grid->_timeZoneID)
		free(grid->_timeZoneID);

	grid->_timeZoneID = strdup(value);
}

int GetTimeZoneRawOffset(zStructSpatialGrid* grid)
{
	return grid->_timeZoneRawOffset;
}

void SetTimeZoneRawOffset(zStructSpatialGrid* grid, int value)
{
	grid->_timeZoneRawOffset = value;
}

bool GetIsInterval(zStructSpatialGrid* grid)
{
	return grid->_isInterval;
}

void SetIsInterval(zStructSpatialGrid* grid, bool value)
{
	grid->_isInterval = value;
}

bool GetIsTimeStamped(zStructSpatialGrid* grid)
{
	return grid->_isTimeStamped;
}

void SetIsTimeStamped(zStructSpatialGrid* grid, bool value)
{
	grid->_isTimeStamped = value;
}

float GetMaxDataValue(zStructSpatialGrid* grid)
{
	return *((float*)grid->_maxDataValue);
}

void SetMaxDataValue(zStructSpatialGrid* grid, float value)
{
	*((float*)grid->_maxDataValue) = value;
}

float GetMinDataValue(zStructSpatialGrid* grid)
{
	return *((float*)grid->_minDataValue);
}

void SetMinDataValue(zStructSpatialGrid* grid, float value)
{
	*((float*)grid->_minDataValue) = value;
}

float GetMeanDataValue(zStructSpatialGrid* grid)
{
	return *((float*)grid->_meanDataValue);
}

void SetMeanDataValue(zStructSpatialGrid* grid, float value)
{
	*((float*)grid->_meanDataValue) = value;
}

float* GetRangeLimitTable(zStructSpatialGrid* grid)
{
	return (float*)grid->_rangeLimitTable;
}

void SetRangeLimitTable(zStructSpatialGrid* grid, float* value)
{
	if (grid->_rangeLimitTable)
		free(grid->_rangeLimitTable);

	grid->_rangeLimitTable = value;
}

float GetUndefinedValue(zStructSpatialGrid* grid)
{
	return UNDEFINED_FLOAT;
}

int* GetNumberEqualOrExceedingRangeLimit(zStructSpatialGrid* grid)
{
	return grid->_numberEqualOrExceedingRangeLimit;
}

void SetNumberEqualOrExceedingRangeLimit(zStructSpatialGrid* grid, int* value)
{
	if (grid->_numberEqualOrExceedingRangeLimit)
		free(grid->_numberEqualOrExceedingRangeLimit);

	grid->_numberEqualOrExceedingRangeLimit = (int*)malloc(sizeof(int) * grid->_numberOfRanges);
	if (grid->_numberEqualOrExceedingRangeLimit)
		memcpy(grid->_numberEqualOrExceedingRangeLimit, value, sizeof(int) * grid->_numberOfRanges);
}

float* GetData(zStructSpatialGrid* grid)
{
	return (float*)grid->_data;
}

void SetData(zStructSpatialGrid* grid, float* value)
{
	if (grid->_data)
		free(grid->_data);

	grid->_data = (float*)malloc(sizeof(float) * grid->_numberOfCellsX * grid->_numberOfCellsY);
	if (grid->_data)
		memcpy(grid->_data, value, sizeof(float) * grid->_numberOfCellsX * grid->_numberOfCellsY);
}
