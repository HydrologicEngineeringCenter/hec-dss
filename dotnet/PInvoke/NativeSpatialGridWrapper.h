#pragma once

#include "DSSGrid.h"
#include "pinvokeTools.h"

extern "C" {
	__declspec(dllexport) BSTR GetPathName(zStructSpatialGrid* grid);
	__declspec(dllexport) void SetPathName(zStructSpatialGrid* grid, const char* value);
	__declspec(dllexport) int GetGridType(zStructSpatialGrid* grid);
	__declspec(dllexport) BSTR GetDataUnits(zStructSpatialGrid* grid);
	__declspec(dllexport) void SetDataUnits(zStructSpatialGrid* grid, const char* value);
	__declspec(dllexport) int GetDataType(zStructSpatialGrid* grid);
	__declspec(dllexport) void SetDataType(zStructSpatialGrid* grid, int value);
	__declspec(dllexport) int GetLowerLeftCellX(zStructSpatialGrid* grid);
	__declspec(dllexport) void SetLowerLeftCellX(zStructSpatialGrid* grid, int value);
	__declspec(dllexport) int GetLowerLeftCellY(zStructSpatialGrid* grid);
	__declspec(dllexport) void SetLowerLeftCellY(zStructSpatialGrid* grid, int value);
	__declspec(dllexport) int GetNumberOfCellsX(zStructSpatialGrid* grid);
	__declspec(dllexport) void SetNumberOfCellsX(zStructSpatialGrid* grid, int value);
	__declspec(dllexport) int GetNumberOfCellsY(zStructSpatialGrid* grid);
	__declspec(dllexport) void SetNumberOfCellsY(zStructSpatialGrid* grid, int value);
	__declspec(dllexport) float GetCellSize(zStructSpatialGrid* grid);
	__declspec(dllexport) void SetCellSize(zStructSpatialGrid* grid);
	__declspec(dllexport) BSTR GetSRSName(zStructSpatialGrid* grid);
	__declspec(dllexport) void SetSRSName(zStructSpatialGrid* grid, const char* value);
	__declspec(dllexport) int GetSRSDefinitionType(zStructSpatialGrid* grid);
	__declspec(dllexport) void SetSRSDefinitionType(zStructSpatialGrid* grid);
	__declspec(dllexport) BSTR GetSRSDefinition(zStructSpatialGrid* grid);
	__declspec(dllexport) void SetSRSDefinition(zStructSpatialGrid* grid, const char* value);
	__declspec(dllexport) float GetXCoordOfGridCellZero(zStructSpatialGrid* grid);
	__declspec(dllexport) void SetXCoordOfGridCellZero(zStructSpatialGrid* grid, float value);
	__declspec(dllexport) float GetYCoordOfGridCellZero(zStructSpatialGrid* grid);
	__declspec(dllexport) void SetYCoordOfGridCellZero(zStructSpatialGrid* grid, float value);
	__declspec(dllexport) int GetStructVersion(zStructSpatialGrid* grid);
	__declspec(dllexport) int GetVersion(zStructSpatialGrid* grid);
	__declspec(dllexport) int GetStorageDataType(zStructSpatialGrid* grid);
	__declspec(dllexport) int GetCompressionMethod(zStructSpatialGrid* grid);
	__declspec(dllexport) int GetSizeOfCompressedElements(zStructSpatialGrid* grid);
	__declspec(dllexport) int GetNumberOfRanges(zStructSpatialGrid* grid);
	__declspec(dllexport) void SetNumberOfRanges(zStructSpatialGrid* grid, int value);
	__declspec(dllexport) float GetNullValue(zStructSpatialGrid* grid);
	__declspec(dllexport) void SetNullValue(zStructSpatialGrid* grid, float value);
	__declspec(dllexport) int GetTimeZoneID(zStructSpatialGrid* grid);
	__declspec(dllexport) void SetTimeZoneID(zStructSpatialGrid* grid, const char* value);
	__declspec(dllexport) int GetTimeZoneRawOffset(zStructSpatialGrid* grid);
	__declspec(dllexport) void SetTimeZoneRawOffset(zStructSpatialGrid* grid, int value);
	__declspec(dllexport) bool GetIsInterval(zStructSpatialGrid* grid);
	__declspec(dllexport) void SetIsInterval(zStructSpatialGrid* grid, bool value);
	__declspec(dllexport) bool GetIsTimeStamped(zStructSpatialGrid* grid);
	__declspec(dllexport) void SetIsTimeStamped(zStructSpatialGrid* grid, bool value);
	__declspec(dllexport) float GetMaxDataValue(zStructSpatialGrid* grid);
	__declspec(dllexport) void SetMaxDataValue(zStructSpatialGrid* grid, float value);
	__declspec(dllexport) float GetMinDataValue(zStructSpatialGrid* grid);
	__declspec(dllexport) void SetMinDataValue(zStructSpatialGrid* grid, float value);
	__declspec(dllexport) float GetMeanDataValue(zStructSpatialGrid* grid);
	__declspec(dllexport) void SetMeanDataValue(zStructSpatialGrid* grid, float value);
	__declspec(dllexport) float* GetRangeLimitTable(zStructSpatialGrid* grid);
	__declspec(dllexport) void SetRangeLimitTable(zStructSpatialGrid* grid, float* value);
	__declspec(dllexport) float GetUndefinedValue(zStructSpatialGrid* grid);
	__declspec(dllexport) int* GetNumberEqualOrExceedingRangeLimit(zStructSpatialGrid* grid);
	__declspec(dllexport) void SetNumberEqualOrExceedingRangeLimit(zStructSpatialGrid* grid, int* value);
	__declspec(dllexport) float* GetData(zStructSpatialGrid* grid);
	__declspec(dllexport) void SetData(zStructSpatialGrid* grid, float* value);

}