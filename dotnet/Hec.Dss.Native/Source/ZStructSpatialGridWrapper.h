#pragma once
#include "managedToUnmanaged.h"
#include "ZStruct.h"


namespace Hec {
	namespace Dss {
		namespace Native {
			public ref class ZStructSpatialGridWrapper : ZStruct
			{
			public:
				ZStructSpatialGridWrapper(zStructSpatialGrid* theStruct)
				{
					this->theStruct = theStruct;
				}
				~ZStructSpatialGridWrapper()
				{
					zstructFree(theStruct);
				}
				!ZStructSpatialGridWrapper()
				{
					zstructFree(theStruct);
				}
				zStructSpatialGrid* theStruct;
				property String^ PathName {
					String^ get() {
						if (theStruct && theStruct->pathname)
							return gcnew String(theStruct->pathname);
						else
							return nullptr;
					}
					void set(String^ val)
					{
						if (theStruct->pathname)
							free(theStruct->pathname);
						theStruct->pathname = managedToUnmanagedString(val);
					}
				}
				property int StructVersion {
					int get() { return theStruct->_structVersion; }
					void set(int val) { theStruct->_structVersion = val; }
				}
				property int GridType {
					int get() { return theStruct->_type; }
					void set(int val) { theStruct->_type = val; }
				}
				property int Version {
					int get() { return theStruct->_version; }
					void set(int val) { theStruct->_version = val; }
				}
				property String^ DataUnits {
					String^ get() {
						if (theStruct && theStruct->_dataUnits)
							return gcnew String(theStruct->_dataUnits);
						else
							return nullptr;
					}
					void set(String^ val)
					{
						if (theStruct->_dataUnits)
							free(theStruct->_dataUnits);
						theStruct->_dataUnits = managedToUnmanagedString(val);

					}
				}
				property int DataType {
					int get() { return theStruct->_dataType; }
					void set(int val) { theStruct->_dataType = val; }
				}
				property String^ DataSource {
					String^ get() {
						if (theStruct && theStruct->_dataSource)
							return gcnew String(theStruct->_dataSource);
						else
							return nullptr;
					}
					void set(String^ val)
					{
						if (theStruct->_dataSource)
							free(theStruct->_dataSource);
						theStruct->_dataSource = managedToUnmanagedString(val);

					}
				}
				property int LowerLeftCellX {
					int get() { return theStruct->_lowerLeftCellX; }
					void set(int val) { theStruct->_lowerLeftCellX = val; }
				}
				property int LowerLeftCellY {
					int get() { return theStruct->_lowerLeftCellY; }
					void set(int val) { theStruct->_lowerLeftCellY = val; }
				}
				property int NumberOfCellsX {
					int get() { return theStruct->_numberOfCellsX; }
					void set(int val) { theStruct->_numberOfCellsX = val; }
				}
				property int NumberOfCellsY {
					int get() { return theStruct->_numberOfCellsY; }
					void set(int val) { theStruct->_numberOfCellsY = val; }
				}
				property float CellSize {
					float get() { return theStruct->_cellSize; }
					void set(float val) { theStruct->_cellSize = val; }
				}
				property int CompressionMethod {
					int get() { return theStruct->_compressionMethod; }
					void set(int val) { theStruct->_compressionMethod = val; }
				}
				property int SizeOfCompressedElements {
					int get() { return theStruct->_sizeofCompressedElements; }
					void set(int val) { theStruct->_sizeofCompressedElements = val; }
				}
				property String^ SRSName
				{
					String^ get() {
						if (theStruct && theStruct->_srsName)
							return gcnew String(theStruct->_srsName);
						else
							return nullptr;
					}
					void set(String^ val)
					{
						if (theStruct->_srsName)
							free(theStruct->_srsName);
						theStruct->_srsName = managedToUnmanagedString(val);
					}
				}
				property int SRSDefinitionType
				{
					int get() { return theStruct->_srsDefinitionType; }
					void set(int val) { theStruct->_srsDefinitionType = val; }
				}
				property String^ SRSDefinition
				{
					String^ get() {
						if (theStruct && theStruct->_srsDefinition)
							return gcnew String(theStruct->_srsDefinition);
						else
							return nullptr;
					}
					void set(String^ val)
					{
						if (theStruct->_srsDefinition)
							free(theStruct->_srsDefinition);
						theStruct->_srsDefinition = managedToUnmanagedString(val);
					}
				}
				property float XCoordOfGridCellZero
				{
					float get() { return theStruct->_xCoordOfGridCellZero; }
					void set(float val) { theStruct->_xCoordOfGridCellZero = val; }
				}
				property float YCoordOfGridCellZero
				{
					float get() { return theStruct->_yCoordOfGridCellZero; }
					void set(float val) { theStruct->_yCoordOfGridCellZero = val; }
				}
				property float NullValue
				{
					float get() { return theStruct->_nullValue; }
					void set(float val) { theStruct->_nullValue = val; }
				}
				property float UndefinedValue
				{
					float get() { return UNDEFINED_FLOAT; }
				}
				property String^ TimeZoneID
				{
					String^ get() {
						if (theStruct && theStruct->_timeZoneID)
							return gcnew String(theStruct->_timeZoneID);
						else
							return nullptr;
					}
					void set(String^ val)
					{
						if (theStruct->_timeZoneID)
							free(theStruct->_timeZoneID);
						theStruct->_timeZoneID = managedToUnmanagedString(val);
					}
				}
				property int TimeZoneRawOffset
				{
					int get() { return theStruct->_timeZoneRawOffset; }
					void set(int val) { theStruct->_timeZoneRawOffset = val; }
				}
				property bool IsInterval
				{
					bool get() { return theStruct->_isInterval; }
					void set(bool val) { theStruct->_isInterval = val; }
				}
				property bool IsTimeStamped
				{
					bool get() { return theStruct->_isTimeStamped; }
					void set(bool val) { theStruct->_isTimeStamped = val; }
				}
				property int NumberOfRanges {
					int get() { return theStruct->_numberOfRanges; }
					void set(int val) { theStruct->_numberOfRanges = val; }
				}
				property int StorageDataType {
					int get() { return theStruct->_storageDataType; }
					void set(int val) { theStruct->_storageDataType = val; }
				}
				property float MaxDataValue {
					float get() { return *((float*)theStruct->_maxDataValue); }
					void set(float val) { *((float*)theStruct->_maxDataValue) = val; }
				}
				property float MinDataValue {
					float get() { return *((float*)theStruct->_minDataValue); }
					void set(float val) { *((float*)theStruct->_minDataValue) = val; }
				}
				property float MeanDataValue {
					float get() { return *((float*)theStruct->_meanDataValue); }
					void set(float val) { *((float*)theStruct->_meanDataValue) = val; }
				}
				property array<float>^ RangeLimitTable {
					array<float>^ get() {
						if (theStruct && theStruct->_rangeLimitTable)
						{
							float* rangeLimitTable = (float*)theStruct->_rangeLimitTable;
							int numberValues = theStruct->_numberOfRanges;
							array<float>^ toReturn = gcnew array<float>(numberValues);
							for (int i = 0; i < numberValues; i++)
							{
								toReturn[i] = rangeLimitTable[i];
							}
							return toReturn;
						}
						else
						{
							return nullptr;
						}
					}
					void set(array<float>^ val)
					{
						if (theStruct->_rangeLimitTable)
							free(theStruct->_rangeLimitTable);
						theStruct->_rangeLimitTable = managedToUnmanagedFloatArr(val);
					}
				}
				property array<int>^ NumberEqualOrExceedingRangeLimit {
					array<int>^ get() {
						if (theStruct && theStruct->_numberEqualOrExceedingRangeLimit)
						{
							int* numberEqualOrExceedingRangeLimit = theStruct->_numberEqualOrExceedingRangeLimit;
							int numberValues = theStruct->_numberOfRanges;
							array<int>^ toReturn = gcnew array<int>(numberValues);
							for (int i = 0; i < numberValues; i++)
							{
								toReturn[i] = numberEqualOrExceedingRangeLimit[i];
							}
							return toReturn;
						}
						else
						{
							return nullptr;
						}
					}
					void set(array<int>^ val)
					{
						if (theStruct->_numberEqualOrExceedingRangeLimit)
							free(theStruct->_numberEqualOrExceedingRangeLimit);
						theStruct->_numberEqualOrExceedingRangeLimit = managedToUnmanagedIntArr(val);
					}
				}
				property array<float>^ Data {
					array<float>^ get() {
						if (theStruct && theStruct->_data)
						{
							float* Data = (float*)theStruct->_data;
							int numberValues = theStruct->_numberOfCellsX * theStruct->_numberOfCellsY;
							array<float>^ toReturn = gcnew array<float>(numberValues);
							for (int i = 0; i < numberValues; i++)
							{
								toReturn[i] = Data[i];
							}
							return toReturn;
						}
						else
						{
							return nullptr;
						}
					}
					void set(array<float>^ val)
					{
						if (theStruct->_data)
							free(theStruct->_data);
						theStruct->_data = managedToUnmanagedFloatArr(val);
					}
				}
			};
		}
	}
}
