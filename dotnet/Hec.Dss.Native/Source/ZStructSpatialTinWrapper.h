#pragma once
#include "managedToUnmanaged.h"
#include "ZStruct.h"

namespace Hec {
	namespace Dss {
		namespace Native {
			public ref class ZStructSpatialTinWrapper : ZStruct
			{
			public:
				ZStructSpatialTinWrapper(zStructSpatialTin* theStruct)
				{
					this->theStruct = theStruct;
				}
				ZStructSpatialTinWrapper()
				{

				}
				~ZStructSpatialTinWrapper()
				{
					zstructFree(theStruct);
				}
				!ZStructSpatialTinWrapper()
				{
					zstructFree(theStruct);
				}
				zStructSpatialTin* theStruct;
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
				property String^ SpatialReferenceSystem {
					String^ get() {
						if (theStruct && theStruct->SpatialReferenceSystem)
							return gcnew String(theStruct->SpatialReferenceSystem);
						else
							return nullptr;
					}
					void set(String^ val)
					{
						if (theStruct->SpatialReferenceSystem)
							free(theStruct->SpatialReferenceSystem);
						theStruct->SpatialReferenceSystem = managedToUnmanagedString(val);
					}
				}
				property int SRSType {
					int get() { return theStruct->SRSType; }
					void set(int val) { theStruct->SRSType = val; }
				}
				property String^ SRSName {
					String^ get() {
						if (theStruct && theStruct->SRSName)
							return gcnew String(theStruct->SRSName);
						else
							return nullptr;
					}
					void set(String^ val)
					{
						if (theStruct->SRSName)
							free(theStruct->SRSName);
						theStruct->SRSName = managedToUnmanagedString(val);
					}
				}
				property String^ SRSUnits {
					String^ get() {
						if (theStruct && theStruct->SRSUnits)
							return gcnew String(theStruct->SRSUnits);
						else
							return nullptr;
					}
					void set(String^ val)
					{
						if (theStruct->SRSUnits)
							free(theStruct->SRSUnits);
						theStruct->SRSUnits = managedToUnmanagedString(val);
					}
				}
				property String^ Units {
					String^ get() {
						if (theStruct && theStruct->units)
							return gcnew String(theStruct->units);
						else
							return nullptr;
					}
					void set(String^ val)
					{
						if (theStruct->units)
							free(theStruct->units);
						theStruct->units = managedToUnmanagedString(val);
					}
				}
				property String^ Type {
					String^ get() {
						if (theStruct && theStruct->type)
							return gcnew String(theStruct->type);
						else
							return nullptr;
					}
					void set(String^ val)
					{
						if (theStruct->type)
							free(theStruct->type);
						theStruct->type = managedToUnmanagedString(val);
					}
				}
				property String^ TimeZoneName {
					String^ get() { return gcnew String(theStruct->timeZoneName); }
					void set(String^ val)
					{
						if (theStruct->timeZoneName)
							free(theStruct->timeZoneName);
						theStruct->timeZoneName = managedToUnmanagedString(val);
					}
				}
				property int NumberPoints {
					int get() { return theStruct->numberPoints; }
					void set(int val) { theStruct->numberPoints = val; }
				}
				property float SlendernessRatio {
					float get() { return theStruct->slendernessRatio; }
					void set(float val) { theStruct->slendernessRatio = val; }
				}
				property array<float>^ XCoordinate {
					array<float>^ get() {
						if (theStruct && theStruct->xCoordinate)
						{
							float* intArray = theStruct->xCoordinate;
							int numVals = theStruct->numberPoints;
							array<float>^ toReturn = gcnew array<float>(numVals);
							for (int i = 0; i < numVals; i++)
							{
								toReturn[i] = intArray[i];
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
						if (theStruct->xCoordinate)
							free(theStruct->xCoordinate);
						theStruct->xCoordinate = managedToUnmanagedFloatArr(val);
					}
				}
				property array<float>^ YCoordinate {
					array<float>^ get() {
						if (theStruct && theStruct->yCoordinate)
						{
							float* intArray = theStruct->yCoordinate;
							int numVals = theStruct->numberPoints;
							array<float>^ toReturn = gcnew array<float>(numVals);
							for (int i = 0; i < numVals; i++)
							{
								toReturn[i] = intArray[i];
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
						if (theStruct->yCoordinate)
							free(theStruct->yCoordinate);
						theStruct->yCoordinate = managedToUnmanagedFloatArr(val);
					}
				}
				property array<float>^ Value {
					array<float>^ get() {
						if (theStruct && theStruct->value)
						{
							float* intArray = theStruct->value;
							int numVals = theStruct->numberPoints;
							array<float>^ toReturn = gcnew array<float>(numVals);
							for (int i = 0; i < numVals; i++)
							{
								toReturn[i] = intArray[i];
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
						if (theStruct->value)
							free(theStruct->value);
						theStruct->value = managedToUnmanagedFloatArr(val);
					}
				}
				property int PointType {
					int get() { return *theStruct->pointType; }
					void set(int val) {
						*(theStruct->pointType) = val;
					}
				}

				property int NumberConnections {
					int get() { return *(theStruct->numberConnections); }
					void set(int val) { *(theStruct->numberConnections); }
				}

			};
		}
	}
}