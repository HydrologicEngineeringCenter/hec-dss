#pragma once
#include "managedToUnmanaged.h"
#include "ZStructLocationWrapper.h"
#include "ZStruct.h"

namespace Hec {
	namespace Dss {
		namespace Native {
			public ref class ZStructTimeSeriesWrapper : ZStruct
			{
			public:
				ZStructTimeSeriesWrapper(zStructTimeSeries* theStruct)
				{
					this->theStruct = theStruct;
				}
				ZStructTimeSeriesWrapper()
				{

				}
				~ZStructTimeSeriesWrapper()
				{
					zstructFree(theStruct);
				}
				!ZStructTimeSeriesWrapper()
				{
					zstructFree(theStruct);
				}
				zStructTimeSeries* theStruct;
				property String^ Pathname {
					String^ get() {
						if (theStruct && theStruct->pathname)
							return gcnew String(theStruct->pathname);
						else
							return nullptr;
					}
					void set(String^ val) {
						if (theStruct->pathname)
							free(theStruct->pathname);
						theStruct->pathname = managedToUnmanagedString(val);
					}
				}
				property int JulianBaseDate {
					int get() { return theStruct->julianBaseDate; }
					void set(int val) { theStruct->julianBaseDate = val; }
				}
				property int StartJulianDate {
					int get() { return theStruct->startJulianDate; }
					void set(int val) { theStruct->startJulianDate = val; }
				}
				property int StartTimeSeconds {
					int get() { return theStruct->startTimeSeconds; }
					void set(int val) { theStruct->startTimeSeconds = val; }
				}
				property int EndJulianDate {
					int get() { return theStruct->endJulianDate; }
					void set(int val) { theStruct->endJulianDate = val; }
				}
				property int EndTimeSeconds {
					int get() { return theStruct->endTimeSeconds; }
					void set(int val) { theStruct->endTimeSeconds = val; }
				}
				property int TimeGranularitySeconds {
					int get() { return theStruct->timeGranularitySeconds; }
					void set(int val) { theStruct->timeGranularitySeconds = val; }
				}
				property int TimeIntervalSeconds {
					int get() { return theStruct->timeIntervalSeconds; }
					void set(int val) { theStruct->timeIntervalSeconds = val; }
				}
				property int TimeOffsetSeconds {
					int get() { return theStruct->timeOffsetSeconds; }
					void set(int val) { theStruct->timeOffsetSeconds = val; }
				}
				property array<int>^ Times {
					array<int>^ get() {
						if (theStruct && theStruct->times)
						{
							int* times = theStruct->times;
							int numPathNames = theStruct->numberValues;
							array<int>^ toReturn = gcnew array<int>(numPathNames);
							for (int i = 0; i < numPathNames; i++)
							{
								toReturn[i] = times[i];
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
						if (theStruct->times)
							free(theStruct->times);
						theStruct->times = managedToUnmanagedIntArr(val);
					}
				}
				property bool RetrieveAllTimes {
					bool get() { return theStruct->boolRetrieveAllTimes; }
					void set(bool val) { theStruct->boolRetrieveAllTimes = val; }
				}
				property int NumberValues {
					int get() { return theStruct->numberValues; }
					void set(int val) { theStruct->numberValues = val; }
				}
				property int SizeEachValueRead {
					int get() { return theStruct->sizeEachValueRead; }
					void set(int val) { theStruct->sizeEachValueRead = val; }
				}
				property int Precision {
					int get() { return theStruct->precision; }
					void set(int val) { theStruct->precision = val; }
				}
				property array<float>^ FloatValues {
					array<float>^ get() {
						if (theStruct && theStruct->floatValues)
						{
							float* floatValues = theStruct->floatValues;
							int numberValues = theStruct->numberValues;
							array<float>^ toReturn = gcnew array<float>(numberValues);
							for (int i = 0; i < numberValues; i++)
							{
								toReturn[i] = floatValues[i];
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
						if (theStruct->floatValues)
							free(theStruct->floatValues);
						theStruct->floatValues = managedToUnmanagedFloatArr(val);
					}
				}
				property array<double>^ DoubleValues {
					array<double>^ get() {
						if (theStruct && theStruct->doubleValues)
						{
							double* floatValues = theStruct->doubleValues;
							int numberValues = theStruct->numberValues;
							array<double>^ toReturn = gcnew array<double>(numberValues);
							for (int i = 0; i < numberValues; i++)
							{
								toReturn[i] = floatValues[i];
							}
							return toReturn;
						}
						else
						{
							return nullptr;
						}
					}
					void set(array<double>^ val)
					{
						if (theStruct->doubleValues)
							free(theStruct->doubleValues);
						theStruct->doubleValues = managedToUnmanagedDoubleArr(val);
					}
				}
				property String^ Units {
					String^ get() {
						if (theStruct && theStruct->units)
							return gcnew String(theStruct->units);

						else
							return nullptr;
					}
					void set(String^ val) {
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
					void set(String^ val) {
						if (theStruct->type)
							free(theStruct->type);
						theStruct->type = managedToUnmanagedString(val);

					}
				}
				property bool Pattern {
					bool get() { return theStruct->boolPattern; }
					void set(bool val) { theStruct->boolPattern = val; }
				}
				property int ProfileDepthsNumber {
					int get() { return theStruct->profileDepthsNumber; }
					void set(int val) { theStruct->profileDepthsNumber = val; }
				}
				property array<float>^ FloatProfileDepths {
					array<float>^ get() {
						if (theStruct && theStruct->floatProfileDepths)
						{
							float* floatprofiledepths = theStruct->floatProfileDepths;
							int numberValues = theStruct->profileDepthsNumber;
							array<float>^ toReturn = gcnew array<float>(numberValues);
							for (int i = 0; i < numberValues; i++)
							{
								toReturn[i] = floatprofiledepths[i];
							}
							return toReturn;
						}
						else
						{
							return nullptr;
						}
					}
					void set(array<float>^ val) {
						if (theStruct->floatProfileDepths)
							free(theStruct->floatProfileDepths);
						theStruct->floatProfileDepths = managedToUnmanagedFloatArr(val);

					}
				}
				property array<float>^ FloatProfileValues {
					array<float>^ get() {
						if (theStruct && theStruct->floatProfileValues)
						{
							float* floatprofilevalues = theStruct->floatProfileValues;
							int numberValues = theStruct->profileDepthsNumber * theStruct->numberValues;
							array<float>^ toReturn = gcnew array<float>(numberValues);
							for (int i = 0; i < numberValues; i++)
							{
								toReturn[i] = floatprofilevalues[i];
							}
							return toReturn;
						}
						else
						{
							return nullptr;
						}
					}

					void set(array<float>^ val) {
						if (theStruct->floatProfileValues)
							free(theStruct->floatProfileValues);
						theStruct->floatProfileValues = managedToUnmanagedFloatArr(val);
					}
				}
				property array<double>^ DoubleProfileDepths {
					array<double>^ get() {
						if (theStruct && theStruct->doubleProfileDepths)
						{
							double* doubleprofiledepths = theStruct->doubleProfileDepths;
							int numberValues = theStruct->profileDepthsNumber;
							array<double>^ toReturn = gcnew array<double>(numberValues);
							for (int i = 0; i < numberValues; i++)
							{
								toReturn[i] = doubleprofiledepths[i];
							}
							return toReturn;
						}
						else
						{
							return nullptr;
						}
					}
					void set(array<double>^ val) {
						if (theStruct->doubleProfileDepths)
							free(theStruct->doubleProfileDepths);
						theStruct->doubleProfileDepths = managedToUnmanagedDoubleArr(val);
					}
				}
				property array<double>^ DoubleProfileValues {
					array<double>^ get() {
						if (theStruct && theStruct->doubleProfileValues)
						{
							double* floatprofilevalues = theStruct->doubleProfileValues;
							int numberValues = theStruct->profileDepthsNumber * theStruct->numberValues;
							array<double>^ toReturn = gcnew array<double>(numberValues);
							for (int i = 0; i < numberValues; i++)
							{
								toReturn[i] = floatprofilevalues[i];
							}
							return toReturn;
						}
						else
						{
							return nullptr;
						}
					}
					void set(array<double>^ val) {
						if (theStruct->doubleProfileValues)
							free(theStruct->doubleProfileValues);
						theStruct->doubleProfileValues = managedToUnmanagedDoubleArr(val);
					}
				}
				property String^ UnitsProfileDepths {
					String^ get() {
						if (theStruct && theStruct->unitsProfileDepths)
							return gcnew String(theStruct->unitsProfileDepths);
						else
							return nullptr;
					}
					void set(String^ val) {
						if (theStruct->unitsProfileDepths)
							free(theStruct->unitsProfileDepths);
						theStruct->unitsProfileDepths = managedToUnmanagedString(val);
					}
				}
				property String^ UnitsProfileValues {
					String^ get() {
						if (theStruct && theStruct->unitsProfileValues)
							return gcnew String(theStruct->unitsProfileValues);
						else
							return nullptr;
					}
					void set(String^ val) {
						if (theStruct->unitsProfileValues)
							free(theStruct->unitsProfileValues);
						theStruct->unitsProfileValues = managedToUnmanagedString(val);
					}
				}
				property String^ TimeZoneName {
					String^ get() {
						if (theStruct && theStruct->timeZoneName)
							return gcnew String(theStruct->timeZoneName);
						else
							return nullptr;
					}
					void set(String^ val) {
						if (theStruct->timeZoneName)
							free(theStruct->timeZoneName);
						theStruct->timeZoneName = managedToUnmanagedString(val);
					}
				}
				property array<int>^ Quality {
					array<int>^ get() {
						if (theStruct && theStruct->quality)
						{
							int* quality = theStruct->quality;
							int numberValues = theStruct->qualityArraySize;
							array<int>^ toReturn = gcnew array<int>(numberValues);
							for (int i = 0; i < numberValues; i++)
							{
								toReturn[i] = quality[i];
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
						if (theStruct->quality)
							free(theStruct->quality);
						theStruct->quality = managedToUnmanagedIntArr(val);

					}
				}
				property int QualityElementSize {
					int get() { return theStruct->qualityElementSize; }
					void set(int val) { theStruct->qualityElementSize = val; }
				}
				property int QualityArraySize {
					int get() { return theStruct->qualityArraySize; }
					void set(int val) { theStruct->qualityArraySize = val; }
				}
				property array<int>^ INotes {
					array<int>^ get() {
						if (theStruct && theStruct->inotes)
						{
							int* INotes = theStruct->inotes;
							int numberValues = theStruct->inotesArraySize;
							array<int>^ toReturn = gcnew array<int>(numberValues);
							for (int i = 0; i < numberValues; i++)
							{
								toReturn[i] = INotes[i];
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
						if (theStruct->inotes)
							free(theStruct->inotes);
						theStruct->inotes = managedToUnmanagedIntArr(val);
					}
				}
				property int INotesElementSize {
					int get() { return theStruct->inoteElementSize; }
					void set(int val) { theStruct->inoteElementSize = val; }
				}
				property int INotesArraySize {
					int get() { return theStruct->inotesArraySize; }
					void set(int val) { theStruct->inotesArraySize = val; }
				}
				property array<char>^ CNotes {
					array<char>^ get() {
						if (theStruct && theStruct->cnotes)
						{
							char* CNotes = theStruct->cnotes;
							int numberValues = theStruct->cnotesSize;
							array<char>^ toReturn = gcnew array<char>(numberValues);
							for (int i = 0; i < numberValues; i++)
							{
								toReturn[i] = CNotes[i];
							}
							return toReturn;
						}
						else
						{
							return nullptr;
						}
					}
					void set(array<char>^ val)
					{
						if (theStruct->cnotes)
							free(theStruct->cnotes);
						theStruct->cnotes = managedToUnmanagedCharArr(val);
					}
				}
				property int CNotesSize {
					int get() { return theStruct->cnotesSize; }
					void set(int val) { theStruct->cnotesSize = val; }
				}
				property int CNotesLengthTotal {
					int get() { return theStruct->cnotesLengthTotal; }
					void set(int val) { theStruct->cnotesLengthTotal = val; }
				}
				property array<int>^ UserHeaders {
					array<int>^ get() {
						if (theStruct && theStruct->userHeader)
						{
							int* userHeaders = theStruct->userHeader;
							int numberValues = theStruct->userHeaderNumber;
							array<int>^ toReturn = gcnew array<int>(numberValues);
							for (int i = 0; i < numberValues; i++)
							{
								toReturn[i] = userHeaders[i];
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
						if (theStruct->userHeader)
							free(theStruct->userHeader);
						theStruct->userHeader = managedToUnmanagedIntArr(val);
					}
				}
				property int UserHeaderSize {
					int get() { return theStruct->userHeaderSize; }
					void set(int val) { theStruct->userHeaderSize = val; }
				}
				property int UserHeaderNumber {
					int get() { return theStruct->userHeaderNumber; }
					void set(int val) { theStruct->userHeaderNumber = val; }
				}
				property ZStructLocationWrapper^ locationStruct {
					ZStructLocationWrapper^ get()
					{
						if (theStruct && theStruct->locationStruct)
							return gcnew ZStructLocationWrapper(theStruct->locationStruct, true);
						else
							return nullptr;
					}
				}
				property int DateOfFirstRecFound {
					int get() { return theStruct->dateOfFirstRecFound; }
				}
				property int DataType {
					int get() { return theStruct->dataType; }
				}
				property long long LastWrittenTime {
					long long get() { return theStruct->lastWrittenTime; }
				}
				property long long FileLastWrittenTime {
					long long get() { return theStruct->fileLastWrittenTime; }
				}
				property String^ ProgramName {
					String^ get() {
						if (theStruct && theStruct->programName)
							return gcnew String(theStruct->programName);
						else
							return nullptr;
					}
				}
				/*		property int NumberAttributes {
							int get() { return theStruct->numberAttributes; }
						}
						property array<String ^> ^ AttributeKeys {
							array<String ^> ^ get() {
								if (theStruct && theStruct->attributeKeys)
								{
									char ** attributeKeys = theStruct->attributeKeys;
									int numberAttributes = theStruct->numberAttributes;
									array<String ^> ^ toReturn = gcnew array<String^>(numberAttributes);
									for (int i = 0; i < numberAttributes; i++)
									{
										toReturn[i] = gcnew String(attributeKeys[i]);
									}
									return toReturn;
								}
								else
								{
									return nullptr;
								}
							}
						}
						property array<String ^> ^ Attributes {
							array<String ^> ^ get() {
								if (theStruct && theStruct->attributes)
								{
									char ** attributes = theStruct->attributes;
									int numberAttributes = theStruct->numberAttributes;
									array<String ^> ^ toReturn = gcnew array<String^>(numberAttributes);
									for (int i = 0; i < numberAttributes; i++)
									{
										toReturn[i] = gcnew String(attributes[i]);
									}
									return toReturn;
								}
								else
								{
									return nullptr;
								}
							}
						}
				*/
			};
		}
	}
}
