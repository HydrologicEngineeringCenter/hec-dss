#pragma once
#include "managedToUnmanaged.h"
#include "ZStruct.h"


namespace Hec {
	namespace Dss {
		namespace Native {
			public ref class ZStructPairedDataWrapper : ZStruct
			{
			public:
				ZStructPairedDataWrapper(zStructPairedData* theStruct)
				{
					this->theStruct = theStruct;
				}
				ZStructPairedDataWrapper()
				{

				}
				~ZStructPairedDataWrapper()
				{
					zstructFree(theStruct);
				}
				!ZStructPairedDataWrapper()
				{
					zstructFree(theStruct);
				}
				zStructPairedData* theStruct;
				property String^ PathName {
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
				property int NumberCurves {
					int get() { return theStruct->numberCurves; }
					void set(int val) { theStruct->numberCurves = val; }
				}
				property int NumberOrdinates {
					int get() { return theStruct->numberOrdinates; }
					void set(int val) { theStruct->numberOrdinates = val; }
				}
				property int StartingCurve {
					int get() { return theStruct->startingCurve; }
					void set(int val) { theStruct->startingCurve = val; }
				}
				property int EndingCurve {
					int get() { return theStruct->endingCurve; }
					void set(int val) { theStruct->endingCurve = val; }
				}
				property int StartingOrdinate {
					int get() { return theStruct->startingOrdinate; }
					void set(int val) { theStruct->startingOrdinate = val; }
				}
				property int EndingOrdinate {
					int get() { return theStruct->endingOrdinate; }
					void set(int val) { theStruct->endingOrdinate = val; }
				}
				property int NumberCurvesInStruct {
					int get() { return theStruct->numberCurvesInStruct; }
				}
				property int NumberOrdinatesInStruct {
					int get() { return theStruct->numberOrdinatesInStruct; }
				}
				property int NumberValues {
					int get() { return theStruct->numberOrdinatesInStruct * theStruct->numberCurves; }
				}
				property array<float>^ FloatOrdinates {
					array<float>^ get() {
						if (theStruct && theStruct->floatOrdinates)
						{
							float* floatOrdinates = theStruct->floatOrdinates;
							int numberValues = theStruct->numberOrdinatesInStruct;
							array<float>^ toReturn = gcnew array<float>(numberValues);
							for (int i = 0; i < numberValues; i++)
							{
								toReturn[i] = floatOrdinates[i];
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
						if (theStruct->floatOrdinates)
							free(theStruct->floatOrdinates);
						theStruct->floatOrdinates = managedToUnmanagedFloatArr(val);
					}
				}
				property array<float>^ FloatValues {
					array<float>^ get() {
						if (theStruct && theStruct->floatValues)
						{
							float* floatValues = theStruct->floatValues;
							array<float>^ toReturn = gcnew array<float>(NumberValues);
							for (int i = 0; i < NumberValues; i++)
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
				property array<double>^ DoubleOrdinates {
					array<double>^ get() {
						if (theStruct && theStruct->doubleOrdinates)
						{
							double* doubleOrdinates = theStruct->doubleOrdinates;
							int numberValues = theStruct->numberOrdinatesInStruct;
							array<double>^ toReturn = gcnew array<double>(numberValues);
							for (int i = 0; i < numberValues; i++)
							{
								toReturn[i] = doubleOrdinates[i];
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
						if (theStruct->doubleOrdinates)
							free(theStruct->doubleOrdinates);
						theStruct->doubleOrdinates = managedToUnmanagedDoubleArr(val);
					}
				}
				property array<double>^ DoubleValues {
					array<double>^ get() {
						if (theStruct && theStruct->doubleValues)
						{
							double* doublevalues = theStruct->doubleValues;
							array<double>^ toReturn = gcnew array<double>(NumberValues);
							for (int i = 0; i < NumberValues; i++)
							{
								toReturn[i] = doublevalues[i];
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
				property int SizeEachValueRead {
					int get() { return theStruct->sizeEachValueRead; }
					void set(int val) { theStruct->sizeEachValueRead = val; }
				}
				property int XPrecision {
					int get() { return theStruct->xprecision; }
					void set(int val) { theStruct->xprecision; }
				}
				property int YPrecision {
					int get() { return theStruct->yprecision; }
					void set(int val) { theStruct->yprecision = val; }
				}
				property String^ UnitsIndependent {
					String^ get() {
						if (theStruct && theStruct->unitsIndependent)
							return gcnew String(theStruct->unitsIndependent);
						else
							return nullptr;
					}
					void set(String^ val)
					{
						if (theStruct->unitsIndependent)
							free(theStruct->unitsIndependent);
						theStruct->unitsIndependent = managedToUnmanagedString(val);

					}
				}
				property String^ TypeIndependent {
					String^ get() {
						if (theStruct && theStruct->typeIndependent)
							return gcnew String(theStruct->typeIndependent);
						else
							return nullptr;
					}
					void set(String^ val)
					{
						if (theStruct->typeIndependent)
							free(theStruct->typeIndependent);
						theStruct->typeIndependent = managedToUnmanagedString(val);

					}
				}
				property String^ UnitsDependent {
					String^ get() {
						if (theStruct && theStruct->unitsDependent)
							return gcnew String(theStruct->unitsDependent);
						else
							return nullptr;
					}
					void set(String^ val)
					{
						if (theStruct->unitsDependent)
							free(theStruct->unitsDependent);
						theStruct->unitsDependent = managedToUnmanagedString(val);
					}
				}
				property String^ TypeDependent {
					String^ get() {
						if (theStruct && theStruct->typeDependent)
							return gcnew String(theStruct->typeDependent);
						else
							return nullptr;
					}
					void set(String^ val)
					{
						if (theStruct->typeDependent)
							free(theStruct->typeDependent);
						theStruct->typeDependent = managedToUnmanagedString(val);
					}
				}
				property bool IndependentIsXAxis {
					bool get() { return theStruct->boolIndependentIsXaxis; }
					void set(bool val) { theStruct->boolIndependentIsXaxis = val; }
				}
				property array<char>^ Labels {
					array<char>^ get() {
						if (theStruct && theStruct->labels)
						{
							char* mem = theStruct->labels;
							int numberValues = theStruct->labelsLength;
							array<char>^ toReturn = gcnew array<char>(numberValues);
							for (int i = 0; i < numberValues; i++)
							{
								toReturn[i] = mem[i];
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
						if (theStruct->labels)
							free(theStruct->labels);
						theStruct->labels = managedToUnmanagedCharArr(val);
					}
				}
				property int LabelsLength {
					int get() { return theStruct->labelsLength; }
					void set(int val) { theStruct->labelsLength = val; }
				}
				property String^ TimeZoneName {
					String^ get() {
						if (theStruct && theStruct->timeZoneName)
							return gcnew String(theStruct->timeZoneName);
						else
							return nullptr;
					}
					void set(String^ val)
					{
						if (theStruct->timeZoneName)
							free(theStruct->timeZoneName);
						theStruct->timeZoneName = managedToUnmanagedString(val);
					}
				}
				property array<int>^ UserHeader {
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
				property int UserHeaderNumber {
					int get() { return theStruct->userHeaderNumber; }
					void set(int val) { theStruct->userHeaderNumber = val; }
				}
				property array<int>^ OtherInfo {
					array<int>^ get() {
						if (theStruct && theStruct->otherInfo)
						{
							int* otherInfo = theStruct->otherInfo;
							int numberValues = theStruct->otherInfoNumber;
							array<int>^ toReturn = gcnew array<int>(numberValues);
							for (int i = 0; i < numberValues; i++)
							{
								toReturn[i] = otherInfo[i];
							}
							return toReturn;
						}
						else
							return nullptr;
					}
					void set(array<int>^ val)
					{
						if (theStruct->otherInfo)
							free(theStruct->otherInfo);
						theStruct->otherInfo = managedToUnmanagedIntArr(val);
					}
				}
				property int OtherInfoNumber {
					int get() { return theStruct->otherInfoNumber; }
					void set(int val) { theStruct->otherInfoNumber = val; }
				}
				property int DataType {
					int get() { return theStruct->dataType; }
					void set(int val) { theStruct->dataType = val; }
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
			};
		}
	}
}
