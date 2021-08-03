#pragma once
#include "managedToUnmanaged.h"
#include "ZStruct.h"

namespace Hec {
	namespace Dss {
		namespace Native {
			public ref class ZStructArrayWrapper : ZStruct
			{
			public:
				ZStructArrayWrapper(zStructArray* theStruct)
				{
					this->theStruct = theStruct;
				}
				ZStructArrayWrapper()
				{

				}
				~ZStructArrayWrapper()
				{
					zstructFree(theStruct);
				}
				!ZStructArrayWrapper()
				{
					zstructFree(theStruct);
				}
				zStructArray* theStruct;
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
				property array<int>^ IntArray {
					array<int>^ get() {
						if (theStruct && theStruct->intArray)
						{
							int* intArray = theStruct->intArray;
							int numVals = theStruct->numberIntArray;
							array<int>^ toReturn = gcnew array<int>(numVals);
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
					void set(array<int>^ val)
					{
						if (theStruct->intArray)
							free(theStruct->intArray);
						theStruct->intArray = managedToUnmanagedIntArr(val);
					}
				}
				property array<float>^ FloatArray {
					array<float>^ get() {
						if (theStruct && theStruct->floatArray)
						{
							float* intArray = theStruct->floatArray;
							int numVals = theStruct->numberFloatArray;
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
						if (theStruct->floatArray)
							free(theStruct->floatArray);
						theStruct->floatArray = managedToUnmanagedFloatArr(val);
					}
				}
				property array<double>^ DoubleArray {
					array<double>^ get() {
						if (theStruct && theStruct->doubleArray)
						{
							double* intArray = theStruct->doubleArray;
							int numVals = theStruct->numberDoubleArray;
							array<double>^ toReturn = gcnew array<double>(numVals);
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
					void set(array<double>^ val)
					{
						if (theStruct->doubleArray)
							free(theStruct->doubleArray);
						theStruct->doubleArray = managedToUnmanagedDoubleArr(val);
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
				property int UserHeaderSize {
					int get() { return theStruct->userHeaderSize; }
					void set(int val) { theStruct->userHeaderSize = val; }
				}
				property int UserHeaderNumber {
					int get() { return theStruct->userHeaderNumber; }
					void set(int val) { theStruct->userHeaderNumber = val; }
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
				property int NumberAttributes {
					int get() { return theStruct->numberAttributes; }
				}
				property array<String^>^ Attributes {
					array<String^>^ get() {
						if (theStruct && theStruct->attributes)
						{
							char** attr = theStruct->attributes;
							int numVals = theStruct->numberAttributes;
							array<String^>^ toReturn = gcnew array<String^>(numVals);
							for (int i = 0; i < numVals; i++)
							{
								toReturn[i] = gcnew String(attr[i]);
							}
							return toReturn;
						}
						else
						{
							return nullptr;
						}
					}
				}
				property array<String^>^ AttributeKeys {
					array<String^>^ get() {
						if (theStruct && theStruct->attributeKeys)
						{
							char** attr = theStruct->attributeKeys;
							int numVals = theStruct->numberAttributes;
							array<String^>^ toReturn = gcnew array<String^>(numVals);
							for (int i = 0; i < numVals; i++)
							{
								toReturn[i] = gcnew String(attr[i]);
							}
							return toReturn;
						}
						else
						{
							return nullptr;
						}
					}
				}

			};
		}
	}
}
