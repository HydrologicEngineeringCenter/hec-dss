#pragma once
#include "managedToUnmanaged.h"
#include "ZStruct.h"

namespace Hec {
	namespace Dss {
		namespace Native {
			public ref class ZStructTextWrapper : ZStruct
			{
			public:
				ZStructTextWrapper(zStructText* theStruct)
				{
					this->theStruct = theStruct;
				}
				ZStructTextWrapper()
				{

				}
				~ZStructTextWrapper()
				{
					zstructFree(theStruct);
				}
				!ZStructTextWrapper()
				{
					zstructFree(theStruct);
				}
				zStructText* theStruct;
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
				property String^ TextString {
					String^ get() {
						if (theStruct && theStruct->textString)
							return gcnew String(theStruct->textString);
						else
							return nullptr;
					}
					void set(String^ val)
					{
						if (theStruct->textString)
							free(theStruct->textString);
						theStruct->textString = managedToUnmanagedString(val);
					}
				}
				property int NumberTextChars {
					int get() { return theStruct->numberTextChars; }
					void set(int val) { theStruct->numberTextChars = val; }
				}
				property String^ TextTable {
					String^ get() {
						if (theStruct && theStruct->textTable)
							return gcnew String(theStruct->textTable);
						else
							return nullptr;
					}
					void set(String^ val)
					{
						if (theStruct->textTable)
							free(theStruct->textTable);
						theStruct->textTable = managedToUnmanagedString(val);
					}
				}
				property int NumberTableChars {
					int get() { return theStruct->numberTableChars; }
					void set(int val) { theStruct->numberTableChars = val; }
				}
				property int NumberRows {
					int get() { return theStruct->numberRows; }
					void set(int val) { theStruct->numberRows; }
				}
				property int numberColumns {
					int get() { return theStruct->numberColumns; }
					void set(int val) { theStruct->numberColumns; }
				}
				property String^ Labels {
					String^ get() {
						if (theStruct->labels)
							return gcnew String(theStruct->labels);
						else
							return nullptr;
					}
					void set(String^ val)
					{
						if (theStruct->labels)
							free(theStruct->labels);
						theStruct->labels = managedToUnmanagedString(val);
					}
				}
				property int NumberLabelChars {
					int get() { return theStruct->numberLabelChars; }
					void set(int val) { theStruct->numberLabelChars = val; }
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