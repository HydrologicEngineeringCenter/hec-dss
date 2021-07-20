#pragma once
#include "managedToUnmanaged.h"
#include "ZStruct.h"

namespace Hec {
	namespace Dss {
		namespace Native {
			public ref class ZStructCatalogWrapper : ZStruct
			{
			public:
				ZStructCatalogWrapper(zStructCatalog* theStruct)
				{
					this->theStruct = theStruct;
				}
				ZStructCatalogWrapper()
				{

				}
				~ZStructCatalogWrapper()
				{
					Debug::WriteLine("Derived Destructor called");
					zstructFree(theStruct);
				}
				!ZStructCatalogWrapper()
				{
					zstructFree(theStruct);
				}
				zStructCatalog* theStruct;
				property int StatusWanted {
					int get() { return theStruct->statusWanted; }
					void set(int val) { theStruct->statusWanted = val; }
				}
				property int TypeWantedStart {
					int get() { return theStruct->typeWantedStart; }
					void set(int val) { theStruct->typeWantedStart = val; }
				}
				property int TypeWantedEnd {
					int get() { return theStruct->typeWantedEnd; }
					void set(int val) { theStruct->typeWantedEnd = val; }
				}
				property long long LastWriteTimeSearch {
					long long get() { return theStruct->lastWriteTimeSearch; }
					void set(long long val) { theStruct->lastWriteTimeSearch = val; }
				}
				property int LastWriteTimeSearchFlag {
					int get() { return theStruct->lastWriteTimeSearchFlag; }
					void set(int val) { theStruct->lastWriteTimeSearchFlag = val; }
				}
				property array<String^>^ PathNameList {
					array<String^>^ get() {
						if (theStruct && theStruct->pathnameList)
						{
							char** pathnameList = theStruct->pathnameList;
							int numPathNames = theStruct->numberPathnames;
							array<String^>^ toReturn = gcnew array<String^>(numPathNames);
							for (int i = 0; i < numPathNames; i++)
							{
								toReturn[i] = gcnew String(pathnameList[i]);
							}
							return toReturn;
						}
						else
						{
							return nullptr;
						}
					}
				}
				property int NumberOfPathNames {
					int get() { return theStruct->numberPathnames; }
				}
				property bool Sorted {
					bool get() { return theStruct->boolSorted; }
				}
				property bool IsCollection {
					bool get() { return theStruct->boolIsCollection; }
				}
				property bool HasAttributes {
					bool get() { return theStruct->boolHasAttribues; }
				}
				property array<String^>^ Attributes {
					array<String^>^ get() {
						if (theStruct && theStruct->attributes)
						{
							char** attr = theStruct->attributes;
							int numPathNames = theStruct->numberPathnames;
							array<String^>^ toReturn = gcnew array<String^>(numPathNames);
							for (int i = 0; i < numPathNames; i++)
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
				property bool IncludeDates {
					bool get() { return theStruct->boolIncludeDates; }
				}
				property array<int>^ StartDates {
					array<int>^ get() {
						if (theStruct && theStruct->startDates)
						{
							int* startDates = theStruct->startDates;
							int numPathNames = theStruct->numberPathnames;
							array<int>^ toReturn = gcnew array<int>(numPathNames);
							for (int i = 0; i < numPathNames; i++)
							{
								toReturn[i] = startDates[i];
							}
							return toReturn;
						}
						else
						{
							return nullptr;
						}
					}
				}
				property array<int>^ EndDates {
					array<int>^ get() {
						if (theStruct && theStruct->endDates)
						{

							int* endDates = theStruct->endDates;
							int numPathNames = theStruct->numberPathnames;
							array<int>^ toReturn = gcnew array<int>(numPathNames);
							for (int i = 0; i < numPathNames; i++)
							{
								toReturn[i] = endDates[i];
							}
							return toReturn;
						}
						else
						{
							return nullptr;
						}
					}
				}
				property array<long long>^ PathNameHash {
					array<long long>^ get() {
						if (theStruct && theStruct->pathnameHash)
						{
							long long* pathnameHash = theStruct->pathnameHash;
							int numPathNames = theStruct->numberPathnames;
							array<long long>^ toReturn = gcnew array<long long>(numPathNames);
							for (int i = 0; i < numPathNames; i++)
							{
								toReturn[i] = pathnameHash[i];
							}
							return toReturn;
						}
						else
						{
							return nullptr;
						}
					}
				}
				property array<int>^ RecordType {
					array<int>^ get() {
						if (theStruct && theStruct->pathnameHash && theStruct->recordType)
						{
							int* recordType = theStruct->recordType;
							int numPathNames = theStruct->numberPathnames;
							array<int>^ toReturn = gcnew array<int>(numPathNames);
							for (int i = 0; i < numPathNames; i++)
							{
								//toReturn[i] = gcnew String(ztypeName(recordType[i],1));
								toReturn[i] = recordType[i];
							}
							return toReturn;
						}
						else
						{
							return nullptr;
						}
					}
				}
				property array<long long>^ LastWriteTimeRecord {
					array<long long>^ get() {
						if (theStruct && theStruct->lastWriteTimeRecord)
						{
							long long* lastWriteTimeRecord = theStruct->lastWriteTimeRecord;
							int numPathNames = theStruct->numberPathnames;
							array<long long>^ toReturn = gcnew array<long long>(numPathNames);
							for (int i = 0; i < numPathNames; i++)
							{
								toReturn[i] = lastWriteTimeRecord[i];
							}
							return toReturn;
						}
						else
						{
							return nullptr;
						}
					}
				}
				property long long LastWriteTimeFile {
					long long get() { return theStruct->lastWriteTimeFile; }
				}
				property array<unsigned int>^ CRCValues {
					array<unsigned int>^ get() {
						if (theStruct && theStruct->crcValues)
						{
							unsigned int* CRCValues = theStruct->crcValues;
							int numPathNames = theStruct->numberPathnames;
							array<unsigned int>^ toReturn = gcnew array<unsigned int>(numPathNames);
							for (int i = 0; i < numPathNames; i++)
							{
								toReturn[i] = CRCValues[i];
							}
							return toReturn;
						}
						else
						{
							return nullptr;
						}
					}
				}
				property bool GetCRCValues {
					bool get() { return theStruct->boolGetCRCvalues; }
				}
			};
		}
	}
}
