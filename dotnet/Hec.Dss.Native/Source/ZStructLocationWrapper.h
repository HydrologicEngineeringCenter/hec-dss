#pragma once
#include "managedToUnmanaged.h"
#include "ZStruct.h"

namespace Hec {
	namespace Dss {
		namespace Native {
			public ref class ZStructLocationWrapper : ZStruct
			{
			public:
				ZStructLocationWrapper(zStructLocation* theStruct)
				{
					this->theStruct = theStruct;
				}
				ZStructLocationWrapper(zStructLocation* theStruct, bool deepCopy)
				{
					if (deepCopy)
					{
						zStructLocation* newStruct = zstructLocationNew(theStruct->pathname);
						for (int i = 0; i < 25; i++)
						{
							newStruct->allocated[i] = theStruct->allocated[i];
						}
						newStruct->coordinateID = theStruct->coordinateID;
						newStruct->coordinateSystem = theStruct->coordinateSystem;
						newStruct->horizontalDatum = theStruct->horizontalDatum;
						newStruct->horizontalUnits = theStruct->horizontalUnits;
						if (theStruct->supplemental)
						{
							newStruct->supplemental = (char*)malloc(strlen(theStruct->supplemental) + 1);
							strcpy(newStruct->supplemental, theStruct->supplemental);
						}
						if (theStruct->timeZoneName)
						{
							newStruct->timeZoneName = (char*)malloc(strlen(theStruct->timeZoneName) + 1);
							strcpy(newStruct->timeZoneName, theStruct->timeZoneName);
						}
						newStruct->verticalDatum = theStruct->verticalDatum;
						newStruct->verticalUnits = theStruct->verticalUnits;
						newStruct->xOrdinate = theStruct->xOrdinate;
						newStruct->yOrdinate = theStruct->yOrdinate;
						newStruct->zOrdinate = theStruct->zOrdinate;
						this->theStruct = newStruct;
					}
					else
					{
						this->theStruct = theStruct;
					}

				}
				ZStructLocationWrapper()
				{

				}
				~ZStructLocationWrapper()
				{
					zstructFree(theStruct);
				}
				!ZStructLocationWrapper()
				{
					zstructFree(theStruct);
				}
				zStructLocation* theStruct;
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
				property double XOrdinate {
					double get() { return theStruct->xOrdinate; }
					void set(double val) { theStruct->xOrdinate = val; }
				}
				property double YOrdinate {
					double get() { return theStruct->yOrdinate; }
					void set(double val) { theStruct->yOrdinate = val; }
				}
				property double ZOrdinate {
					double get() { return theStruct->zOrdinate; }
					void set(double val) { theStruct->zOrdinate = val; }
				}
				property int CoordinateSystem {
					int get() { return theStruct->coordinateSystem; }
					void set(int val) { theStruct->coordinateSystem = val; }
				}
				property int CoordinateID {
					int get() { return theStruct->coordinateID; }
					void set(int val) { theStruct->coordinateID = val; }
				}
				property int HorizontalUnits {
					int get() { return theStruct->horizontalUnits; }
					void set(int val) { theStruct->horizontalUnits = val; }
				}
				property int HorizontalDatum {
					int get() { return theStruct->horizontalDatum; }
					void set(int val) { theStruct->horizontalDatum = val; }
				}
				property int VerticalUnits {
					int get() { return theStruct->verticalUnits; }
					void set(int val) { theStruct->verticalUnits = val; }
				}
				property int VerticalDatum {
					int get() { return theStruct->verticalDatum; }
					void set(int val) { theStruct->verticalDatum = val; }
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
				property String^ Supplemental {
					String^ get() {
						if (theStruct && theStruct->supplemental)
							return gcnew String(theStruct->supplemental);
						else
							return nullptr;
					}
					void set(String^ val)
					{
						if (theStruct->supplemental)
							free(theStruct->supplemental);
						theStruct->supplemental = managedToUnmanagedString(val);
					}
				}
			};
		}
	}
}
