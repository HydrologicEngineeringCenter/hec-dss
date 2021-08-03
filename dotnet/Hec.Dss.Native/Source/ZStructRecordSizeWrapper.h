#pragma once
#include "managedToUnmanaged.h"
#include "ZStruct.h"

namespace Hec {
	namespace Dss {
		namespace Native {
			public ref class ZStructRecordSizeWrapper : ZStruct
			{
			public:
				ZStructRecordSizeWrapper(zStructRecordSize* theStruct)
				{
					this->theStruct = theStruct;
				}
				ZStructRecordSizeWrapper()
				{

				}
				~ZStructRecordSizeWrapper()
				{
					zstructFree(theStruct);
				}
				!ZStructRecordSizeWrapper()
				{
					zstructFree(theStruct);
				}
				zStructRecordSize* theStruct;
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
				property int DataType {
					int get() { return theStruct->dataType; }
					void set(int val) { theStruct->dataType = val; }
				}
				property int Version {
					int get() { return theStruct->dataType; }
					void set(int val) { theStruct->dataType = val; }
				}
				property int LogicalNumberValues {
					int get() { return theStruct->logicalNumberValues; }
					void set(int val) { theStruct->logicalNumberValues = val; }
				}
				property int Values1Number {
					int get() { return theStruct->values1Number; }
					void set(int val) { theStruct->values1Number = val; }
				}
				property int Values2Number {
					int get() { return theStruct->values2Number; }
					void set(int val) { theStruct->values2Number = val; }
				}
				property int Values3Number {
					int get() { return theStruct->values3Number; }
					void set(int val) { theStruct->values3Number = val; }
				}
				property int InternalHeaderNumber {
					int get() { return theStruct->internalHeaderNumber; }
					void set(int val) { theStruct->internalHeaderNumber; }
				}
				property int Header2Number {
					int get() { return theStruct->header2Number; }
					void set(int val) { theStruct->header2Number; }
				}
				property int UserHeaderNumber {
					int get() { return theStruct->userHeaderNumber; }
					void set(int val) { theStruct->userHeaderNumber = val; }
				}
				property int AllocatedSize {
					int get() { return theStruct->allocatedSize; }
					void set(int val) { theStruct->allocatedSize = val; }
				}
				property String^ ProgramLastWrite {
					String^ get() {
						if (theStruct && theStruct->programLastWrite)
							return gcnew String(theStruct->programLastWrite);
						else
							return nullptr;
					}
				}
				property int NumberRecordsFound {
					int get() { return theStruct->numberRecordsFound; }
					void set(int val) { theStruct->numberRecordsFound = val; }
				}
				property int ItsPrecisionStored {
					int get() { return theStruct->itsTimePrecisionStored; }
					void set(int val) { theStruct->itsTimePrecisionStored = val; }
				}
				property int TSPrecision {
					int get() { return theStruct->tsPrecision; }
					void set(int val) { theStruct->tsPrecision = val; }
				}
				property int TSTimeOffset {
					int get() { return theStruct->tsTimeOffset; }
					void set(int val) { theStruct->tsTimeOffset = val; }
				}
				property int TSProfileDepthsNumber {
					int get() { return theStruct->tsProfileDepthsNumber; }
					void set(int val) { theStruct->tsProfileDepthsNumber = val; }
				}
				property int TSBlockStartPosition {
					int get() { return theStruct->tsBlockStartPosition; }
					void set(int val) { theStruct->tsBlockStartPosition = val; }
				}
				property int TSBlockEndPosition {
					int get() { return theStruct->tsBlockEndPosition; }
					void set(int val) { theStruct->tsBlockEndPosition = val; }
				}
				property int TSValueSize {
					int get() { return theStruct->tsValueSize; }
					void set(int val) { theStruct->tsValueSize = val; }
				}
				property int TSValueElementSize {
					int get() { return theStruct->tsValueElementSize; }
					void set(int val) { theStruct->tsValueElementSize = val; }
				}
				property int TSValueCompressionFlag {
					int get() { return theStruct->tsValuesCompressionFlag; }
					void set(int val) { theStruct->tsValuesCompressionFlag = val; }
				}
				property int TSQualityElementSize {
					int get() { return theStruct->tsQualityElementSize; }
					void set(int val) { theStruct->tsQualityElementSize = val; }
				}
				property int TSQualityCompressionFlag {
					int get() { return theStruct->tsQualityCompressionFlag; }
					void set(int val) { theStruct->tsQualityCompressionFlag = val; }
				}
				property int TSINotesElementSize {
					int get() { return theStruct->tsInotesElementSize; }
					void set(int val) { theStruct->tsInotesElementSize = val; }
				}
				property int TSINotesCompressionFlag {
					int get() { return theStruct->tsInotesCompressionFlag; }
					void set(int val) { theStruct->tsInotesCompressionFlag = val; }
				}
				property int TSCNotesLength {
					int get() { return theStruct->tsCnotesLength; }
					void set(int val) { theStruct->tsCnotesLength = val; }
				}
				property int PDNumberCurves {
					int get() { return theStruct->pdNumberCurves; }
					void set(int val) { theStruct->pdNumberCurves = val; }
				}
				property int PDNumberOrdinates {
					int get() { return theStruct->pdNumberOrdinates; }
					void set(int val) { theStruct->pdNumberOrdinates = val; }
				}
				property int IPDValueSize {
					int get() { return theStruct->ipdValueSize; }
					void set(int val) { theStruct->ipdValueSize = val; }
				}
				property bool PDIndependentISXAxis {
					bool get() { return theStruct->pdBoolIndependentIsXaxis; }
					void set(bool val) { theStruct->pdBoolIndependentIsXaxis = val; }
				}
				property int PDLabelIsLength {
					int get() { return theStruct->pdLabelsLength; }
					void set(int val) { theStruct->pdLabelsLength = val; }
				}
				property int PDPrecision {
					int get() { return theStruct->pdPrecision; }
					void set(int val) { theStruct->pdPrecision = val; }
				}
			};
		}
	}
}
