#pragma once
#include "managedToUnmanaged.h"
#include "ZStruct.h"

namespace Hec {
	namespace Dss {
		namespace Native {
			public ref class ZStructRecordAddressesWrapper : ZStruct
			{
			public:
				ZStructRecordAddressesWrapper(zStructRecordAddresses* theStruct)
				{
					this->theStruct = theStruct;
				}
				ZStructRecordAddressesWrapper()
				{

				}
				~ZStructRecordAddressesWrapper()
				{
					zstructFree(theStruct);
				}
				zStructRecordAddresses* theStruct;
			};
		}
	}
}