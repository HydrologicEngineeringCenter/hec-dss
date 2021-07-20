#pragma once
#include "managedToUnmanaged.h"
#include "ZStruct.h"

namespace Hec {
	namespace Dss {
		namespace Native {
			public ref class ZStructTransferWrapper : ZStruct
			{
			public:
				ZStructTransferWrapper(zStructTransfer* theStruct)
				{
					this->theStruct = theStruct;
				}
				ZStructTransferWrapper()
				{

				}
				~ZStructTransferWrapper()
				{
					zstructFree(theStruct);
				}
				!ZStructTransferWrapper()
				{
					zstructFree(theStruct);
				}
				zStructTransfer* theStruct;
			};
		}
	}
}
