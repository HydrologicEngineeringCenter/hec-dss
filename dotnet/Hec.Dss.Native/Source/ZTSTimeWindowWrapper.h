#pragma once
#include "managedToUnmanaged.h"
#include "ZStruct.h"

namespace Hec {
	namespace Dss {
		namespace Native {
			public ref class ZTSTimeWindowWrapper : ZStruct
			{
			public:
				ZTSTimeWindowWrapper(ztsTimeWindow* theStruct)
				{
					this->theStruct = theStruct;
				}
				ZTSTimeWindowWrapper()
				{

				}
				~ZTSTimeWindowWrapper()
				{
					zstructFree(theStruct);
				}
				!ZTSTimeWindowWrapper()
				{
					zstructFree(theStruct);
				}
				ztsTimeWindow* theStruct;
			};
		}
	}
}