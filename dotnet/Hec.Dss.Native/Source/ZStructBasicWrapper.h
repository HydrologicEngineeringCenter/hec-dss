#pragma once
#include "managedToUnmanaged.h"
#include "ZStruct.h"


namespace Hec {
	namespace Dss {
		namespace Native {
			public ref class ZStructBasicWrapper : ZStruct
			{
			public:
				ZStructBasicWrapper(zStructBasic* theStruct)
				{
					this->theStruct = theStruct;
				}
				ZStructBasicWrapper()
				{

				}
				~ZStructBasicWrapper()
				{
					zstructFree(theStruct);
				}
				!ZStructBasicWrapper()
				{
					zstructFree(theStruct);
				}
				zStructBasic* theStruct;
			};
		}
	}
}