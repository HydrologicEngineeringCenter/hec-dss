#pragma once
#include "managedToUnmanaged.h"

namespace Hec {
	namespace Dss {
		namespace Native {
			public ref class ZStruct
			{
			public:
				virtual ~ZStruct()
				{
					Debug::WriteLine("Base Destructor called");
				}
			};
		}
	}
}