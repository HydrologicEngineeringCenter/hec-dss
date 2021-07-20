#pragma once
#include "DSS.h"
namespace Hec {
	namespace Dss {
		namespace Native {
			public ref class HecDateTime
			{
			public:
				HecDateTime();
				static bool Undefined(int julian)
				{
					return julian == UNDEFINED_TIME;
				}
			};

		}
	}
}