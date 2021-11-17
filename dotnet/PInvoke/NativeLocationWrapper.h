#pragma once

#include "DSSGrid.h"
#include "pinvokeTools.h"

extern "C" {
	__declspec(dllexport) double GetXOrdinate(zStructLocation* loc);
	__declspec(dllexport) double GetYOrdinate(zStructLocation* loc);
	__declspec(dllexport) double GetZOrdinate(zStructLocation* loc);
	__declspec(dllexport) int GetCoordinateSystem(zStructLocation* loc);
	__declspec(dllexport) int GetCoordinateID(zStructLocation* loc);
	__declspec(dllexport) int GetHorizontalUnits(zStructLocation* loc);
	__declspec(dllexport) int GetHorizontalDatum(zStructLocation* loc);
	__declspec(dllexport) int GetVerticalUnits(zStructLocation* loc);
	__declspec(dllexport) int GetVerticalDatum(zStructLocation* loc);
	__declspec(dllexport) BSTR GetTimeZoneName(zStructLocation* loc);
	__declspec(dllexport) BSTR GetSupplemental(zStructLocation* loc);
}