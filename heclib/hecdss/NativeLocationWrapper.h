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
	__declspec(dllexport) void SetXOrdinate(zStructLocation* loc, double value);
	__declspec(dllexport) void SetYOrdinate(zStructLocation* loc, double value);
	__declspec(dllexport) void SetZOrdinate(zStructLocation* loc, double value);
	__declspec(dllexport) void SetCoordinateSystem(zStructLocation* loc, int value);
	__declspec(dllexport) void SetCoordinateID(zStructLocation* loc, int value);
	__declspec(dllexport) void SetHorizontalUnits(zStructLocation* loc, int value);
	__declspec(dllexport) void SetHorizontalDatum(zStructLocation* loc, int value);
	__declspec(dllexport) void SetVerticalUnits(zStructLocation* loc, int value);
	__declspec(dllexport) void SetVerticalDatum(zStructLocation* loc, int value);
	__declspec(dllexport) void SetTimeZoneName(zStructLocation* loc, const char* value);
	__declspec(dllexport) void SetSupplemental(zStructLocation* loc, const char* value);
	__declspec(dllexport) void SetPathName(zStructLocation* loc, const char* value);
}