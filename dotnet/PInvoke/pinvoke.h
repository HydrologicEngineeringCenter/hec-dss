#pragma once

#include "DSSGrid.h"

extern "C" {
	__declspec(dllexport) int ZOpen(long long* ifltab, const char* dssFilename);
	__declspec(dllexport) int ZSet(const char* parameter, const char* charVal, int integerValue);
}