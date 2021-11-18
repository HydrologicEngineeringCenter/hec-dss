#pragma once

#include "DSSGrid.h"
#include "pinvokeTools.h"

extern "C" {
	__declspec(dllexport) void GetCatalogPathName(zStructCatalog* cat, char* inputPathName, int pathNameLength, int pathNameIndex);
	__declspec(dllexport) int GetPathNameLength(zStructCatalog* cat, int pathNameIndex);
	__declspec(dllexport) int GetNumberPathNames(zStructCatalog* cat);
	__declspec(dllexport) int* GetRecordType(zStructCatalog* cat);
}