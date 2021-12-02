#pragma once

#include "DSSGrid.h"
#include "pinvokeTools.h"

extern "C" {
	__declspec(dllexport) BSTR GetCatalogPathName(zStructCatalog* cat, int pathNameIndex);
	__declspec(dllexport) int GetPathNameLength(zStructCatalog* cat, int pathNameIndex);
	__declspec(dllexport) int GetNumberPathNames(zStructCatalog* cat);
	__declspec(dllexport) int* GetRecordType(zStructCatalog* cat);
	__declspec(dllexport) int GetTypeWantedStart(zStructCatalog* cat);
	__declspec(dllexport) void SetTypeWantedStart(zStructCatalog* cat, int value);
	__declspec(dllexport) int GetTypeWantedEnd(zStructCatalog* cat);
	__declspec(dllexport) void SetTypeWantedEnd(zStructCatalog* cat, int value);

}