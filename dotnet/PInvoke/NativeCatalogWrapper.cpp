#include "pch.h"
#include "NativeCatalogWrapper.h"



void GetCatalogPathName(zStructCatalog* cat, char* inputPathName, int pathNameLength, int pathNameIndex)
{
	if (inputPathName) {
		std::copy(cat->pathnameList[pathNameIndex], cat->pathnameList[pathNameIndex] + pathNameLength, inputPathName);
	}
}

int GetPathNameLength(zStructCatalog* cat, int pathNameIndex)
{
	if (pathNameIndex >= 0 && pathNameIndex <= cat->numberPathnames)
		return strlen(cat->pathnameList[pathNameIndex]);

	return -1;
}

int GetNumberPathNames(zStructCatalog* cat)
{
	return cat->numberPathnames;
}

int* GetRecordType(zStructCatalog* cat)
{
	return cat->recordType;
}
