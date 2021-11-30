#include "pch.h"
#include "NativeCatalogWrapper.h"



BSTR GetCatalogPathName(zStructCatalog* cat, int pathNameIndex)
{
	return ANSItoBSTR(cat->pathnameList[pathNameIndex]);
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

int GetTypeWantedStart(zStructCatalog* cat)
{
	return cat->typeWantedStart;
}

void SetTypeWantedStart(zStructCatalog* cat, int value)
{
	cat->typeWantedStart = value;
}

int GetTypeWantedEnd(zStructCatalog* cat)
{
	return cat->typeWantedEnd;
}

void SetTypeWantedEnd(zStructCatalog* cat, int value)
{
	cat->typeWantedEnd = value;
}
