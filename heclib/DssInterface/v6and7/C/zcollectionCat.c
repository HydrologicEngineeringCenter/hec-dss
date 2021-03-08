#include "heclib.h"



int zcollectionCat(long long *ifltab, const char *seedPathname, zStructCatalog *catStruct)
{
	if (zgetVersion(ifltab) == 6) {
		return zcatalog6Internal (ifltab, seedPathname, catStruct, 0, 0, 0, 1, 0);
	}
	else {
		 return zcatalogInternal(ifltab, seedPathname, catStruct, 0, 0, 0, 1, 0);
	 }
}

