#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "heclib.h"
#include "hecdssInternal.h"

#define LOCATION_PATH "Location Info////"



int zlocationDelete(long long *ifltab, const char* pathname)
{
	int status;
	int positions[7];
	char locationPathname[200];

	zpathnamePartPositions (pathname, strlen(pathname), positions, 7);
	stringCopy (locationPathname, sizeof(locationPathname), pathname, (size_t)positions[2]) ;
	stringCat(locationPathname, sizeof(locationPathname), LOCATION_PATH, _TRUNCATE);

	status = zdelete(ifltab, locationPathname);

	return status;
}

