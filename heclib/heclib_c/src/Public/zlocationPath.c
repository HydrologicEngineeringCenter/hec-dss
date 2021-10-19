#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "hecdssInternal.h"

#define LOCATION_PATH "Location Info////"

/**
*  Function:	zlocationPath
*
*  Use:			Semi-Public
*
*  Description:	Utility functions to create a location pathname from a regular pathname
*
*  Declaration: char* zlocationPath(const char* pathname)
*
*
*  Parameters:	const char* pathname
*					A regular pathname to obtain the A and B parts to create a location pathname.
*					A location pathname is in the form:
*						/A part/B part/Location Info////
*
*  Returns:		A malloc'ed location pathname.  Be sure to free this when done!
*
*
*  See Also:	zlocationStore and zlocationRetrieve
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


char* zlocationPath(const char* pathname)
{
	int len;
	int positions[7];
	char* locationPathname;

	zpathnamePartPositions (pathname, strlen(pathname), positions, 7);
	len = positions[2] + (int)strlen(LOCATION_PATH) + 1;
	locationPathname = (char *)calloc(len, 1);
	stringCopy (locationPathname, len, pathname, (size_t)positions[2]) ;
	stringCat (locationPathname, len, LOCATION_PATH, _TRUNCATE);

	return locationPathname;
}

