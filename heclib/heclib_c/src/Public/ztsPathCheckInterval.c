#include <stdlib.h>

#include "heclib7.h"
#include "hecdssInternal.h"
#include "fortran_string_len_size.h"

/**
*  Function:	ztsPathCheckInterval
*
*  Use:			Public
*
*  Description:	Makes sure a time series pathname has a standard interval for the DSS version used
*
*  Declaration:int ztsPathCheckInterval(long long *ifltab, char *pathname, size_t sizeofPathname);
*
*  Parameters:	long long *ifltab
*					The integer file table array passed among DSS functions.  Used to get DSS version
*
*				char *pathname
*					The time series pathname to check and set the interval (E part)
*
*				size_t sizeofPathname
*					The size of pathname.
*
*
*  Returns:		pathname with valid E part, if time series
*				int status:
*					-1:  Not time series
*					 0:  Regular interval time series
*					 1:  Irregular interval time series
*
*
*
*	Author:			Bill Charley
*	Date:			2013
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int ztsPathCheckInterval(long long *ifltab, char *pathname, size_t sizeofPathname)
{

	int istat;
	int len;
	int flagSecondsToChar;
	int interval;
	int version;
	char ePart[MAX_PART_SIZE];

	istat = -1;
	len = zpathnameGetPart (pathname, 5, ePart, sizeof(ePart));
	if (len > 3) {
		flagSecondsToChar = 0;
		version = zgetVersion(ifltab);
		istat = ztsGetStandardInterval(version, &interval, ePart, sizeof(ePart), &flagSecondsToChar);
		if ((istat == 0) || (istat == 1)) {
			zpathnameSetPart (pathname, (size_t)sizeofPathname, (const char*)ePart, 5);
		}
	}
	return istat;
}


//  Fortran interface
void ztspathcheckinterval_ (long long *ifltab, char *pathname, slen_t sizeofPathname)
{
	char *path;

	path = stringFortToC(pathname, sizeofPathname);
	ztsPathCheckInterval(ifltab, path, sizeof(path));
	stringCToFort(pathname, sizeofPathname, path);
	free(path);
}

