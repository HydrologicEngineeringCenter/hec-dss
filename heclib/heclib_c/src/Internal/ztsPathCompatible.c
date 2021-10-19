#include <ctype.h>
#include <stdlib.h>

#include "heclib7.h"
#include "hecdssInternal.h"


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

char *ztsPathCompatible(int version, const char *pathname, size_t pathnameLen)
{
	int count;
	int i;
	int startE;
	int endE;
	int ipos;
	int istart;
	int len;
	int found;
	char *newPath;
	size_t newPathLen;


	count = 0;
	startE = 0;
	endE = 0;
	for (i=0; i<(int)pathnameLen; i++) {
		if (pathname[i] == '/') {
			count++;
			if (count == 5) {
				startE = i;
			}
			else if (count == 6) {
				endE = i;
				break;
			}
		}
	}

	if (endE == 0) {
		return (char *)0;
	}

	len = endE - startE - 1;
	if (len < 4) {
		return (char *)0;
	}

	//  Look for "Min"
	//  1Min  or 10Min
	ipos = startE + 2;
	istart = 0;
	if (toupper(pathname[ipos]) == 'M') {
		istart = ipos;
	}
	else {
		ipos++;
		if (toupper(pathname[ipos]) == 'M') {
			istart = ipos;
		}
	}

	if (istart == 0) {
		return (char *)0;
	}

	//  We have an M in the second or third character position for the E part
	if (version == 6) {
		if (len < 6) {
			//  If less than 6, then it is not "Minute"
			return (char *)0;
		}
	}
	else {		//  if (version == 7) {
		if (len > 5) {
			//  If greater than 5, then it is not "MIN"
			return (char *)0;
		}
	}

	//  We have a candidate to be switched
	//  Check the remaining characters for "MINUTE/"
	found = 0;
	if (version == 6) {
		if (toupper(pathname[++ipos]) == 'I') {
			if (toupper(pathname[++ipos]) == 'N') {
				if (toupper(pathname[++ipos]) == 'U') {
					if (toupper(pathname[++ipos]) == 'T') {
						if (toupper(pathname[++ipos]) == 'E') {
							if (pathname[++ipos] == '/') {
								found = 1;
							}
						}
					}
				}
			}
		}
	}
	else {	//  if (version == 7) {
		//  Look for "IN/"
		if (toupper(pathname[++ipos]) == 'I') {
			if (toupper(pathname[++ipos]) == 'N') {
				if (pathname[++ipos] == '/') {
					found = 1;
				}
			}
		}
	}

	if (found == 0) {
		return (char *)0;
	}

	//  Create a new path string with the correct E part for the version
	if (version == 6) {
		newPathLen = pathnameLen - 3;
		istart += 3;
		newPath = (char *)malloc(newPathLen+1);
		//  Copy up through Min
		stringCopy (newPath, istart+1, pathname, istart+1);
		stringCopy (&newPath[istart], (newPathLen-istart+1), &pathname[endE], (pathnameLen-endE+1));
	}
	else {
		newPathLen = pathnameLen + 3;
		ipos = istart + 6;
		newPath = (char *)malloc(newPathLen+1);
		stringCopy (newPath, istart+1, pathname, istart+1);
		stringCopy (&newPath[istart], 7, "Minute", 6);
		stringCopy (&newPath[ipos], (newPathLen-ipos+1), &pathname[endE], (pathnameLen-endE+1));
	}

	return newPath;
}

