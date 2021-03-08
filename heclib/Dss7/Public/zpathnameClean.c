#include <string.h>

#include "hecdssInternal.h"


/**
*  Function:	zpathnameClean
*
*  Use:			Public
*
*  Description:	Copies a pathname removing invalid characters (follows strncpy_s)
*
*  Declaration: int zpathnameClean(char *newPathname, size_t sizeofNewPathname, const char *oldPathname);
*
*  Parameters:	char *newPathname:  The string to copy into (the resulting pathname, with invalid characters removed)
*
*				size_t sizeofNewPathname:  The size of newPathname.  A pathname can be up to 392 characters
*										long, so this should be 393 (to hold terminating null)
*
*				const char *oldPathname:  The pathname to copy from.
*
*
*  Returns:		int length:	The length of the resulting pathname
*
*
*  Remarks:		Invalid characters are
*						escape codes and DEL  (<32, >126)
*						pathname is limited to seven slashes "/"
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zpathnameClean(char *newPathname, size_t sizeofNewPathname, const char *oldPathname)
{
	int len;
	int i;
	int count;
	int ch;
	int slashCount;

	len = (int)strlen(oldPathname);
	count = 0;
	slashCount = 0;
	for (i=0; i<len; i++) {
		ch = (int)oldPathname[i];
		if (ch < 32) {
			//  Remove escape codes
			continue;
		}
		if (ch > 126) {
			//  Remove DEL
			continue;
		}
	/*	if (ch == 34) {
			//  Remove "
			continue;
		}
		if (ch == 39) {
			//  Remove '
			continue;
		}
		if (ch == 92) {
			//  Remove backslash
			continue;
		}  */
		if (ch == 47) {
			//  Count slashes
			slashCount++;
		}
		if (count >= (int)sizeofNewPathname) {
			//  Don't go beyond buffer length
			break;
		}
		newPathname[count++] = oldPathname[i];
		if (slashCount == 7) {
			//  Truncate anything past seventh slash
			break;
		}
	}
	//  Null terminate
	newPathname[count] = '\0';
	return count;
}

