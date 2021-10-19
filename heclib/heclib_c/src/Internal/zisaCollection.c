#include <ctype.h>

#include "hecdssInternal.h"

/**
*  Function:	zisaCollection
*
*  Use:			Private (Internal)
*
*  Description:	Determines if a pathname is a collections path
*
*  Declaration: int zisaCollection(const char* pathname, size_t pathnameLength);
*
*  Parameters:
*				const char *pathname
*					The pathname to determine if a collections path
*
*				size_t pathnameLength
*					The length of the pathname
*
*
*	Returns:	int collection start position
*					position of "C", from  from C:.......|" if is a collections path, otherwise 0.
*
*	Note:		Collections are identified by an F part of /C:000000|REST OF FPART/
*				Where 00000 is generally a sequence number, for example
*				/YUBA/SMARTSVILLE/FLOW/01JAN1997/1HOUR/C:000042|OPERATION A/
*               The length of the collection sequence and what it contains is not
*				constrained (it may be of any length and contain any characters, as long
*				as the F part starts with C: and the end of the sequence is "|".
*
*
*
*  Called By:	catalog functions
*
*	Author:			Bill Charley, 2015
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/



int zisaCollection(char* pathname, size_t pathLength)
{
	int i;
	int ipos;
	int numbSlashes;
	char c;
	int pathnameLength;


	pathnameLength = (int)pathLength;
	if (pathnameLength < 12)
		return 0;

	//  look for a collections identifier in the F part, which is
	//  "/C:", followed by an character sequence, then a bar "|".
	//  The sequence is usually a number, but that is not a requirement.

	//  Search backwards for second "/" (beginning of F part)

	numbSlashes = 0;
	ipos = 0;
	for (i=(pathnameLength-1); i>=5; i--) {
		c = pathname[i];
		if (c == '/') {
			numbSlashes++;
			if (numbSlashes >=2 ) {
				return 0;
			}
		}
		if (c == '|') {
			ipos = i;
			break;
		}
	}

	if (ipos < 5)
		return 0;

	for (i=(ipos-1); i>=5; i--) {
		c = pathname[i];
		if (c == '/') {
			if ((toupper(pathname[i+1]) == 'C') &&
				(pathname[i+2] == ':')) {
					return (i+1);
			}
		}
	}
	return 0;
}

