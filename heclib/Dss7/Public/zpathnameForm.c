#include <string.h>

#include "hecdssInternal.h"
#include "hecdss7.h"


/**
*  Function:	zpathnameForm
*
*  Use:			Public
*
*  Description:	Builds a pathname from 6 pathname parts
*
*  Declaration: int zpathnameForm(const char *aPart, const char *bPart, const char *cPart, const char *dPart,
*								  const char *ePart, const char *fPart, char *pathname, size_t sizeofPathname);
*
*  Parameters:
*				const char *aPart:  The "A" (first) part of the pathname (input).  This should NOT contain slashes (/)
*
*				const char *bPart:  The "B" (second) part of the pathname(input).
*
*				const char *cPart:  The "C" (third) part of the pathname(input).
*
*				const char *dPart:  The "D" (fourth) part of the pathname(input).
*
*				const char *ePart:  The "E" (fifth) part of the pathname(input).
*
*				const char *fPart:  The "F" (sixth) part of the pathname(input).
*
*				char *pathname:		A string to contain the resulting pathname (output).  A pathname can be
*									up to 392 characters long, so this should be sized to 393 (to hold terminating null)
*
*				size_t sizeofPathname:  The size of pathname (input).
*
*
*
*  Returns:		int length:	The length of the resulting pathname
*
*
*  See Also:	zpathnameSetPart() to change a pathname part
*				zpathnameGetPart() to get a pathname part
*				zpathnameClean() to remove any invalid characters
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zpathnameForm(const char *aPart, const char *bPart, const char *cPart, const char *dPart,
				  const char *ePart, const char *fPart, char *pathname, size_t sizeofPathname)
{
	int count;

	count = 0;
	if (count < (int)sizeofPathname) {
		pathname[count++] = '/';
	}
	else {
		return 0;
	}
	count = zpathnameAddPart(aPart, count, pathname, sizeofPathname);
	count = zpathnameAddPart(bPart, count, pathname, sizeofPathname);
	count = zpathnameAddPart(cPart, count, pathname, sizeofPathname);
	count = zpathnameAddPart(dPart, count, pathname, sizeofPathname);
	count = zpathnameAddPart(ePart, count, pathname, sizeofPathname);
	count = zpathnameAddPart(fPart, count, pathname, sizeofPathname);
	if (count < (int)sizeofPathname) {
		pathname[count++] = '\0';
	}
	else {
		if ((int)sizeofPathname > 0) {
			pathname[(int)sizeofPathname-1] = '\0';
		}
	}
	return count;
}

