#include <string.h>
#include "heclib.h"

/**
*  Function:	zcatParsePath
*
*  Use:			Private
*
*  Description:	Parses a pathname with wild characters in preparation for later matching real paths.
*
*  Declaration: int zcatParsePath(const char *pathWithWild, int *partAction, int *lengths,
				  char *apart, size_t apartSize,
				  char *bpart, size_t bpartSize,
				  char *cpart, size_t cpartSize,
				  char *dpart, size_t dpartSize,
				  char *epart, size_t epartSize,
				  char *fpart, size_t fpartSize);
*
*  Parameters:	const char *pathWithWildChars
*					String that represents a pathname with wild characters represented by a star (*) to match
*					any string in the pathname part.  If this string is '\0', all parts are matched.
*					Wild characters can only be at the beginning or end of a part,
*					not inside of a string.  An example is a C part with "*Flow*" , which
*					will match all pathnames that have "Flow" anywhere in the C part, such as "Flow", "Inflow", "Outflow-Reg", etc.
*					A part such as "Flow*Reg" is not legal.  A null (//) will only match a null, where only a star (*) will match all.
*/ //						/Sacramento/Natomas/Flow*/*/1Day/Run*/
//							Note '\0' == /*/*/*/*/*/*/,  returns 0 (match all)
/*
*				int partAction[6]
*					A 6 element integer array that is returned with flags on how to mach each of the 6 pathname parts.
*					The flags returned are as follows
*/
//					  Number		Action				Examples
//						0		Match all (ignore)	/*/
//						1		Match Exactly		/Flow/
//						2		Starts with			/Flow*/
//						3		Ends with			/*Loc/
//						4		Contains			/*Flow*/
/*
*				int *lengths
*					A 6 element integer array that is returned with the lengths of each of the 6 pathname parts to match.
*					The lengths will exclude any beginning or trailing asterisks, e.g., *Flow* has a length of 4.
*
*				char *apart, size_t apartSize, etc.
*					A character string that will be returned with the part to match.  For example, *Flow* will be returned as Flow.
*					Each part should be MAX_PART_SIZE elements long.  That length is to be passed in the size parameter following the part.
*
*
*	Returns		0:	Match all (don't do compare)
*				1:	Do compare
*
*	Remarks:	zcatComparePath() is a companion function
*
*
*
*	See Also:	zcatComparePath(), zcatalogInternal()
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zcatParsePath(const char *pathWithWild, int *partAction, int *lengths,
				  char *apart, size_t apartSize,
				  char *bpart, size_t bpartSize,
				  char *cpart, size_t cpartSize,
				  char *dpart, size_t dpartSize,
				  char *epart, size_t epartSize,
				  char *fpart, size_t fpartSize)
{
	int i;
	int number;
	int len;
	int positions[7];
	int istart;
	int startsWith;
	int endsWith;
	char part[MAX_PART_SIZE]; // size to largest (F)

	if (!pathWithWild)
		return 0;

	len = (int)strlen(pathWithWild);
	if (len == 0)
		return 0;
	number = zpathnamePartPositions (pathWithWild, strlen(pathWithWild), positions, 7);
	//  If we don't have 6 parts, return to avoid an error
	if (number != 7)
		return 0;

	for (i=0; i<6; i++) {
		zpathnameGetPart (pathWithWild, i+1, part, sizeof(part));
		istart = 0;
		len = (int)strlen(part);
		if (len == 1) {
			if (part[0] == '*') {
				partAction[i] = 0;
			}
			else {
				partAction[i] = 1;
			}
		}
		else if (len == 0) {
			partAction[i] = 1;
		}
		else {
			if (part[0] == '*') {
				endsWith = 1;
			}
			else {
				endsWith = 0;
			}
			if (part[len-1] == '*') {
				startsWith = 1;
			}
			else {
				startsWith = 0;
			}
			if (startsWith) {
				if (endsWith) {
					partAction[i] = 4;
					istart = 1;
					len -= 2;
				}
				else {
					partAction[i] = 2;
					len -= 1;
				}
			}
			else if (endsWith) {
				partAction[i] = 3;
				istart = 1;
				len -= 1;
			}
			else {
				//  Exact
				partAction[i] = 1;
			}
		}
		if (partAction[i]) {
			//  stringCopy (char *destination, int sizeOfDestination, const char* source, size_t lenSource)
			if (i == 0) {
				stringCopy (apart, apartSize, &part[istart], (size_t)len);
			}
			else if (i == 1) {
				stringCopy (bpart, bpartSize, &part[istart], (size_t)len);
			}
			else if (i == 2) {
				stringCopy (cpart, cpartSize, &part[istart], (size_t)len);
			}
			else if (i == 3) {
				stringCopy (dpart, dpartSize, &part[istart], (size_t)len);
			}
			else if (i == 4) {
				stringCopy (epart, epartSize, &part[istart], (size_t)len);
			}
			else if (i == 5) {
				stringCopy (fpart, fpartSize, &part[istart], (size_t)len);
			}
			lengths[i] = len;
		}
	}
	return 1;
}

