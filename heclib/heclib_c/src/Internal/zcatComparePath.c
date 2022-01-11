#include <string.h>
#include <ctype.h>

#include "heclib.h"


/**
*  Function:	zcatComparePath
*
*  Use:			Private
*
*  Description:	Compares a pathname with the parsed wild character pathname to see if they match
*
*  Declaration: int zcatComparePath(const char *cpath, int *partAction, int *lengths,
*									char *apart, char *bpart, char *cpart,
*									char *dpart, char *epart, char *fpart);
*
*  Parameters:	const char *cpath
*					The pathname to match.
*
*				int partAction[6]
*					A 6 element integer array from zcatParsePath with flags on how to mach each of the 6 pathname parts.
*					The flags are as follows
*/
//					  Number		Action				Examples
//						0		Match all (ignore)	/*/
//						1		Match Exactly		/Flow/
//						2		Starts with			/Flow*/
//						3		Ends with			/*Loc/
//						4		Contains			/*Flow*/
/*
*				int *lengths
*					A 6 element integer array from zcatParsePath with the lengths of each of the 6 pathname parts to match.
*					The lengths will exclude any beginning or trailing asterisks, e.g., *Flow* has a length of 4.
*
*				char *apart, size_t apartSize, etc.
*					The pathname part character strings from zcatParsePath with the part to match.
*
*
*	Returns		0:	Doesn't Match
*				1:	Matches
*
*	Remarks:	zcatParsePath() is a companion function
*
*
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zcatComparePath(const char *cpath, int *partAction, int *lengths,
					char *apart, char *bpart, char *cpart, char *dpart, 
					char *epart, char *fpart, int boolCollection)
{
	int i;
	int j;
	int result;
	int len;
	int positions[7];
	int istart;
	int number;
	int i1;
	int i2;
	int boolCollectionPart;
	char pathPart[MAX_PART_SIZE]; 
	char stringToFind[MAX_PART_SIZE];

	if (!cpath)
		return 1;

	len = (int)strlen(cpath);
	if (len == 0)
		return 1;
	number = zpathnamePartPositions (cpath, strlen(cpath), positions, 7);
	//  If we don't have 6 parts, return to avoid an error
	if (number != 7)
		return 0;

	for (i=0; i<6; i++) {
		if (partAction[i]) {
			len = zpathnameGetPart (cpath, i+1, pathPart, sizeof(pathPart));
			if (partAction[i] == 1) {
				if (len != lengths[i]) {
					return 0;
				}
			}
			if (i == 0) {
				stringCopy (stringToFind, (size_t)sizeof(stringToFind), apart, (size_t)lengths[i]);
			}
			else if (i == 1) {
				stringCopy (stringToFind, sizeof(stringToFind), bpart, (size_t)lengths[i]);
			}
			else if (i == 2) {
				stringCopy (stringToFind, sizeof(stringToFind), cpart, (size_t)lengths[i]);
			}
			else if (i == 3) {
				stringCopy (stringToFind, sizeof(stringToFind), dpart, (size_t)lengths[i]);
			}
			else if (i == 4) {
				stringCopy (stringToFind, sizeof(stringToFind), epart, (size_t)lengths[i]);
			}
			else if (i == 5) {
				stringCopy (stringToFind, sizeof(stringToFind), fpart, (size_t)lengths[i]);
				if (boolCollection) {
					//  Are both F parts collections?
					//  i.e.,   .../C:123456|rest of F/
					//  If so, and boolCollection is true, then we are looking for all sequences for this collection
					//  If both are collections, change the collection sequence to "XXXXXX"
					//  so that both will match, regardless of the sequqnce
					if ((lengths[i] > 9) && (strlen(pathPart) > 9)) {
						i1 = toupper(stringToFind[0]);
						i2 = toupper(pathPart[0]);
						//  Look for "C:"
						if ((i1 == i2) && (i1 == 'C')) {
							if ((stringToFind[1] == ':') && (pathPart[1] == ':')) {
								boolCollectionPart = 0;
								for (j = 2; j < lengths[i]; j++) {
									if (stringToFind[j] == '|') {
										boolCollectionPart = 1;
									}
								}
								if (boolCollectionPart) {
									//  Now look for "|"
									boolCollectionPart = 0;
									for (j = 2; j < strlen(pathPart); j++) {
										if (pathPart[j] == '|') {
											boolCollectionPart = 1;
										}
									}
								}
								if (boolCollectionPart) {
									//  Yes!  change .../C:123456|rest of F/ to .../C:XXXXXX|rest of F/ for both
									for (j = 2; j < lengths[i]; j++) {
										if (stringToFind[j] == '|') {
											break;
										}
										else {
											stringToFind[j] = 'X';
										}
									}
									for (j = 2; j < strlen(pathPart); j++) {
										if (pathPart[j] == '|') {
											break;
										}
										else {
											pathPart[j] = 'X';
										}
									}
								}
							}
						}
					}
				}
			}
			if (partAction[i] == 1) {
				result = zstringCompare(pathPart, stringToFind, (size_t)lengths[i]);
				if (!result) {
					return 0;
				}
			}
			else if (partAction[i] == 2) {
				//  Starts With
				result = zstringCompare(pathPart, stringToFind, (size_t)lengths[i]);
				if (!result) {
					return 0;
				}
			}
			else if (partAction[i] == 3) {
				//  Ends With
				istart = len - lengths[i];
				result = zstringCompare(&pathPart[istart], stringToFind, (size_t)lengths[i]);
				if (!result) {
					return 0;
				}
			}
			else if (partAction[i] == 4) {
				//  contains
				result = zfindString(pathPart, len, stringToFind, lengths[i]);
				if (result < 0) {
					return 0;
				}
			}
		}
	}
	//   Matches
	return 1;
}

