#include "heclib7.h"
#include "hecdssInternal.h"
#include <string.h>
/*
Karl Tarbet  April 2020

appendStringToHeader is used to append strings (units,type,timezone) to
a header character array.

src  -- source string to append
dest -- destination(header) 
ipos -- position in dest to store  (incremented before function return)
destSize -- size of destination
*/
int appendStringToHeader(char* src, char* dest, int* ipos, int destSize)
{
	char blankString[9];

	if (src) {
		zcharTo8Byte(dest, src, ipos, destSize);
	}
	else {
		//  put in a blank word (7 bytes) if no units
		stringFill(blankString, ' ', sizeof(blankString) - 1);
		blankString[7] = '\0';
		zcharTo8Byte(dest, blankString, ipos, destSize);
	}
	if (*ipos < destSize-1)
		dest[*ipos] = '\0';
	(*ipos)++;
	return *ipos;
}


char* readStringFromHeader(char* carray, int *ipos, int size)
{
	char* rval = 0;
	if (*ipos > size - 1)
		return 0;
	int len = (int)strlen(carray);

	if (len <= 0)
		return 0;

	if (trimLength(carray)) {
		rval = mallocAndCopyTrim(carray);
	}
	*ipos = *ipos + len + 1;
	return rval;
}

/*
unitsHavePadding detects if units are padded for Solaris compatability

internalHeader -- header to investiage for padding
offsetToUnits  -- index to units in the internalHeader 
*/
int unitsHavePadding(int* internalHeader, int offsetToUnits) {
	char cstr[9];
	int hasPadding = 1;
	int i;
	if (internalHeader[offsetToUnits] != 0) {
		if (getEndian()) {
			charInt(&internalHeader[offsetToUnits - 1], cstr, 8, sizeof(cstr), 0, 0, 0);
		}
		else {
			charInt(&internalHeader[offsetToUnits], cstr, 4, sizeof(cstr), 0, 0, 0);
		}
		for (i = 0; i < 4; i++) {
			if (cstr[i] != ' ') hasPadding = 0;
		}
	}
	return hasPadding;
}