#include <string.h>

#include "hecdssInternal.h"


/**
*  Function:	stringCToFort
*
*  Use:			Public
*
*  Description:	A function to convert a C string into Fortran.
*					Fortran doesn't use char(0) to terminate, but knows it's length,
*					So this blank fills characters past the null.
*					Use when returning a C string to Fortran.
*
*  Declaration: void stringCToFort(char *fortString, size_t maxFortLen, const char *cfromString);
*
*  Parameters:	char* fortString:
*					Character string from Fortran to copy into
*
*				size_t maxFortLen:
*					The size of the fortString.
*
*				const char *cfromString
*					The C string to copy from
*
*
*  Returns:		Nothing
*
*	Remarks:	For returning a C string to Fortran.
*					Generally, when sending, a "const char*" C string to Fortran, the string can be passed directly,
*					with strlen() passed at the end as the length.
*					However, a "char*" C string needs to go through this process also to allow for correct return.
*
*
*	Author:			Bill Charley
*	Date:			2013
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

void stringCToFort(char *fortString, size_t maxFortLen, const char *cfromString)
{
	int len;
	int i;
	int fromStringLen;
	int boolFoundNull;

	if (!cfromString) {
		for (i=0; i<(int)maxFortLen; i++) {
			fortString[i] = ' ';
		}
		return;
	}

	fromStringLen = (int)strlen(cfromString);
	// len = min((int)maxFortLen, fromStringLen);
	len =(int)maxFortLen;
	if (len > fromStringLen) len = fromStringLen;

	//  Don't use strncpy, as it has fits when it cannot null terminate
	//  Make sure string is not null terminated and has no nulls
	boolFoundNull = 0;
	for (i=0; i<len; i++) {
		if (boolFoundNull) {
			fortString[i] = ' ';
		}
		else {
			if (cfromString[i] == '\0') {
				boolFoundNull = 1;
				fortString[i] = ' ';
			}
			else {
				fortString[i] = cfromString[i];
			}
		}
	}

	//  Blank fill any remainder
	if (len < (int)maxFortLen) {
		for (i=len; i<(int)maxFortLen; i++) {
			fortString[i] = ' ';
		}
	}
}

