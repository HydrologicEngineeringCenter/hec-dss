#include "hecdssInternal.h"

/**
*  Function:	stringLastNonBlank
*
*  Description:	Finds the last non blank in a string (usually from Fortran), changes that
*					character to null ('\0') and returns the length
*
*  Declaration: int stringLastNonBlank (char *string, size_t len);
*
*  Parameters:	char *string
*					The character string to find the last non-blank in.
*
*				size_t stringSize
*					The size of the string (assuming blank filled)
*
*
*	Returns:	string length
*
*	Remarks:	Function usually used for printing a Fortran string in C, or
*				used for converting a Fortran string to C.
*
*
*	Author:			Bill Charley
*	Date:			2015
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int stringLastNonBlank (char *string, size_t stringSize)
{
	int i;
	int length;
	int end;

	length = (int)stringSize -1;
	for(i=length; i>=0; i--) {
		if (string[i] != ' ') {
			end = i + 1;
			if (end < (int)stringSize) {
				string[end] = '\0';
			}
			return i;
		}
	}
	//  All blanks?
	if (stringSize > 0) {
		string[0] = '\0';
	}
	return 0;
}

