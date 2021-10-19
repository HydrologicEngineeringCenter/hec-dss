#include <string.h>

#include "heclib7.h"


/**
*  Function:	trimLength
*				trimLengthLen
*
*  Use:			Public
*
*  Description:	Returns the length of the last non-blank character of a string
*
*  Declaration: size_t trimLength(const char *string);
*				size_t trimLengthLen(const char *string, size_t len)
*
*  Parameters:	const char *string
*					The character string to find the last non-blank character in (exclusive of '\0')
*
*				size_t len
*					The length of the string to start scanning backwards from.  This is useful is the
*					string is not necessarily null terminated or you know where the blanks start.
*
*
*	Returns:	size_t len
*					The length to the last non-blank character.
*
*   Note:		Returns the number of characters, not the position or index.  (pos = len - 1)
*
*	Examples:	len = trimLength(" AB C DE     ");  //  len = 8, length to of "E"
*				len = trimLengthLen(" AB C DE     ", 9);  //  len = 8
*				len = trimLengthLen(" AB C DE     ", 5);  //  len = 5, length to of "C"
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


size_t trimLength(const char *string)
{
	return trimLengthLen(string, strlen(string));
}
size_t trimLengthLen(const char *string, size_t len)
{
	int i;
	int ilen;

	ilen = (int)len;
	for (i=ilen-1; i>=0; i--) {
		if (string[i] != ' ') {
			return (size_t)(i+1);
		}
	}
	return (size_t)0;
}

