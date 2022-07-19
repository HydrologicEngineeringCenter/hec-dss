#include <ctype.h>

#include "hecdssInternal.h"

/**
*  Function:	upperCase
*
*  Use:			Public
*
*  Description:	 A small utility function that converts a string to upper case
*					string Must be null terminated
*
*  Declaration: void upperCase(char *string)
*
*  Parameters:	char *string - string to convert to upper case
*
*  Returns:		None
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

void upcase_(char* string, size_t len) {

	//	printf("\nupcase_(%s,%d)", string, len);
	int i = 0;
	while (string[i] != '\0' && i < len) {
		string[i] = toupper(string[i]);
		i++;
	}
}

void upperCase(char *string)
{
	int i;

	i = 0;
	while(string[i] != '\0') {
		string[i] = toupper(string[i]);
		i++;
	}
}

