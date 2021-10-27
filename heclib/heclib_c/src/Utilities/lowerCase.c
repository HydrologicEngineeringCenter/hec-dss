#include <ctype.h>

#include "hecdssInternal.h"

/**
*  Function:	lowerCase
*
*  Use:			Public
*
*  Description:	 A small utility function that converts a strint to lower case
*					string Must be null terminated
*
*  Declaration: void lowerCase(char *string)
*
*  Parameters:	char *string - string to convert to lower case
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


void lowerCase(char *string)
{
	int i;

	i = 0;
	while(string[i] != '\0') {
		string[i] = tolower(string[i]);
		i++;
	}
}

