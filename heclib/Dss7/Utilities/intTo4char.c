#include <string.h>
#include <stdio.h>

#include "hecdssInternal.h"

/**
*  Function:	intTo4char
*
*  Use:			Private
*
*  Description:	A small utility function to write an int into a 4 digit character string
*					with leading zeros, e.g., 900 converts to "0900".
*
*  Declaration: void intTo4char(int i, char *c);
*
*  Parameters:	int i:  Integer to write as 4 char digit in c.
*
*				char *c:  The destination to write i into.  MUST be 5 character long.  No length checking is done.
*
*
*  Returns:		None
*
*  Remarks:		c will always be null terminated.
*
*  See Also:	intTo2char
*
*
*	Author:			Bill Charley
*	Date:			2013
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

void intTo4char(int i, char *c)
{
	_snprintf_s(c, 5, _TRUNCATE, "%04d", i);
	c[4] = '\0';
}

