#include <string.h>
#include <stdio.h>

#include "hecdssInternal.h"

/**
*  Function:	intTo2char
*
*  Use:			Private
*
*  Description:	A small utility function to write an int into a 2 digit character string
*					with leading zeros, e.g., 3 converts to "03".
*
*  Declaration: void intTo2char(int i, char *c);
*
*  Parameters:	int i:  Integer to write as 2 char digit in c.
*
*				char *c:  The destination to write i into.  MUST be 3 character long.  No length checking is done.
*
*
*  Returns:		None
*
*  Remarks:		c will always be null terminated.
*
*  See Also:	intTo4char
*
*
*	Author:			Bill Charley
*	Date:			2013
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

void intTo2char(int i, char *c)
{
	_snprintf_s(c, 3, _TRUNCATE, "%02d", i);
	c[2] = '\0';
}

