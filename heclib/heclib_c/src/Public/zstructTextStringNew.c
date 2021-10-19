#include <stdlib.h>
#include <string.h>

#include "heclib7.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"

/**
*  Function:	zstructTextStringNew
*
*  Use:			Public
*
*  Description:	Creates a new text struct with a character string for storing text data.
*					This struct is used for storing a single null terminated string.
*					Use zstructTextNew for storing text tables and other text types.
*
*  Declaration: zStructText* zstructTextStringNew(const char* pathname, char *text);
*
*  Parameters:	const char* pathname
*					The pathname of the record to store.
*
*				char *text
*					The null terminated character string to store.  Does not make a copy.
*
*	Returns:	zStructText*
*					An address to the struct for use with ztextStore().
*
*   Note:		ALWAYS call function zstructFree(struct) when finished using.
*
*	Remarks:	You must not reuse this struct.  Make a new one for every dataset.
*
*	See:		ztextStore for use and definition
*
*	See Also:	zstructTextStringNew()
*				ztextStore()
*				ztextRetrieve()
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

zStructText* zstructTextStringNew(const char* pathname, char *text)
{
	zStructText *textStruct;

	textStruct = zstructTextNew(pathname);

	//  Be sure null terminator is included in number of characters!
	textStruct->textString = text;
	textStruct->numberTextChars = (int)strlen(text)+1;

	return textStruct;
}

