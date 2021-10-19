#include <string.h>

#include "hecdss7.h"

/**
*  Function:	zmessConcat1
*				zmessConcat2
*
*  Use:			Private
*
*  Description:	Simple utility functions to concatenate messages for error processing
*
*  Declaration: char *zmessConcat1(char *messIn, size_t sizeIn, const char *message)
*				char *zmessConcat2(char *messIn, size_t sizeIn, const char *message1, const char *message2)
*
*
*  Parameters:	char *messIn:  The string to append the message too.  This must be preallocated and null terminated
*				size_t sizeIn:  The size of messIn (not length).  Must be large enough to hold both messIn and messages and \n
*				const char *message:  The message to be appended to messIn
*				const char *message1:  The first of two messages to be appended to messIn
*				const char *message2:  Second part of message
*
*  Returns:		messIn with messages appended, null terminated.
*
*
*  Notes:		These functions do not allocated any space - Calling functions must
*				make sure that messIn has sufficient space to hold messages,
*				new line characters and null terminator.
*
*  See Also:	zerrorProcessing
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

char *zmessConcat1(char *messIn, size_t sizeIn, const char *message)
{
	int lenCombined;
	int lenMessage;
	int lenMessIn;
	char *messOut;

	lenMessage = (int)strlen(message);
	lenMessIn = (int)strlen(messIn);
	lenCombined = lenMessIn + lenMessage + 1;	//  size - includes null terminatior

	messOut = messIn;

	if (lenMessIn >= (int)sizeIn) {
		return messOut;
	}

	if (lenCombined > (int)sizeIn) lenCombined = (int)sizeIn;

	if (lenMessIn > 0)  {
		lenCombined++;  //  For new line at beginning of message
		stringCat(messOut, (size_t)lenCombined, "\n", _TRUNCATE);
	}
	stringCat(messOut, (size_t)lenCombined, message, _TRUNCATE);

	return messOut;
}

/*
messIn must be predefined space!
*/

char *zmessConcat2(char *messIn, size_t sizeIn, const char *message1, const char *message2)
{
	int len;
	int lenIn;
	char *messOut;

	//  The last "1" is for new line char
	len = (int)strlen(message1) + (int)strlen(message2) + 1;
	lenIn = (int)strlen(messIn);
	len += lenIn;
	if (lenIn > 0) {
		len++;  //  For new line
	}
	if (len > (int)sizeIn) {
		len = (int)sizeIn;
	}

	messOut = messIn;
	if (lenIn > 0) {
		stringCat(messOut, (size_t)len, "\n", _TRUNCATE);
	}
	stringCat(messOut, (size_t)len, message1, _TRUNCATE);
	stringCat(messOut, (size_t)len, message2, _TRUNCATE);

	return messOut;
}

