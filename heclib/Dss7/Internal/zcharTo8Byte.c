#include <string.h>
#include <stdlib.h>

#include "heclib7.h"
#include "hecdssInternal.h"


/**
*  Function:	ztsInternalHeaderPack
*
*  Use:			Private
*
*  Description:	Copies a character string into a character array, making sure string fits fully in
*					8 byte words.  Trims blanks, then blank fills any trailing bytes.  For example, 
*					"cfs" gets copied as "cfs____\0" (where _ means a blank character)
*					and "cfs       " also gets copied as "cfs____\0".
*
*  Declaration: int zcharTo8Byte(char *carray, char *itemToCopy, int *ipos, int carraySize) ;
*
*  Parameters:	char *carray
*					The char array that the string will be copied too.
*
*				char *itemToCopy
*					The string to copy (e.g., "cfs")
*
*				int *ipos
*					The character position to start the copy (in carray) at.  This is returned
*					updated with the position for the next string, but will be on 8 byte boundaries.
*
*				int carraySize
*					The size of carray, in bytes
*				
*
*	Returns:	int numberInts 
*					The number of ints (used) in the header (but on 8-byte boundary,
*					so this number will always be even.)
*	
*	Remarks:	The position for the next string.

*
*	Author:			Bill Charley
*	Date:			2018
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zcharTo8Byte(char *carray, char *itemToCopy, int *ipos, int carraySize) 
{
	int i;
	int len;
	int numb;
	int jlen;
	char *cstr;


	len = (int)trimLength(itemToCopy);
	//  On 8-byte boundary?
	numb = numberLongsInBytes(len + 1);
	jlen = numb * 8;
	if (jlen == (len + 1)) {
		//  On 8-byte boundary.  Good to go
		stringCopy(&carray[*ipos], (size_t)carraySize - *ipos, itemToCopy, (size_t)len);
		*ipos += len;
	}
	else {
		//  Not on 8-byte boundary.  Pad with blanks, leaving room for null char
		//  e.g., "inst-val" goes to inst -val  ____ ___0
		//		   12345678          1234 5678  ____ ___0
		cstr = calloc(jlen, 1);
		stringCopy(cstr, (size_t)jlen, itemToCopy, (size_t)len);
		jlen--;
		//  Blank pad 
		for (i = len; i < jlen; i++) {
			cstr[i] = ' ';
		}
		stringCopy(&carray[*ipos], (size_t)carraySize - *ipos, cstr, (size_t)jlen);
		*ipos += jlen;
		free(cstr);
	}
	return *ipos;
}

