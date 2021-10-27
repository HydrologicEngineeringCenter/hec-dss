#include <stdlib.h>
#include <string.h>

#include "hecdssInternal.h"

/**
*  Function:	strnlen_hec
*
*  Use:			Semi-private
*
*  Description:	 Get the length of a string, not going beyond number chacaters.
*					strnlen() is not a standard function, although it should be
*
*  Declaration: int strnlen_hec(char* str, int number)
*
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int strnlen_hec(const char* str, int number)
{
	int rval=0;
	if (number <= 0) return 0;
#ifndef __sun
	return (int)strnlen(str, (size_t)number);
#else
     rval = strlen(str);
	 if( rval > number)
	     rval = number; 
	 return rval;
//	return (int)(strlen(str) < number ? strlen(str) : number); 
#endif
}

