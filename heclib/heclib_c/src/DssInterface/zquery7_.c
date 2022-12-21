#include <string.h>

#include "zdssKeys.h"
#include "zdssVals.h"
#include "zprogress.h"
#include "zdssMessages.h"
#include "heclib.h"


/**
*  Function:	zquery
*
*  Use:			Public
*
*  Description:	Query global parameters
*
*  Declaration: int zquery(const char* parameter,  char* charVal, size_t sizeofCharVal, int *integerValue);
*
*  Parameters:	const char* parameter:
*					The parameter to obtain the value for.
*
*				char* charVal (output)
*					Returns the character value associated with this parameter.
*
*				size_t sizeofCharVal:
*					The size of charVal, in bytes.
*
*				int *integerValue  (output)
*					Returns the integer value associated with this parameter.
*
*	Returns:	int status
*					STATUS_OKAY for successful operation.
*					STATUS_NOT_OKAY if parameter not recognized
*
*
*  See Also:	zinquir for parameters associated with an individual file.
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


//  Fortran compatible interface
void zquery7_(const char* parameter, char* charVal, int *integerVal, size_t lenParameter, size_t lenCharVal)
{
	int istat;
	char *param;
	char *cval;

	param = stringFortToC(parameter, lenParameter);
	cval = (char *)calloc((size_t)lenCharVal + 1, CHAR_SIZE);

	istat = zquery(parameter, cval, lenCharVal, integerVal);
	if (istat == -1) {
		*integerVal = -1;
		if (lenCharVal > 1) {
			stringFill(charVal, ' ', lenCharVal);
		}
	}
	else {
		stringCToFort(charVal, lenCharVal,  cval);
	}
	free(param);
	free(cval);
}

