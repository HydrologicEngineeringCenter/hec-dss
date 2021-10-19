#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "zdssMessages.h"
#include "heclib.h"


/**
*  Function:	ztextRetrieveUnit
*
*  Use:			Public
*
*  Description:	Retrieves a text record and then writes the contents to a Fortran file
*
*  Declaration: void ztextretrieveunit_(long long *ifltab, const char *pathname, int *ifortUnit,
*										int *numberLines, int *status, size_t lenPathname);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				const char *pathname
*					The pathname of the text record to retrieve
*
*				int *ifortUnit
*					The unit number of the Fortran file to write to
*
*				int *numberLines  (output)
*					The number of lines written to the file
*
*				int *status  (output)
*					The status of the operation, 0 if good, < 0 if error
*
*				size_t lenPathname
*					Length of the pathname, implictly passed by Fortran
*
*
*	See also:	ztextRetrieveToFile()
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*/



void ztextretrieveunit_(long long *ifltab, const char *pathname, int *ifortUnit,
					    int *numberLines, int *status, size_t lenPathname)
{
	int i;
	int len;
	int charCount;
	int one;
	char *path;
	zStructText *textStruct;

	path = stringFortToC(pathname, lenPathname);
	if (path == NULL) {
		//  Memory error
		*status = zerrorProcessing(ifltab, DSS_FUNCTION_ztextRetrieve_ID,
						zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, (int)lenPathname, 0,
						zdssErrorSeverity.MEMORY_ERROR, pathname,
						"Allocating String for pathname");
		return;
	}

	if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztextRetrieve_ID, "Enter ztextRetrieveUnit, Pathname: ", path);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztextRetrieve_ID, "File unit: ", *ifortUnit);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztextRetrieve_ID, " DSS Handle: ",  zhandle(ifltab));
	}

	textStruct = zstructTextNew(path);
	if (!textStruct) {
		*status =  zerrorProcessing(ifltab, DSS_FUNCTION_ztextRetrieve_ID,
						zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
						zdssErrorSeverity.MEMORY_ERROR, pathname,
						"Allocating textStruct");
		free(path);
		return;
	}

	*status = ztextRetrieve(ifltab, textStruct);
	if (zisError(*status)) {
		//  An error code
		*status = zerrorUpdate(ifltab, *status, DSS_FUNCTION_ztextRetrieve_ID);
		zstructFree(textStruct);
		free(path);
		return;
	}


	charCount = 0;
	*numberLines = 0;
	one = 1;
	for (i=0; i<textStruct->numberRows; i++) {
		len = (int)strlen(&textStruct->textTable[charCount]);
		*status = fortranwritelc_(ifortUnit, (const char*)&textStruct->textTable[charCount], &one, (size_t)len);
		charCount += len + 1;
		*numberLines += 1;
	}

	if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztextRetrieve_ID, "Exit ztextRetrieveToFile, Pathname: ", path);
	}
	*status = 0;

	free(path);
	zstructFree(textStruct);

	return;
}

