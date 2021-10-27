#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "zdssMessages.h"
#include "heclib.h"


/**
*  Function:	ztextRetrieveToFile
*
*  Use:			Public
*
*  Description:	Retrieves a text record and then writes the contents to a file
*
*  Declaration: int ztextRetrieveToFile(long long *ifltab, const char *pathname, const char *filename);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				const char *pathname
*					The pathname of the text record to retrieve
*
*				const char *filename
*					The full name of the file to write the text output to.  It does not have to exist.
*
*
*	See also:	ztextStructPrint()
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*/

int ztextRetrieveToFile(long long *ifltab, const char *pathname, const char *filename)
{

	int i;
	int len;
	int charCount;
	int status;
	zStructText *textStruct;
	FILE *textFileFP;


	if (!pathname) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztextRetrieve_ID, zdssErrorCodes.NULL_PATHNAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "");
	}
	if (!filename) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztextRetrieve_ID, zdssErrorCodes.NULL_FILENAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "");
	}

	if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztextRetrieve_ID, "Enter ztextRetrieveToFile, Pathname: ", pathname);
		zmessageDebug(ifltab, DSS_FUNCTION_ztextRetrieve_ID, "File name: ", filename);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztextRetrieve_ID, " DSS Handle: ",  zhandle(ifltab));
	}

	textStruct = zstructTextNew(pathname);
	if (!textStruct) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztextRetrieve_ID,
					zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
					zdssErrorSeverity.MEMORY_ERROR, pathname, "Allocating textStruct");
	}

	status = ztextRetrieve(ifltab, textStruct);
	if (zisError(status)) {
		//  An error code
		status = zerrorUpdate(ifltab, status, DSS_FUNCTION_ztextRetrieve_ID);
		zstructFree(textStruct);
		return status;
	}

#ifdef _MSC_VER
	status = fopen_s(&textFileFP, filename, "w");
#else
	textFileFP = fopen(filename, "w");
	if (textFileFP) {
		status = 0;
	}
	else {
		status = -1;
	}
#endif
	if (status != 0) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztextRetrieve_ID,
					zdssErrorCodes.UNABLE_TO_WRITE_FILE, status, 0,
					zdssErrorSeverity.WARNING, pathname, filename);
	}

	//  For big endian, text is swapped at read
	if (textStruct->numberTextChars > 0) {
		len = (int)strlen(textStruct->textString);
		fwrite(textStruct->textString, sizeof(char), (size_t)len, textFileFP);
	}

	if (textStruct->numberTableChars > 0) {
		charCount = 0;
		for (i = 0; i < textStruct->numberRows; i++) {
			len = (int)strlen(&textStruct->textTable[charCount]);
			fwrite((const char*)&textStruct->textTable[charCount], sizeof(char), (size_t)len, textFileFP);
			charCount += len + 1;
		}
	}

	fclose(textFileFP);

	if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztextRetrieve_ID, "Exit ztextRetrieveToFile, Pathname: ", pathname);
		zmessageDebug(ifltab, DSS_FUNCTION_ztextRetrieve_ID, "File name: ", filename);
	}
	zstructFree(textStruct);

	return status;
}

